use std::collections::{HashMap, HashSet};

use wast::{
    Wat,
    core::{
        Expression, Func, FuncKind, FunctionType, ImportItems, InlineExport, InnerTypeKind,
        Instruction, ItemKind, Local, ModuleField, ModuleKind, TypeUse, ValType,
    },
    token::{Id, Index, NameAnnotation, Span},
};

#[derive(Debug)]
pub enum FunctionsToStub {
    All,
    Some(HashSet<String>),
}

#[derive(Debug)]
pub struct ShouldStub {
    pub modules: HashMap<String, FunctionsToStub>,
}
impl Default for ShouldStub {
    fn default() -> Self {
        Self {
            modules: [(String::from("wasi_snapshot_preview1"), FunctionsToStub::All)]
                .into_iter()
                .collect(),
        }
    }
}
impl ShouldStub {
    fn should_stub(&self, module: &str, function: &str) -> bool {
        if let Some(functions) = self.modules.get(module) {
            match functions {
                FunctionsToStub::All => true,
                FunctionsToStub::Some(functions) => functions.contains(function),
            }
        } else {
            false
        }
    }
}

enum ImportIndex {
    ToStub(u32),
    Keep(u32),
}

/// The signature of a function to be stubbed.
struct ToStub<'a> {
    /// The span of the original import field.
    span: Span,
    /// The type of the function, with parameters and results.
    ty: TypeUse<'a, FunctionType<'a>>,
    /// The identifier used during name resolution to refer to the function from the rest of the module.
    id: Option<Id<'a>>,
    /// The name of the function.
    name: Option<NameAnnotation<'a>>,
    /// The parameters of the function.
    locals: Vec<Local<'a>>,
    /// The results types of the function.
    results: Vec<ValType<'a>>,
}

pub fn stub_wasi_functions(
    binary: &[u8],
    should_stub: ShouldStub,
    return_value: u32,
) -> crate::Result<Vec<u8>> {
    let wat = wasmprinter::print_bytes(binary).map_err(std::io::Error::other)?;
    let parse_buffer = wast::parser::ParseBuffer::new(&wat)?;

    let mut wat: Wat = wast::parser::parse(&parse_buffer)?;
    let module = match &mut wat {
        Wat::Module(m) => m,
        Wat::Component(_) => return Err(Error::message("components are not supported")),
    };
    let fields = match &mut module.kind {
        ModuleKind::Text(f) => f,
        ModuleKind::Binary(_) => {
            println!("[WARNING] binary directives are not supported");
            return Ok(binary.to_owned());
        }
    };

    // Three edits are needed to stub a function:
    // - Remove the corresponding import field.
    // - Define a stub function of the same type.
    // - Update references to the original function.
    //
    // We achieve it in two for-loops:
    // 1. Scan functions to be stubbed, replace their imports with stub functions, and update references to the original functions.
    // 2. Move stub functions to the place immediately before other functions, so that all functions are adjacent to each other.

    // Types defined in the module
    let mut types = Vec::new();
    // Imports to be kept; only the number of them matters, so the actual import fields are not stored
    let mut num_kept = 0;
    // Imports to be stubbed, represented as the indices of their import fields in the original module
    let mut to_stub = Vec::new();
    // The place to insert definitions of stubs
    let mut insert_stubs_index = None;
    // Imports after stubbing
    // `new_import_indices[n]` represents the n-th import field in the original module.
    // If that import was kept, then `new_import_indices[n]` means its index among all kept imports.
    // If that import was stubbed, then `new_import_indices[n]` is its index in `to_stub`.
    let mut new_import_indices = Vec::new();

    for (field_idx, field) in fields.iter_mut().enumerate() {
        // Assuming `ModuleField`s are iterated in order: Type → Import → … → Func → …
        match field {
            ModuleField::Type(t) => types.push(t),
            ModuleField::Import(i) => {
                // If the import is a function, extract its module, name, signature, and type (as index in `types`).
                let info = (match &i.items {
                    ImportItems::Single { module, name, sig } => Some((*module, *name, sig)),
                    ImportItems::Group1 { .. } | ImportItems::Group2 { .. } => {
                        println!("[WARNING] Stubbing compact import groups is not yet supported");
                        None
                    }
                })
                .and_then(|(module, name, sig)| {
                    if let ItemKind::Func(typ) = &sig.kind
                        && let Some(Index::Num(index, _)) = typ.index
                    {
                        Some((module, name, sig, index as usize))
                    } else {
                        None
                    }
                });

                // Push the import to either `to_stub` or `0..num_kept`, and save the new index
                let new_index = match info {
                    Some((module, name, sig, type_index))
                        if should_stub.should_stub(module, name) =>
                    {
                        println!("Stubbing function {}::{}", module, name);
                        let typ = &types[type_index];
                        let ty = TypeUse::new_with_index(Index::Num(type_index as u32, typ.span));
                        let wast::core::TypeDef {
                            kind: InnerTypeKind::Func(func_typ),
                            ..
                        } = &typ.def
                        else {
                            continue;
                        };

                        to_stub.push(field_idx);

                        // Replace the import field with a stub function. We will move the stub to the end of other imports later.
                        *field = ModuleField::Func(
                            ToStub::build(i.span, sig, ty, func_typ)
                                .try_into_func_with(return_value)?,
                        );

                        ImportIndex::ToStub(to_stub.len() as u32 - 1)
                    }
                    _ => {
                        num_kept += 1;
                        ImportIndex::Keep(num_kept - 1)
                    }
                };
                // Save the import to `new_import_indices`
                new_import_indices.push(new_index);
            }
            ModuleField::Func(func) => {
                // Determine `insert_stubs_index`
                if insert_stubs_index.is_none() {
                    insert_stubs_index = Some(field_idx);
                }
                // Update references to functions in `to_stub`
                match &mut func.kind {
                    FuncKind::Import(f, ..) => {
                        if should_stub.should_stub(f.module, f.field) {
                            println!("[WARNING] Stubbing inline function is not yet supported");
                            println!(
                                "[WARNING] ignoring inline function \"{}\" \"{}\"",
                                f.module, f.field
                            );
                        }
                    }
                    FuncKind::Inline { expression, .. } => {
                        for inst in expression.instrs.as_mut().iter_mut() {
                            match inst {
                                Instruction::ref_func(Index::Num(index, _))
                                | Instruction::return_call(Index::Num(index, _))
                                | Instruction::call(Index::Num(index, _)) => {
                                    if let Some(new_index) = new_import_indices.get(*index as usize)
                                    {
                                        // The relationship between the old `index` and the `new_index` is guaranteed by the moves below.
                                        *index = match new_index {
                                            ImportIndex::ToStub(idx) => *idx + num_kept,
                                            ImportIndex::Keep(idx) => *idx,
                                        };
                                    }
                                }
                                _ => {}
                            }
                        }
                    }
                }
            }
            _ => {}
        }
    }
    drop(types);

    let insert_stubs_index = insert_stubs_index
        .expect("This is weird: there are no code sections in this Wasm executable!");

    for (num_already_moved, old_index) in to_stub.into_iter().enumerate() {
        // Before this for-loop, we have put the stubs among the kept imports.
        // Now we move the stubs to the place immediately before other functions.
        //
        // Suppose we are moving `stub_c` in this iteration, then the layouts of `fields` are as follows.
        //
        //                                                 ↓ old_index
        // - The original module:    import_a, import_b, import_c, import_d,   ……     func_e, func_f
        // - Before this for-loop:   import_a,  stub_b,  [stub_c], import_d,   ……     func_e, func_f
        // - Before this iteration:  import_a, [stub_c], import_d,    ……     stub_b,  func_e, func_f
        // - After `fields.remove`:  import_a, import_d,    ……      stub_b,  func_e,  func_f
        // - After `fields.insert`:  import_a, import_d,    ……      stub_b, [stub_c], func_e, func_f
        //                                       ↑ old_index - num_already_moved        ↑ insert_stubs_index
        let stub = fields.remove(old_index - num_already_moved);
        assert!(
            matches!(stub, ModuleField::Func(_)),
            "The import field {stub:?} was not stubbed as planned. This is a bug of wasi-stub. Please report an issue.",
        );
        fields.insert(insert_stubs_index - 1, stub);
    }

    Ok(module.encode()?)
}

impl<'a> ToStub<'a> {
    fn build(
        span: Span,
        sig: &wast::core::ItemSig<'a>,
        ty: TypeUse<'a, FunctionType<'a>>,
        func_typ: &FunctionType<'a>,
    ) -> Self {
        ToStub {
            span,
            ty,
            id: sig.id,
            name: sig.name,
            locals: func_typ
                .params
                .iter()
                .map(|&(id, name, ty)| Local { id, name, ty })
                .collect(),
            results: func_typ.results.to_vec(),
        }
    }

    /// Convert the function signature `ToStub` into a stub function with the given return value.
    ///
    /// We use a weird return value. Hopefully this makes it easier to track usage of the stubbed function.
    fn try_into_func_with(self, return_value: u32) -> Result<Func<'a>> {
        let instructions = self
            .results
            .iter()
            .map(|val_type| {
                // Return the weird value as the expected type.
                let instruction = match val_type {
                    ValType::I32 => Instruction::i32_const(return_value as i32),
                    ValType::I64 => Instruction::i64_const(return_value as i64),
                    ValType::F32 => Instruction::f32_const(wast::token::F32 {
                        bits: (return_value as f32).to_bits(),
                    }),
                    ValType::F64 => Instruction::f64_const(wast::token::F64 {
                        bits: (return_value as f64).to_bits(),
                    }),
                    _ => {
                        return Err(Error::message(format!(
                            "Unsupported stub return type {:?} for function {:?}",
                            val_type, self.name
                        )));
                    }
                };
                Ok(instruction)
            })
            .collect::<Result<Vec<_>>>()?;
        Ok(Func {
            span: self.span,
            ty: self.ty,
            id: self.id,
            name: self.name,
            // no exports
            exports: InlineExport { names: Vec::new() },
            kind: wast::core::FuncKind::Inline {
                locals: self.locals.into_boxed_slice(),
                expression: Expression {
                    instrs: instructions.into_boxed_slice(),
                    branch_hints: Box::new([]),
                    instr_spans: None,
                },
            },
        })
    }
}

// Error handling
pub struct Error(Box<dyn std::error::Error + Send + Sync + 'static>);
impl Error {
    pub fn message(reason: impl AsRef<str>) -> Self {
        Self(Box::new(std::io::Error::other(reason.as_ref().to_string())))
    }
}
impl std::fmt::Debug for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        std::fmt::Display::fmt(&self.0, f)
    }
}
impl<E> From<E> for Error
where
    E: std::error::Error + Send + Sync + 'static,
{
    fn from(err: E) -> Self {
        Self(Box::new(err))
    }
}
pub type Result<T> = std::result::Result<T, Error>;
