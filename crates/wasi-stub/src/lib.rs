use std::collections::{HashMap, HashSet};

use wast::{
    Wat,
    core::{
        Expression, Func, FuncKind, FunctionType, HeapType, ImportItems, InlineExport,
        InnerTypeKind, Instruction, ItemKind, Local, ModuleField, ModuleKind, RefType, TypeUse,
        ValType,
    },
    token::{Id, Index, NameAnnotation},
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

enum ImportIndex {
    ToStub(u32),
    Keep(u32),
}

/// A function to be stubbed.
struct ToStub {
    /// The index of the original import field in the module.
    fields_index: usize,
    /// The span of the original import field.
    span: wast::token::Span,
    /// The results types of the function.
    results: Vec<ValType<'static>>,
    /// The type of the function, with parameters and results.
    ty: TypeUse<'static, FunctionType<'static>>,
    /// The name of the function.
    name: Option<NameAnnotation<'static>>,
    /// The identifier used during name resolution to refer to the function from the rest of the module.
    id: Option<Id<'static>>,
    /// The parameters of the function.
    locals: Vec<Local<'static>>,
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

/// Make an `Id` static
fn static_id(id: Option<Id>) -> Option<Id<'static>> {
    id.map(|id| {
        let mut name = id.name().to_owned();
        name.insert(0, '$');
        let parser = Box::leak(Box::new(
            wast::parser::ParseBuffer::new(name.leak()).unwrap(),
        ));
        wast::parser::parse::<Id>(parser).unwrap()
    })
}
/// Make a `NameAnnotation` static
fn static_name_annotation(name: Option<NameAnnotation>) -> Option<NameAnnotation<'static>> {
    name.map(|name| NameAnnotation {
        name: String::from(name.name).leak(),
    })
}
/// Make a `ValType` static
fn static_val_type(val_type: &ValType) -> ValType<'static> {
    // FIXME: This long match dance is _only_ to make the lifetime of ty 'static. A lot of things have to go through this dance (see the `static_*` function...)
    // Instead, we should write the new function here, in place, by replacing `field`. This is currently done in the for loop at the very end of the function `stub_wasi_functions`.
    // THEN, at the end of the loop, swap every function in its right place. No need to do more!
    match val_type {
        ValType::I32 => ValType::I32,
        ValType::I64 => ValType::I64,
        ValType::F32 => ValType::F32,
        ValType::F64 => ValType::F64,
        ValType::V128 => ValType::V128,
        ValType::Ref(r) => ValType::Ref(RefType {
            nullable: r.nullable,
            heap: match r.heap {
                HeapType::Concrete(index) => HeapType::Concrete(match index {
                    Index::Num(n, s) => Index::Num(n, s),
                    Index::Id(id) => Index::Id(static_id(Some(id)).unwrap()),
                }),
                HeapType::Exact(index) => HeapType::Exact(match index {
                    Index::Num(n, s) => Index::Num(n, s),
                    Index::Id(id) => Index::Id(static_id(Some(id)).unwrap()),
                }),
                HeapType::Abstract { shared, ty } => HeapType::Abstract { shared, ty },
            },
        }),
    }
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
    // 1. Scan functions to be stubbed, and update references to them.
    // 2. For each function to be stubbed, swap its import field with the definition of its stub.

    // Types defined in the module
    let mut types = Vec::new();
    // Imports to be kept
    let mut kept = Vec::new();
    // Imports to be stubbed
    let mut to_stub = Vec::new();
    // The place to insert definitions of stubs
    let mut insert_stubs_index = None;
    // Imports after stubbing, represented as indices in `to_stub` or `kept`
    let mut new_import_indices = Vec::new();

    for (field_idx, field) in fields.iter_mut().enumerate() {
        // Assuming `ModuleField`s are iterated in order: Type → Import → … → Func → …
        match field {
            ModuleField::Type(t) => types.push(t),
            ModuleField::Import(i) => {
                let (module, field, sig) = match &i.items {
                    ImportItems::Single { module, name, sig } => (*module, *name, sig),
                    _ => {
                        println!("[WARNING] Stubbing compact import groups are not yet supported");
                        continue;
                    }
                };

                let typ = match &sig.kind {
                    ItemKind::Func(typ) => typ.index.and_then(|index| match index {
                        Index::Num(index, _) => Some(index as usize),
                        Index::Id(_) => None,
                    }),
                    _ => None,
                };
                // Push the import to either `to_stub` or `kept`
                let new_index = match typ {
                    Some(type_index) if should_stub.should_stub(module, field) => {
                        println!("Stubbing function {}::{}", module, field);
                        let typ = &types[type_index];
                        let ty = TypeUse::new_with_index(Index::Num(type_index as u32, typ.span));
                        let wast::core::TypeDef {
                            kind: InnerTypeKind::Func(func_typ),
                            ..
                        } = &typ.def
                        else {
                            continue;
                        };
                        let id = static_id(sig.id);
                        let locals: Vec<Local> = func_typ
                            .params
                            .iter()
                            .map(|(id, name, val_type)| Local {
                                id: static_id(*id),
                                name: static_name_annotation(*name),
                                ty: static_val_type(val_type),
                            })
                            .collect();
                        let results: Vec<_> =
                            func_typ.results.iter().map(static_val_type).collect();
                        to_stub.push(ToStub {
                            fields_index: field_idx,
                            span: i.span,
                            results,
                            ty,
                            name: sig.name.map(|n| NameAnnotation {
                                name: n.name.to_owned().leak(),
                            }),
                            id,
                            locals,
                        });
                        ImportIndex::ToStub(to_stub.len() as u32 - 1)
                    }
                    _ => {
                        kept.push(i);
                        ImportIndex::Keep(kept.len() as u32 - 1)
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
                                Instruction::RefFunc(Index::Num(index, _))
                                | Instruction::ReturnCall(Index::Num(index, _))
                                | Instruction::Call(Index::Num(index, _)) => {
                                    if let Some(new_index) = new_import_indices.get(*index as usize)
                                    {
                                        *index = match new_index {
                                            ImportIndex::ToStub(idx) => *idx + kept.len() as u32,
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
    drop(kept);
    drop(types);

    let insert_stubs_index = insert_stubs_index
        .expect("This is weird: there are no code sections in this wasm executable !");

    for (
        already_stubbed,
        ToStub {
            fields_index,
            span,
            results,
            ty,
            name,
            id,
            locals,
        },
    ) in to_stub.into_iter().enumerate()
    {
        // Define the stubbed function
        let instructions = results
            .iter()
            .map(|val_type| {
                // Return weird values as the expected types, hopefully this makes it easier to track usage of these stubbed functions.
                let instruction = match val_type {
                    ValType::I32 => Instruction::I32Const(return_value as i32),
                    ValType::I64 => Instruction::I64Const(return_value as i64),
                    ValType::F32 => Instruction::F32Const(wast::token::F32 {
                        bits: (return_value as f32).to_bits(),
                    }),
                    ValType::F64 => Instruction::F64Const(wast::token::F64 {
                        bits: (return_value as f64).to_bits(),
                    }),
                    _ => {
                        return Err(Error::message(format!(
                            "Unsupported stub return type {:?} for function {:?}",
                            val_type, name
                        )));
                    }
                };
                Ok(instruction)
            })
            .collect::<Result<Vec<_>>>()?;
        let function = Func {
            span,
            id,
            name,
            // no exports
            exports: InlineExport { names: Vec::new() },
            kind: wast::core::FuncKind::Inline {
                locals: locals.into_boxed_slice(),
                expression: Expression {
                    instrs: instructions.into_boxed_slice(),
                    branch_hints: Box::new([]),
                    instr_spans: None,
                },
            },
            ty,
        };
        // Swap the import field with the stubbed `function`.
        // - Before: import_a, [import_b], import_c, func_d, func_e
        // - After:  import_a,  import_c,  [func_b], func_d, func_e
        //                                    ↑ insert_stubs_index
        //                        ↑ fields_index - already_stubbed
        fields.insert(insert_stubs_index, ModuleField::Func(function));
        fields.remove(fields_index - already_stubbed);
    }

    Ok(module.encode()?)
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
