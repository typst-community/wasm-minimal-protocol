# MoonBit wasm plugin example

This is a bare-bones Typst plugin, written in [MoonBit](https://www.moonbitlang.com/).

## Compile

To compile this example, you need the [MoonBit toolchain](https://www.moonbitlang.com/download/). Install it in Linux & macOS with:

```sh
curl -fsSL https://cli.moonbitlang.com/install/unix.sh | bash
```

Windows users can install it with:

```sh
Set-ExecutionPolicy RemoteSigned -Scope CurrentUser; irm https://cli.moonbitlang.com/install/powershell.ps1 | iex
```

After installing, `moon` should be available on your PATH. Then, build the project for the `wasm` (linear-memory) target:

```sh
moon build --target wasm --release
cp _build/wasm/release/build/hello.wasm .
```

## Build with Typst

Simply run `typst compile hello.typ`, and observe that it works!

## Notes on the implementation

### Protocol FFI

MoonBit's `wasm` target uses linear memory. For FFI imports, only **primitive types** (`Int`, `UInt`, `Int64`, `Float`, `Double`, `Bool`) are accepted as parameter types — using `FixedArray[Byte]` directly in `fn = "module" "func"` declarations is a compile error ([4042] Invalid stub type).

The two Typst protocol functions are therefore declared with `Int` (i32) pointer parameters:

```moonbit
fn send_result_to_host_raw(ptr : Int, len : Int) = "typst_env" "wasm_minimal_protocol_send_result_to_host"
fn write_args_to_buffer_raw(ptr : Int) = "typst_env" "wasm_minimal_protocol_write_args_to_buffer"
```

### Getting the data pointer

`FixedArray[Byte]` and `Bytes` share the same linear-memory layout:

```
heap_ptr + 0 ..  3 : refcount (i32)
heap_ptr + 4 ..  7 : packed length + type info (i32)
heap_ptr + 8 .. +N : actual byte data
```

The data pointer is therefore `heap_ptr + 8`. A single `extern "wasm"` helper works for both types — `FixedArray[Byte]` is converted to `Bytes` via a zero-cost `"%identity"` cast:

```moonbit
#borrow(bs)
extern "wasm" fn data_ptr(bs : Bytes) -> Int =
  #|(func (param i32) (result i32)
  #|  local.get 0
  #|  i32.const 8
  #|  i32.add)

fn to_bytes(arr : FixedArray[Byte]) -> Bytes = "%identity"
```

Static byte strings use `b"..."` literals, which are stored as constants in the wasm data section (no heap allocation):

```moonbit
pub fn hello() -> Int {
  send_result(b"Hello from wasm!!!")
  0
}
```

### Preventing early decref

MoonBit's optimizer may schedule a reference count decrement immediately after the last visible use of a value. Without precaution, the buffer could be freed *before* the host finishes reading or writing it.

A dummy `let _ = bs.length()` placed *after* the host call prevents this by giving the compiler a later visible use of `bs`:

```moonbit
fn send_result(bs : Bytes) -> Unit {
  send_result_to_host_raw(data_ptr(bs), bs.length())
  let _ = bs.length() // keep bs alive until after the host read
}
```

### Avoiding unexpected imports

MoonBit's `wasm` target imports `spectest.print_char` when printing (e.g., via `println` or `panic`). Because Typst does not provide this function, the example deliberately avoids any printing. For `will_panic`, an inline `unreachable` WAT instruction is used instead:

```moonbit
extern "wasm" fn wasm_trap() -> Int =
  #|(func (result i32) unreachable)
```

