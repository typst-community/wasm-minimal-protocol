# C wasm plugin example

This is a bare-bones Typst plugin, written in C.

## Compile

To compile this example, you need the [emcc compiler](https://emscripten.org/docs/getting_started/downloads.html). Then, run the command

```sh
emcc --no-entry -O3 -s ERROR_ON_UNDEFINED_SYMBOLS=0 -o hello.wasm hello.c
```

Emcc always build with WASI, so we need to stub WASI functions:

```sh
wasi-stub hello.wasm -o hello.wasm
```

## Build with Typst

Simply run `typst compile hello.typ`, and observe that it works!

## Further explanation

### Standalone mode

By default, [emcc emits both `.js` and `.wasm` files](https://emscripten.org/docs/compiling/WebAssembly.html#compiler-output). The `.wasm` file imports functions from `.js`, which is disallowed by Typst.

By specifying `-o hello.wasm` (instead of `-o hello.js`), we tell emcc to generate a [standalone](https://github.com/emscripten-core/emscripten/wiki/WebAssembly-Standalone) `.wasm` file. That's more suitable for Typst.

### Stubbing

We use [wasi-stub](../../crates/wasi-stub/) to replace WASI functions with stubbed functions.

However, emcc sometimes generates a helper `env` module. If you try to load that kind of `.wasm` in Typst, you will get an error like this:

> error: cannot find definition for import `env::__syscall_faccessat` with type `Func(FuncType { core: FuncType { params: [I32, I32, I32, I32], results: [I32] } })`

In that case, you have to stub `env` in addition to the default WASI module:

```sh
wasi-stub hello.wasm -o hello.wasm --stub-module env,wasi_snapshot_preview1
```
