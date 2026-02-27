# Haskell WASM plugin example

This is a bare-bone Typst plugin, written in Haskell.

## Prerequisite and supported platforms

To compile this example, you need to [install `wasm32-wasi-ghc`](https://gitlab.haskell.org/haskell-wasm/ghc-wasm-meta). Further guidance can be found in [Using the GHC WebAssembly backend — Glasgow Haskell Compiler User's Guide](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/wasm.html).

As of 2026, `wasm32-wasi-ghc` only supports Linux and macOS. The support status for Windows and WSL (Windows Subsystem for Linux) is unclear, and feedback is welcomed.

## Compile

First, run the command below. Here we export the functions explicitly. You should change the exports when implementing your own plugin. See the comments in [`Main.hs`](./Main.hs) for details.

```sh
wasm32-wasi-ghc Main.hs typ_wrapper.c -o hello.wasm \
    -optl-Wl,--export=hs_init_wrapped,--export=hello,--export=double_it,--export=concatenate,--export=shuffle,--export=returns_ok,--export=will_panic,--export=returns_err
```

Then, stub the resulting binary with `--return-value 0`:

```sh
cargo run --manifest-path ../../crates/wasi-stub/Cargo.toml \
    --return-value 0 hello.wasm -o hello.wasm
```

## Build with Typst

Run `typst compile hello.typ` with Typst `0.14.0` or later, and observe that it works!

Note that in `hello.typ`, it is important to initialize the plugin properly by writing:

```typst
#let p = plugin.transition(plugin("./hello.wasm").hs_init_wrapped)
```

This is different from the simple `#let p = plugin("./hello.wasm")` in other languages' examples.

## Further explanation

### Initialization

Haskell is a language with a runtime system, and requires an explicit `hs_init()` call to initialize.

Therefore, we use the [`plugin.transition`](https://typst.app/docs/reference/foundations/plugin/#definitions-transition) API to call `hs_init_wrapped` when loading `hello.wasm`.

Haskell also provides a `hs_exit()` function, but it is useless because of Typst's plugin design.

### SIMD

The initializer depends on the WebAssembly SIMD functionality, which was supported by Typst [starting from 0.14.0](https://github.com/typst/typst/pull/6997).

If you use an older version, you may encounter the following error when loading `hello.wasm`:

> error: failed to load WebAssembly module (unexpected SIMD opcode: 0xfd (at offset 0x8b82))

### Stubbing

We use [wasi-stub](../../crates/wasi-stub/) to replace WASI functions with stubbed functions. By default, wasi-stub sets a weird return value to help track usage of the stubs. However, `hs_init()` expects reasonable return values. As such, wasi-stub must be called with `--return-value 0`.
