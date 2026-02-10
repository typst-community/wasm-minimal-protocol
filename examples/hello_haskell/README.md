# Haskell WASM plugin example

This is a bare-bone Typst plugin, written in Haskell.

## Compile

To compile this example, you need the [GHC WebAssembly backend](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/wasm.html). 

Make sure that the GHC executable `wasm32-wasi-ghc`, the Python executable `python3` (or `python`), and the utility `wasm-tools` is on your `PATH`. Then, run the build script `build.sh` or `build.bat` (depending on whether you are using Linux or Windows). 

## Stubbing

The `wasi-stub` utility used by, say, the C and Rust example doesn't work in this Haskell case. 

This is because Haskell is a language with a runtime system, and requires an explicit `hs_init()` call to initialize. In this call, it calls some of the WASI functions and expects reasonable outcomes. 

To counteract this, I AI-generated a Python script `replace_wasi.py` that calls the executable `wasm-tools` to parse a WASM module into text, use regex to find all WASI imports and replace them with dummy implementations that does nothing but has reasonable return values, and then calls `wasm-tools` again to parse the text format back to WASM. It is a very naive implementation but it works. 

Calling this `replace_wasi.py` has been integrated into the build script. 

## Build with Typst

Simply run `typst compile hello.typ`, and observe that it works! 

## Notice

Since Haskell has a runtime system, it is important that you not just load the Haskell-based Typst plugin, but also initialize it properly. You must write: 
```
let p0 = plugin("./hello.wasm")
let p = plugin.transition(p0.hs_init_wrapped)
```
in Typst, as opposed to other languages' `let p = plugin("./hello.wasm")`. 