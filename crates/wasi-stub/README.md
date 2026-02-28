# Wasi Stub

This is a tool allowing you to take a [wasi](https://wasi.dev/) compliant WebAssembly file and replace all functions wasi depends on by meaningless stubs.

If you don't depend on printing or reading/writing files, your code will probably still work and it will now be compatible with typst or host-wasmi.

## How to install

You can download binaries from [GitHub Releases](https://github.com/typst-community/wasm-minimal-protocol/releases).

Alternatively, if you have a working rust toolchain, you can compile from source by running `cargo install --path .` from the wasi-stub directory (where this README is).

## How to use

Once you installed wasi-stub, you can simply run `wasi-stub my_library.wasm` from the terminal.

Run `wasi-stub --help` for more options.

# Alternatives (?)

Inspiration for this comes from [https://github.com/dicej/stubber]. It replaces stubbed functions with a trap, while `wasi-stub` replaces them with functions that do nothing.
