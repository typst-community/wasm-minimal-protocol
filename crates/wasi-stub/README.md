# Wasi Stub

This is a tool allowing you to take a [wasi](https://wasi.dev/) compliant WebAssembly file and replace all functions wasi depends on by meaningless stubs.

If you don't depend on printing or reading/writing files, your code will probably still work, and will now be compatible with [Typst](https://typst.app/docs/reference/foundations/plugin/) or [Wasmi](https://crates.io/crates/wasmi).

## How to install

You can download binaries from [GitHub Releases](https://github.com/typst-community/wasm-minimal-protocol/releases) or via [`cargo binstall wasi-stub`](https://crates.io/crates/cargo-binstall).

Alternatively, if you have a working rust toolchain, you can compile from source by running `cargo install wasi-stub`. You can also test the latest unreleased version by running `cargo install --path .` from the wasi-stub directory (where this README is).

For GitHub Actions, use [taiki-e/install-action](https://github.com/marketplace/actions/install-development-tools) to install:

```yaml
- uses: taiki-e/install-action@v2
  with:
    tool: wasi-stub
- run: wasi-stub hello.wasm -o hello.wasm
```

## How to use

Once you installed wasi-stub, you can simply run `wasi-stub my_library.wasm` from the terminal.

Run `wasi-stub --help` for more options.

# Alternatives (?)

Inspiration for this comes from [dicej/stubber](https://github.com/dicej/stubber). It replaces stubbed functions with a trap, while `wasi-stub` replaces them with functions that do nothing.
