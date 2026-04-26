# Contributing guidelines

## Tests

To run the tests, you will need:

- The emcc compiler: <https://emscripten.org/docs/getting_started/downloads.html>
- An up-to-date Rust toolchain: <https://www.rust-lang.org/>
- A zig compiler: <https://ziglang.org/learn/getting-started/#installing-zig>
- A TinyGo compiler: <https://tinygo.org/getting-started/install/>
- The GHC WebAssembly backend for Haskell: <https://gitlab.haskell.org/haskell-wasm/ghc-wasm-meta>
- The MoonBit toolchain: <https://www.moonbitlang.com/download/>

Then, you can run the tests with `cargo test`.

You may also test specific examples. For example, running `cargo test test_rust` only requires the Rust toolchain.

If `$CARGO_TEST_SAVE_EXAMPLE_RESULT` is set to any non-empty string, the resulting `hello.{wasm,pdf}` from `examples/hello_*/` will be saved to `target/example-result/`. This is useful for comparing examples, as some examples can be built in multiple ways.

> [!NOTE]
>
> At present, the tests are guaranteed to pass only on Linux.
>
> - macOS should work in theory but has not been tested.
> - Windows is unlikely to work. Some compilers do not support Windows, and some tests assume file permissions behave like on Linux.
>
> Feedback and contributions for macOS and Windows are welcome!

## Git hooks

To run tests automatically, and check that you don't accidentally bump dependencies of `wasm-minimal-protocol`, add the pre-push [git hook](https://git-scm.com/docs/githooks):

```sh
git config --local core.hooksPath .githooks
```

The script `.githooks/pre-push` will be run each time you `git push` (except if you use `--no-verify`).

## Minimum supported Typst version

`examples/hello_*/hello.typ` should document the minimum Typst compiler version required for it to work. Specifically, a comment like the following should be added at the top of the file. The first line will be parsed by the test script.

```typst
//! Minimum supported Typst version: 0.13.0
//! Before that, decoding CBOR used a different function.
```

## Update the versions of compilers

The [examples](./README.md#examples) in this repository use a few compilers.
Their versions in [`ci.yml`](./.github/workflows/ci.yml) should be kept up to date.

Here are the notes for finding the latest versions:

- `hello_c`: [Emscripten change log](https://github.com/emscripten-core/emscripten/blob/main/ChangeLog.md)
- `hello_go`: [TinyGo releases](https://github.com/tinygo-org/tinygo/releases)
- `hello_zig`: [Zig downloads index](https://ziglang.org/download/)
- `hello_haskell`: [the default `$FLAVOUR` in `setup.sh`](https://gitlab.haskell.org/haskell-wasm/ghc-wasm-meta/-/blob/master/setup.sh?ref_type=heads#L5)
- `hello_moonbit`: [MoonBit updates](https://www.moonbitlang.com/updates)

As for `hello_rust`, the Rust toolchain is quite stable. Therefore, we just specify `stable` and don't need to update it manually.
