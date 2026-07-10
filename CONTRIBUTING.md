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

  ![current version in ci.yaml](https://img.shields.io/badge/dynamic/yaml?url=https%3A%2F%2Fgithub.com%2Ftypst-community%2Fwasm-minimal-protocol%2Fraw%2Fmain%2F.github%2Fworkflows%2Fci.yml&query=%24.env.X_C_COMPILER_VERSION&label=current&logo=githubactions&labelColor=white) vs. ![GitHub Release](https://img.shields.io/github/v/release/emscripten-core/emscripten?logo=github) ![GitHub Release Date](https://img.shields.io/github/release-date/emscripten-core/emscripten?logo=github)

- `hello_go`: [TinyGo releases](https://github.com/tinygo-org/tinygo/releases)

  ![current version in ci.yaml](https://img.shields.io/badge/dynamic/yaml?url=https%3A%2F%2Fgithub.com%2Ftypst-community%2Fwasm-minimal-protocol%2Fraw%2Fmain%2F.github%2Fworkflows%2Fci.yml&query=%24.env.X_GO_COMPILER_VERSION&label=current&logo=githubactions&labelColor=white) vs. ![GitHub Release](https://img.shields.io/github/v/release/tinygo-org/tinygo?logo=github) ![GitHub Release Date](https://img.shields.io/github/release-date/tinygo-org/tinygo?logo=github)

- `hello_zig`: [Zig downloads index](https://ziglang.org/download/)

  ![current version in ci.yaml](https://img.shields.io/badge/dynamic/yaml?url=https%3A%2F%2Fgithub.com%2Ftypst-community%2Fwasm-minimal-protocol%2Fraw%2Fmain%2F.github%2Fworkflows%2Fci.yml&query=%24.env.X_ZIG_COMPILER_VERSION&label=current&logo=githubactions&labelColor=white) vs. ![latest version on ziglang.org](https://img.shields.io/badge/dynamic/xml?url=https%3A%2F%2Fziglang.org%2Fdownload%2F&query=%2F%2F*%5B%40id%3D%22content%22%5D%2F%2Fh2%5B2%5D&logo=zig&label=version) ![date of the latest version on ziglang.org](https://img.shields.io/badge/dynamic/xml?url=https%3A%2F%2Fziglang.org%2Fdownload%2F&query=%2F%2F*%5B%40id%3D%22content%22%5D%2F%2Fh2%5B2%5D%2Ffollowing-sibling%3A%3Aul%5B1%5D%2Fli%5B1%5D&logo=zig&label=date)

- `hello_haskell`: [the default `$FLAVOUR` in `setup.sh`](https://gitlab.haskell.org/haskell-wasm/ghc-wasm-meta/-/blob/master/setup.sh?ref_type=heads#L5)

  ![current version in ci.yaml](https://img.shields.io/badge/dynamic/yaml?url=https%3A%2F%2Fgithub.com%2Ftypst-community%2Fwasm-minimal-protocol%2Fraw%2Fmain%2F.github%2Fworkflows%2Fci.yml&query=%24.env.X_HASKELL_COMPILER_VERSION&label=current&logo=githubactions&labelColor=white) vs. ![flavour in setup.sh](https://img.shields.io/badge/dynamic/regex?url=https%3A%2F%2Fgitlab.haskell.org%2Fhaskell-wasm%2Fghc-wasm-meta%2F-%2Fraw%2Fmaster%2Fsetup.sh&search=%7BFLAVOUR%3A-(%5Cd%2B%5C.%5Cd%2B)%7D&replace=%241&logo=haskell&label=flavour)

- `hello_moonbit`: [MoonBit updates](https://www.moonbitlang.com/updates)

  ![current version in ci.yaml](https://img.shields.io/badge/dynamic/yaml?url=https%3A%2F%2Fgithub.com%2Ftypst-community%2Fwasm-minimal-protocol%2Fraw%2Fmain%2F.github%2Fworkflows%2Fci.yml&query=%24.env.X_MOONBIT_COMPILER_VERSION&label=current&logo=githubactions&labelColor=white) vs. ![latest blog title](https://img.shields.io/badge/dynamic/xml?url=https%3A%2F%2Fwww.moonbitlang.com%2Fupdates%2F&query=%2F%2Fmain%2Farticle%5B1%5D%2Fheader%2Fh2%2F%2Ftext()&label=title&logo=docusaurus)

As for `hello_rust`, the Rust toolchain is quite stable. Therefore, we just specify `stable` and don't need to update it manually.
