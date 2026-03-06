# Changelog

All notable changes to this project will be documented in this file.

- [wasm-minimal-protocol](#wasm-minimal-protocol) — The Rust macro wrapper library for writing Typst plugins
- [wasi-stub](#wasi-stub) — The CLI tool for stubbing WASI-related functions

Changes to the [examples](./examples/) are not documented here. At present, their changes mainly involve adding support for more programming languages and complex data types. See the [_Examples_ section in the main README](./README.md#examples) and READMEs in `examples/hello_*/` for more information.

Changes before 2023-08-21 are not documented either. Before that day, [Wasm plugin system by astrale-sharp · Pull Request #1555 · typst/typst](https://github.com/typst/typst/pull/1555) had not yet been merged, and this repository was conducting experiments on protocols and hosts[^initial-experiments]. The first commit in this repository after that is [`60b235b`][tree-60b235b] (committed on 2023-08-23 in [#15](https://github.com/typst-community/wasm-minimal-protocol/pull/15)).

[^initial-experiments]: Tested hosts include [wasmi], [wasmer], [wasmtime], and [rust-wasm], and `wasm_minimal_protocol_free_byte_buffer` was removed in the final protocol.

[wasmi]: https://crates.io/crates/wasmi
[wasmer]: https://crates.io/crates/wasmer
[wasmtime]: https://crates.io/crates/wasmtime
[rust-wasm]: https://crates.io/crates/rust-wasm
[tree-60b235b]: https://github.com/typst-community/wasm-minimal-protocol/tree/60b235b "Repository tree at 60b235b"

All dates in this file are in UTC.

## wasm-minimal-protocol

### [Unreleased]

#### Added
- More flexibility in the types of arguments and returns (#41):
  - Argument/input type: `&mut [u8]` is now supported. Previously, only `&[u8]` was supported.
  - Return/output type: `&[u8]`, `Box<[u8]>`, and their `Result<T, E>` variants are now supported. Previously, only `Vec<u8>` and `Result<Vec<u8>, E>` were supported.

#### Changed
- Restructure the project to allow inclusion as a git submodule in other projects (#28 and #31).
- Allow compiling for `#![no_std]` targets by replacing `std` with `core` (#35).
- Introduce dev-dependencies (regex, semver, which) to test all supported Typst versions from 0.8.0 to 0.14.2 (#51).

#### Fixed
- Improve parsing of slices to support lifetimes (#41).

### [0.1.0] - 2024-10-08

> Notes on the date:
>
> This version was published to GitHub Releases retrospectively on 2026-02-28 according to [`.cargo_vcs_info.json` on crates.io](https://docs.rs/crate/wasm-minimal-protocol/0.1.0/source/.cargo_vcs_info.json#3).

#### Changed
- Lower the minimum dependencies (#20).

## wasi-stub

### [Unreleased]

#### Changed
- Update dependencies 219 → 245 and the rust edition 2021 → 2024 (#64).

#### Fixed
- `*.tar.gz` for macOS/Linux published to GitHub Releases are now correctly gzipped; files in previous releases have been manually renamed to `*.tar` to match their actual contents (#63).

### [0.3.0] - 2026-03-06

#### Added
- Support stubbing functions that return `i64`, `f32`, or `f64` — only `i32` was supported previously (#32).
- Support [cargo-binstall](https://crates.io/crates/cargo-binstall) (#59).

#### Changed
- **(Breaking)** The WASI module will no longer be stubbed by default if `--stub-module` or `--stub-function` is given, making it possible to only stub some of the WASI functions; you can pass `--stub-module wasi_snapshot_preview1,other_module` to restore the old behavior (#60).

### [0.2.1] - 2026-03-02

This version is mainly for testing [trusted publishing](https://crates.io/docs/trusted-publishing) after the repository has been [transferred from astrale-sharp to typst-community](https://github.com/orgs/typst-community/discussions/35).

The content is essentially the same as the last version.

#### Changed
- Update dependencies (#30).

### [0.2.0] - 2025-03-27

> Notes on the date:
>
> This version was published to crates.io later on 2026-03-02. The git worktree was [dirty](https://doc.rust-lang.org/cargo/commands/cargo-package.html?highlight=dirty#cargo_vcs_infojson-format) because `license-file` and `description` fields were added to `Cargo.toml`.

`wasi-stub` is meant to be used on WASM libraries, which are compiled using [`WASI`](https://wasi.dev/) (for example, with the `wasm32-wasip1` rust target), but will be run in an environment where WASI functions are not available (like Typst plugins). `wasi-stub` will replace all the WASI functions with stubs, that immediately return a value without doing anything.

#### Added
- Make it possible to stub modules other than `wasi_snapshot_preview1` with `--stub-module`, and even single functions with `--stub-function` (#17).
- Allow specifying the dummy value with `-r/--return-value` (#18).
- Add integration tests and CI (#18 and #36).
- Make a release script to publish precompiled executables to GitHub Releases (2b6103e).

#### Changed
- Update the wast dependency to 65.0 (#20).
- Remove anyhow dependency to speed up builds (#29).

#### Fixed
- Rework to leverage [wast](https://crates.io/crates/wast) and avoid spurious panics (#17).
- Fix the `-o` option doing nothing (#18).

#### Removed
- Remove the custom test runner and use the `typst` binary instead (#18).
