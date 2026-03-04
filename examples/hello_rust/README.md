# Rust wasm plugin example

This is a bare-bones Typst plugin, written in Rust. It uses the [wasm-minimal-protocol](../../) crate to easily define plugin functions.

## Compile

To compile this example, you need to have a working [Rust toolchain](https://www.rust-lang.org/). Then we need to install the `wasm32-unknown-unknown` target:

```sh
rustup target add wasm32-unknown-unknown
```

Then, build the crate with this target:

```sh
cargo build --release --target wasm32-unknown-unknown
cp ./target/wasm32-unknown-unknown/release/hello.wasm ./
```

## Compile with wasi

If you want to build with WASI, use the `wasm32-wasip1` target:

```sh
rustup target add wasm32-wasip1
cargo build --release --target wasm32-wasip1
cp ./target/wasm32-wasip1/release/hello.wasm ./
```

Then, stub the resulting binary:

```sh
cargo run --manifest-path ../../crates/wasi-stub/Cargo.toml hello.wasm -o hello.wasm
```

## Build with Typst

Simply run `typst compile hello.typ`, and observe that it works!

## Further tip: When is stubbing needed?

Stubbing is needed when the resulting binary (e.g., `hello.wasm`) imports functions other than the ones defined by the Typst plugin protocol.

For Rust projects, two common cases are as follows. Usually, there are alternative solutions other than stubbing.

### The build target is `wasm32-wasip1`

In this case, the resulting binary imports WASI functions. If you try to load it in Typst, you will get an error like this:

> error: cannot find definition for import `wasi_snapshot_preview1::fd_write` with type `Func(FuncType { core: FuncType { params: [I32, I32, I32, I32], results: [I32] } })`

**Solutions:** [Stub WASI functions as described above](#compile-with-wasi), or switch to the `wasm32-unknown-unknown` target.

### A dependency uses `wasm-bindgen`

Specifically, one of your direct or indirect dependencies uses [`wasm_bindgen::prelude`](https://docs.rs/wasm-bindgen/latest/wasm_bindgen/prelude/index.html).

`wasm-bindgen` is a library and tool for interactions between Wasm modules and JavaScript, and requires [special build steps](https://drager.github.io/wasm-pack/book/quickstart.html). If compiled with `cargo build`, the resulting binary will import `__wbindgen_*` helper modules and functions. If you try to load it in Typst, you will get an error like this:

> error: cannot find definition for import `__wbindgen_placeholder__::__wbindgen_describe` with type `Func(FuncType { core: FuncType { params: [I32], results: [] } })`

**Solution A:** Stub `wasm-bindgen` imports as below.

```sh
wasi-stub --stub-module __wbindgen_placeholder__,__wbindgen_externref_xform__ hello.wasm -o hello.wasm
```

**Solution B:** Remove the dependency or adjust its features.

<details>
<summary>An example for Solution B</summary>

(This example is derived from [#47](https://github.com/typst-community/wasm-minimal-protocol/issues/47).)

You run this command to find out which dependency uses `wasm-bindgen`:

```sh
$ cargo tree --invert wasm-bindgen --target wasm32-unknown-unknown
wasm-bindgen v0.2.114
└── v_frame v0.3.9
    ├── av-scenechange v0.14.1
    │   └── rav1e v0.8.1
    │       └── ravif v0.12.0
    │           └── image v0.25.9
    │               └── hello v0.1.0 (path/to/hello_rust)
    ├── av1-grain v0.2.5
    │   └── rav1e v0.8.1 (*)
    └── rav1e v0.8.1 (*)
```

According to the tree graph, it is the `image` crate that uses `wasm-bindgen` via `ravif`. Refer to [readme](https://crates.io/crates/image/0.25.9#feature-flags), [Docs.rs](https://docs.rs/crate/image/0.25.9/features#avif), or [Lib.rs](https://lib.rs/crates/image/features#feature-avif) of `image`, and you will find that `ravif` is only required for its `avif` feature. The `avif` feature is enabled by default, but you don't actually need support for the AVIF image format.

Therefore, you disable the `avif` feature by setting `default-features = false` and explicitly enabling necessary features:

```sh
cargo add image --no-default-features --features jpeg,png,webp
```

Now Typst can load the resulting binary.

</details>

<!--
Editor note: Using `wasm-pack build` is not a solution.
It replaces `__wbindgen_*` imports with js imports, which are not supported by Typst either.
-->
