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

### Basic usage

```sh
# Stub `my_library.wasm` and save as `my_library - stubbed.wasm`:
wasi-stub my_library.wasm

# Stub the file in place:
wasi-stub my_library.wasm -o my_library.wasm

# List what functions will be stubbed without actually stubbing:
wasi-stub my_library.wasm --list

# See all options:
wasi-stub --help
```

See [`examples/hello_{c,rust,…}/README.md`](../../README.md#examples) for concrete examples.

### Advanced usage

To help track usage of stubbed functions, this tool lets them return the weird number 76 by default. If it causes problems, you can specify `-r`/`--return-value`:

```sh
# Change the return value to zero:
wasi-stub a.wasm -r 0
```

For some build systems, the resulting `*.wasm` binary imports other disallowed modules. This tool is capable of stubbing them as well:

```sh
# Stub another module other than the default `wasi_snapshot_preview1`:
wasi-stub a.wasm --stub-module horrible_module

# Stub multiple modules:
wasi-stub a.wasm --stub-module wasi_snapshot_preview1,horrible_module,holodeck

# Stub only a specific function:
wasi-stub a.wasm --stub-function horrible_module:terrible_function

# Stub multiple functions:
wasi-stub a.wasm --stub-function horrible_module:terrible_function,holodeck:freeze_program
```

The above three options can be combined to fit complex scenarios. For example, [pyrunner](https://typst.app/universe/package/pyrunner) uses the following [build steps][pyrunner-make] to stub different functions with different return values.

[pyrunner-make]: https://github.com/peng1999/typst-pyrunner/blob/f6370d1b4b81c9e2c14c24e44d3439bb8cd22ec9/Makefile#L14-L17

```sh
wasi-stub pyrunner.wasm -o pyrunner.wasm \
  --stub-function wasi_snapshot_preview1:random_get,wasi_snapshot_preview1:environ_get,wasi_snapshot_preview1:environ_sizes_get,wasi_snapshot_preview1:clock_time_get \
  --return-value 0

wasi-stub pyrunner.wasm -o pyrunner.wasm \
  --stub-function wasi_snapshot_preview1:fd_prestat_get \
  --return-value 8

wasi-stub pyrunner.wasm -o pyrunner.wasm \
  --stub-module wasi_snapshot_preview1
```

# Alternatives (?)

Inspiration for this comes from [dicej/stubber](https://github.com/dicej/stubber). It replaces stubbed functions with a trap, while `wasi-stub` replaces them with functions that do nothing.
