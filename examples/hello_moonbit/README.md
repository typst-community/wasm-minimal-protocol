# MoonBit WASM plugin example

This is a bare-bones Typst plugin, written in [MoonBit](https://www.moonbitlang.com/).

## Compile

To compile this example, you need to [install MoonBit CLI Tools](https://www.moonbitlang.com/download/#moonbit-cli-tools). Then, build the project for the `wasm` (linear-memory) target:

```sh
moon build --target wasm --release
cp _build/wasm/release/build/hello.wasm .
```

Here we export the functions explicitly in [`moon.pkg`](./moon.pkg). You should change the exports when implementing your own plugin.

## Build with Typst

Simply run `typst compile hello.typ`, and observe that it works!

## Further explanation

### Unstable interface

Typst's plugin protocol relies on the memory to exchange data. However, the core MoonBit does not provide a stable [foreign function interface](https://docs.moonbitlang.com/en/latest/language/ffi.html#types) for memory or bytes.

To implement the plugin, this example assumes the specific representation of `Bytes` and `FixedArray[Byte]` in MoonBit, that they place an 8-byte header before the actual data. As of 2026-03, the representation is undocumented, but many MoonBit projects assume it[^repr]. Therefore, it can be considered quite stable in practice.

[^repr]: For instance, [moonbitlang/wasm4](https://github.com/moonbitlang/wasm4/blob/af7bc4e/ffi.wasm.mbt) and [tonyfettes/memory](https://github.com/moonbit-community/tonyfettes-memory.mbt/blob/d35a9fe/src/memory.wasm.mbt#L43-L45). The former is made by the official. The latter is made by moonbit-community and referenced by [moonbitlang/awesome-moonbit](https://github.com/moonbitlang/awesome-moonbit). As another evidence, if you convert `.wasm` to `.wat` (WebAssembly text format) and paste it to a general-purpose LLM, it will likely recognize the MoonBit representation.

Nonetheless, there are other approaches that do not rely on the unstable representation. For example, we can write data outside the heap (below [heap-start-address](https://docs.moonbitlang.com/en/latest/toolchain/moon/package.html#wasm-linear-backend-link-options)) and manage it manually[^heap]. It is not yet clear whether these approaches are better or not. Feedback and contributions are welcome.

[^heap]: This approach was proposed by tonyfettes in 2026-03 on the [MoonBit Chinese forum](https://taolun.moonbitlang.com/t/topic/1397/2).
