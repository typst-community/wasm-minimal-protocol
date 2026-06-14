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

To implement the plugin, this example assumes the specific ABI layout of `Bytes` and `FixedArray[Byte]` in MoonBit, that they contain the actual data without any headers. As of 2026-06, the representation is undocumented.

<details><summary>Historical notes for moonc v0.9.2+bbe2b338f (2026-05-13) and earlier</summary>

`Bytes` and `FixedArray[Byte]` place an 8-byte header before the actual data in earlier versions of moonc (the [first tested version](https://github.com/typst-community/wasm-minimal-protocol/pull/65) was v0.8.2+8cca5f22a). The representation was never documented, but as of 2026-03, many MoonBit projects assumed it[^repr]. Therefore, it was once considered quite stable in practice.

[^repr]: For instance, [moonbitlang/wasm4](https://github.com/moonbitlang/wasm4/blob/af7bc4e/ffi.wasm.mbt) and [tonyfettes/memory](https://github.com/moonbit-community/tonyfettes-memory.mbt/blob/d35a9fe/src/memory.wasm.mbt#L43-L45). The former is made by the official. The latter is made by moonbit-community and referenced by [moonbitlang/awesome-moonbit](https://github.com/moonbitlang/awesome-moonbit). As another evidence, if you convert `.wasm` to `.wat` (WebAssembly text format) and paste it to a general-purpose LLM, it will likely recognize the MoonBit representation.

The header was skipped in [moonc v0.10.0+84519ca0a (2026-06-08)](https://www.moonbitlang.com/updates/2026/06/08/moonbit-0-10-0-release). However, the change was only mentioned in [an informal post published in Chinese on zhihu.com on 2026-05-21T15:51+08:00](https://www.zhihu.com/pin/2040821890055414896):

> MoonBit Wasm ABI 即将调整 |
>
> 下一个版本中，MoonBit 的 Wasm backend ABI 将发生变更。
>
> Pointer 不再指向对象头（object header），而是直接指向 payload 起始位置。
>
> 在编写 FFI 时，不再需要处理那些神秘的额外 8 字节，同时也为未来进一步调整对象头结构留出了空间。
>
> ```diff
>  ///|
>  pub extern "wasm" fn malloc(size : Int) -> Int =
>  #| (func (param i32) (result i32) (local i32)
>  #|   local.get 0 i32.const 4 i32.add call $moonbit.gc.malloc
>  #|   local.tee 1 i32.const 0 call $moonbit.init_array8
> -#|   local.get 1 i32.const 8 i32.add)
> +#|   local.get 1)
>
>  ///|
>  pub extern "wasm" fn free(position : Int) =
> -#| (func (param i32) local.get 0 i32.const 8 i32.sub call $moonbit.decref)
> +#| (func (param i32) local.get 0 call $moonbit.decref)
>
>  ///|
>  #owned(str)
>  pub extern "wasm" fn str2ptr(str : String) -> Int =
> -#| (func (param i32) (result i32) local.get 0 i32.const 8 i32.add)
> +#| (func (param i32) (result i32) local.get 0)
> ```

The diff was extracted from [refactor: Remove inline wasm ABI pointer shifts by peter-jerry-ye · Pull Request #1616 · bytecodealliance/wit-bindgen](https://github.com/bytecodealliance/wit-bindgen/pull/1616). Note that the description of this PR said the change was introduced in MoonBit 0.9.3, not 0.10.0. [Updates | MoonBit](https://www.moonbitlang.com/updates) lists 0.9.2 and 0.10.0, so it is unknown when 0.9.3 was released.

</details>

Nonetheless, there are other approaches that do not rely on the unstable representation. For example, we can write data outside the heap (below [heap-start-address](https://docs.moonbitlang.com/en/latest/toolchain/moon/package.html#wasm-linear-backend-link-options)) and manage it manually[^heap]. It is not yet clear whether these approaches are better or not. Feedback and contributions are welcome.

[^heap]: This approach was proposed by tonyfettes in 2026-03 on the [MoonBit Chinese forum](https://taolun.moonbitlang.com/t/topic/1397/2).
