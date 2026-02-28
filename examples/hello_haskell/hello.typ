//! Minimum supported Typst version: 0.14.0
//! Before that, SIMD was not supported by the WebAssembly runtime, but WASM files compiled from Haskell always require SIMD.
//! See https://github.com/typst/typst/issues/6679 for further details.

#{
  let p = plugin.transition(plugin("./hello.wasm").hs_init_wrapped)

  assert.eq(str(p.hello()), "Hello from wasm!!!")
  assert.eq(str(p.double_it(bytes("abc"))), "abcabc")
  assert.eq(str(p.concatenate(bytes("hello"), bytes("world"))), "hello*world")
  assert.eq(str(p.shuffle(bytes("s1"), bytes("s2"), bytes("s3"))), "s3-s1-s2")
  assert.eq(str(p.returns_ok()), "This is an `Ok`")
  // p.will_panic()  // Fails compilation
  // p.returns_err() // Fails compilation with an error message
}