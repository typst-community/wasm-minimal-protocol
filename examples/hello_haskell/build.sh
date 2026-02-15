wasm32-wasi-ghc Main.hs typ_wrapper.c -o hello.wasm -optc-g -optl-g -optl-Xlinker -optl--allow-undefined -optl-Wl,--export=hs_init,--export=hs_exit,--export=hs_init_wrapped,--export=hello,--export=double_it,--export=concatenate,--export=shuffle,--export=returns_ok,--export=will_panic,--export=returns_err
wasi-stub --return-value 0 hello.wasm -o hello.wasm
