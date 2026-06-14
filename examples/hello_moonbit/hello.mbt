// Part 1: Import functions from the protocol and create wrappers.
// Don't edit this part unless you know what you are doing.

///|
fn __send_result_to_host(ptr : Int, len : Int) = "typst_env" "wasm_minimal_protocol_send_result_to_host"

///|
fn __write_args_to_buffer(ptr : Int) = "typst_env" "wasm_minimal_protocol_write_args_to_buffer"

///|
/// Returns a raw pointer to the first data byte of `bytes`.
#borrow(bytes)
fn _as_ptr(bytes : Bytes) -> Int = "%identity"
// Note that the ABI for `Bytes` is unstable. Refer to README for further explanation.

///|
/// Reinterpret the byte sequence as Bytes.
///
/// Notice that this will make the `Bytes` object to be a view of the original
/// byte sequence, so any modification to the original byte sequence will be
/// reflected in the `Bytes` object.
///
/// This is a copy of `FixedArray::unsafe_reinterpret_as_bytes`.
/// https://github.com/moonbitlang/core/blob/a2cc59d/builtin/bytes.mbt#L15-L25
fn as_bytes(arr : FixedArray[Byte]) -> Bytes = "%identity"

///|
fn send_result(bytes : Bytes) -> Unit {
  __send_result_to_host(_as_ptr(bytes), bytes.length())
}

///|
fn write_args(arr : FixedArray[Byte]) -> Unit {
  let bytes = as_bytes(arr)
  __write_args_to_buffer(_as_ptr(bytes))
}

// ===
// Part 2: Implement your own plugin below.

///|
pub fn hello() -> Int {
  send_result(b"Hello from wasm!!!")
  0
}

///|
pub fn double_it(arg1_len : Int) -> Int {
  let result = FixedArray::make(arg1_len * 2, b'\x00')
  write_args(result)
  result.blit_to(result, len=arg1_len, dst_offset=arg1_len)
  send_result(as_bytes(result))
  0
}

///|
pub fn concatenate(arg1_len : Int, arg2_len : Int) -> Int {
  let args = FixedArray::make(arg1_len + arg2_len, b'\x00')
  write_args(args)

  let result = FixedArray::make(arg1_len + 1 + arg2_len, b'\x00')
  args.blit_to(result, len=arg1_len)
  result[arg1_len] = b'*'
  args.blit_to(
    result,
    len=arg2_len,
    src_offset=arg1_len,
    dst_offset=arg1_len + 1,
  )

  send_result(as_bytes(result))
  0
}

///|
pub fn shuffle(arg1_len : Int, arg2_len : Int, arg3_len : Int) -> Int {
  let total_len = arg1_len + arg2_len + arg3_len
  let args = FixedArray::make(total_len, b'\x00')
  write_args(args)

  // Result layout: arg3 '-' arg1 '-' arg2
  let result = FixedArray::make(total_len + 2, b'\x00')
  let mut pos = 0

  args.blit_to(
    result,
    len=arg3_len,
    src_offset=arg1_len + arg2_len,
    dst_offset=pos,
  )
  pos += arg3_len

  result[pos] = b'-'
  pos += 1

  args.blit_to(result, len=arg1_len, dst_offset=pos)
  pos += arg1_len

  result[pos] = b'-'
  pos += 1

  args.blit_to(result, len=arg2_len, src_offset=arg1_len, dst_offset=pos)
  pos += arg2_len

  send_result(as_bytes(result))
  0
}

///|
pub fn returns_ok() -> Int {
  send_result(b"This is an `Ok`")
  0
}

///|
pub fn returns_err() -> Int {
  send_result(b"This is an `Err`")
  1
}

///|
pub fn will_panic() -> Int {
  panic()
}
