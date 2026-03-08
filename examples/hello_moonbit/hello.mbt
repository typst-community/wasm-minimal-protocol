// Protocol host imports — only primitive types are valid for wasm stub FFI.
fn send_result_to_host_raw(ptr : Int, len : Int) = "typst_env" "wasm_minimal_protocol_send_result_to_host"
fn write_args_to_buffer_raw(ptr : Int) = "typst_env" "wasm_minimal_protocol_write_args_to_buffer"

// Returns a raw pointer to the first data byte of bs (heap_ptr + 8).
// FixedArray[Byte] and Bytes share the same linear-memory layout, so this
// helper works for both via the zero-cost `to_bytes` cast below.
#borrow(bs)
extern "wasm" fn data_ptr(bs : Bytes) -> Int =
  #|(func (param i32) (result i32)
  #|  local.get 0
  #|  i32.const 8
  #|  i32.add)

// Inline trap — avoids importing spectest.print_char which Typst doesn't provide.
extern "wasm" fn wasm_trap() -> Int =
  #|(func (result i32) unreachable)

// FixedArray[Byte] and Bytes have the same representation; this cast is free.
fn to_bytes(arr : FixedArray[Byte]) -> Bytes = "%identity"

// The dummy bs.length() prevents MoonBit from scheduling a decref on bs
// before the host has finished reading the buffer.
fn send_result(bs : Bytes) -> Unit {
  send_result_to_host_raw(data_ptr(bs), bs.length())
  let _ = bs.length()
}

// Same rationale: keep bs alive until after the host write completes.
fn write_args(arr : FixedArray[Byte]) -> Unit {
  let bs = to_bytes(arr)
  write_args_to_buffer_raw(data_ptr(bs))
  let _ = bs.length()
}

// ===

pub fn hello() -> Int {
  send_result(b"Hello from wasm!!!")
  0
}

pub fn double_it(arg1_len : Int) -> Int {
  let result = FixedArray::make(arg1_len * 2, b'\x00')
  write_args(result)
  for i in 0..<arg1_len {
    result[arg1_len + i] = result[i]
  }
  send_result(to_bytes(result))
  0
}

pub fn concatenate(arg1_len : Int, arg2_len : Int) -> Int {
  let args = FixedArray::make(arg1_len + arg2_len, b'\x00')
  write_args(args)
  let result = FixedArray::make(arg1_len + 1 + arg2_len, b'\x00')
  for i in 0..<arg1_len {
    result[i] = args[i]
  }
  result[arg1_len] = b'*'
  for i in 0..<arg2_len {
    result[arg1_len + 1 + i] = args[arg1_len + i]
  }
  send_result(to_bytes(result))
  0
}

pub fn shuffle(arg1_len : Int, arg2_len : Int, arg3_len : Int) -> Int {
  let total_len = arg1_len + arg2_len + arg3_len
  let args = FixedArray::make(total_len, b'\x00')
  write_args(args)
  // Result layout: arg3 '-' arg1 '-' arg2
  let result = FixedArray::make(total_len + 2, b'\x00')
  let mut pos = 0
  for i in 0..<arg3_len {
    result[pos] = args[arg1_len + arg2_len + i]
    pos += 1
  }
  result[pos] = b'-'
  pos += 1
  for i in 0..<arg1_len {
    result[pos] = args[i]
    pos += 1
  }
  result[pos] = b'-'
  pos += 1
  for i in 0..<arg2_len {
    result[pos] = args[arg1_len + i]
    pos += 1
  }
  send_result(to_bytes(result))
  0
}

pub fn returns_ok() -> Int {
  send_result(b"This is an `Ok`")
  0
}

pub fn returns_err() -> Int {
  send_result(b"This is an `Err`")
  1
}

pub fn will_panic() -> Int {
  wasm_trap()
}
