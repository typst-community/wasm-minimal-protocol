/*
 * Do not edit the content of this file, unless you know what you are doing!
 *
 * The two pairs of extern functions with import attributes and with wrapper functions
 *  is mandatory. They are not redundant, for otherwise the compiler won't properly import
 *  these functions from correct WASM modules.
 *
 * `hs_init()` from <HsFFI.h> returns `void`, while Typst plugin requires a function returning
 *  `int32_t`. `hs_init()` also requires two arguments. Hence the wrapper `hs_init_wrapped()`.
 */
#include <stdint.h>
#include <HsFFI.h>

extern void _wasm_minimal_protocol_write_args_to_buffer(void *buf) __attribute__((
    import_module("typst_env"),
    import_name("wasm_minimal_protocol_write_args_to_buffer")));

void wasm_minimal_protocol_write_args_to_buffer(void *buf)
{
    _wasm_minimal_protocol_write_args_to_buffer(buf);
}

extern void _wasm_minimal_protocol_send_result_to_host(void *buf, int32_t len) __attribute__((
    import_module("typst_env"),
    import_name("wasm_minimal_protocol_send_result_to_host")));

void wasm_minimal_protocol_send_result_to_host(void *buf, int32_t len)
{
    _wasm_minimal_protocol_send_result_to_host(buf, len);
}

int32_t hs_init_wrapped()
{
    hs_init(0, 0);
    return 0;
}