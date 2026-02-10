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