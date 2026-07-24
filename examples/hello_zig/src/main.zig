//! Provides functions of the Typst plugin protocol
//! and plugin functions used by `hello.typ`.
//!
//! See https://typst.app/docs/reference/foundations/plugin/.
const std = @import("std");
const cbor = @import("zbor");
const gpa = std.heap.wasm_allocator;

pub const panic = std.debug.no_panic;

extern "typst_env" fn wasm_minimal_protocol_send_result_to_host(ptr: [*]const u8, len: usize) void;
extern "typst_env" fn wasm_minimal_protocol_write_args_to_buffer(ptr: [*]u8) void;

/// Sends `bytes` to Typst.
///
/// `bytes` should be encoded as valid UTF-8
/// when the plugin function signals an error.
fn sendResultToHost(bytes: []const u8) void {
    wasm_minimal_protocol_send_result_to_host(bytes.ptr, bytes.len);
}

/// Writes arguments into a single buffer.
///
/// `buf` must be large enough to hold all the arguments.
fn writeArgsToBuffer(buf: []u8) void {
    wasm_minimal_protocol_write_args_to_buffer(buf.ptr);
}

/// Return values used by the plugin protocol.
const Retval = enum(i32) {
    success,
    failure,
};

export fn hello() Retval {
    sendResultToHost("Hello from wasm!!!");
    return .success;
}

export fn double_it(arg_len: usize) Retval {
    const buf = gpa.alloc(u8, arg_len * 2) catch return .failure;
    defer gpa.free(buf);
    writeArgsToBuffer(buf);

    @memcpy(buf[arg_len..], buf[0..arg_len]);

    sendResultToHost(buf);
    return .success;
}

export fn concatenate(arg1_len: usize, arg2_len: usize) Retval {
    const buf = gpa.alloc(u8, arg1_len + arg2_len + 1) catch return .failure;
    defer gpa.free(buf);
    writeArgsToBuffer(buf);

    @memmove(buf[arg1_len + 1 ..], buf[arg1_len..][0..arg2_len]);
    buf[arg1_len] = '*';

    sendResultToHost(buf);
    return .success;
}

export fn shuffle(arg1_len: usize, arg2_len: usize, arg3_len: usize) Retval {
    const buf = gpa.alloc(u8, arg1_len + arg2_len + arg3_len) catch return .failure;
    defer gpa.free(buf);
    writeArgsToBuffer(buf);

    const arg1 = buf[0..arg1_len];
    const arg2 = buf[arg1_len..][0..arg2_len];
    const arg3 = buf[arg1_len + arg2_len ..];

    const result = std.mem.concat(gpa, u8, &.{ arg3, "-", arg1, "-", arg2 }) catch return .failure;
    defer gpa.free(result);

    sendResultToHost(result);
    return .success;
}

export fn returns_ok() Retval {
    sendResultToHost("This is an `Ok`");
    return .success;
}

export fn returns_err() Retval {
    sendResultToHost("This is an `Err`");
    return .failure;
}

export fn will_panic() Retval {
    @panic("unconditional panic");
}

comptime {
    @export(&struct {
        fn func(len: usize) callconv(.c) Retval {
            complexDataImpl(len) catch |err| {
                sendResultToHost(@errorName(err));
                return .failure;
            };
            return .success;
        }
    }.func, .{ .name = "complex_data" });
}

const ComplexDataArgs = struct {
    x: i32,
    y: f64,

    pub fn cborParse(item: cbor.DataItem, o: cbor.Options) !ComplexDataArgs {
        return cbor.parse(ComplexDataArgs, item, .{
            .ignore_override = true,
            .allocator = o.allocator,
        });
    }
};

fn complexDataImpl(len: usize) !void {
    const buf = try gpa.alloc(u8, len);
    defer gpa.free(buf);
    writeArgsToBuffer(buf);

    const data_item: cbor.DataItem = try .new(buf);
    const args: ComplexDataArgs = try .cborParse(data_item, .{});

    var result: std.Io.Writer.Allocating = .init(gpa);
    defer result.deinit();
    try cbor.stringify(args.x + args.y, .{}, &result.writer);
    sendResultToHost(result.written());
}
