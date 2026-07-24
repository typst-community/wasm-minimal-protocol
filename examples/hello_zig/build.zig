const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{
        .default_target = .{
            .cpu_arch = .wasm32,
            // If you want WASI by default, replace `freestanding` with `wasi`.
            .os_tag = .freestanding,
            .cpu_model = .{
                .explicit = &std.Target.wasm.cpu.lime1,
            },
            .cpu_features_add = std.Target.wasm.featureSet(&.{
                .bulk_memory,
                .reference_types,
                .simd128,
            }),
        },
    });
    const optimize = b.standardOptimizeOption(.{});

    const zbor_dep = b.dependency("zbor", .{
        .target = target,
        .optimize = optimize,
    });

    const exe = b.addExecutable(.{
        .name = "hello",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/main.zig"),
            .target = target,
            .optimize = optimize,
            .imports = &.{
                .{ .name = "zbor", .module = zbor_dep.module("zbor") },
            },
            .strip = true,
            .omit_frame_pointer = true,
            .unwind_tables = .none,
        }),
    });
    exe.entry = .disabled;
    exe.rdynamic = true;
    if (target.result.os.tag == .wasi)
        exe.wasi_exec_model = .reactor;

    const install_exe = b.addInstallArtifact(exe, .{
        .dest_dir = .{ .override = .prefix },
    });
    b.getInstallStep().dependOn(&install_exe.step);
}
