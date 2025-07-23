const std = @import("std");

const Build = std.Build;

const StepOpts = struct {
    target: ?Build.ResolvedTarget = null,
    optimize: ?std.builtin.OptimizeMode = null,
};

fn stepTest(
    b: *Build,
    parzer: *Build.Module,
) *Build.Step {
    const tests = b.addTest(.{
        .root_module = parzer,
    });

    const run = b.addRunArtifact(tests);

    const step = b.step("test", "Run all tests");

    step.dependOn(&run.step);

    return step;
}

const ModuleOpts = struct {
    root_path: []const u8,
    target: ?Build.ResolvedTarget = null,
    optimize: ?std.builtin.OptimizeMode = null,
};

fn moduleParzerCommon(
    b: *Build,
    opts: ModuleOpts,
) *Build.Module {
    return b.addModule("parzer-common", .{
        .root_source_file = b.path(opts.root_path),
        .target = opts.target orelse b.standardTargetOptions(.{}),
        .optimize = opts.optimize orelse b.standardOptimizeOption(.{}),
    });
}

fn moduleParzer(
    b: *Build,
    opts: ModuleOpts,
) *Build.Module {
    return b.addModule("parzer", .{
        .root_source_file = b.path(opts.root_path),
        .target = opts.target orelse b.standardTargetOptions(.{}),
        .optimize = opts.optimize orelse b.standardOptimizeOption(.{}),
    });
}

pub fn build(
    b: *Build,
) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const parzer_common = moduleParzerCommon(b, .{
        .root_path = "src/parzer-common/parzer-common.zig",
        .target = target,
        .optimize = optimize,
    });

    const parzer = moduleParzer(b, .{
        .root_path = "src/parzer.zig",
        .target = target,
        .optimize = optimize,
    });

    parzer.addImport("parzer-common", parzer_common);

    _ = stepTest(b, parzer);
}
