const std = @import("std");

const Build = std.Build;

const StepOpts = struct {
    target: ?Build.ResolvedTarget = null,
    optimize: ?std.builtin.OptimizeMode = null,
};

fn stepTest(b: *Build, comptime test_root: []const u8, opts: StepOpts) *Build.Step {
    const tests = b.addTest(.{
        .root_source_file = b.path(test_root),
        .target = opts.target orelse b.standardTargetOptions(.{}),
        .optimize = opts.optimize orelse b.standardOptimizeOption(.{}),
    });

    const run = b.addRunArtifact(tests);

    const step = b.step("test", "Run all tests");

    step.dependOn(&run.step);

    return step;
}

const ModuleOpts = struct {
    target: ?Build.ResolvedTarget = null,
    optimize: ?std.builtin.OptimizeMode = null,
};

fn moduleParzer(b: *Build, opts: ModuleOpts) *Build.Module {
    return b.addModule("parzer", .{
        .root_source_file = b.path("src/parzer.zig"),
        .target = opts.target orelse b.standardTargetOptions(.{}),
        .optimize = opts.optimize orelse b.standardOptimizeOption(.{}),
    });
}

pub fn build(b: *Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    _ = moduleParzer(b, .{
        .target = target,
        .optimize = optimize,
    });

    _ = stepTest(b, "src/test.zig", .{
        .target = target,
        .optimize = optimize,
    });
}
