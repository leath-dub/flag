const std = @import("std");

pub fn build(b: *std.Build) !void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const lib_mod = b.addModule("flag", .{
        .root_source_file = b.path("src/root.zig"),
        .target = target,
        .optimize = optimize,
    });

    const lib = b.addLibrary(.{
        .linkage = .static,
        .name = "flag",
        .root_module = lib_mod,
    });

    b.installArtifact(lib);

    const lib_unit_tests = b.addTest(.{
        .root_module = lib_mod,
    });

    const run_lib_unit_tests = b.addRunArtifact(lib_unit_tests);

    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(&run_lib_unit_tests.step);

    const examples_step = b.step("examples", "Check examples");

    const examples_path = try std.fs.path.join(b.allocator, &.{ b.build_root.path.?, "examples" });

    // Check all files in examples/ directory
    const examples_dir = try std.fs.openDirAbsolute(examples_path, .{ .iterate = true });

    var it = examples_dir.iterate();
    while (try it.next()) |entr| {
        if (entr.kind == .file and std.mem.endsWith(u8, entr.name, ".zig")) {
            const file_path = try std.fs.path.join(b.allocator, &.{ "examples", entr.name });
            const mod_name = try std.mem.concat(b.allocator, u8, &.{ entr.name[0..std.mem.lastIndexOfScalar(u8, entr.name, '.').?], "_check" });

            const check_mod = b.createModule(.{
                .root_source_file = b.path(file_path),
                .target = target,
                .optimize = optimize,
            });

            check_mod.addImport("flag", lib_mod);

            const check = b.addExecutable(.{
                .name = mod_name,
                .root_module = check_mod,
            });

            const run_step = b.addRunArtifact(check);
            examples_step.dependOn(&run_step.step);
        }
    }

    test_step.dependOn(examples_step);
}
