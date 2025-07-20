const std = @import("std");
const log = std.log;
const posix = std.posix;
const mem = std.mem;

const flag = @import("./root.zig");

pub fn main() !void {
    const argv = std.os.argv;
    
    var gpa = std.heap.DebugAllocator(.{}){};
    defer {
        if (gpa.deinit() == .leak and gpa.detectLeaks()) {
            @panic("leaked memory");
        }
    }

    var cg = flag.CommandGroup.init(gpa.allocator(), mem.span(argv[0]));
    defer cg.deinit();

    try cg.globalFlags().declare("log-level", 0, "specify log `level` from warnings (0) to fatal errors (3)");

    try cg.subcommand("ls", "list some stuff");
    try cg.subcommand("rm", "remove some stuff\nNOTE: this distructive (be careful)");

    try cg.lookup("ls").?.declare("x", 10, "the value of x");
    try cg.lookup("ls").?.declare("v", false, "verbose output");

    _ = try cg.parse(argv[1..]);
    _ = cg.ran();
    _ = cg.vargs();
}
