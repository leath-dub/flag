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

    var s = flag.FlagSet.init(std.heap.smp_allocator, mem.span(argv[0]));
    defer s.deinit();

    var x: u32 = 10;
    var xptr = flag.ValuePtr { .ref = &x, .tag = .uint };

    var y: u32 = 10;
    var yptr0 = flag.ValuePtr { .ref = &y, .tag = .uint };
    var yptr1 = flag.ValuePtr { .ref = &yptr0, .tag = .ptr };
    var yptr2 = flag.ValuePtr { .ref = &yptr1, .tag = .ptr };
    var yptr3 = flag.ValuePtr { .ref = &yptr2, .tag = .ptr };
    var yptr4 = flag.ValuePtr { .ref = &yptr3, .tag = .ptr };

    try s.variable(&xptr, "foo", "a nested pointer flag");
    try s.variable(&yptr4, "bar", "a *really* nested pointer flag");

    try s.set("foo", "50");
    try s.set("bar", "1000");

    _ = try s.parse(argv[1..]);
}
