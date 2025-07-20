const std = @import("std");
const io = std.io;
const mem = std.mem;
const debug = std.debug;

pub fn writeReplace(w: io.AnyWriter, s: []const u8, repl: u8, with: []const u8) io.AnyWriter.Error!void {
    var rd: usize = 0;
    while (mem.indexOfScalarPos(u8, s, rd, repl)) |ri| {
        try w.writeAll(s[rd..ri]);
        try w.writeAll(with);
        rd = ri + 1;
        debug.assert(rd <= s.len);
        if (rd == s.len) break;
    }
    try w.writeAll(s[rd..]);
}

test "writeReplace helper" {
    const testing = std.testing;

    var arr = std.ArrayList(u8).init(testing.allocator);
    defer arr.deinit();

    try writeReplace(arr.writer().any(), "first line\nsecond line\nthird line", '\n', "\n    \t");
    try testing.expectEqualStrings(arr.items, "first line\n    \tsecond line\n    \tthird line");

    arr.clearRetainingCapacity();

    try writeReplace(arr.writer().any(), "", '\n', "\n    \t");
    try testing.expectEqualStrings(arr.items, "");
}
