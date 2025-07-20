const std = @import("std");
const testing = std.testing;
const meta = std.meta;
const mem = std.mem;
const io = std.io;

const flag = @import("root.zig");

test "Builtin types" {
    var s = flag.FlagSet.init(testing.allocator, "");
    defer s.deinit();

    try s.declare("int", 0, "int value");
    // This sucks: try s.declare("uint", @as(u32, @intCast(0)), "uint value");
    // Thats why the explicit functions can be nice
    try s.uint("uint", 0, "uint value");
    try s.declare("string", "", "string value");
    try s.declare("boolean", false, "boolean value");
    try s.declare("float", 0.0, "float value");

    var fs = try s.items(testing.allocator);

    for (fs) |f| {
        switch (meta.stringToEnum(flag.ValueType, f.name).?) {
            .int => try testing.expectEqual(f.value.int, 0),
            .uint => try testing.expectEqual(f.value.uint, 0),
            .string => try testing.expectEqual(f.value.string.len, 0),
            .boolean => try testing.expectEqual(f.value.boolean, false),
            .float => try testing.expectEqual(f.value.float, 0),
            else => unreachable,
        }
    }

    try s.set("int", "1");
    try s.set("uint", "1");
    try s.set("string", "1");
    try s.set("boolean", "true");
    try s.set("float", "1");

    testing.allocator.free(fs);

    fs = try s.items(testing.allocator);

    for (fs) |f| {
        switch (meta.stringToEnum(flag.ValueType, f.name).?) {
            .int => try testing.expectEqual(f.value.int, 1),
            .uint => try testing.expectEqual(f.value.uint, 1),
            .string => try testing.expectEqual(f.value.string[0], '1'),
            .boolean => try testing.expectEqual(f.value.boolean, true),
            .float => try testing.expectEqual(f.value.float, 1),
            else => unreachable,
        }
    }

    testing.allocator.free(fs);
}

test "Pointer to all builtin types" {
    var s = flag.FlagSet.init(testing.allocator, "");
    defer s.deinit();

    var int: i32 = undefined;
    var uint: u32 = undefined;
    var string: []const u8 = undefined;
    var boolean: bool = undefined;
    var float: f32 = undefined;

    try s.intVar(&int, "int", 0, "int value");
    try s.uintVar(&uint, "uint", 0, "uint value");
    try s.stringVar(&string, "string", "", "string value");
    try s.booleanVar(&boolean, "boolean", false, "boolean value");
    try s.floatVar(&float, "float", 0, "float value");

    // Make sure the anytype pointer matching compiles properly
    try s.variable(&int, "int_", "int value");
    try s.variable(&uint, "uint_", "uint value");
    try s.variable(&string, "string_", "string value");
    try s.variable(&boolean, "boolean_", "boolean value");
    try s.variable(&float, "float_", "float value");

    try testing.expectEqual(s.lookup("int").?.value.deref(.int), 0);
    try testing.expectEqual(s.lookup("uint").?.value.deref(.uint), 0);
    try testing.expectEqual(s.lookup("string").?.value.deref(.string).len, 0);
    try testing.expectEqual(s.lookup("boolean").?.value.deref(.boolean), false);
    try testing.expectEqual(s.lookup("float").?.value.deref(.float), 0);

    try testing.expectEqual(s.lookup("int_").?.value.deref(.int), 0);
    try testing.expectEqual(s.lookup("uint_").?.value.deref(.uint), 0);
    try testing.expectEqual(s.lookup("string_").?.value.deref(.string).len, 0);
    try testing.expectEqual(s.lookup("boolean_").?.value.deref(.boolean), false);
    try testing.expectEqual(s.lookup("float_").?.value.deref(.float), 0);

    try s.set("int", "1");
    try s.set("uint", "1");
    try s.set("string", "1");
    try s.set("boolean", "true");
    try s.set("float", "1");

    try testing.expectEqual(s.lookup("int").?.value.deref(.int), 1);
    try testing.expectEqual(s.lookup("uint").?.value.deref(.uint), 1);
    try testing.expectEqual(s.lookup("string").?.value.deref(.string)[0], '1');
    try testing.expectEqual(s.lookup("boolean").?.value.deref(.boolean), true);
    try testing.expectEqual(s.lookup("float").?.value.deref(.float), 1);

    try testing.expectEqual(s.lookup("int_").?.value.deref(.int), 1);
    try testing.expectEqual(s.lookup("uint_").?.value.deref(.uint), 1);
    try testing.expectEqual(s.lookup("string_").?.value.deref(.string)[0], '1');
    try testing.expectEqual(s.lookup("boolean_").?.value.deref(.boolean), true);
    try testing.expectEqual(s.lookup("float_").?.value.deref(.float), 1);
}

// To be honest I don't know why I allow this. But it is allowed so I may
// aswell write a test case for sanity to see that you can have arbitrary
// pointer nests.
test "Nested Pointer" {
    var s = flag.FlagSet.init(testing.allocator, "");
    defer s.deinit();

    var x: u32 = 10;
    var xptr = flag.ValuePtr{ .ref = &x, .tag = .uint };

    var y: u32 = 10;
    var yptr0 = flag.ValuePtr{ .ref = &y, .tag = .uint };
    var yptr1 = flag.ValuePtr{ .ref = &yptr0, .tag = .ptr };
    var yptr2 = flag.ValuePtr{ .ref = &yptr1, .tag = .ptr };
    var yptr3 = flag.ValuePtr{ .ref = &yptr2, .tag = .ptr };
    var yptr4 = flag.ValuePtr{ .ref = &yptr3, .tag = .ptr };

    try s.variable(&xptr, "foo", "a nested pointer flag");
    try s.variable(&yptr4, "bar", "a *really* nested pointer flag");

    try s.set("foo", "50");
    try s.set("bar", "1000");

    try testing.expectEqual(s.lookup("foo").?.value.deref(.uint), 50);
    try testing.expectEqual(s.lookup("bar").?.value.deref(.uint), 1000);
}

// TODO: add tests for custom flag types (basic + one using builtin json)

test "Custom flag type" {
    const json = std.json;

    const AnyJson = struct {
        pub const vtable: flag.Custom.VTable = .{
            .set = set,
            .print = print,
        };

        allocator: mem.Allocator,
        value: ?json.Parsed(json.Value) = null,

        pub fn init(allocator: mem.Allocator) @This() {
            return .{
                .allocator = allocator,
            };
        }

        pub fn deinit(this: @This()) void {
            if (this.value) |v| {
                v.deinit();
            }
        }

        pub fn custom(this: *@This()) flag.Custom {
            return .{
                .ptr = this,
                .vtable = &vtable,
            };
        }

        pub fn set(ptr: *anyopaque, vs: []const u8) anyerror!void {
            const this = @as(*@This(), @ptrCast(@alignCast(ptr)));
            if (this.value) |v| {
                v.deinit();
            }
            this.value = try json.parseFromSlice(json.Value, this.allocator, vs, .{});
        }

        pub fn print(ptr: *anyopaque, w: io.AnyWriter) anyerror!void {
            const this = @as(*@This(), @ptrCast(@alignCast(ptr)));
            try json.stringify(this.value.?.value, .{}, w);
        }
    };

    var any_json = AnyJson.init(testing.allocator);
    defer any_json.deinit();

    var arr = std.ArrayList(u8).init(testing.allocator);
    defer arr.deinit();

    var s = flag.FlagSet.initWithOptions(testing.allocator, "", .{
        .output = arr.writer().any(),
    });
    defer s.deinit();

    try s.declare("json", any_json.custom(), "pass `json` message");

    _ = try s.parse(&.{
        \\-json={"foo":{"bar":0}}
        ,
        "-json",
        \\{"foo":{"bar":1}}
        ,
        \\--json={"foo":{"bar":2}}
        ,
        "--json",
        \\{"foo":{"bar":3}}
    });

    try testing.expectEqual(meta.activeTag(s.lookup("json").?.value), .custom);
    try s.lookup("json").?.value.custom.print(s.output);
    try testing.expectEqualStrings(
        \\{"foo":{"bar":3}}
    , arr.items);
}
