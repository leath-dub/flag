//! flag module - a port of Golang's builtin 'flag' package
const std = @import("std");
const mem = std.mem;
const io = std.io;
const fmt = std.fmt;
const meta = std.meta;
const debug = std.debug;

pub const Error = error{ help, parse, range, invalid, no_such_flag } || mem.Allocator.Error;
pub const log = std.log.scoped(.flag);

pub const Custom = struct {
    ptr: *anyopaque,
    vtable: *const VTable,

    pub const VTable = struct {
        set: *const fn (*anyopaque, []const u8) Error!void,
        print: *const fn (*anyopaque, io.AnyWriter) io.AnyWriter.Error!void,
    };

    pub fn set(value: Custom, value_str: []const u8) Error!void {
        return value.vtable.set(value.ptr, value_str);
    }

    pub fn print(value: Custom, writer: io.AnyWriter) io.AnyWriter.Error!void {
        return value.vtable.print(value.ptr, writer);
    }
};

pub const ValueType = enum { ptr, int, uint, string, boolean, float, custom };

pub const ValuePtr = struct {
    ref: *anyopaque,
    tag: ValueType,

    // This is a safe cast function to allow you to get the concrete type
    pub inline fn reify(ptr: ValuePtr, comptime as: ValueType) *@FieldType(Value, @tagName(as)) {
        if (ptr.tag != as) {
            @panic("(flag) tried cast opaque pointer into the incorrect type");
        }
        return @as(*@FieldType(Value, @tagName(as)), @ptrCast(@alignCast(ptr.ref)));
    }
};

pub const Value = union(ValueType) {
    ptr: ValuePtr,
    int: i32,
    uint: u32,
    string: []const u8,
    boolean: bool,
    float: f32,
    custom: Custom,

    pub fn set(value: *Value, value_str: []const u8) Error!void {
        switch (meta.activeTag(value.*)) {
            .ptr => {
                var vptr = value.ptr;
                switch (vptr.tag) {
                    inline else => |ptag| {
                        const real = vptr.reify(ptag);
                        if (@TypeOf(real) == *ValuePtr) {
                            var tmp = Value{ .ptr = real.* };
                            try tmp.set(value_str);
                        } else {
                            real.* = try parseValue(ptag, value_str);
                        }
                    },
                }
            },
            .custom => try value.custom.set(value_str),
            inline else => |tag| (&@field(value, @tagName(tag))).* = try parseValue(tag, value_str),
        }
    }

    fn parseValue(comptime tag: ValueType, value_str: []const u8) Error!@FieldType(Value, @tagName(tag)) {
        return switch (tag) {
            .int => fmt.parseInt(i32, value_str, 10) catch return Error.parse,
            .uint => fmt.parseUnsigned(u32, value_str, 10) catch return Error.parse,
            .string => value_str,
            .boolean => parseBool(value_str) orelse return Error.parse,
            .float => fmt.parseFloat(f32, value_str) catch return Error.parse,
            else => @panic("(flag) 'parseValue' was called on a non primitive value type"),
        };
    }

    // Transitive dereference of a pointer value
    pub fn deref(value: Value, comptime tag: ValueType) @FieldType(Value, @tagName(tag)) {
        return val: switch (value) {
            .ptr => |p| switch (p.tag) {
                inline else => |pt| {
                    if (pt == .ptr) {
                        continue :val @unionInit(Value, @tagName(pt), p.reify(pt).*);
                    } else {
                        return p.reify(tag).*;
                    }
                }
            },
            inline else => @panic("(flag) deref was called on non pointer value type"),
        };
    }

    /// Checks whether value stored is given value type. This is like checking
    /// the tag directly but addition checks for pointer tags if the transitive
    /// type is the given value type.
    pub fn is(value: Value, comptime tag: ValueType) bool {
        return val: switch (value) {
            .ptr => |p| switch (p.tag) {
                inline else => |pt| continue :val @unionInit(Value, @tagName(pt), p.reify(pt).*),
            },
            inline else => meta.activeTag(value) == tag,
        };
    }

    pub fn print(value: Value, w: io.AnyWriter) io.AnyWriter.Error!void {
        val: switch (value) {
            .ptr => |p| switch (p.tag) {
                inline else => |pt| continue :val @unionInit(Value, @tagName(pt), p.reify(pt).*),
            },
            inline .int, .uint, .float => |n| try w.print("{d}", .{n}),
            .string => |s| try w.print("{s}", .{s}),
            .boolean => |b| try w.print("{any}", .{b}),
            .custom => |c| try c.print(w),
        }
    }
};

pub const Flag = struct {
    name: []const u8,
    usage: []const u8,
    value: Value,

    pub fn lessThan(_: void, lhs: *const Flag, rhs: *const Flag) bool {
        return std.ascii.lessThanIgnoreCase(lhs.name, rhs.name);
    }
};

pub const FlagSet = struct {
    allocator: mem.Allocator,
    name: []const u8,
    output: io.AnyWriter,
    flags: std.StringHashMap(Flag),

    pub const Options = struct {
        output: io.AnyWriter = io.getStdErr().writer().any(),
    };

    pub fn init(allocator: mem.Allocator, name: []const u8) FlagSet {
        return initWithOptions(allocator, name, .{});
    }

    pub fn initWithOptions(allocator: mem.Allocator, name: []const u8, opts: Options) FlagSet {
        return .{
            .allocator = allocator,
            .name = name,
            .output = opts.output,
            .flags = @FieldType(FlagSet, "flags").init(allocator),
        };
    }

    pub fn deinit(fset: *FlagSet) void {
        fset.flags.clearAndFree();
        fset.flags.deinit();
    }

    pub fn int(fset: *FlagSet, name: []const u8, value: i32, usage_: []const u8) Error!void {
        try fset.declare(name, value, usage_);
    }

    pub fn uint(fset: *FlagSet, name: []const u8, value: u32, usage_: []const u8) Error!void {
        try fset.declare(name, value, usage_);
    }

    pub fn string(fset: *FlagSet, name: []const u8, value: []const u8, usage_: []const u8) Error!void {
        try fset.declare(name, value, usage_);
    }

    pub fn boolean(fset: *FlagSet, name: []const u8, value: bool, usage_: []const u8) Error!void {
        try fset.declare(name, value, usage_);
    }

    pub fn float(fset: *FlagSet, name: []const u8, value: f32, usage_: []const u8) Error!void {
        try fset.declare(name, value, usage_);
    }

    pub fn intVar(fset: *FlagSet, value: *i32, name: []const u8, init_value: i32, usage_: []const u8) Error!void {
        value.* = init_value;
        try fset.variable(value, name, usage_);
    }

    pub fn uintVar(fset: *FlagSet, value: *u32, name: []const u8, init_value: u32, usage_: []const u8) Error!void {
        value.* = init_value;
        try fset.variable(value, name, usage_);
    }

    pub fn stringVar(fset: *FlagSet, value: *[]const u8, name: []const u8, init_value: []const u8, usage_: []const u8) Error!void {
        value.* = init_value;
        try fset.variable(value, name, usage_);
    }

    pub fn booleanVar(fset: *FlagSet, value: *bool, name: []const u8, init_value: bool, usage_: []const u8) Error!void {
        value.* = init_value;
        try fset.variable(value, name, usage_);
    }

    pub fn floatVar(fset: *FlagSet, value: *f32, name: []const u8, init_value: f32, usage_: []const u8) Error!void {
        value.* = init_value;
        try fset.variable(value, name, usage_);
    }

    pub fn declare(fset: *FlagSet, name: []const u8, value: anytype, usage_: []const u8) Error!void {
        try fset.flag(valueFrom(value), name, usage_);
    }

    /// Add a new flag which references memory owned by the caller. This allows for user variables
    /// to store the values of the flags transparently.
    pub fn variable(fset: *FlagSet, value: anytype, name: []const u8, usage_: []const u8) Error!void {
        if (comptime !isNonConstPtr(@TypeOf(value))) {
            @compileError("(flag) 'value' parameter must be a non-const single element pointer type");
        }
        const value_field = comptime getValueFieldName(@typeInfo(@TypeOf(value)).pointer.child) orelse {
            @compileError("(flag) the 'value' parameter's pointed to type does not match any valid 'Value' type");
        };

        try fset.flag(.{
            .ptr = .{
                .ref = @ptrCast(value),
                .tag = meta.stringToEnum(ValueType, value_field).?,
            },
        }, name, usage_);
    }

    pub fn parse(fset: FlagSet, args: []const [*:0]const u8) anyerror![]const [*:0]const u8 {
        var it: usize = 0;
        while (it != args.len) {
            var s: []const u8 = mem.span(args[it]);
            if (s.len < 2 or s[0] != '-') {
                // It is clearly not a flag, stop parsing flags
                break;
            }
            var minus_count: usize = 1;
            if (s[1] == '-') {
                // "--" terminates
                if (s.len == 2) {
                    break;
                }
                minus_count += 1;
            }
            var name = s[minus_count..];
            if (name.len == 0 or name[0] == '-' or name[0] == '=') {
                log.err("bad flag syntax: {s}", .{s});
                return Error.parse;
            }
            // consume the flag
            it += 1;
            const value = value: for (name, 0..) |c, i| {
                if (c == '=') {
                    const tmp = name[i + 1..];
                    name = name[0..i];
                    break :value tmp;
                }
            } else null;

            const fopt = fset.lookup(name);
            if (fopt == null) {
                if (meta.stringToEnum(enum { @"help", @"h" }, name) != null) {
                    try fset.usage();
                    return Error.help;
                }
                log.err("flag provided but not defined: -{s}", .{name});
                return Error.no_such_flag;
            }

            const f = fopt.?;
            switch (f.value) {
                .boolean => {
                    if (value) |v| {
                        f.value.set(v) catch |e| {
                            log.err("invalid boolean value \"{s}\" for -{s}: {!}", .{v, name, e});
                            return e;
                        };
                    } else {
                        f.value.set("true") catch unreachable;
                    }
                },
                else => {
                    const v = value orelse fallback: {
                        if (it < args.len) {
                            defer it += 1;
                            break :fallback mem.span(args[it]);
                        } else {
                            log.err("flag needs argument: -{s}", .{name});
                            return Error.parse;
                        }
                    };
                    f.value.set(v) catch |e| {
                        log.err("invalid value \"{s}\" for flag -{s}: {!}", .{v, name, e});
                        return e;
                    };
                },
            }
        }
        return args[it..];
    }

    pub fn lookup(fset: FlagSet, name: []const u8) ?*Flag {
        return fset.flags.getPtr(name);
    }

    pub fn set(fset: *FlagSet, name: []const u8, value_str: []const u8) Error!void {
        const flag_ = fset.lookup(name) orelse {
            log.err("no such flag -{s}", .{name});
            return Error.no_such_flag;
        };
        try flag_.value.set(value_str);
    }

    pub fn defaults(fset: FlagSet) anyerror!void {
        const flags = try fset.items(fset.allocator);
        defer fset.allocator.free(flags);

        for (flags) |f| {
            try fset.output.print("  -{s}", .{f.name});
            const hadq, const start, const name, const end = unquoteUsage(f.*);
            if (name.len > 0) {
                try fset.output.print(" {s}", .{name});
            }

            // Boolean flags are returned as empty string from 'unquoteUsage'
            if (name.len == 0) {
                try fset.output.writeByte('\t');
            } else {
                try fset.output.writeAll("\n    \t");
            }

            // Output the usage message. This is done piece by piece to avoid
            // needing to allocate a new string.
            try writeReplace(fset.output, start, '\n', "\n    \t");
            // Only print if the name was not synthesized and was actually
            // part of the usage message.
            if (hadq) {
                try writeReplace(fset.output, name, '\n', "\n    \t");
            }
            try writeReplace(fset.output, end, '\n', "\n    \t");

            if (!isZeroValue(f.value)) {
                try fset.output.writeAll(" (default ");
                if (f.value.is(.string)) {
                    try fset.output.writeByte('"');
                }
                try f.value.print(fset.output);
                if (f.value.is(.string)) {
                    try fset.output.writeByte('"');
                }
                try fset.output.writeByte(')');
            }

            try fset.output.writeByte('\n');
        }
    }

    pub fn usage(fset: FlagSet) anyerror!void {
        if (fset.name.len == 0) {
            try fset.output.writeAll("Usage:\n");
        } else {
            try fset.output.print("Usage of {s}:\n", .{fset.name});
        }
        try fset.defaults();
    }

    /// Returns the flags sorted lexicographically by their names.
    /// The returned slice is owned by the callee.
    pub fn items(fset: FlagSet, allocator: mem.Allocator) mem.Allocator.Error![]*Flag {
        var flags = try allocator.alloc(*Flag, fset.flags.count());

        var i: usize = 0;
        var it = fset.flags.iterator();
        while (it.next()) |f| : (i += 1) {
            flags[i] = f.value_ptr;
        }

        mem.sort(*Flag, flags, {}, Flag.lessThan);

        return flags;
    }

    fn flag(fset: *FlagSet, value: Value, name: []const u8, usage_: []const u8) Error!void {
        if (name.len != 0 and name[0] == '-') {
            log.err("flag {s} begins with -", .{name});
            return Error.invalid;
        }
        if (mem.containsAtLeastScalar(u8, name, 1, '=')) {
            log.err("flag {s} contains =", .{name});
            return Error.invalid;
        }

        const view = try fset.flags.getOrPut(name);
        if (view.found_existing) {
            if (view.value_ptr.name.len == 0) {
                log.err("flag redefined: {s}", .{name});
            } else {
                log.err("{s} flag redefined: {s}", .{ view.value_ptr.name, name });
            }
            return Error.invalid;
        }

        view.value_ptr.* = .{
            .name = name,
            .usage = usage_,
            .value = value,
        };
    }

    fn unquoteUsage(f: Flag) meta.Tuple(&.{ bool, []const u8, []const u8, []const u8 }) {
        // Try find text in quotes
        if (mem.indexOfScalar(u8, f.usage, '`')) |qs| {
            if (qs + 1 < f.usage.len) {
                if (mem.indexOfScalarPos(u8, f.usage, qs + 1, '`')) |qe| {
                    return .{ true, f.usage[0..qs], f.usage[qs + 1 .. qe], f.usage[qe + 1..] };
                }
            }
        }
        // No explicit name, fallback to type.
        return val: switch (f.value) {
            .ptr => |p| switch (p.tag) {
                inline else => |pt| continue :val @unionInit(Value, @tagName(pt), p.reify(pt).*),
            },
            .custom => .{ false, f.usage, "value", "" },
            .boolean => .{ false, f.usage, "", "" },
            .int => .{ false, f.usage, "int", "" },
            .uint => .{ false, f.usage, "uint", "" },
            .float => .{ false, f.usage, "float", "" },
            .string => .{ false, f.usage, "string", "" },
        };
    }

    fn isNonConstPtr(comptime T: type) bool {
        const valinfo = @typeInfo(T);
        return valinfo == .pointer and valinfo.pointer.size == .one and !valinfo.pointer.is_const;
    }
};

fn valueFrom(value: anytype) Value {
    if (comptime getValueFieldName(@TypeOf(value))) |name| {
        return @unionInit(Value, name, value);
    }
    @compileError("(flag) cast cannot be performed: the 'value' parameter's type does not match any valid 'Value'");
}

/// Checks if type can implicitly cast into a []const u8
fn isImpliedString(comptime T: type) bool {
    switch (@typeInfo(T)) {
        .pointer => |p| {
            if (p.size == .one) {
                switch (@typeInfo(p.child)) {
                    .array => |a| return a.child == u8,
                    else => {},
                }
            }
            if (p.size == .slice) {
                return p.child == u8;
            }
        },
        else => {},
    }
    return false;
}

fn getValueFieldName(comptime T: type) ?[:0]const u8 {
    if (isImpliedString(T)) return "string";
    if (T == comptime_int) return "int";
    if (T == comptime_float) return "float";

    inline for (@typeInfo(Value).@"union".fields) |vfield| {
        if (T == vfield.type) {
            return vfield.name;
        }
    }
    return null;
}

/// A equivalent to golang's strconv.ParseBool function.
/// accepts any of 1, t, T, TRUE, true, True, 0, f, F, FALSE, false, False.
fn parseBool(s: []const u8) ?bool {
    const True = enum { @"1", t, T, TRUE, true, True };
    const False = enum { @"0", f, F, FALSE, false, False };
    if (meta.stringToEnum(True, s)) |_| return true;
    if (meta.stringToEnum(False, s)) |_| return false;
    return null;
}

fn writeReplace(w: io.AnyWriter, s: []const u8, repl: u8, with: []const u8) io.AnyWriter.Error!void {
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

fn isZeroValue(value: Value) bool {
    return val: switch (value) {
        .ptr => |p| switch (p.tag) {
            inline else => |pt| continue :val @unionInit(Value, @tagName(pt), p.reify(pt).*),
        },
        inline .int, .uint, .float => |n| n == 0,
        .string => |s| s.len == 0,
        .boolean => |b| b == false,
        .custom => false,
    };
}

test "writeReplace helper" {
    const testing = @import("std").testing;

    var arr = std.ArrayList(u8).init(testing.allocator);
    defer arr.deinit();

    try writeReplace(arr.writer().any(), "first line\nsecond line\nthird line", '\n', "\n    \t");
    try testing.expect(mem.eql(u8, arr.items, "first line\n    \tsecond line\n    \tthird line"));

    arr.clearRetainingCapacity();

    try writeReplace(arr.writer().any(), "", '\n', "\n    \t");
    try testing.expect(mem.eql(u8, arr.items, ""));
}

test "User storage" {
    const testing = @import("std").testing;

    var fset = FlagSet.init(testing.allocator, "testcmd");
    defer fset.deinit();

    var x: i32 = 5;
    try fset.variable(&x, "x", "set the value of `x`");

    try testing.expect(fset.lookup("x").?.value.deref(.int) == 5);
    try fset.set("x", "20");
    try testing.expect(fset.lookup("x").?.value.deref(.int) == 20);
}

test "Builtin types" {
    const testing = @import("std").testing;

    var s = FlagSet.init(testing.allocator, "");
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
        try testing.expect(switch (meta.stringToEnum(ValueType, f.name).?) {
            .int => f.value.int == 0,
            .uint => f.value.uint == 0,
            .string => f.value.string.len == 0,
            .boolean => f.value.boolean == false,
            .float => f.value.float == 0,
            else => unreachable,
        });
    }

    try s.set("int", "1");
    try s.set("uint", "1");
    try s.set("string", "1");
    try s.set("boolean", "true");
    try s.set("float", "1");

    testing.allocator.free(fs);

    fs = try s.items(testing.allocator);

    for (fs) |f| {
        try testing.expect(switch (meta.stringToEnum(ValueType, f.name).?) {
            .int => f.value.int == 1,
            .uint => f.value.uint == 1,
            .string => f.value.string[0] == '1',
            .boolean => f.value.boolean == true,
            .float => f.value.float == 1,
            else => unreachable,
        });
    }

    testing.allocator.free(fs);
}

test "Pointer to all builtin types" {
    const testing = @import("std").testing;

    var s = FlagSet.init(testing.allocator, "");
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

    try testing.expect(s.lookup("int").?.value.deref(.int) == 0);
    try testing.expect(s.lookup("uint").?.value.deref(.uint) == 0);
    try testing.expect(s.lookup("string").?.value.deref(.string).len == 0);
    try testing.expect(s.lookup("boolean").?.value.deref(.boolean) == false);
    try testing.expect(s.lookup("float").?.value.deref(.float) == 0);

    try testing.expect(s.lookup("int_").?.value.deref(.int) == 0);
    try testing.expect(s.lookup("uint_").?.value.deref(.uint) == 0);
    try testing.expect(s.lookup("string_").?.value.deref(.string).len == 0);
    try testing.expect(s.lookup("boolean_").?.value.deref(.boolean) == false);
    try testing.expect(s.lookup("float_").?.value.deref(.float) == 0);

    try s.set("int", "1");
    try s.set("uint", "1");
    try s.set("string", "1");
    try s.set("boolean", "true");
    try s.set("float", "1");

    try testing.expect(s.lookup("int").?.value.deref(.int) == 1);
    try testing.expect(s.lookup("uint").?.value.deref(.uint) == 1);
    try testing.expect(s.lookup("string").?.value.deref(.string)[0] == '1');
    try testing.expect(s.lookup("boolean").?.value.deref(.boolean) == true);
    try testing.expect(s.lookup("float").?.value.deref(.float) == 1);

    try testing.expect(s.lookup("int_").?.value.deref(.int) == 1);
    try testing.expect(s.lookup("uint_").?.value.deref(.uint) == 1);
    try testing.expect(s.lookup("string_").?.value.deref(.string)[0] == '1');
    try testing.expect(s.lookup("boolean_").?.value.deref(.boolean) == true);
    try testing.expect(s.lookup("float_").?.value.deref(.float) == 1);
}

// To be honest I don't know why I allow this. But it is allowed so I may
// aswell write a test case for sanity to see that you can have arbitrary
// pointer nests.
test "Nested Pointer" {
    const testing = @import("std").testing;

    var s = FlagSet.init(testing.allocator, "");
    defer s.deinit();

    var x: u32 = 10;
    var xptr = ValuePtr { .ref = &x, .tag = .uint };

    var y: u32 = 10;
    var yptr0 = ValuePtr { .ref = &y, .tag = .uint };
    var yptr1 = ValuePtr { .ref = &yptr0, .tag = .ptr };
    var yptr2 = ValuePtr { .ref = &yptr1, .tag = .ptr };
    var yptr3 = ValuePtr { .ref = &yptr2, .tag = .ptr };
    var yptr4 = ValuePtr { .ref = &yptr3, .tag = .ptr };

    try s.variable(&xptr, "foo", "a nested pointer flag");
    try s.variable(&yptr4, "bar", "a *really* nested pointer flag");

    try s.set("foo", "50");
    try s.set("bar", "1000");

    try testing.expect(s.lookup("foo").?.value.deref(.uint) == 50);
    try testing.expect(s.lookup("bar").?.value.deref(.uint) == 1000);
}

// TODO: add tests for custom flag types (basic + one using builtin json)
