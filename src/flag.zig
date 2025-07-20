const std = @import("std");

const mem = std.mem;
const io = std.io;
const fmt = std.fmt;
const meta = std.meta;
const debug = std.debug;

const txt = @import("text.zig");
const log = @import("log.zig").log;
const Error = @import("error.zig").Error;

pub const Custom = struct {
    ptr: *anyopaque,
    vtable: *const VTable,

    pub const VTable = struct {
        set: *const fn (*anyopaque, []const u8) anyerror!void,
        print: *const fn (*anyopaque, io.AnyWriter) anyerror!void,
    };

    pub fn set(value: Custom, value_str: []const u8) anyerror!void {
        return value.vtable.set(value.ptr, value_str);
    }

    pub fn print(value: Custom, writer: io.AnyWriter) anyerror!void {
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

    pub fn set(value: *Value, value_str: []const u8) anyerror!void {
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
                },
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
    auto_print_usage: bool,
    flags: std.StringHashMap(Flag),

    pub const Options = struct {
        output: io.AnyWriter = io.getStdErr().writer().any(),
        auto_print_usage: bool = false,
    };

    pub fn init(allocator: mem.Allocator, name: []const u8) FlagSet {
        return initWithOptions(allocator, name, .{});
    }

    pub fn initWithOptions(allocator: mem.Allocator, name: []const u8, opts: Options) FlagSet {
        return .{
            .allocator = allocator,
            .name = name,
            .output = opts.output,
            .auto_print_usage = opts.auto_print_usage,
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
                    const tmp = name[i + 1 ..];
                    name = name[0..i];
                    break :value tmp;
                }
            } else null;

            const fopt = fset.lookup(name);
            if (fopt == null) {
                if (meta.stringToEnum(enum { help, h }, name) != null) {
                    if (fset.auto_print_usage) {
                        try fset.usage();
                    }
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
                            log.err("invalid boolean value \"{s}\" for -{s}: {!}", .{ v, name, e });
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
                        log.err("invalid value \"{s}\" for flag -{s}: {!}", .{ v, name, e });
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

    pub fn set(fset: *FlagSet, name: []const u8, value_str: []const u8) anyerror!void {
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
            try txt.writeReplace(fset.output, start, '\n', "\n    \t");
            // Only print if the name was not synthesized and was actually
            // part of the usage message.
            if (hadq) {
                try txt.writeReplace(fset.output, name, '\n', "\n    \t");
            }
            try txt.writeReplace(fset.output, end, '\n', "\n    \t");

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

    /// Return the number of flags defined
    pub fn count(fset: FlagSet) usize {
        return @intCast(fset.flags.count());
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
                    return .{ true, f.usage[0..qs], f.usage[qs + 1 .. qe], f.usage[qe + 1 ..] };
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

// Internal Module tests

test "User storage" {
    const testing = std.testing;

    var fset = FlagSet.init(testing.allocator, "testcmd");
    defer fset.deinit();

    var x: i32 = 5;
    try fset.variable(&x, "x", "set the value of `x`");

    try testing.expect(fset.lookup("x").?.value.deref(.int) == 5);
    try fset.set("x", "20");
    try testing.expect(fset.lookup("x").?.value.deref(.int) == 20);
}
