//! flag module - a port of Golang's builtin 'flag' package
const std = @import("std");

pub const FlagSet = @import("flag.zig").FlagSet;
pub const Flag = @import("flag.zig").Flag;
pub const Custom = @import("flag.zig").Custom;
pub const Value = @import("flag.zig").Value;
pub const ValuePtr = @import("flag.zig").ValuePtr;
pub const ValueType = @import("flag.zig").ValueType;
pub const CommandGroup = @import("group.zig").CommandGroup;
pub const Command = @import("group.zig").Command;
pub const Error = @import("error.zig").Error;

test {
    _ = @import("tests.zig");
}
