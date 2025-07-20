const std = @import("std");
const mem = std.mem;

pub const Error = error{ help, parse, range, invalid, no_such_flag, no_such_command, expected_arguments } || mem.Allocator.Error;
