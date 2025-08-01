const std = @import("std");
const flag = @import("flag");

pub fn main() !void {
    // You can also initialize with options:
    // flag.FlagSet.initWithOptions(std.heap.smp_allocator, "", .{
    //     .output = <some io.AnyWriter>,
    //     .auto_print_usage = false,
    // })
    //
    // Empty string ("") name changes usage message to not print name
    var flags = flag.FlagSet.init(std.heap.smp_allocator, "mycmd");

    // You can use `declare()` to declare any flags
    // contents backticks (``) in usage string define how the type is printed
    // in the usage message.
    try flags.declare("x", 10, "`x` is my flag");
    // You can also more explicit (don't use `anytype`)
    try flags.int("y", 0, "this describes the vertical component");

    // You can also define "variable" flags, these are flags whos value can
    // be stored external to the flag set itself
    var my_x: u32 = 10;
    try flags.variable(&my_x, "my_x", "a more *indirect* x");
    // Could also call `flag.intVar` instead ...

    const vargs = try flags.parse(std.os.argv[1..]);
    _ = vargs; // `vargs` stores the positional arguments after parsing
               // the flags

    // Print the usage message (also works if user passes flag 'h' or 'help')
    try flags.usage();
}
