const std = @import("std");
const mem = std.mem;
const io = std.io;

const flag = @import("flag.zig");
const txt = @import("text.zig");
const log = @import("log.zig").log;
const Error = @import("error.zig").Error;

pub const Command = struct {
    name: []const u8,
    usage: []const u8,
    flags: flag.FlagSet,

    pub fn lessThan(_: void, lhs: *const Command, rhs: *const Command) bool {
        return std.ascii.lessThanIgnoreCase(lhs.name, rhs.name);
    }
};

pub const CommandGroup = struct {
    arena: std.heap.ArenaAllocator,
    name: []const u8,
    output: io.AnyWriter,
    global: ?flag.FlagSet = null, // References arena allocator so needs to be
    // initialized later
    commands_: ?std.ArrayList(Command) = null, // Same story as `global`

    ran_: ?[]const u8 = null, // the command that got ran
    vargs_: ?[]const [*:0]const u8 = null,

    pub const Result = enum {
        ran,
        help, // help for executed command was requested
        global_help, // global help was requested
    };

    pub const Options = struct {
        output: io.AnyWriter = io.getStdErr().writer().any(),
    };

    pub fn init(allocator: mem.Allocator, name: []const u8) CommandGroup {
        return initWithOptions(allocator, name, .{});
    }

    pub fn initWithOptions(allocator: mem.Allocator, name: []const u8, opts: Options) CommandGroup {
        const arena = std.heap.ArenaAllocator.init(allocator);
        return .{
            .arena = arena,
            .name = name,
            .output = opts.output,
        };
    }

    pub fn deinit(cg: CommandGroup) void {
        cg.arena.deinit();
    }

    /// Parses the arguments against the command group. The result is a tagged
    /// union returning the tags:
    ///
    /// "help"       storing the optional name of the subcommand that help was
    ///              requested on (null means it was requested at the top
    ///              level)
    /// "executed"   storing what command was executed as well as the remaining
    ///              positional arguments
    pub fn parse(cg: *CommandGroup, args_: []const [*:0]const u8) anyerror!Result {
        var args = cg.globalFlags().parse(args_) catch |e| if (e == Error.help) {
            try cg.summary();
            return .global_help;
        } else return e;
        if (args.len == 0) {
            try cg.summary();
            log.err("missing required subcommand", .{});
            return Error.expected_arguments;
        }

        const sc: []const u8 = mem.span(args[0]);
        if (cg.lookup(sc)) |fs| {
            cg.ran_ = sc;
            cg.vargs_ = fs.parse(args[1..]) catch |e| if (e == Error.help) {
                return .help;
            } else return e;
            return .ran;
        }

        // Command not found
        log.err("command provided but not defined: \"{s}\"", .{sc});
        return Error.no_such_command;
    }

    /// Returns the command that was ran if any
    pub fn ran(cg: CommandGroup) ?[]const u8 {
        return cg.ran_;
    }

    /// Returns the positional arguments passed to `ran` command
    pub fn vargs(cg: CommandGroup) ?[]const [*:0]const u8 {
        return cg.vargs_;
    }

    pub fn globalFlags(cg: *CommandGroup) *flag.FlagSet {
        cg.ensureGlobal();
        return &cg.global.?;
    }

    pub fn subcommand(cg: *CommandGroup, name: []const u8, usage: []const u8) mem.Allocator.Error!void {
        cg.ensureCommands();
        try cg.commands_.?.append(.{
            .name = name,
            .usage = usage,
            .flags = flag.FlagSet.initWithOptions(
                cg.arena.allocator(),
                name,
                .{
                    .output = cg.output,
                    .auto_print_usage = true,
                },
            ),
        });
    }

    pub fn lookup(cg: *CommandGroup, sc: []const u8) ?*flag.FlagSet {
        for (cg.commands_.?.items) |*c| {
            if (mem.eql(u8, sc, c.name)) {
                return &c.flags;
            }
        }
        return null;
    }

    pub fn summary(cg: *CommandGroup) anyerror!void {
        const flags = cg.globalFlags();
        if (flags.count() > 0) {
            try flags.usage();
        }

        if (cg.name.len == 0) {
            try cg.output.writeAll("Commands:\n");
        } else {
            try cg.output.print("Commands for {s}:\n", .{cg.name});
        }

        const cmds = try cg.commands(cg.arena.allocator());
        defer cg.arena.allocator().free(cmds);

        for (cmds) |c| {
            if (mem.containsAtLeastScalar(u8, c.usage, 1, '\n')) {
                try cg.output.print("  {s}\n    \t", .{c.name});
            } else {
                try cg.output.print("  {s}\t", .{c.name});
            }
            try txt.writeReplace(cg.output, c.usage, '\n', "\n    \t");
            try cg.output.writeByte('\n');
        }
    }

    /// Returns an owned copy of the commands, sorted lexicographically
    pub fn commands(cg: *CommandGroup, allocator: mem.Allocator) mem.Allocator.Error![]*Command {
        cg.ensureCommands();

        var cmds = try allocator.alloc(*Command, cg.commands_.?.items.len);

        for (cg.commands_.?.items, 0..) |*c, i| {
            cmds[i] = c;
        }

        mem.sort(*Command, cmds, {}, Command.lessThan);

        return cmds;
    }

    fn ensureGlobal(cg: *CommandGroup) void {
        if (cg.global == null) {
            cg.global = flag.FlagSet.initWithOptions(cg.arena.allocator(), cg.name, .{
                .output = cg.output,
                .auto_print_usage = false,
            });
        }
    }

    fn ensureCommands(cg: *CommandGroup) void {
        if (cg.commands_ == null) {
            cg.commands_ = std.ArrayList(Command).init(cg.arena.allocator());
        }
    }
};
