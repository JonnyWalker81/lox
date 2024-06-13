const std = @import("std");
const chunk = @import("chunk.zig");
const debug = @import("debug.zig");
const build_options = @import("build_options");

pub const InterpretResult = enum {
    ok,
    compile_error,
    runtime_error,
};

pub const VM = struct {
    const Self = @This();

    arena: std.heap.ArenaAllocator,
    chnk: ?*chunk.Chunk = null,
    ip: usize = 0,

    pub fn init(allocator: std.mem.Allocator) *Self {
        const vm = allocator.create(Self) catch unreachable;
        vm.* = .{
            .arena = std.heap.ArenaAllocator.init(allocator),
        };
        return vm;
    }

    pub fn deinit(self: *Self) void {
        self.arena.deinit();
        if (self.chnk) |c| {
            c.deinit();
        }
    }

    fn readByte(self: *Self) u8 {
        if (self.chnk) |c| {
            if (c.code) |code| {
                const b = code[self.ip];
                self.ip += 1;
                return b;
            }
        }

        return 0;
    }

    pub fn interpret(self: *Self, c: *chunk.Chunk) !InterpretResult {
        self.chnk = c;
        self.ip = 0;
        return try self.run();
    }

    fn run(self: *Self) !InterpretResult {
        while (true) {
            if (build_options.debug_trace_execution) {
                if (self.chnk) |c| {
                    _ = debug.disassembleInstruction(c, self.ip);
                }
            }
            const instruction: chunk.OpCode = @enumFromInt(self.readByte());
            switch (instruction) {
                .OpReturn => return .ok,
                .OpConstant => {
                    try self.run_constant();
                },
            }
        }
    }

    fn run_constant(self: *Self) !void {
        if (self.chnk) |c| {
            if (c.constants.values) |constants| {
                const constant = constants[self.readByte()];
                debug.printValue(constant);
                std.debug.print("\n", .{});
            }
        }
    }
};
