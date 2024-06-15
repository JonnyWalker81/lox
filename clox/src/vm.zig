const std = @import("std");
const chunk = @import("chunk.zig");
const debug = @import("debug.zig");
const build_options = @import("build_options");
const value = @import("value.zig");
const compiler = @import("compiler.zig");

pub const InterpretResult = enum {
    ok,
    compile_error,
    runtime_error,
};

const BinaryOp = enum {
    add,
    subtract,
    multiply,
    divide,
};

const StackSize = 256;

pub const VM = struct {
    const Self = @This();

    arena: std.heap.ArenaAllocator,
    chnk: ?*chunk.Chunk = null,
    ip: usize = 0,
    stack: [StackSize]value.Value,
    stackTop: usize = 0,
    comp: compiler.Compiler,

    pub fn init(allocator: std.mem.Allocator) *Self {
        var stack: [StackSize]value.Value = undefined;
        @memset(&stack, 0);

        const vm = allocator.create(Self) catch unreachable;
        vm.* = .{
            .arena = std.heap.ArenaAllocator.init(allocator),
            .stack = stack,
            .comp = compiler.Compiler.init(allocator),
        };
        return vm;
    }

    pub fn deinit(self: *Self) void {
        self.arena.deinit();
        if (self.chnk) |c| {
            c.deinit();
        }
    }

    fn resetStack(self: *Self) void {
        self.stackTop = 0;
    }

    fn push(self: *Self, v: value.Value) void {
        self.stack[self.stackTop] = v;
        self.stackTop += 1;
    }

    fn pop(self: *Self) value.Value {
        self.stackTop -= 1;
        return self.stack[self.stackTop];
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

    pub fn interpret(self: *Self, source: []const u8) !InterpretResult {
        const c = try self.comp.compile(source);
        defer c.deinit();

        self.chnk = c;
        self.ip = 0;
        return try self.run();
        // self.chnk = c;
        // self.ip = 0;
        // return try self.run();
    }

    fn run(self: *Self) !InterpretResult {
        while (true) {
            if (build_options.debug_trace_execution) {
                if (self.chnk) |c| {
                    _ = debug.disassembleInstruction(c, self.ip);
                }

                std.debug.print("          ", .{});
                for (0..self.stackTop) |i| {
                    std.debug.print("[ ", .{});
                    debug.printValue(self.stack[i]);
                    std.debug.print(" ]", .{});
                }
                std.debug.print("\n", .{});
            }
            const instruction: chunk.OpCode = @enumFromInt(self.readByte());
            switch (instruction) {
                .OpReturn => return try self.run_return(),
                .OpConstant => {
                    try self.run_constant();
                },
                .OpNegate => {
                    try self.run_negate();
                },
                .OpAdd => {
                    try self.run_binary_op(.add);
                },
                .OpSubtract => {
                    try self.run_binary_op(.subtract);
                },
                .OpMultiply => {
                    try self.run_binary_op(.multiply);
                },
                .OpDivide => {
                    try self.run_binary_op(.divide);
                },
            }
        }
    }

    fn run_binary_op(self: *Self, op: BinaryOp) !void {
        const b = self.pop();
        const a = self.pop();
        switch (op) {
            .add => {
                self.push(a + b);
            },
            .subtract => {
                self.push(a - b);
            },
            .multiply => {
                self.push(a * b);
            },
            .divide => {
                self.push(a / b);
            },
        }
    }

    fn run_negate(self: *Self) !void {
        const a = self.pop();
        self.push(-a);
    }

    fn run_return(self: *Self) !InterpretResult {
        const val = self.pop();
        debug.printValue(val);
        std.debug.print("\n", .{});
        return InterpretResult.ok;
    }

    fn run_constant(self: *Self) !void {
        if (self.chnk) |c| {
            if (c.constants.values) |constants| {
                const constant = constants[self.readByte()];
                self.push(constant);
                // debug.printValue(constant);
                // std.debug.print("\n", .{});
            }
        }
    }
};
