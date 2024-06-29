const std = @import("std");
const chunk = @import("chunk.zig");
const debug = @import("debug.zig");
const build_options = @import("build_options");
const value = @import("value.zig");
const compiler = @import("compiler.zig");

pub const InterpreterError = error{
    compile_error,
    runtime_error,
};

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
    greater,
    less,
};

const FrameMax = 64;
const StackSize = FrameMax * 256;

pub const CallFrame = struct {
    function: value.Value,
    ip: usize = 0,
    slots: [*]value.Value = undefined,
};

pub const VM = struct {
    const Self = @This();

    arena: std.heap.ArenaAllocator,
    // chnk: ?*chunk.Chunk = null,
    // ip: usize = 0,
    stack: [StackSize]value.Value,
    stackTop: usize = 0,
    comp: compiler.Compiler,
    globals: std.StringHashMap(value.Value),
    frames: [FrameMax]CallFrame,
    frameCount: usize = 0,
    // strings: std.StringHashMap(void),

    pub fn init(allocator: std.mem.Allocator) *Self {
        var stack: [StackSize]value.Value = undefined;
        @memset(&stack, undefined);

        var frames: [FrameMax]CallFrame = undefined;
        @memset(&frames, undefined);

        const vm = allocator.create(Self) catch unreachable;
        vm.* = .{
            .arena = std.heap.ArenaAllocator.init(allocator),
            .stack = stack,
            .comp = compiler.Compiler.init(allocator),
            .globals = std.StringHashMap(value.Value).init(allocator),
            .frames = frames,
            // .strings = std.StringHashMap(void).init(allocator),
        };
        return vm;
    }

    pub fn deinit(self: *Self) void {
        self.arena.deinit();
        // if (self.chnk) |c| {
        //     c.deinit();
        // }

        self.globals.deinit();
    }

    fn resetStack(self: *Self) void {
        self.stackTop = 0;
    }

    fn runtimeError(self: *Self, comptime fmt: []const u8, args: anytype) void {
        const frame = &self.frames[self.frameCount - 1];
        std.debug.print(fmt, args);
        const instruction = frame.ip - 1;
        const line = frame.function.functionValue().chnk.lines.?[instruction];
        std.debug.print("[line {}] in script\n", .{line});
        self.resetStack();
    }

    fn push(self: *Self, v: value.Value) void {
        self.stack[self.stackTop] = v;
        self.stackTop += 1;
    }

    fn pop(self: *Self) value.Value {
        self.stackTop -= 1;
        return self.stack[self.stackTop];
    }

    fn peek(self: *Self, distance: usize) value.Value {
        return self.stack[self.stackTop - 1 - distance];
    }

    fn callValue(self: *Self, callee: value.Value, argCount: u8) bool {
        switch (callee) {
            .function => |f| {
                // if (f.arity != argCount) {
                //     self.runtimeError("Expected {d} arguments but got {d}", .{ f.arity, argCount });
                //     return false;
                // }
                return self.call(f, argCount);
            },
            else => {
                self.runtimeError("Can only call functions and classes", .{});
                return false;
            },
        }
    }

    fn call(self: *Self, f: value.Function, argCount: u8) bool {
        if (f.arity != argCount) {
            self.runtimeError("Expected {d} arguments but got {d}", .{ f.arity, argCount });
            return false;
        }

        if (self.frameCount == FrameMax) {
            self.runtimeError("Stack overflow", .{});
            return false;
        }

        const frame = &self.frames[self.frameCount];
        frame.* = .{
            .function = .{ .function = f },
            .ip = 0,
            .slots = @ptrCast(&self.stack[self.stackTop - argCount - 1]),
        };
        self.frameCount += 1;
        return true;
    }

    fn readByte(self: *Self) u8 {
        const frame = &self.frames[self.frameCount - 1];
        const b = frame.function.functionValue().chnk.code.?[frame.ip];
        frame.ip += 1;
        return b;
    }

    fn readShort(self: *Self) u16 {
        const frame = &self.frames[self.frameCount - 1];
        frame.ip += 2;
        const b2: u16 = frame.function.functionValue().chnk.code.?[frame.ip - 2];
        const b1: u16 = frame.function.functionValue().chnk.code.?[frame.ip - 1];
        return b2 << 8 | b1;
    }

    pub fn interpret(self: *Self, source: []const u8) !InterpretResult {
        const c = try self.comp.compile(source);

        self.push(c);

        _ = self.call(c.functionValue(), 0);
        // frame.function = c.functionValue();
        // frame.ip = 0;
        // frame.slots = &self.stack;

        return try self.run();
    }

    fn run(self: *Self) !InterpretResult {
        var frame = &self.frames[self.frameCount - 1];
        while (true) {
            if (build_options.debug_trace_execution) {
                // std.debug.print("chnk: {any} ip: {d} ", .{ frame, frame.ip });
                _ = debug.disassembleInstruction(frame.function.functionValue().chnk, frame.ip);

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
                .OpPrint => {
                    try self.run_print();
                },
                .OpJump => {
                    const offset = self.readShort();
                    frame.ip += offset;
                },
                .OpJumpIfFalse => {
                    const offset = self.readShort();
                    const val = self.peek(0);
                    if (val.isFalsey()) {
                        frame.ip += offset;
                    }
                },
                .OpLoop => {
                    const offset = self.readShort();
                    frame.ip -= offset;
                },
                .OpCall => {
                    const argCount = self.readByte();
                    const callee = self.peek(argCount);
                    if (!self.callValue(callee, argCount)) {
                        return InterpreterError.runtime_error;
                    }
                    frame = &self.frames[self.frameCount - 1];
                },
                .OpReturn => return InterpretResult.ok,
                .OpConstant => {
                    try self.run_constant();
                },
                .OpNil => {
                    self.push(.nil);
                },
                .OpTrue => {
                    self.push(.{ .bool = true });
                },
                .OpFalse => {
                    self.push(.{ .bool = false });
                },
                .OpPop => {
                    _ = self.pop();
                },
                .OpGetLocal => {
                    const slot = self.readByte();
                    self.push(frame.slots[slot]);
                },
                .OpSetLocal => {
                    const slot = self.readByte();
                    frame.slots[slot] = self.peek(0);
                },
                .OpGetGlobal => {
                    const name = self.read_constant();
                    if (self.globals.contains(name.stringValue())) {
                        const val = self.globals.get(name.stringValue());
                        self.push(val.?);
                    } else {
                        self.runtimeError("Undefined variable '{s}'", .{name.stringValue()});
                        return InterpreterError.runtime_error;
                    }
                },
                .OpDefineGlobal => {
                    const name = self.read_constant();
                    const val = self.peek(0);
                    try self.globals.put(name.stringValue(), val);
                    _ = self.pop();
                    // try self.strings.put(name.stringValue(), void{});
                    // debug.printValue(name);
                    // std.debug.print("\n", .{});
                },
                .OpSetGlobal => {
                    const name = self.read_constant();
                    if (self.globals.contains(name.stringValue())) {
                        const val = self.peek(0);
                        try self.globals.put(name.stringValue(), val);
                    } else {
                        self.runtimeError("Undefined variable '{s}'", .{name.stringValue()});
                        return InterpreterError.runtime_error;
                    }
                },
                .OpEqual => {
                    const b = self.pop();
                    const a = self.pop();
                    self.push(.{ .bool = a.equalTo(b) });
                },
                .OpGreater => {
                    try self.run_binary_op(.greater);
                },
                .OpLess => {
                    try self.run_binary_op(.less);
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
                .OpNot => {
                    try self.run_not();
                },
                .OpNegate => {
                    try self.run_negate();
                },
            }
        }
    }

    fn run_binary_op(self: *Self, op: BinaryOp) !void {
        if (!self.peek(0).isNumber() and self.peek(1).isNumber()) {
            self.runtimeError("Operands must be numbers", .{});
            return InterpreterError.runtime_error;
        }

        const b = self.pop();
        const a = self.pop();
        switch (op) {
            .greater => {
                const result = .{ .bool = a.numberValue() > b.numberValue() };
                self.push(result);
            },
            .less => {
                const result = .{ .bool = a.numberValue() < b.numberValue() };
                self.push(result);
            },
            .add => {
                if (a.isNumber() and b.isNumber()) {
                    const result = .{ .number = a.numberValue() + b.numberValue() };
                    self.push(result);
                    return;
                } else if (a.isString() and b.isString()) {
                    const s = try std.fmt.allocPrint(self.arena.allocator(), "{s}{s}", .{ a.stringValue(), b.stringValue() });
                    // try self.strings.put(s, void{});
                    const result = .{ .string = s };
                    self.push(result);
                    return;
                }

                self.runtimeError("Operands must be two numbers or two strings", .{});
                return InterpreterError.runtime_error;
            },
            .subtract => {
                const result = .{ .number = a.numberValue() - b.numberValue() };
                self.push(result);
            },
            .multiply => {
                const result = .{ .number = a.numberValue() * b.numberValue() };
                self.push(result);
            },
            .divide => {
                const result = .{ .number = a.numberValue() / b.numberValue() };
                self.push(result);
            },
        }
    }

    fn run_print(self: *Self) !void {
        const val = self.pop();
        debug.printValue(val);
        std.debug.print("\n", .{});
    }

    fn run_not(self: *Self) !void {
        const val = self.pop();
        self.push(.{ .bool = !val.isFalsey() });
    }

    fn run_negate(self: *Self) !void {
        switch (self.peek(0)) {
            .number => {
                const a = self.pop();
                self.push(.{ .number = -a.number });
            },
            else => {
                self.runtimeError("Operand must be a number", .{});
                return InterpreterError.runtime_error;
            },
        }
    }

    // fn run_return(self: *Self) !InterpretResult {
    //     const val = self.pop();
    //     debug.printValue(val);
    //     std.debug.print("\n", .{});
    //     return InterpretResult.ok;
    // }

    fn run_constant(self: *Self) !void {
        const frame = &self.frames[self.frameCount - 1];
        const b = self.readByte();
        const constant = frame.function.functionValue().chnk.constants.items[b];

        self.push(constant);
        // debug.printValue(constant);
        // std.debug.print("\n", .{});
        // }
    }

    fn read_constant(self: *Self) value.Value {
        const frame = &self.frames[self.frameCount - 1];
        const b = self.readByte();
        return frame.function.functionValue().chnk.constants.items[b];
    }
};
