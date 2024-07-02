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
    closure: value.Value,
    ip: usize = 0,
    // slots: [*]value.Value = undefined,
    slot: usize,
    slots: *[StackSize]value.Value,
};

pub fn clockNative(_: u8, _: []value.Value) value.Value {
    return .{ .number = @floatFromInt(@divFloor(std.time.milliTimestamp(), 1000)) };
}

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

        vm.defineNative("clock", clockNative) catch unreachable;
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
        std.debug.print(fmt, args);
        var i: i32 = @intCast(self.frameCount - 1);
        while (i >= 0) : (i -= 1) {
            const frame = &self.frames[@intCast(i)];
            const instruction = frame.ip - 1;
            const line = frame.closure.closure.function.chnk.lines.?[instruction];
            std.debug.print("[line {}] in ", .{line});
            if (frame.closure.closure.function.name.len == 0) {
                std.debug.print("script\n", .{});
            } else {
                std.debug.print("{s}()\n", .{frame.closure.closure.function.name});
            }
        }

        // const instruction = frame.ip - 1;
        // const line = frame.function.functionValue().chnk.lines.?[instruction];
        // std.debug.print("[line {}] in script\n", .{line});
        self.resetStack();
    }

    fn defineNative(self: *Self, name: []const u8, function: value.NativeFn) !void {
        self.push(.{ .string = name });
        self.push(.{ .native = .{
            .allocator = self.arena.allocator(),
            .function = function,
        } });
        // const val = .{ .native = .{ .function = function } };
        try self.globals.put(self.stack[0].stringValue(), self.stack[1]);
        _ = self.pop();
        _ = self.pop();
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
            // .function => |f| {
            //     // if (f.arity != argCount) {
            //     //     self.runtimeError("Expected {d} arguments but got {d}", .{ f.arity, argCount });
            //     //     return false;
            //     // }
            //     return self.call(f, argCount);
            // },
            .native => |n| {
                const native = n.function;
                // if (result.isError()) {
                //     self.runtimeError("Error in native function", .{});
                //     return false;
                // }
                const result = native(argCount, self.stack[self.stackTop - argCount .. self.stackTop]);
                self.stackTop -= argCount + 1;
                self.push(result);
                return true;
            },
            .closure => {
                const closure = callee.closure;
                return self.call(closure, argCount);
            },
            else => {
                self.runtimeError("Can only call functions and classes", .{});
                return false;
            },
        }
    }

    fn captureValue(self: *Self, local: *value.Value) *value.Upvalue {
        const upvalue = value.Upvalue.init(self.arena.allocator(), local);
        return upvalue;
    }

    fn call(self: *Self, c: value.Closure, argCount: u8) bool {
        if (c.function.arity != argCount) {
            self.runtimeError("Expected {d} arguments but got {d}.\n", .{ c.function.arity, argCount });
            return false;
        }

        if (self.frameCount == FrameMax) {
            self.runtimeError("Stack overflow", .{});
            return false;
        }

        const frame = &self.frames[self.frameCount];
        frame.* = .{
            .closure = .{ .closure = value.Closure.init(self.arena.allocator(), c.function) },
            .ip = 0,
            .slot = self.stackTop - argCount - 1,
            .slots = &self.stack,
        };
        self.frameCount += 1;
        return true;
    }

    fn readByte(self: *Self) u8 {
        const frame = &self.frames[self.frameCount - 1];
        std.debug.print("ip: {d} ", .{frame.ip});
        // std.debug.print("function: {any} ", .{frame.closure.closure.function});
        const b = frame.closure.closure.function.chnk.code.?[frame.ip];
        std.debug.print("byte: {d}", .{b});
        std.debug.print("{any}\n", .{frame.closure.closure.function.chnk.code.?});
        frame.ip += 1;
        return b;
    }

    fn readShort(self: *Self) u16 {
        const frame = &self.frames[self.frameCount - 1];
        frame.ip += 2;
        const b2: u16 = frame.closure.closure.function.chnk.code.?[frame.ip - 2];
        const b1: u16 = frame.closure.closure.function.chnk.code.?[frame.ip - 1];
        return b2 << 8 | b1;
    }

    pub fn interpret(self: *Self, source: []const u8) !InterpretResult {
        var c = try self.comp.compile(source);

        self.push(c);

        const closure = value.Closure.init(self.arena.allocator(), &c.function);
        _ = self.pop();
        self.push(.{ .closure = closure });

        _ = self.call(closure, 0);
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
                _ = debug.disassembleInstruction(frame.closure.closure.function.chnk, frame.ip);

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
                    std.debug.print("callee: {any}\n", .{callee});
                    if (!self.callValue(callee, argCount)) {
                        return InterpreterError.runtime_error;
                    }
                    frame = &self.frames[self.frameCount - 1];
                },
                .OpClosure => {
                    var function = self.read_constant();
                    const closure = .{ .closure = value.Closure.init(self.arena.allocator(), &function.function) };
                    self.push(closure);
                    for (0..closure.closure.upvalues.len) |i| {
                        const isLocal = self.readByte();
                        const index = self.readByte();
                        if (isLocal == 1) {
                            closure.closure.upvalues[i] = self.captureValue(&frame.slots[frame.slot + index]);
                        } else {
                            closure.closure.upvalues[i] = frame.closure.closure.upvalues[index];
                        }
                    }
                    // for (0..closure.function.arity) |i| {
                    //     const val = self.peek(closure.function.arity - i);
                    //     self.stack[self.stackTop - i - 1] = val;
                    // }
                },
                .OpReturn => {
                    const result = self.pop();
                    self.frameCount -= 1;
                    if (self.frameCount == 0) {
                        _ = self.pop();
                        return InterpretResult.ok;
                    }

                    self.stackTop = frame.slot;
                    self.push(result);
                    frame = &self.frames[self.frameCount - 1];
                },
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
                    self.push(frame.slots[frame.slot + slot]);
                },
                .OpSetLocal => {
                    const slot = self.readByte();
                    frame.slots[frame.slot + slot] = self.peek(0);
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
                .OpGetUpvalue => {
                    const slot = self.readByte();
                    self.push(frame.closure.closure.upvalues[slot].location.*);
                },
                .OpSetUpvalue => {
                    const slot = self.readByte();
                    frame.closure.closure.upvalues[slot].location.* = self.peek(0);
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
        std.debug.print("print: ", .{});
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
        const constant = frame.closure.closure.function.chnk.constants.items[b];

        self.push(constant);
        // debug.printValue(constant);
        // std.debug.print("\n", .{});
        // }
    }

    fn read_constant(self: *Self) value.Value {
        const frame = &self.frames[self.frameCount - 1];
        const b = self.readByte();
        return frame.closure.closure.function.chnk.constants.items[b];
    }
};
