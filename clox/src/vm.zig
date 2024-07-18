const std = @import("std");
const chunk = @import("chunk.zig");
const debug = @import("debug.zig");
const build_options = @import("build_options");
const value = @import("value.zig");
const compiler = @import("compiler.zig");
const memory = @import("memory.zig");

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
    const Self = @This();
    // closure: *const value.Value,
    closure: *value.Closure,
    ip: usize = 0,
    // slots: [*]value.Value = undefined,
    slot: usize,
    slots: *[StackSize]value.Value,

    pub fn dump(self: *Self) void {
        std.debug.print("frame: {d}\n", .{self.ip});
        const name = if (self.closure.function.name) |name| name.bytes else "";
        std.debug.print("  closure: {s}\n", .{name});
        std.debug.print("  code: {any}\n", .{self.closure.function.chnk.code.items});
        std.debug.print("\n", .{});
    }
};

pub fn clockNative(_: u8, _: []value.Value) value.Value {
    return .{ .number = @floatFromInt(@divFloor(std.time.milliTimestamp(), 1000)) };
}

var testFrame: CallFrame = undefined;

pub const VM = struct {
    const Self = @This();

    allocator: std.mem.Allocator = undefined,
    gcAllocator: memory.GCAllocator = undefined,
    // chnk: ?*chunk.Chunk = null,
    // ip: usize = 0,
    stack: [StackSize]value.Value,
    stackTop: usize = 0,
    comp: compiler.Compiler = undefined,
    strings: std.StringHashMap(*value.String) = undefined,
    // globals: std.AutoHashMap(*value.String, value.Value) = undefined,
    globals: std.AutoHashMap(*value.String, value.Value) = undefined,
    frames: [FrameMax]CallFrame,
    // frames: std.ArrayList(CallFrame),
    frameCount: usize = 0,
    // strings: std.StringHashMap(void),
    openUpvalues: ?*value.Upvalue = null,
    objects: ?*value.Obj = null,
    grayStack: std.ArrayList(*value.Obj) = undefined,
    initString: *value.String = undefined,

    pub fn init(allocator: std.mem.Allocator) Self {
        var stack: [StackSize]value.Value = undefined;
        @memset(&stack, undefined);

        var frames: [FrameMax]CallFrame = undefined;
        @memset(&frames, undefined);

        // const vm = allocator.create(Self) catch unreachable;
        const vm: VM = .{
            .allocator = allocator,
            .stack = stack,
            // .globals = std.AutoHashMap(*value.String, value.Value).init(allocator),
            .frames = frames,
            // .frames = std.ArrayList(CallFrame).init(allocator),
            // .strings = std.StringHashMap(void).init(allocator),
            // .grayStack = std.ArrayList(*value.Obj).init(allocator),
        };

        return vm;
    }

    pub fn setup(self: *Self) void {
        self.comp = compiler.Compiler.init(self, null, .script);
        self.gcAllocator = memory.GCAllocator.init(self.allocator, self);
        self.grayStack = std.ArrayList(*value.Obj).init(self.allocator);
        self.strings = std.StringHashMap(*value.String).init(self.gcAllocator.allocator());
        self.globals = std.AutoHashMap(*value.String, value.Value).init(self.gcAllocator.allocator());
        self.initString = value.String.init(self, "init") catch unreachable;

        self.defineNative("clock", clockNative) catch unreachable;
    }

    pub fn deinit(self: *Self) void {
        // if (self.chnk) |c| {
        //     c.deinit();
        // }

        // var iter = self.globals.iterator();
        // while (iter.next()) |entry| {
        //     entry.key_ptr().deinit();
        // }

        self.strings.deinit();
        self.globals.deinit();
        self.comp.deinit();
        self.grayStack.deinit();

        self.freeObjects();
    }

    fn freeObjects(self: *Self) void {
        // std.debug.print("freeObjects\n", .{});
        var obj = self.objects;
        while (obj) |o| {
            const next = o.next;
            o.destroy(self);
            obj = next;
        }

        // self.grayStack.deinit();
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
            const line = frame.closure.function.chnk.lines.items[instruction];
            std.debug.print("[line {}] in ", .{line});
            if (frame.closure.function.name) |s| {
                if (s.bytes.len == 0) {
                    std.debug.print("script\n", .{});
                } else {
                    std.debug.print("{s}()\n", .{s.bytes});
                }
            }
        }

        // const instruction = frame.ip - 1;
        // const line = frame.function.functionValue().chnk.lines.?[instruction];
        // std.debug.print("[line {}] in script\n", .{line});
        self.resetStack();
    }

    fn defineNative(self: *Self, name: []const u8, function: value.NativeFn) !void {
        const s = try value.String.init(self, name);
        const native = try value.Native.init(self, function);
        // defer s.string.deinit(self);

        self.push(s.obj.value());
        self.push(native.obj.value());
        // const val = .{ .native = .{ .function = function } };
        try self.globals.put(s, self.peek(0));
        _ = self.pop();
        _ = self.pop();
    }

    pub fn push(self: *Self, v: value.Value) void {
        self.stack[self.stackTop] = v;
        self.stackTop += 1;
    }

    pub fn pop(self: *Self) value.Value {
        self.stackTop -= 1;
        return self.stack[self.stackTop];
    }

    fn peek(self: *Self, distance: usize) value.Value {
        return self.stack[self.stackTop - 1 - distance];
    }

    fn callValue(self: *Self, callee: value.Value, argCount: u8) bool {
        switch (callee.asObject().type) {
            .native => {
                const native = callee.asObject().asNative().function;
                // if (result.isError()) {
                //     self.runtimeError("Error in native function", .{});
                //     return false;
                // }
                const result = native(argCount, self.stack[self.stackTop - argCount .. self.stackTop]);
                self.stackTop -= argCount + 1;
                self.push(result);
                return true;
            },
            .boundMethod => {
                const bound = callee.asObject().asBoundMethod();
                self.stack[self.stackTop - argCount - 1] = bound.receiver;
                return self.call(bound.method, argCount);
            },
            .class => {
                const class = callee.asObject().asClass();
                const instance = value.Instance.init(self, class) catch @panic("could not create instance.");
                self.stack[self.stackTop - argCount - 1] = instance.obj.value();

                if (class.methods.get(self.initString)) |initializer| {
                    return self.call(initializer.asObject().asClosure(), argCount);
                } else if (argCount != 0) {
                    self.runtimeError("Expected 0 arguments but got {d}.", .{argCount});
                    return false;
                }

                return true;
            },
            .closure => {
                const closure = callee.asObject().asClosure();
                return self.call(closure, argCount);
            },
            else => {
                self.runtimeError("Can only call functions and classes", .{});
                return false;
            },
        }
    }

    fn invokeFromClass(self: *Self, class: *value.Class, name: *value.String, argCount: u8) bool {
        std.debug.print("invokeFromClass: {s} -> {s}\n", .{ class.name.bytes, name.bytes });
        std.debug.print("   methods: {d}\n", .{class.methods.count()});
        var iter = class.methods.iterator();
        while (iter.next()) |entry| {
            std.debug.print("   method: {s}\n", .{entry.key_ptr.*.bytes});
        }

        if (class.methods.get(name)) |method| {
            return self.call(method.asObject().asClosure(), argCount);
        }

        self.runtimeError("Undefined property '{s}'", .{name.bytes});
        return false;
    }

    fn invoke(self: *Self, name: *value.String, argCount: u8) bool {
        const receiver = self.peek(argCount);

        if (!receiver.isObjType(.instance)) {
            self.runtimeError("Only instances have methods.", .{});
            return false;
        }

        const instance = receiver.asObject().asInstance();

        if (instance.fields.get(name)) |val| {
            self.stack[self.stackTop - argCount - 1] = val;
            return self.callValue(val, argCount);
        }

        return self.invokeFromClass(instance.class, name, argCount);
    }

    fn bindMethod(self: *Self, class: *value.Class, name: *value.String) !bool {
        if (class.methods.get(name)) |method| {
            const bound = try value.BoundMethod.init(self, self.peek(0), method.asObject().asClosure());

            _ = self.pop();
            self.push(bound.obj.value());
            return true;
        }

        self.runtimeError("Undefined property '{s}'", .{name.bytes});
        return false;
    }

    fn captureValue(self: *Self, local: *value.Value) !*value.Upvalue {
        var prevUpvalue: ?*value.Upvalue = null;
        var upvalue = self.openUpvalues;
        while (upvalue != null and @intFromPtr(upvalue.?.location) > @intFromPtr(local)) : (upvalue = upvalue.?.next) {
            // if (upvalue.location == local) {
            //     return upvalue;
            // }
            prevUpvalue = upvalue;
        }

        if (upvalue != null and @intFromPtr(upvalue.?.location) == @intFromPtr(local)) {
            return upvalue.?;
        }

        const createdUpvalue = try value.Upvalue.initUpval(self, local);
        createdUpvalue.next = upvalue;

        if (prevUpvalue == null) {
            self.openUpvalues = createdUpvalue;
        } else {
            prevUpvalue.?.next = createdUpvalue;
        }

        return createdUpvalue;
    }

    fn closeUpvalues(self: *Self, last: *value.Value) void {
        while (self.openUpvalues != null and @intFromPtr(self.openUpvalues.?.location) >= @intFromPtr(last)) {
            const upvalue = self.openUpvalues.?;
            upvalue.closed = upvalue.location.*;
            upvalue.location = &upvalue.closed.?;
            self.openUpvalues = upvalue.next;
        }
    }

    fn defineMethod(self: *Self, name: *value.String) !void {
        const method = self.peek(0);
        const class = self.peek(1).asObject().asClass();
        try class.methods.put(name, method);
        _ = self.pop();
    }

    fn call(self: *Self, c: *value.Closure, argCount: u8) bool {
        if (c.function.arity != argCount) {
            self.runtimeError("Expected {d} arguments but got {d}.\n", .{ c.function.arity, argCount });
            return false;
        }

        if (self.frameCount == FrameMax) {
            self.runtimeError("Stack overflow", .{});
            return false;
        }

        const frame = &self.frames[self.frameCount];
        // std.debug.print("    code:\n", .{});
        // _ = debug.disassembleInstruction(c.function.chnk, 0);
        // std.debug.print("------ end code: {s}  --------\n", .{c.function.name});
        // self.frames.append(.{
        //     .closure = c,
        //     .ip = 0,
        //     .slot = self.stackTop - argCount - 1,
        //     .slots = &self.stack,
        // }) catch unreachable;
        frame.* = .{
            // .closure = .{ .closure = value.Closure.init(self.arena.allocator(), c.function) },
            .closure = c,
            .ip = 0,
            .slot = self.stackTop - argCount - 1,
            .slots = &self.stack,
        };

        self.frameCount += 1;
        return true;
    }

    fn readByte(self: *Self) u8 {
        var frame = &self.frames[self.frameCount - 1];
        const b = frame.closure.function.chnk.code.items[frame.ip];

        frame.ip += 1;
        return b;
    }

    fn readShort(self: *Self) u16 {
        var frame = &self.frames[self.frameCount - 1];
        frame.ip += 2;
        const b2: u16 = frame.closure.function.chnk.code.items[frame.ip - 2];
        const b1: u16 = frame.closure.function.chnk.code.items[frame.ip - 1];
        return b2 << 8 | b1;
    }

    pub fn interpret(self: *Self, source: []const u8) !InterpretResult {
        const f = try self.comp.compile(source, self);

        self.push(f.obj.value());

        const closure = try value.Closure.init(self, f);
        _ = self.pop();
        self.push(closure.obj.value());

        _ = self.call(closure, 0);
        // frame.function = c.functionValue();
        // frame.ip = 0;
        // frame.slots = &self.stack;

        return try self.run();
    }

    fn dumpFrames(self: *Self) void {
        std.debug.print("dumpFrames: {d}\n", .{self.frameCount});
        for (0..self.frameCount) |i| {
            const frame = &self.frames[i];
            std.debug.print("frame: {d} \n", .{i});
            const name = if (frame.closure.function.name) |name| name.bytes else "";
            std.debug.print("  closure: {s}\n ", .{name});
            std.debug.print("  code: {any} \n", .{frame.closure.function.chnk.code.items});
            std.debug.print("\n", .{});
        }
    }
    fn run(self: *Self) !InterpretResult {
        var f = &self.frames[self.frameCount - 1];
        while (true) {
            if (build_options.debug_trace_execution) {
                // std.debug.print("tracing execution...\n", .{});
                // std.debug.print("chnk: {any} ip: {d} ", .{ frame, frame.ip });

                std.debug.print("          ", .{});
                for (0..self.stackTop) |i| {
                    std.debug.print("[ ", .{});
                    debug.printValue(self.stack[i]);
                    std.debug.print(" ]", .{});
                }
                std.debug.print("\n", .{});
                _ = debug.disassembleInstruction(f.closure.function.chnk, f.ip);
            }
            const instruction: chunk.OpCode = @enumFromInt(self.readByte());
            switch (instruction) {
                .OpPrint => {
                    try self.run_print();
                },
                .OpJump => {
                    const offset = self.readShort();
                    f.ip += offset;
                },
                .OpJumpIfFalse => {
                    const offset = self.readShort();
                    const val = self.peek(0);
                    if (val.isFalsey()) {
                        f.ip += offset;
                    }
                },
                .OpLoop => {
                    const offset = self.readShort();
                    f.ip -= offset;
                },
                .OpCall => {
                    const argCount = self.readByte();
                    const callee = self.peek(argCount);
                    if (!self.callValue(callee, argCount)) {
                        return InterpreterError.runtime_error;
                    }

                    f = &self.frames[self.frameCount - 1];
                },
                .OpInvoke => {
                    const method = self.readString();
                    const argCount = self.readByte();
                    if (!self.invoke(method, argCount)) {
                        return InterpreterError.runtime_error;
                    }

                    f = &self.frames[self.frameCount - 1];
                },
                .OpSuperInvoke => {
                    const method = self.readString();
                    const argCount = self.readByte();
                    const superClass = self.pop();
                    const super = superClass.asObject().asClass();
                    std.debug.print("super invoke (superClass): {any}\n", .{super.obj.type});

                    std.debug.print("super invoke: {s}\n", .{method.bytes});
                    if (!self.invokeFromClass(super, method, argCount)) {
                        return InterpreterError.runtime_error;
                    }

                    f = &self.frames[self.frameCount - 1];
                },
                .OpClosure => {
                    const function = self.read_constant();
                    const func = function.asObject().asFunction();
                    std.debug.print("closure: {s}\n", .{func.name.?});
                    const closure = try value.Closure.init(self, func);
                    self.push(closure.obj.value());
                    for (0..closure.upvalues.len) |i| {
                        const isLocal = self.readByte();
                        const index = self.readByte();
                        if (isLocal == 1) {
                            const uv = try self.captureValue(&f.slots[f.slot + index]);
                            std.debug.print("captured upvalue: {any}\n", .{uv});
                            closure.upvalues[i] = uv;
                        } else {
                            // closure.closure.upvalues[i] = frame.closure.upvalues[index];
                            closure.upvalues[i] = f.closure.upvalues[index];
                        }
                    }
                    // for (0..closure.function.arity) |i| {
                    //     const val = self.peek(closure.function.arity - i);
                    //     self.stack[self.stackTop - i - 1] = val;
                    // }
                },
                .OpCloseUpvalue => {
                    self.closeUpvalues(&self.stack[self.stackTop - 1]);
                    // self.closeUpvalues(f.slot);
                    _ = self.pop();
                },
                .OpReturn => {
                    const result = self.pop();
                    self.closeUpvalues(&self.stack[f.slot]);
                    self.frameCount -= 1;
                    if (self.frameCount == 0) {
                        _ = self.pop();
                        return InterpretResult.ok;
                    }

                    self.stackTop = f.slot;
                    self.push(result);
                    f = &self.frames[self.frameCount - 1];
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
                    self.push(f.slots[f.slot + slot]);
                },
                .OpSetLocal => {
                    const slot = self.readByte();
                    f.slots[f.slot + slot] = self.peek(0);
                },
                .OpGetGlobal => {
                    const name = self.read_constant();
                    if (self.globals.get(name.asObject().asString())) |val| {
                        self.push(val);
                    } else {
                        self.runtimeError("GetGobal: Undefined variable '{s}'", .{name.asObject().asString()});
                        return InterpreterError.runtime_error;
                    }
                },
                .OpDefineGlobal => {
                    const name = self.read_constant();
                    const val = self.peek(0);
                    try self.globals.put(name.asObject().asString(), val);
                    _ = self.pop();
                    // try self.strings.put(name.stringValue(), void{});
                    // debug.printValue(name);
                    // std.debug.print("\n", .{});
                },
                .OpSetGlobal => {
                    const name = self.read_constant();
                    if (self.globals.contains(name.asObject().asString())) {
                        const val = self.peek(0);
                        try self.globals.put(name.asObject().asString(), val);
                    } else {
                        self.runtimeError("SetGlobal: Undefined variable '{s}'", .{name.asObject().asString()});
                        return InterpreterError.runtime_error;
                    }
                },
                .OpGetUpvalue => {
                    const slot = self.readByte();
                    std.debug.print("get upvalue: {d}\n", .{slot});
                    std.debug.print("upvalue closure: {any}\n", .{f.closure.upvalues});
                    const val = f.closure.upvalues[slot].location.*;
                    std.debug.print("get upvalue: {any}\n", .{val});
                    self.push(val);
                },
                .OpSetUpvalue => {
                    const slot = self.readByte();
                    f.closure.upvalues[slot].location.* = self.peek(0);
                },
                .OpGetProperty => {
                    if (!self.peek(0).isObjType(.instance)) {
                        self.runtimeError("Only instances have properties.", .{});
                        return InterpreterError.runtime_error;
                    }

                    const instance = self.peek(0).asObject().asInstance();
                    const name = self.readString();

                    if (instance.fields.get(name)) |val| {
                        _ = self.pop();
                        self.push(val);
                    } else {

                        // self.runtimeError("Undefined property '{s}'", .{name.bytes});
                        // return InterpreterError.runtime_error;

                        if (!try self.bindMethod(instance.class, name)) {
                            return InterpreterError.runtime_error;
                        }
                    }
                },
                .OpSetProperty => {
                    if (!self.peek(1).isObjType(.instance)) {
                        self.runtimeError("Only instances have fields.", .{});
                        return InterpreterError.runtime_error;
                    }

                    const instance = self.peek(1).asObject().asInstance();
                    const name = self.readString();
                    try instance.fields.put(name, self.peek(0));
                    const val = self.pop();
                    _ = self.pop();
                    self.push(val);
                },
                .OpGetSuper => {
                    const name = self.readString();
                    const superClass = self.pop().asObject().asClass();

                    if (!try self.bindMethod(superClass, name)) {
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
                .OpClass => {
                    const className = self.readString();
                    const class = try value.Class.init(self, className);
                    std.debug.print("class: {any}\n", .{className});
                    self.push(class.obj.value());
                },
                .OpInherit => {
                    const superClass = self.peek(1);
                    if (!superClass.isObjType(.class)) {
                        self.runtimeError("Superclass must be a class.", .{});
                        return InterpreterError.runtime_error;
                    }

                    const super = superClass.asObject().asClass();
                    const sub = self.peek(0);
                    const subClass = sub.asObject().asClass();
                    var iter = super.methods.iterator();
                    std.debug.print("subClasss: {s}\n", .{subClass.name.bytes});
                    while (iter.next()) |entry| {
                        std.debug.print("adding method...{s}\n", .{entry.key_ptr.*.bytes});
                        try subClass.methods.put(entry.key_ptr.*, entry.value_ptr.*);
                    }
                    _ = self.pop();
                },
                .OpMethod => {
                    try self.defineMethod(self.readString());
                },
            }
        }
    }

    fn run_binary_op(self: *Self, op: BinaryOp) !void {
        if (!self.peek(0).isNumber() and self.peek(1).isNumber()) {
            self.runtimeError("Operands must be numbers", .{});
            return InterpreterError.runtime_error;
        }

        const b = self.peek(0);
        const a = self.peek(1);
        var result: value.Value = .nil;
        switch (op) {
            .greater => {
                result = .{ .bool = a.asNumber() > b.asNumber() };
            },
            .less => {
                result = .{ .bool = a.asNumber() < b.asNumber() };
            },
            .add => {
                if (a.isNumber() and b.isNumber()) {
                    result = .{ .number = a.asNumber() + b.asNumber() };
                } else if (a.asObject().is(.string) and b.asObject().is(.string)) {
                    const s = try std.fmt.allocPrint(self.allocator, "{s}{s}", .{ a.asObject().asString(), b.asObject().asString() });
                    defer self.allocator.free(s);
                    const str = try value.String.init(self, s);
                    result = str.obj.value();
                } else {
                    self.runtimeError("Operands must be two numbers or two strings", .{});
                    return InterpreterError.runtime_error;
                }
            },
            .subtract => {
                result = .{ .number = a.asNumber() - b.asNumber() };
            },
            .multiply => {
                result = .{ .number = a.asNumber() * b.asNumber() };
                self.push(result);
            },
            .divide => {
                result = .{ .number = a.asNumber() / b.asNumber() };
                self.push(result);
            },
        }

        _ = self.pop();
        _ = self.pop();
        self.push(result);
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
        const constant = frame.closure.function.chnk.constants.items[b];

        self.push(constant);
        // debug.printValue(constant);
        // std.debug.print("\n", .{});
        // }
    }

    fn read_constant(self: *Self) value.Value {
        const frame = &self.frames[self.frameCount - 1];
        const b = self.readByte();

        return frame.closure.function.chnk.constants.items[b];
    }

    fn readString(self: *Self) *value.String {
        const constant = self.read_constant();
        return constant.obj.asString();
    }
};

const test_allocator = std.testing.allocator;

test "vm" {
    var vm = VM.init(test_allocator);
    vm.setup();
    defer vm.deinit();

    const input =
        \\
        \\print 123;
    ;

    _ = try vm.interpret(input);
}
