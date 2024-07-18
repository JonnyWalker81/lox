const std = @import("std");
const memory = @import("memory.zig");
const chunk = @import("chunk.zig");
const VM = @import("vm.zig").VM;
const build_options = @import("build_options");
const debug = @import("debug.zig");

pub const Obj = struct {
    const Self = @This();

    pub const Type = enum {
        string,
        function,
        native,
        closure,
        upvalue,
        class,
        instance,
        boundMethod,
    };

    isMarked: bool = false,
    next: ?*Self = null,
    type: Type,

    pub fn create(vm: *VM, comptime T: type, typ: Type) !*Self {
        const ptr = try vm.allocator.create(T);

        ptr.obj = .{
            .isMarked = false,
            .type = typ,
            .next = vm.objects,
        };

        vm.objects = &ptr.obj;

        if (build_options.debug_log_gc) {
            std.debug.print("{} allocate {} for {s}\n", .{ @intFromPtr(&ptr.obj), @sizeOf(T), @typeName(T) });
        }

        return &ptr.obj;
    }

    pub fn destroy(self: *Self, vm: *VM) void {
        if (build_options.debug_log_gc) {
            std.debug.print("{} free {}\n", .{ @intFromPtr(self), self.type });
        }

        switch (self.type) {
            .string => {
                self.asString().deinit(vm);
            },
            .function => {
                self.asFunction().deinit(vm);
            },
            .native => {
                self.asNative().deinit(vm);
            },
            .closure => {
                self.asClosure().deinit(vm);
            },
            .upvalue => {
                self.asUpvalue().deinit(vm);
            },
            .class => {
                self.asClass().deinit(vm);
            },
            .instance => {
                self.asInstance().deinit(vm);
            },
            .boundMethod => {
                self.asBoundMethod().deinit(vm);
            },
        }
    }

    pub fn is(self: *Self, typ: Type) bool {
        return self.type == typ;
    }

    pub fn isMarked(self: *Self) bool {
        return self.isMarked;
    }

    pub fn value(self: *Self) Value {
        return .{ .obj = self };
    }

    pub fn asString(self: *Self) *String {
        return @fieldParentPtr("obj", self);
    }

    pub fn asFunction(self: *Self) *Function {
        return @fieldParentPtr("obj", self);
    }

    pub fn asNative(self: *Self) *Native {
        return @fieldParentPtr("obj", self);
    }

    pub fn asClosure(self: *Self) *Closure {
        return @fieldParentPtr("obj", self);
    }

    pub fn asUpvalue(self: *Self) *Upvalue {
        return @fieldParentPtr("obj", self);
    }

    pub fn asClass(self: *Self) *Class {
        return @fieldParentPtr("obj", self);
    }

    pub fn asInstance(self: *Self) *Instance {
        return @fieldParentPtr("obj", self);
    }

    pub fn asBoundMethod(self: *Self) *BoundMethod {
        return @fieldParentPtr("obj", self);
    }
};

pub const String = struct {
    const Self = @This();

    obj: Obj,
    bytes: []const u8,

    pub fn init(vm: *VM, bytes: []const u8) !*Self {
        const interned = vm.strings.get(bytes);
        if (interned) |s| return s;

        const heapChars = try vm.allocator.alloc(u8, bytes.len);

        @memcpy(heapChars, bytes);

        return try allocate(vm, heapChars);
    }

    fn allocate(vm: *VM, bytes: []const u8) !*Self {
        const obj = try Obj.create(vm, Self, .string);
        const string = obj.asString();
        string.bytes = bytes;

        // Make sure string is visible to the GC during allocation
        vm.push(obj.value());
        _ = try vm.strings.put(bytes, string);
        _ = vm.pop();
        return string;
    }

    pub fn deinit(self: *Self, vm: *VM) void {
        std.debug.print("deinit string\n", .{});
        // _ = self;
        // _ = vm;
        vm.allocator.free(self.bytes);
        vm.allocator.destroy(self);
    }

    pub fn format(
        self: Self,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;

        try writer.print("{s}", .{self.bytes});
    }
};

pub const Function = struct {
    const Self = @This();

    obj: Obj,
    arity: u8 = 0,
    upvalueCount: u8 = 0,
    chnk: chunk.Chunk = undefined,
    name: ?*String = null,

    pub fn init(vm: *VM) !*Self {
        const obj = try Obj.create(vm, Self, .function);
        // vm.push(obj.value());
        const f = obj.asFunction();
        f.* = .{
            // .chnk = chunk.Chunk.init(vm.allocator),
            .obj = obj.*,
            .name = null,
            .arity = 0,
            .upvalueCount = 0,
        };

        // f.chnk = chunk.Chunk.init(vm.gcAllocator.allocator());
        f.chnk = chunk.Chunk.init(vm.allocator);
        // _ = vm.pop();
        return f;
    }

    pub fn deinit(self: *Self, vm: *VM) void {
        std.debug.print("deinit function\n", .{});
        // _ = self;
        // _ = vm;
        self.chnk.deinit();
        vm.allocator.destroy(self);
    }

    pub fn incrementArity(self: *Self) void {
        self.arity += 1;
    }

    pub fn incrementUpvalueCount(self: *Self) void {
        self.upvalueCount += 1;
    }
};

pub const NativeFn = *const fn (u8, []Value) Value;

pub const Native = struct {
    const Self = @This();

    obj: Obj,
    function: NativeFn,

    pub fn init(vm: *VM, function: NativeFn) !*Self {
        const obj = try Obj.create(vm, Self, .native);
        const n = obj.asNative();
        n.* = .{
            .function = function,
            .obj = obj.*,
        };

        return n;
    }

    pub fn deinit(self: *Self, vm: *VM) void {
        std.debug.print("deinit native\n", .{});
        vm.allocator.destroy(self);
    }
};

pub const Closure = struct {
    const Self = @This();

    obj: Obj,
    function: *Function,
    upvalues: []*Upvalue,

    pub fn init(vm: *VM, function: *Function) !*Self {
        std.debug.print("init closure: {any}\n", .{function.name});
        const upvalues = vm.allocator.alloc(*Upvalue, function.upvalueCount) catch unreachable;
        @memset(upvalues, undefined);

        const obj = try Obj.create(vm, Closure, .closure);
        const closure = obj.asClosure();
        closure.* = .{
            .function = function,
            .upvalues = upvalues,
            .obj = obj.*,
        };

        return closure;
    }

    pub fn deinit(self: *Self, vm: *VM) void {
        std.debug.print("deinit closure\n", .{});
        vm.allocator.free(self.upvalues);
        vm.allocator.destroy(self);
    }
};

pub const Upvalue = struct {
    const Self = @This();

    // isLocal: bool,
    obj: Obj,
    location: *Value,
    next: ?*Self = null,
    closed: ?Value = null,

    pub fn initUpval(vm: *VM, location: *Value) !*Self {
        std.debug.print("init upvalue: {any}\n", .{location});
        const obj = try Obj.create(vm, Self, .upvalue);
        const upvalue = obj.asUpvalue();
        upvalue.* = .{
            .location = location,
            .obj = obj.*,
        };

        return upvalue;
    }

    pub fn deinit(self: *Self, vm: *VM) void {
        std.debug.print("deinit upvalue\n", .{});
        vm.allocator.destroy(self);
    }

    pub fn close(self: *Self, value: Value) void {
        self.closed = value;
        self.location = &self.closed;
    }

    pub fn closedValue(self: *Self) Value {
        return self.closed;
    }
};

pub const Class = struct {
    const Self = @This();

    obj: Obj,
    name: *String,
    methods: std.AutoHashMap(*String, Value),

    pub fn init(vm: *VM, name: *String) !*Self {
        const obj = try Obj.create(vm, Self, .class);
        const class = obj.asClass();
        class.* = .{
            .obj = obj.*,
            .name = name,
            .methods = std.AutoHashMap(*String, Value).init(vm.allocator),
        };

        return class;
    }

    pub fn deinit(self: *Self, vm: *VM) void {
        std.debug.print("deinit class\n", .{});
        self.methods.deinit();
        vm.allocator.destroy(self);
    }
};

pub const Instance = struct {
    const Self = @This();

    obj: Obj,
    class: *Class,
    fields: std.AutoHashMap(*String, Value),

    pub fn init(vm: *VM, class: *Class) !*Self {
        const obj = try Obj.create(vm, Self, .instance);
        const instance = obj.asInstance();
        instance.* = .{
            .obj = obj.*,
            .class = class,
            .fields = std.AutoHashMap(*String, Value).init(vm.allocator),
        };

        return instance;
    }

    pub fn deinit(self: *Self, vm: *VM) void {
        std.debug.print("deinit instance\n", .{});
        self.fields.deinit();
        vm.allocator.destroy(self);
    }
};

pub const BoundMethod = struct {
    const Self = @This();

    obj: Obj,
    receiver: Value,
    method: *Closure,

    pub fn init(vm: *VM, receiver: Value, method: *Closure) !*Self {
        const obj = try Obj.create(vm, Self, .boundMethod);
        const boundMethod = obj.asBoundMethod();

        boundMethod.* = .{
            .obj = obj.*,
            .receiver = receiver,
            .method = method,
        };

        return boundMethod;
    }

    pub fn deinit(self: *Self, vm: *VM) void {
        std.debug.print("deinit bound method\n", .{});
        vm.allocator.destroy(self);
    }
};

pub const ValueType = enum {
    nil,
    bool,
    number,
    obj,
};

pub const Value = union(ValueType) {
    const Self = @This();

    nil,
    bool: bool,
    number: f64,
    obj: *Obj,

    pub fn isNil(self: Self) bool {
        return self == .nil;
    }

    pub fn isBool(self: Self) bool {
        return self == .bool;
    }

    pub fn isNumber(self: Self) bool {
        return self == .number;
    }

    pub fn isObject(self: Self) bool {
        return self == .obj;
    }

    pub fn isObjType(self: Self, typ: Obj.Type) bool {
        if (self.isObject()) {
            return self.asObject().type == typ;
        }

        return false;
    }

    pub fn asBool(self: Self) bool {
        switch (self) {
            .bool => |b| return b,
            else => std.debug.panic("expected bool, not a bool, got: {s}", .{@tagName(self)}),
        }
    }

    pub fn asNumber(self: Self) f64 {
        switch (self) {
            .number => |n| return n,
            else => std.debug.panic("expected number, not a number, got: {s}", .{@tagName(self)}),
        }
    }

    pub fn asObject(self: Self) *Obj {
        switch (self) {
            .obj => |o| return o,
            else => std.debug.panic("expected object, not a object, got: {s}", .{@tagName(self)}),
        }
    }

    pub fn nil() Self {
        return .nil;
    }

    pub fn isFalsey(self: Self) bool {
        switch (self) {
            .bool => |b| return !b,
            .nil => return true,
            else => return false,
        }
    }

    pub fn equalTo(self: Self, other: Self) bool {
        if (self.isNil() and other.isNil()) {
            return true;
        }

        if (self.isNumber() and other.isNumber()) {
            return self.asNumber() == other.asNumber();
        }

        if (self.isBool() and other.isBool()) {
            return self.asBool() == other.asBool();
        }

        // if (self.isString() and other.isString()) {
        //     return std.mem.eql(u8, self.stringValue(), other.stringValue());
        // }

        if (self.isObject() and other.isObject()) {
            return self.asObject() == other.asObject();
        }

        return false;
    }

    pub fn format(
        self: Self,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;

        switch (self) {
            .bool => |b| {
                if (b) {
                    try writer.print("true", .{});
                } else {
                    try writer.print("false", .{});
                }
            },
            .nil => {
                try writer.print("nil", .{});
            },
            .number => |n| {
                try writer.print("{d}", .{n});
            },
            .obj => |o| {
                try printObject(writer, o);
            },
        }
    }
};

fn printObject(writer: anytype, obj: *Obj) !void {
    switch (obj.type) {
        .string => {
            const s = obj.asString();
            try writer.print("{s}", .{s.bytes});
        },
        .function => {
            const f = obj.asFunction();
            try printFunctionName(writer, f);
        },
        .native => {
            try writer.print("<native fn>", .{});
        },
        .closure => {
            const c = obj.asClosure();
            try printFunctionName(writer, c.function);
        },
        .upvalue => {
            try writer.print("<upvalue>", .{});
        },
        .class => {
            const c = obj.asClass();
            try writer.print("{s}", .{c.name.bytes});
        },
        .instance => {
            const i = obj.asInstance();
            try writer.print("{s} instance", .{i.class.name.bytes});
        },
        .boundMethod => {
            const b = obj.asBoundMethod();
            try printFunctionName(writer, b.method.function);
        },
    }
}

pub fn printFunctionName(writer: anytype, function: *Function) !void {
    if (function.name) |name| {
        try writer.print("<fn {s}>", .{name.bytes});
    } else {
        try writer.print("<script>", .{});
    }
}

pub const ValueArray = struct {
    const Self = @This();

    allocator: std.mem.Allocator,
    capacity: usize = 0,
    count: usize = 0,
    values: ?[]Value = null,

    pub fn init(allocator: std.mem.Allocator) *ValueArray {
        const valArray = allocator.create(ValueArray) catch unreachable;
        valArray.* = .{
            .allocator = allocator,
        };
        return valArray;
    }

    pub fn deinit(self: *Self) void {
        self.free();
    }

    pub fn write(self: *Self, value: Value) !void {
        if (self.capacity < self.count + 1) {
            const oldCapacity = self.capacity;
            self.capacity = memory.growCapacity(oldCapacity);
            const newValues = try memory.growArray(Value, self.allocator, self.values, oldCapacity, self.capacity);
            self.values = newValues;
        }

        if (self.values) |values| {
            values[self.count] = value;
            self.count += 1;
        }
    }

    pub fn free(self: *Self) void {
        self.allocator.free(self.values);
    }
};
