const std = @import("std");
const memory = @import("memory.zig");
const chunk = @import("chunk.zig");
const VM = @import("vm.zig").VM;
const build_options = @import("build_options");
const debug = @import("debug.zig");

pub const Value = if (build_options.nan_boxing) NanValue else UnionValue;

pub const NanValue = packed struct {
    const Self = @This();

    val: u64,

    pub const QNAN: u64 = 0x7FFC000000000000;
    pub const SIGN_BIT: u64 = 0x8000000000000000;

    pub const TAG_NIL: u64 = 1;
    pub const TAG_FALSE: u64 = 2;
    pub const TAG_TRUE: u64 = 3;

    const NIL_VAL = Self{ .val = QNAN | TAG_NIL };
    const TRUE_VAL = Self{ .val = QNAN | TAG_TRUE };
    const FALSE_VAL = Self{ .val = QNAN | TAG_FALSE };

    pub fn isNumber(self: Self) bool {
        return (self.val & QNAN) != QNAN;
    }

    pub fn isNil(self: Self) bool {
        return self.val == TAG_NIL;
    }

    pub fn isBool(self: Self) bool {
        return (self.val | 1) == TRUE_VAL.val;
    }

    pub fn isString(self: Self) bool {
        return (self.data & (QNAN | SIGN_BIT)) == (QNAN | SIGN_BIT);
    }

    pub fn isObject(self: Self) bool {
        return (self.val & (QNAN | SIGN_BIT)) == (QNAN | SIGN_BIT);
    }

    pub fn isObjType(self: Self, objType: Obj.Type) bool {
        return self.isObject() and self.asObject().type == objType;
    }

    pub fn asNumber(self: Self) f64 {
        return @bitCast(self.val);
    }

    pub fn asBool(self: Self) bool {
        return self.val == TRUE_VAL.val;
    }

    pub fn asObject(self: Self) *Obj {
        return @as(*Obj, @ptrFromInt(@as(usize, @intCast(self.val & ~(SIGN_BIT | QNAN)))));
    }

    pub fn asObjType(self: Self, comptime objType: Obj.Type) *Obj.ObjType(objType) {
        return self.asObject().asObjType(objType);
    }

    pub fn fromBool(b: bool) Self {
        return if (b) TRUE_VAL else FALSE_VAL;
    }

    pub fn fromNumber(n: f64) Self {
        return .{ .val = @as(u64, @bitCast(n)) };
    }

    pub fn fromObject(obj: *Obj) Self {
        return .{ .val = @intFromPtr(obj) | (SIGN_BIT | QNAN) };
    }

    pub fn isFalsey(self: Self) bool {
        if (self.isBool()) {
            return !self.asBool();
        }

        if (self.isNil()) {
            return true;
        }

        return false;
    }

    pub fn nil() Self {
        return NIL_VAL;
    }

    pub fn equalTo(self: NanValue, other: NanValue) bool {
        // Be careful about IEEE NaN equality semantics
        if (self.isNumber() and other.isNumber()) return self.asNumber() == other.asNumber();
        return self.val == other.val;
    }

    pub fn format(
        self: Self,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;

        if (self.isNumber()) {
            try writer.print("{d}", .{self.asNumber()});
        } else if (self.isNil()) {
            try writer.print("nil", .{});
        } else if (self.isBool()) {
            if (self.asBool()) {
                try writer.print("true", .{});
            } else {
                try writer.print("false", .{});
            }
        } else if (self.isObject()) {
            try printObject(writer, self.asObject());
        }
    }
};

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
        return Value.fromObject(self);
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

    pub fn asObjType(self: *Obj, comptime objType: Obj.Type) *ObjType(objType) {
        return switch (objType) {
            .instance => self.asInstance(),
            .class => self.asClass(),
            .closure => self.asClosure(),
            .function => self.asFunction(),
            .boundMethod => self.asBoundMethod(),
            .native => self.asNative(),
            // .List => self.asList(),
            // .Map => self.asMap(),
            // .Enum => self.asEnum(),
            .string => self.asString(),
            .upvalue => self.asUpvalue(),
        };
    }

    pub fn nil() Self {
        return .nil;
    }

    pub fn fromBool(b: bool) Value {
        return .{ .bool = b };
    }

    pub fn ObjType(comptime objType: Obj.Type) type {
        return switch (objType) {
            .instance => Instance,
            .class => Class,
            .closure => Closure,
            .function => Function,
            .boundMethod => BoundMethod,
            .native => Native,
            // .List => List,
            // .Map => Map,
            // .Enum => Enum,
            .string => String,
            .upvalue => Upvalue,
        };
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
    arity: u16 = 0,
    upvalueCount: u16 = 0,
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
        vm.allocator.destroy(self);
    }
};

pub const Closure = struct {
    const Self = @This();

    obj: Obj,
    function: *Function,
    upvalues: []*Upvalue,

    pub fn init(vm: *VM, function: *Function) !*Self {
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
        const obj = try Obj.create(vm, Self, .upvalue);
        const upvalue = obj.asUpvalue();
        upvalue.* = .{
            .location = location,
            .obj = obj.*,
        };

        return upvalue;
    }

    pub fn deinit(self: *Self, vm: *VM) void {
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
        vm.allocator.destroy(self);
    }
};

pub const ValueType = enum {
    nil,
    bool,
    number,
    obj,
};

pub const UnionValue = union(ValueType) {
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

    pub fn fromBool(b: bool) Self {
        return .{ .bool = b };
    }

    pub fn fromNumber(n: f64) Self {
        return .{ .number = n };
    }

    pub fn fromObject(o: *Obj) Self {
        return .{ .obj = o };
    }

    pub fn asObjType(self: Self, comptime objType: Obj.Type) *Obj.ObjType(objType) {
        return self.asObject().asObjType(objType);
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
