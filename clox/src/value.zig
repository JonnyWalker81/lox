const std = @import("std");
const memory = @import("memory.zig");
const chunk = @import("chunk.zig");
const VM = @import("vm.zig").VM;

pub const Obj = struct {
    const Self = @This();

    isMarked: bool = false,
    next: ?*Self = null,

    pub fn init() Self {
        return .{ .isMarked = false };
    }

    pub fn isMarked(self: *Self) bool {
        return self.isMarked;
    }
};

pub const String = struct {
    const Self = @This();

    obj: Obj,
    string: []const u8,

    pub fn init(vm: *VM, bytes: []const u8) *Self {
        std.debug.print("String.init: {any}\n", .{vm.allocator});
        // std.debug.print("String.init\n", .{});
        const interned = vm.strings.get(bytes);
        if (interned) |s| return s;

        const heapChars = vm.allocator.alloc(u8, bytes.len) catch unreachable;

        @memcpy(heapChars, bytes);

        return allocate(vm, heapChars) catch unreachable;
        // if (vm.strings.get(string)) |s| {
        //     return s;
        // }

        // const str = allocator.create(Self) catch unreachable;
        // str.* = .{
        //     .allocator = allocator,
        //     .string = bytes,
        //     .obj = Obj.init(),
        // };

        // // vm.strings.put(string, str) catch @panic("failed to put string in vm.strings");

        // return str;
    }

    fn allocate(vm: *VM, bytes: []const u8) !*Self {
        const string = vm.allocator.create(Self) catch unreachable;
        string.* = .{
            // .allocator = allocator,
            .string = bytes,
            .obj = Obj.init(),
        };
        // string.bytes = bytes;

        // Make sure string is visible to the GC during allocation
        vm.push(.{ .string = string });
        _ = try vm.strings.put(bytes, string);
        _ = vm.pop();
        return string;
    }

    pub fn deinit(self: *Self, vm: *VM) void {
        vm.allocator.free(self.string);
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

        try writer.print("{s}", .{self.string});
    }
};

pub const Function = struct {
    const Self = @This();

    allocator: std.mem.Allocator,
    obj: Obj,
    arity: u8 = 0,
    upvalueCount: u8 = 0,
    chnk: *chunk.Chunk,
    name: []const u8 = "",

    pub fn init(allocator: std.mem.Allocator) *Self {
        const f = allocator.create(Self) catch unreachable;
        f.* = .{
            .allocator = allocator,
            .chnk = chunk.Chunk.init(allocator),
            .obj = Obj.init(),
        };

        return f;
    }

    pub fn deinit(self: *Self) void {
        self.chnk.deinit();
        self.allocator.destroy(self);
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

    allocator: std.mem.Allocator,
    obj: Obj,
    function: NativeFn,

    pub fn init(allocator: std.mem.Allocator, function: NativeFn) *Self {
        const n = allocator.create(Native) catch unreachable;
        n.* = .{
            .allocator = allocator,
            .function = function,
            .obj = Obj.init(),
        };

        return n;
    }
};

pub const Closure = struct {
    const Self = @This();

    allocator: std.mem.Allocator,
    obj: Obj,
    function: *Function,
    upvalues: []*Upvalue,

    pub fn init(allocator: std.mem.Allocator, function: *Function) *Self {
        const upvalues = allocator.alloc(*Upvalue, function.upvalueCount) catch unreachable;
        @memset(upvalues, undefined);
        const closure = allocator.create(Closure) catch unreachable;
        closure.* = .{
            .allocator = allocator,
            .function = function,
            .upvalues = upvalues,
            .obj = Obj.init(),
        };

        return closure;
    }
};

pub const Upvalue = struct {
    const Self = @This();

    allocator: std.mem.Allocator,
    // isLocal: bool,
    obj: Obj,
    location: *Value,
    next: ?*Self = null,
    closed: Value = undefined,

    pub fn init(allocator: std.mem.Allocator, location: *Value) *Self {
        const upvalue = allocator.create(Self) catch unreachable;
        upvalue.* = .{
            .allocator = allocator,
            .location = location,
            .obj = Obj.init(),
        };

        return upvalue;
    }
};

pub const Value = union(enum) {
    const Self = @This();

    bool: bool,
    nil,
    number: f64,
    string: *String,
    function: *Function,
    native: *Native,
    closure: *Closure,
    upvalue: *Upvalue,

    pub fn isNil(self: Value) bool {
        return self == .nil;
    }

    pub fn isBool(self: Value) bool {
        switch (self) {
            .bool => return true,
            else => return false,
        }
    }

    pub fn isNumber(self: Value) bool {
        switch (self) {
            .number => return true,
            else => return false,
        }
    }

    pub fn isString(self: Value) bool {
        switch (self) {
            .string => return true,
            else => return false,
        }
    }

    pub fn isFunction(self: Value) bool {
        switch (self) {
            .function => return true,
            else => return false,
        }
    }

    pub fn isNative(self: Value) bool {
        switch (self) {
            .native => return true,
            else => return false,
        }
    }

    pub fn isClosure(self: Value) bool {
        switch (self) {
            .closure => return true,
            else => return false,
        }
    }

    pub fn isObject(self: Value) bool {
        switch (self) {
            .string => return true,
            .function => return true,
            .native => return true,
            .closure => return true,
            .upvalue => return true,
            else => return false,
        }
    }

    pub fn boolValue(self: Value) bool {
        switch (self) {
            .bool => |b| return b,
            else => return false,
        }
    }

    pub fn numberValue(self: Value) f64 {
        switch (self) {
            .number => |n| return n,
            else => return 0.0,
        }
    }

    pub fn stringValue(self: Value) []const u8 {
        switch (self) {
            .string => |s| return s.string,
            else => return "",
        }
    }

    pub fn functionValue(self: Value) *Function {
        switch (self) {
            .function => |f| return f,
            else => @panic("expected function, not a function."),
        }
    }

    pub fn nativeValue(self: Value) Native {
        switch (self) {
            .native => |n| return n,
            else => @panic("expected native, not a native."),
        }
    }

    pub fn closureValue(self: Value) Closure {
        switch (self) {
            .closure => |c| return c,
            else => @panic("expected closure, not a closure."),
        }
    }

    pub fn asString(self: Value) *String {
        switch (self) {
            .string => |s| return @fieldParentPtr("obj", &s.obj),
            else => @panic("expected string, not a string."),
        }
    }

    pub fn asObject(self: Value) *Obj {
        switch (self) {
            .string => |s| return &s.obj,
            .function => |f| return &f.obj,
            .native => |n| return &n.obj,
            .closure => |c| return &c.obj,
            .upvalue => |u| return &u.obj,
            else => @panic("expected object, not a object."),
        }
    }

    pub fn isFalsey(self: Value) bool {
        switch (self) {
            .bool => |b| return !b,
            .nil => return true,
            else => return false,
        }
    }

    pub fn equalTo(self: Value, other: Value) bool {
        if (self.isNil()) {
            return true;
        }

        if (self.isNumber() and other.isNumber()) {
            return self.numberValue() == other.numberValue();
        }

        if (self.isBool() and other.isBool()) {
            return self.boolValue() == other.boolValue();
        }

        if (self.isString() and other.isString()) {
            return std.mem.eql(u8, self.stringValue(), other.stringValue());
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
            .string => |s| {
                try writer.print("{s}", .{s.string});
            },
            .function => |f| {
                if (f.name.len > 0) {
                    try writer.print("<fn {s}>", .{f.name});
                } else {
                    try writer.print("<script> {s}", .{f.name});
                }
            },
            .native => |_| {
                try writer.print("<native fn>", .{});
            },
            .closure => |c| {
                try printFunctionName(writer, c.function);
                // if (c.function.name.len > 0) {
                //     try writer.print("<fn {s}>", .{c.function.name});
                // } else {
                //     try writer.print("<script>", .{});
                // }
            },
            .upvalue => |_| {
                try writer.print("<upvalue>", .{});
            },
        }
    }
};

fn printFunctionName(writer: anytype, function: Function) !void {
    if (function.name.len > 0) {
        try writer.print("<fn {s}>", .{function.name});
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
