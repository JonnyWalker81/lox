const std = @import("std");
const memory = @import("memory.zig");
const chunk = @import("chunk.zig");

pub const Function = struct {
    const Self = @This();

    allocator: std.mem.Allocator,
    arity: u8 = 0,
    upvalueCount: u8 = 0,
    chnk: *chunk.Chunk,
    name: []const u8 = "",

    pub fn init(allocator: std.mem.Allocator) Self {
        return .{
            .allocator = allocator,
            .chnk = chunk.Chunk.init(allocator),
        };
    }

    pub fn deinit(self: *Self) void {
        self.chnk.deinit();
    }

    pub fn incrementArity(self: *Self) void {
        self.arity += 1;
    }
};

pub const NativeFn = *const fn (u8, []Value) Value;

pub const Native = struct {
    allocator: std.mem.Allocator,
    function: NativeFn,

    pub fn init(allocator: std.mem.Allocator, function: NativeFn) Native {
        return .{
            .allocator = allocator,
            .function = function,
        };
    }
};

pub const Closure = struct {
    allocator: std.mem.Allocator,
    function: *Function,

    pub fn init(allocator: std.mem.Allocator, function: *Function) Closure {
        return .{
            .allocator = allocator,
            .function = function,
        };
    }
};

pub const Value = union(enum) {
    const Self = @This();

    bool: bool,
    nil,
    number: f64,
    string: []const u8,
    function: Function,
    native: Native,
    closure: Closure,

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
            .string => |s| return s,
            else => return "",
        }
    }

    pub fn functionValue(self: Value) Function {
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
                try writer.print("{s}", .{s});
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
                printFunctionName(writer, c.function);
                // if (c.function.name.len > 0) {
                //     try writer.print("<fn {s}>", .{c.function.name});
                // } else {
                //     try writer.print("<script>", .{});
                // }
            },
        }
    }
};

fn printFunctionName(writer: anytype, function: *Function) !void {
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
