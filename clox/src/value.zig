const std = @import("std");
const memory = @import("memory.zig");

pub const Value = union(enum) {
    const Self = @This();

    bool: bool,
    nil,
    number: f64,
    string: []const u8,

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
        }
    }
};

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
