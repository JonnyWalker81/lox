const std = @import("std");
const callable = @import("callable.zig");
const class = @import("class.zig");

pub const Object = union(enum) {
    const Self = @This();

    nil,
    boolean: bool,
    number: f64,
    string: []const u8,
    callable: *callable.Callable,
    returnValue: *Self,
    class: *callable.Callable,
    implementation: *class.LoxInstance,

    pub fn isNumber(self: *Self) bool {
        switch (self.*) {
            .number => return true,
            .returnValue => |r| return r.isNumber(),
            else => return false,
        }
    }

    pub fn isString(self: *Self) bool {
        switch (self.*) {
            .string => return true,
            .returnValue => |r| return r.isString(),
            else => return false,
        }
    }

    pub fn isBoolean(self: *Self) bool {
        switch (self.*) {
            .boolean => return true,
            .returnValue => |r| return r.isBoolean(),
            else => return false,
        }
    }

    pub fn numberValue(self: *Self) f64 {
        return switch (self.*) {
            .number => |n| return n,
            .returnValue => |r| return r.numberValue(),
            else => return 0.0,
        };
    }

    pub fn stringValue(self: *Self) []const u8 {
        return switch (self.*) {
            .string => |s| return s,
            .returnValue => |r| return r.stringValue(),
            else => return "",
        };
    }

    pub fn booleanValue(self: *Self) bool {
        return switch (self.*) {
            .boolean => |b| return b,
            .number => |_| return true,
            .returnValue => |r| return r.booleanValue(),
            else => return false,
        };
    }

    pub fn callableValue(self: *Self) *callable.Callable {
        return switch (self.*) {
            .callable => |c| return c,
            .returnValue => |r| return r.callableValue(),
            .class => |c| return c,
            else => std.debug.panic("not callable", .{}),
        };
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
            .nil => try writer.print("nil", .{}),
            .boolean => |b| {
                try writer.print("{}", .{b});
            },
            .number => |n| {
                try writer.print("{d}", .{n});
            },
            .string => |s| {
                try writer.print("{s}", .{s});
            },
            .callable => |_| {
                try writer.print("callable", .{});
            },
            .returnValue => |r| {
                try writer.print("{s}", .{r});
            },
            .class => |c| {
                try writer.print("{s}", .{c.inner(class.LoxClass)});
            },
            .implementation => |i| {
                try writer.print("{s}", .{i});
            },
        }
    }
};
