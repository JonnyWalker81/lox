const std = @import("std");
const callable = @import("callable.zig");

pub const Object = union(enum) {
    const Self = @This();

    nil,
    boolean: bool,
    number: f64,
    string: []const u8,
    callable: *callable.Callable,

    pub fn numberValue(self: *Self) f64 {
        return switch (self.*) {
            .number => |n| return n,
            else => return 0.0,
        };
    }

    pub fn stringValue(self: *Self) []const u8 {
        return switch (self.*) {
            .string => |s| return s,
            else => return "",
        };
    }

    pub fn booleanValue(self: *Self) bool {
        return switch (self.*) {
            .boolean => |b| return b,
            .number => |_| return true,
            else => return false,
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
        }
    }
};
