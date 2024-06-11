const std = @import("std");
const interpreter = @import("interpreter.zig");
const object = @import("object.zig");

pub const LoxClass = struct {
    const Self = @This();

    allocator: std.mem.Allocator,
    name: []const u8,

    pub fn init(allocator: std.mem.Allocator, className: []const u8) *Self {
        const class = allocator.create(Self) catch unreachable;
        class.* = .{
            .allocator = allocator,
            .name = className,
        };

        return class;
    }

    pub fn arity(self: *Self) usize {
        _ = self;

        return 0;
    }

    pub fn call(self: *Self, i: *interpreter.Interpreter, arguments: []*object.Object) anyerror!?*object.Object {
        _ = i;
        _ = arguments;

        const instance = LoxInstance.init(self.allocator, self);
        const obj = try self.allocator.create(object.Object);
        obj.* = .{
            .implementation = instance,
        };

        return obj;
    }

    pub fn format(
        self: Self,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;

        try writer.print("{s}", .{self.name});
    }
};

pub const LoxInstance = struct {
    const Self = @This();

    allocator: std.mem.Allocator,
    klass: *LoxClass,

    pub fn init(allocator: std.mem.Allocator, klass: *LoxClass) *Self {
        const instance = allocator.create(Self) catch unreachable;
        instance.* = .{
            .allocator = allocator,
            .klass = klass,
        };

        return instance;
    }

    pub fn format(
        self: Self,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;

        try writer.print("{s} instance", .{self.klass.name});
    }
};