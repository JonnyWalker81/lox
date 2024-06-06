const std = @import("std");
const object = @import("object.zig");

pub const EnvironmentErrors = error{
    UndefinedVariable,
};

pub const Environment = struct {
    const Self = @This();

    allocator: std.mem.Allocator,
    values: std.StringHashMap(*object.Object),

    pub fn init(allocator: std.mem.Allocator) Self {
        return Self{
            .allocator = allocator,
            .values = std.StringHashMap(*object.Object).init(allocator),
        };
    }

    pub fn deinit(self: *Self) void {
        self.values.deinit();
    }

    pub fn define(self: *Self, name: []const u8, value: *object.Object) !void {
        std.log.info("Defining variable: {s}", .{name});
        try self.values.put(name, value);
        std.log.info("Defined variable: {}", .{self.values});
    }

    pub fn get(self: *Self, name: []const u8) !*object.Object {
        std.log.info("Getting variable: {s}", .{name});
        std.log.info("Defined variable map: {}", .{self.values});
        const v = self.values.get(name);
        if (v) |val| {
            return val;
        }

        return EnvironmentErrors.UndefinedVariable;
    }
};
