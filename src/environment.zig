const std = @import("std");
const object = @import("object.zig");

pub const EnvironmentErrors = error{
    UndefinedVariable,
};

pub const Environment = struct {
    const Self = @This();

    allocator: std.mem.Allocator,
    values: std.StringHashMap(*object.Object),
    enclosing: ?*Environment = null,

    pub fn init(allocator: std.mem.Allocator) *Self {
        const e = allocator.create(Self) catch unreachable;
        e.* = .{
            .allocator = allocator,
            .values = std.StringHashMap(*object.Object).init(allocator),
        };

        return e;
    }

    pub fn initWithEnclosing(allocator: std.mem.Allocator, env: *Self) *Self {
        // env.dumpValues();

        const e = allocator.create(Self) catch unreachable;
        e.* = .{
            .allocator = allocator,
            .values = std.StringHashMap(*object.Object).init(allocator),
            .enclosing = env,
        };

        return e;
    }

    pub fn deinit(self: *Self) void {
        // self.dumpValues();
        self.values.deinit();
    }

    pub fn dumpValues(self: *Self) void {
        std.log.warn("Env Dumping values...", .{});
        var valuesIter = self.values.iterator();
        while (valuesIter.next()) |entry| {
            std.log.warn("Entry: {s} -> {s}", .{ entry.key_ptr.*, entry.value_ptr.* });
        }

        if (self.enclosing) |e| {
            std.log.warn("Env Enclosing environment...", .{});
            e.dumpValues();
        }
        std.log.warn("Env Done Dumping values...", .{});
    }

    pub fn define(self: *Self, name: []const u8, value: *object.Object) !void {
        // std.log.warn("Defining variable: {s} to {s}", .{ name, value });
        try self.values.put(name, value);
        // self.dumpValues();
        // std.log.warn("Defined variable: {}", .{self.values});
    }

    pub fn get(self: *Self, name: []const u8) !*object.Object {
        // std.log.warn("Defined variable map: {}", .{self.values});
        // self.dumpValues();

        const v = self.values.get(name);
        if (v) |val| {
            return val;
        }

        if (self.enclosing) |env| {
            return try env.get(name);
        }

        return EnvironmentErrors.UndefinedVariable;
    }

    pub fn assign(self: *Self, name: []const u8, value: *object.Object) !void {
        // std.log.warn("Assigning variable: {s} to {s}", .{ name, value });
        if (self.values.contains(name)) {
            try self.values.put(name, value);
            return;
        }

        if (self.enclosing) |env| {
            try env.assign(name, value);
            return;
        }

        return EnvironmentErrors.UndefinedVariable;
    }

    pub fn getAt(self: *Self, distance: usize, name: []const u8) !*object.Object {
        const s = try self.ancestor(distance);
        if (s.values.get(name)) |v| {
            return v;
        }

        return EnvironmentErrors.UndefinedVariable;
    }

    pub fn ancestor(self: *Self, distance: usize) !*Self {
        var env = self;
        for (0..distance) |_| {
            if (env.enclosing) |e| {
                env = e;
            }
        }

        return env;
    }

    pub fn assignAt(self: *Self, distance: usize, name: []const u8, value: *object.Object) !void {
        var s = try self.ancestor(distance);
        try s.values.put(name, value);
    }
};
