const std = @import("std");
const interpreter = @import("interpreter.zig");
const object = @import("object.zig");
const token = @import("token.zig");
const environment = @import("environment.zig");
const callable = @import("callable.zig");

pub const LoxClass = struct {
    const Self = @This();

    allocator: std.mem.Allocator,
    name: []const u8,
    methods: std.StringHashMap(*object.Object),
    superclass: ?*object.Object,

    pub fn init(
        allocator: std.mem.Allocator,
        className: []const u8,
        methods: std.StringHashMap(*object.Object),
        superclass: ?*object.Object,
    ) *Self {
        const class = allocator.create(Self) catch unreachable;
        class.* = .{
            .allocator = allocator,
            .name = className,
            .methods = methods,
            .superclass = superclass,
        };

        return class;
    }

    pub fn arity(self: *Self) usize {
        const initializer = self.findMethod("init");
        if (initializer) |in| {
            if (in.isCallable()) {
                return in.callable.inner(callable.LoxFunction).arity();
            }
        }

        return 0;
    }

    pub fn call(self: *Self, i: *interpreter.Interpreter, arguments: []*object.Object) anyerror!?*object.Object {
        const instance = LoxInstance.init(self.allocator, self);
        const initializer = self.findMethod("init");
        if (initializer) |in| {
            if (in.isCallable()) {
                // in.callable.inner(callable.LoxFunction).call(i, arguments);
                const obj = try self.allocator.create(object.Object);
                obj.* = .{
                    .implementation = instance,
                };
                const c = try in.callable.inner(callable.LoxFunction).bind(obj);
                if (c.isCallable()) {
                    _ = try c.callable.inner(callable.LoxFunction).call(i, arguments);
                }
            }
        }

        const obj = try self.allocator.create(object.Object);
        obj.* = .{
            .implementation = instance,
        };

        return obj;
    }

    pub fn findMethod(
        self: *Self,
        name: []const u8,
    ) ?*object.Object {
        if (self.methods.contains(name)) {
            return self.methods.get(name);
        }

        if (self.superclass) |superclass| {
            return superclass.class.inner(Self).findMethod(name);
        }

        return null;
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
    fields: std.StringHashMap(*object.Object),

    pub fn init(allocator: std.mem.Allocator, klass: *LoxClass) *Self {
        const instance = allocator.create(Self) catch unreachable;
        instance.* = .{
            .allocator = allocator,
            .klass = klass,
            .fields = std.StringHashMap(*object.Object).init(allocator),
        };

        return instance;
    }

    pub fn get(
        self: *Self,
        name: token.Token,
    ) anyerror!?*object.Object {
        const nameStr = name.toString(self.allocator);
        if (self.fields.contains(nameStr)) {
            return self.fields.get(nameStr);
        }

        const method = self.klass.findMethod(nameStr);
        if (method) |m| {
            if (m.isCallable()) {
                const obj = try self.allocator.create(object.Object);
                obj.* = .{
                    .implementation = self,
                };
                return m.callable.inner(callable.LoxFunction).bind(obj);
            }
        }

        return error.UndefinedProperty;
    }

    pub fn set(
        self: *Self,
        name: token.Token,
        value: *object.Object,
    ) anyerror!void {
        const nameStr = name.toString(self.allocator);
        try self.fields.put(nameStr, value);
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
