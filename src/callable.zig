const std = @import("std");
const interpreter = @import("interpreter.zig");
const object = @import("object.zig");
const ast = @import("ast.zig");
const environment = @import("environment.zig");

// pub fn Callable(
//     comptime Pointer: type,
//     comptime arityFn: *const fn (ptr: Pointer) void,
//     comptime callFn: *const fn (ptr: Pointer, i: *interpreter.Interpreter, arguments: []*object.Object) anyerror!*object.Object,
// ) type {
//     return struct {
//         ptr: Pointer,
//         const Self = @This();
//         pub fn init(p: Pointer) Self {
//             return .{ .ptr = p };
//         }
//         // interface methods wrapping passed-in funcs/methods
//         pub fn arity(self: Self) i32 {
//             arityFn(self.ptr);
//         }
//         pub fn call(self: Self, i: *interpreter.Interpreter, arguments: []*object.Object) anyerror!*object.Object {
//             callFn(self.ptr, i, arguments);
//         }
//     };
// }

pub const Callable = struct {
    // define interface fields: ptr,vtab
    ptr: *anyopaque, //ptr to instance
    vtab: *const VTab, //ptr to vtab
    const VTab = struct {
        arity: *const fn (ptr: *anyopaque) usize,
        call: *const fn (ptr: *anyopaque, i: *interpreter.Interpreter, arguments: []*object.Object) anyerror!?*object.Object,
    };

    // define interface methods wrapping vtable calls
    pub fn arity(self: Callable) usize {
        return self.vtab.arity(self.ptr);
    }

    pub fn call(self: Callable, i: *interpreter.Interpreter, arguments: []*object.Object) anyerror!?*object.Object {
        return self.vtab.call(self.ptr, i, arguments);
    }

    // cast concrete implementation types/objs to interface
    pub fn init(obj: anytype) Callable {
        const Ptr = @TypeOf(obj);
        const PtrInfo = @typeInfo(Ptr);
        std.debug.assert(PtrInfo == .Pointer); // Must be a pointer
        std.debug.assert(PtrInfo.Pointer.size == .One); // Must be a single-item pointer
        std.debug.assert(@typeInfo(PtrInfo.Pointer.child) == .Struct); // Must point to a struct
        // const alignment = PtrInfo.Pointer.alignment;
        const impl = struct {
            fn arity(ptr: *anyopaque) usize {
                const self: Ptr = @ptrCast(@alignCast(ptr));
                return self.arity();
            }

            fn call(ptr: *anyopaque, i: *interpreter.Interpreter, arguments: []*object.Object) anyerror!?*object.Object {
                const self: Ptr = @ptrCast(@alignCast(ptr));
                return self.call(i, arguments);
            }
        };
        return .{
            .ptr = obj,
            .vtab = &.{
                .arity = impl.arity,
                .call = impl.call,
            },
        };
    }

    pub fn inner(self: Callable, comptime T: anytype) *T {
        return @ptrCast(@alignCast(self.ptr));
    }
};

pub const Clock = struct {
    const Self = @This();

    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) Clock {
        return .{
            .allocator = allocator,
        };
    }

    pub fn arity(self: *Self) usize {
        _ = self;
        return 0;
    }

    pub fn call(self: *Self, _: *interpreter.Interpreter, _: []*object.Object) anyerror!?*object.Object {
        const obj = try self.allocator.create(object.Object);
        obj.* = .{
            .number = @floatFromInt(now()),
        };

        return obj;
    }

    fn now() i64 {
        return @divExact(std.time.milliTimestamp(), 1000);
    }
};

pub const LoxFunction = struct {
    const Self = @This();

    allocator: std.mem.Allocator,
    declaration: *ast.FunctionStatement,
    closure: *environment.Environment,
    isInitializer: bool = false,

    pub fn init(allocator: std.mem.Allocator, declaration: *ast.FunctionStatement, closure: *environment.Environment, isInitializer: bool) Self {
        return .{
            .allocator = allocator,
            .declaration = declaration,
            .closure = closure,
            .isInitializer = isInitializer,
        };
    }

    pub fn bind(
        self: *Self,
        method: *object.Object,
    ) anyerror!*object.Object {
        var env = environment.Environment.initWithEnclosing(self.allocator, self.closure);
        try env.define("this", method);
        const bound = try self.allocator.create(object.Object);
        const c = try self.allocator.create(Callable);
        const func = try self.allocator.create(LoxFunction);
        func.* = LoxFunction.init(self.allocator, self.declaration, env, self.isInitializer);
        c.* = Callable.init(func);
        bound.* = .{
            .callable = c,
        };

        return bound;
    }

    pub fn arity(self: *Self) usize {
        return self.declaration.parameters.len;
    }

    pub fn call(self: *Self, i: *interpreter.Interpreter, arguments: []*object.Object) anyerror!?*object.Object {
        // std.debug.print("calling function {}\n", .{self.declaration.name.typ});
        const env = environment.Environment.initWithEnclosing(self.allocator, self.closure);
        for (self.declaration.parameters, 0..) |parameter, idx| {
            const value = arguments[idx];
            const name = try std.fmt.allocPrint(self.allocator, "{}", .{parameter.typ});
            // std.debug.print("setting {s} to {s}\n", .{ name, value });
            try env.define(name, value);
        }
        const obj = try i.executeBlock(self.declaration.body, env);
        if (obj.* == .returnValue) {
            // std.debug.print("return value: {}\n", .{obj.returnValue});
            // std.log.warn("return value: {}\n", .{obj.returnValue});
            if (self.isInitializer) {
                return self.closure.getAt(0, "this");
            }

            return obj;
        }

        if (self.isInitializer) {
            return self.closure.getAt(0, "this");
        }

        // return i.env.getAt(0, "return");
        return null;
    }
};

// pub fn raise(message: []const u8, value: u64, opts: anytype) anyerror {
//     comptime if (has_error(@TypeOf(opts))) {
//         opts.error_payload.message = message;
//         opts.error_payload.value = value;
//     };
//     return error.RetrunValue;
// }

// fn has_error(comptime T: type) bool {
//     return @hasField(T, "error_payload");
//     // consider checking for error allocator, too, if necessary.
// }
