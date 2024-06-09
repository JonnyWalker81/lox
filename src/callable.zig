const std = @import("std");
const interpreter = @import("interpreter.zig");
const object = @import("object.zig");

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
        call: *const fn (ptr: *anyopaque, i: *interpreter.Interpreter, arguments: []*object.Object) anyerror!*object.Object,
    };

    // define interface methods wrapping vtable calls
    pub fn arity(self: Callable) usize {
        return self.vtab.arity(self.ptr);
    }
    pub fn call(self: Callable, i: *interpreter.Interpreter, arguments: []*object.Object) anyerror!*object.Object {
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

            fn call(ptr: *anyopaque, i: *interpreter.Interpreter, arguments: []*object.Object) anyerror!*object.Object {
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

    pub fn call(self: *Self, _: *interpreter.Interpreter, _: []*object.Object) anyerror!*object.Object {
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
