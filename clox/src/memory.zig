const std = @import("std");
const build_options = @import("build_options");
const value = @import("value.zig");
const VM = @import("vm.zig").VM;
const debug = @import("debug.zig");
const Compiler = @import("compiler.zig").Compiler;

pub const GCAllocator = struct {
    const Self = @This();

    backingAllocator: std.mem.Allocator,
    vm: *VM,

    const vtable: std.mem.Allocator.VTable = .{ .alloc = alloc, .resize = resize, .free = free };

    pub fn init(backingAllocator: std.mem.Allocator, vm: *VM) Self {
        return Self{
            .backingAllocator = backingAllocator,
            .vm = vm,
        };
    }

    pub fn allocator(self: *Self) std.mem.Allocator {
        return .{
            .ptr = self,
            .vtable = &vtable,
        };
    }

    fn alloc(
        ctx: *anyopaque,
        len: usize,
        ptr_align: u8,
        ra: usize,
    ) ?[*]u8 {
        std.debug.print("alloc\n", .{});
        const self: *Self = @ptrCast(@alignCast(ctx));

        self.collectGarbage() catch @panic("gc failed");
        return self.backingAllocator.rawAlloc(len, ptr_align, ra);
    }

    fn resize(
        ctx: *anyopaque,
        buf: []u8,
        log2_buf_align: u8,
        new_len: usize,
        ra: usize,
    ) bool {
        std.debug.print("resize\n", .{});
        const self: *Self = @ptrCast(@alignCast(ctx));
        const size = self.backingAllocator.rawResize(buf, log2_buf_align, new_len, ra);

        if (new_len > buf.len) {
            self.collectGarbage() catch @panic("gc failed");
        }

        return size;
    }

    fn free(
        ctx: *anyopaque,
        buf: []u8,
        log2_buf_align: u8,
        ra: usize,
    ) void {
        std.debug.print("free\n", .{});
        const self: *Self = @ptrCast(@alignCast(ctx));
        self.backingAllocator.rawFree(buf, log2_buf_align, ra);
    }

    pub fn collectGarbage(self: *Self) !void {
        if (build_options.debug_log_gc) {
            std.debug.print("-- gc begin\n", .{});
        }

        self.markRoots();

        if (build_options.debug_log_gc) {
            std.debug.print("-- gc begin\n", .{});
        }
    }

    fn markRoots(self: *Self) void {
        for (0..self.vm.stackTop) |i| {
            self.markValue(self.vm.stack[i]);
        }

        self.markTable(&self.vm.globals);

        for (0..self.vm.frameCount) |i| {
            self.markObject(@constCast(&.{ .closure = self.vm.frames[i].closure }));
        }

        var upvalue = self.vm.openUpvalues;
        while (upvalue != null) : (upvalue = upvalue.?.next) {
            self.markObject(@constCast(&.{ .upvalue = upvalue.? }));
        }
    }

    fn traceReferences(self: *Self) void {
        while (self.vm.grayStack.len > 0) : (self.vm.grayStack.pop()) {
            const obj = self.vm.grayStack[self.vm.grayStack.len - 1];
            self.blackenObject(obj);
        }
    }

    fn markValue(self: *Self, v: value.Value) void {
        if (v.isObject()) {
            const obj = @constCast(&v);
            self.markObject(obj);
        }
    }

    fn markObject(self: *Self, v: *value.Value) void {
        var obj = v.asObject();

        if (build_options.debug_log_gc) {
            std.debug.print("{*} mark ", .{obj});
            debug.printValue(v.*);
            std.debug.print("\n", .{});
        }

        obj.isMarked = true;

        self.vm.grayStack.append(obj) catch @panic("grayStack append failed");
    }

    fn markTable(self: *Self, table: *std.AutoHashMap(*value.String, value.Value)) void {
        var iter = table.iterator();
        while (iter.next()) |entry| {
            self.markObject(@constCast(&.{ .string = @constCast(entry.key_ptr.*) }));
            self.markValue(entry.value_ptr.*);
        }
    }

    fn markCompilerRoots(self: *Self) void {
        var compiler: ?*Compiler = self.vm.compiler;
        while (compiler) : (compiler = compiler.?.enclosing) {
            self.markObject(&compiler.function);
        }
    }
};

pub fn growCapacity(capacity: usize) usize {
    if (capacity < 8) {
        return 8;
    }
    return capacity * 2;
}

pub fn reallocate(comptime T: anytype, allocator: std.mem.Allocator, array: ?[]T, oldSize: usize, newSize: usize) !?[]T {
    _ = oldSize;

    if (array == null) {
        return allocator.alloc(T, newSize) catch unreachable;
    }

    if (newSize == 0) {
        allocator.free(array.?);
        return null;
    }

    return allocator.realloc(array.?, newSize) catch unreachable;
}
