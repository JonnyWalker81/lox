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
            .vtable = &.{
                .alloc = alloc,
                .resize = resize,
                .free = free,
            },
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
        self.traceReferences();
        self.sweep();

        if (build_options.debug_log_gc) {
            std.debug.print("-- gc end\n", .{});
        }
    }

    fn markRoots(self: *Self) void {
        for (0..self.vm.stackTop) |i| {
            self.markValue(self.vm.stack[i]);
        }

        self.markTable(&self.vm.globals);

        self.markCompilerRoots();

        for (0..self.vm.frameCount) |i| {
            self.markObject(&self.vm.frames[i].closure.obj);
        }

        var upvalue = self.vm.openUpvalues;
        while (upvalue != null) : (upvalue = upvalue.?.next) {
            self.markObject(&upvalue.?.obj);
        }
    }

    fn traceReferences(self: *Self) void {
        while (self.vm.grayStack.items.len > 0) : (_ = self.vm.grayStack.pop()) {
            const val = self.vm.grayStack.items[self.vm.grayStack.items.len - 1];
            self.blackenObject(val);
        }
    }

    fn sweep(self: *Self) void {
        var previous: ?*value.Obj = null;
        var object = self.vm.objects;
        while (object) |obj| {
            if (obj.isMarked) {
                obj.isMarked = false;
                previous = obj;
                object = obj.next;
            } else {
                const unreached = obj;
                object = obj.next;
                if (previous) |p| {
                    p.next = object;
                } else {
                    self.vm.objects = object;
                }

                unreached.destroy(self.vm);
            }
        }
    }

    fn markValue(self: *Self, v: value.Value) void {
        if (v.isObject()) {
            self.markObject(v.asObject());
        }
    }

    fn markArray(self: *Self, array: []value.Value) void {
        for (array) |val| {
            self.markValue(val);
        }
    }

    fn blackenObject(self: *Self, obj: *value.Obj) void {
        if (build_options.debug_log_gc) {
            std.debug.print("{*} blacken ", .{obj});
            debug.printObject(obj);
            std.debug.print("\n", .{});
        }

        switch (obj.type) {
            .upvalue => {
                if (obj.asUpvalue().closed) |c| {
                    self.markValue(c);
                }
            },
            .function => {
                const f = obj.asFunction();
                if (f.name) |name| {
                    self.markObject(&name.obj);
                }
                self.markArray(f.chnk.constants.items);
            },
            .closure => {
                const c = obj.asClosure();
                self.markObject(&c.function.obj);
                for (c.upvalues) |up| {
                    self.markObject(&up.obj);
                }
            },
            else => {
                std.debug.print("unhandled object type {}\n", .{obj.type});
            },
        }
    }

    fn markObject(self: *Self, obj: *value.Obj) void {
        if (obj.isMarked) {
            return;
        }

        if (build_options.debug_log_gc) {
            std.debug.print("{*} mark ", .{obj});
            debug.printObject(obj);
            std.debug.print("\n", .{});
        }

        obj.isMarked = true;

        self.vm.grayStack.append(obj) catch @panic("grayStack append failed");
    }

    fn markTable(self: *Self, table: *std.AutoHashMap(*value.String, value.Value)) void {
        var iter = table.iterator();
        while (iter.next()) |entry| {
            self.markObject(&entry.key_ptr.*.obj);
            self.markValue(entry.value_ptr.*);
        }
    }

    fn markCompilerRoots(self: *Self) void {
        var maybeCompiler: ?*Compiler = &self.vm.comp;
        while (maybeCompiler) |compiler| {
            self.markObject(&compiler.function.obj);
            maybeCompiler = compiler.enclosing;
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
