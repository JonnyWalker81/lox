const std = @import("std");
const build_options = @import("build_options");
const value = @import("value.zig");

pub const GCAllocator = struct {
    const Self = @This();

    backingAllocator: std.mem.Allocator,
    stack: [*]value.Value,

    pub fn init(backingAllocator: std.mem.Allocator, stack: [*]value.Value) Self {
        return Self{
            .backingAllocator = backingAllocator,
            .stack = stack,
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
        log2_ptr_align: u8,
        ra: usize,
    ) ?[*]u8 {
        const self: *Self = @ptrCast(@alignCast(ctx));
        return self.backingAllocator.rawAlloc(len, log2_ptr_align, ra);
    }

    fn resize(
        ctx: *anyopaque,
        buf: []u8,
        log2_buf_align: u8,
        new_len: usize,
        ra: usize,
    ) bool {
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
        _ = self;
        // for (0..self.stack.len) |i| {
        //     self.markValue(self.stack[i]);
        // }
    }

    // fn markValue(self: *Self, v: value.Value) void {
    //     if (v.isObject()) {
    //         self.markObject(v);
    //     }
    // }

    // fn markObject(self: *Self, v: value.Value) void {
    //     const obj = v.asObject();

    //     if (obj.isMarked()) {
    //         return;
    //     }

    //     obj.mark();

    //     for (obj.fields) |field| {
    //         self.markValue(*field);
    //     }
    // }
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
