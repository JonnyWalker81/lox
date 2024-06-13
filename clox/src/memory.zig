const std = @import("std");

pub fn growCapacity(capacity: usize) usize {
    if (capacity < 8) {
        return 8;
    }
    return capacity * 2;
}

pub fn growArray(comptime T: anytype, allocator: std.mem.Allocator, array: ?[]T, oldSize: usize, newSize: usize) !?[]T {
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
