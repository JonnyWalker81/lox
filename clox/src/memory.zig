const std = @import("std");
const build_options = @import("build_options");

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

pub fn collectGarbage() !void {
    if (build_options.debug_log_gc) {
        std.debug.print("-- gc begin\n");
    }

    if (build_options.debug_log_gc) {
        std.debug.print("-- gc begin\n");
    }
}
