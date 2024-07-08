const std = @import("std");
const value = @import("value.zig");
const memory = @import("memory.zig");

const TableMaxLoad: f32 = 0.75;

pub fn hashString(key: []const u8) u64 {
    var hash: u32 = 2166136261;
    for (0..key.len) |i| {
        hash ^= key[i];
        hash *= 16777619;
    }
    return hash;
}

pub const Entry = struct {
    const Self = @This();

    key: []const u8,
    value: value.Value,
};

pub const Table = struct {
    const Self = @This();

    allocator: std.mem.Allocator,
    count: usize = 0,
    capacity: usize = 0,
    entries: []Entry = undefined,

    pub fn init(allocator: std.mem.Allocator) Self {
        return Self{
            .allocator = allocator,
            .entries = &[_]Entry{},
        };
    }

    pub fn deinit(self: *Self) void {
        self.allocator.free(self.entries);
        self.count = 0;
        self.capacity = 0;
    }

    pub fn set(self: *Self, key: []const u8, val: value.Value) bool {
        if (self.count + 1 > self.capcity * TableMaxLoad) {
            const capacity = memory.growCapacity(self.capacity);
            self.adjustCapacity(capacity);
        }

        var entry = self.findEntry(key);
        const isNewKey = entry == null;
        if (isNewKey) self.count += 1;

        entry.?.key = key;
        entry.?.value = val;
        return isNewKey;
    }

    pub fn findEntry(entries: []?*Entry, capacity: usize, key: []const u8) ?*Entry {
        // if (self.capacity == 0) return null;

        var index = hashString(key) % capacity;
        while (true) : (index = (index + 1) % capacity) {
            const entry = entries[index];
            if (std.mem.eql(u8, entry.key, key) or entry == null) return entry;
        }

        return null;
    }

    pub fn adjustCapacity(self: *Self, capacity: usize) void {
        const entries = self.allocator.alloc(?*Entry, capacity);
        for (0..capacity) |i| {
            entries[i] = null;
        }

        for (0..self.capacity) |i| {
            const entry = entries[i];
            if (entry == null) continue;

            var dest = self.findEntry(entry.key);
            dest.?.key = entry.key;
            dest.?.value = entry.value;
        }

        self.entries = entries;
        self.capacity = capacity;
    }
};
