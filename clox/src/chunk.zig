const std = @import("std");
const memory = @import("memory.zig");
const debug = @import("debug.zig");
const value = @import("value.zig");
const vm = @import("vm.zig");

pub const OpCode = enum(u8) {
    OpNegate,
    OpPrint,
    OpReturn,
    OpConstant,
    OpNil,
    OpTrue,
    OpFalse,
    OpPop,
    OpGetGlobal,
    OpDefineGlobal,
    OpEqual,
    OpGreater,
    OpLess,
    OpAdd,
    OpSubtract,
    OpMultiply,
    OpDivide,
    OpNot,
};

pub const Chunk = struct {
    const Self = @This();

    arena: std.heap.ArenaAllocator,
    count: usize = 0,
    capacity: usize = 0,
    code: ?[]u8 = null,
    // constants: *value.ValueArray,
    constants: std.ArrayList(value.Value),
    lines: ?[]usize = null,

    pub fn init(allocator: std.mem.Allocator) *Chunk {
        const arena = std.heap.ArenaAllocator.init(allocator);
        const chunk = allocator.create(Chunk) catch unreachable;
        // const constants = value.ValueArray.init(allocator);
        const constants = std.ArrayList(value.Value).init(allocator);
        chunk.* = .{
            .arena = arena,
            .constants = constants,
        };

        return chunk;
    }

    pub fn deinit(self: *Self) void {
        self.arena.deinit();
    }

    pub fn writeChunk(self: *Self, byte: u8, line: usize) !void {
        if (self.capacity < self.count + 1) {
            const oldCapacity = self.capacity;
            self.capacity = memory.growCapacity(oldCapacity);
            const newArray = try memory.growArray(u8, self.arena.allocator(), self.code, oldCapacity, self.capacity);
            self.code = newArray;

            const newLinesArray = try memory.growArray(usize, self.arena.allocator(), self.lines, oldCapacity, self.capacity);
            self.lines = newLinesArray;
        }

        if (self.code) |code| {
            if (self.lines) |lines| {
                lines[self.count] = line;
                code[self.count] = byte;
                self.count += 1;
            }
        }
    }

    pub fn addConstant(self: *Self, val: value.Value) !usize {
        // try self.constants.write(val);
        // return self.constants.count - 1;
        try self.constants.append(val);
        return self.constants.items.len - 1;
    }

    pub fn freeChunk(self: *Self) void {
        self.count = 0;
        self.capacity = 0;
        self.code = null;
    }
};

var test_allocator = std.testing.allocator;
// test "test chunk" {
//     var arena = std.heap.ArenaAllocator.init(test_allocator);
//     defer arena.deinit();
//     var chunk = Chunk.init(arena.allocator());
//     defer chunk.freeChunk();

//     var v = vm.VM.init(arena.allocator());
//     defer v.deinit();

//     var constant = try chunk.addConstant(1.2);
//     try chunk.writeChunk(@intFromEnum(OpCode.OpConstant), 123);
//     try chunk.writeChunk(@intCast(constant), 123);

//     constant = try chunk.addConstant(3.4);
//     try chunk.writeChunk(@intFromEnum(OpCode.OpConstant), 123);
//     try chunk.writeChunk(@intCast(constant), 123);

//     try chunk.writeChunk(@intFromEnum(OpCode.OpAdd), 123);

//     constant = try chunk.addConstant(5.6);
//     try chunk.writeChunk(@intFromEnum(OpCode.OpConstant), 123);
//     try chunk.writeChunk(@intCast(constant), 123);

//     try chunk.writeChunk(@intFromEnum(OpCode.OpDivide), 123);

//     try chunk.writeChunk(@intFromEnum(OpCode.OpNegate), 123);
//     try chunk.writeChunk(@intFromEnum(OpCode.OpReturn), 123);

//     debug.disassembleChunk(chunk, "test chunk");

//     _ = try v.interpret(chunk);

//     // try std.testing.expectEqual(4, chunk.count);
//     // try std.testing.expectEqual(8, chunk.capacity);
//     if (chunk.code) |code| {
//         try std.testing.expectEqual(code[0], @intFromEnum(OpCode.OpConstant));
//     }
//     // std.testing.expectEqual(chunk.code[1], OpCode.OpReturn);
// }
