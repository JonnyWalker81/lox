const std = @import("std");
const memory = @import("memory.zig");
const debug = @import("debug.zig");
const value = @import("value.zig");
const vm = @import("vm.zig");

pub const OpCode = enum(u8) {
    OpNegate,
    OpPrint,
    OpJump,
    OpJumpIfFalse,
    OpLoop,
    OpCall,
    OpClosure,
    OpCloseUpvalue,
    OpReturn,
    OpConstant,
    OpNil,
    OpTrue,
    OpFalse,
    OpPop,
    OpGetLocal,
    OpSetLocal,
    OpGetGlobal,
    OpDefineGlobal,
    OpSetGlobal,
    OpGetUpvalue,
    OpSetUpvalue,
    OpEqual,
    OpGetProperty,
    OpSetProperty,
    OpGreater,
    OpLess,
    OpAdd,
    OpSubtract,
    OpMultiply,
    OpDivide,
    OpNot,
    OpClass,
    OpMethod,
};

pub const Chunk = struct {
    const Self = @This();

    count: usize = 0,
    code: std.ArrayList(u8),
    constants: std.ArrayList(value.Value),
    lines: std.ArrayList(usize),

    pub fn init(allocator: std.mem.Allocator) Chunk {
        return .{
            .constants = std.ArrayList(value.Value).init(allocator),
            .code = std.ArrayList(u8).init(allocator),
            .lines = std.ArrayList(usize).init(allocator),
        };
    }

    pub fn deinit(self: *Self) void {
        self.code.deinit();
        self.lines.deinit();
        self.constants.deinit();
    }

    pub fn writeChunk(self: *Self, byte: u8, line: usize) !void {
        // if (self.capacity < self.count + 1) {
        //     const oldCapacity = self.capacity;
        //     self.capacity = memory.growCapacity(oldCapacity);
        //     const newArray = try memory.growArray(u8, self.arena.allocator(), self.code, oldCapacity, self.capacity);
        //     self.code = newArray;

        //     const newLinesArray = try memory.growArray(usize, self.arena.allocator(), self.lines, oldCapacity, self.capacity);
        //     self.lines = newLinesArray;
        // }

        // if (self.code) |code| {
        // if (self.lines) |lines| {
        //     lines[self.count] = line;
        //     //         code[self.count] = byte;
        //     //         self.count += 1;
        // }
        // }
        //
        try self.code.append(byte);
        try self.lines.append(line);
        self.count += 1;
    }

    // pub fn freeChunk(self: *Self) void {
    //     self.count = 0;
    //     self.capacity = 0;
    //     self.code = null;
    // }
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
