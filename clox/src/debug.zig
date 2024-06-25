const std = @import("std");
const chunk = @import("chunk.zig");
const value = @import("value.zig");

pub fn disassembleChunk(c: *chunk.Chunk, name: []const u8) void {
    std.debug.print("== {s} ==\n", .{name});

    var offset: usize = 0;
    while (offset < c.count) {
        offset = disassembleInstruction(c, offset);
    }
}

pub fn disassembleInstruction(c: *chunk.Chunk, offset: usize) usize {
    std.debug.print("{d:0>4} ", .{offset});

    if (offset > 0) {
        if (c.lines) |lines| {
            if (lines[offset] == lines[offset - 1]) {
                std.debug.print("   | ", .{});
            } else {
                std.debug.print("{d:4} ", .{lines[offset]});
            }
        }
    } else {
        if (c.lines) |lines| {
            std.debug.print("{d:4} ", .{lines[offset]});
        }
    }

    if (c.code) |code| {
        const instruction: chunk.OpCode = @enumFromInt(code[offset]);
        switch (instruction) {
            .OpPrint => {
                return simpleInstruction("OP_PRINT", offset);
            },
            .OpJump => {
                return jumpInstruction("OP_JUMP", 1, c, offset);
            },
            .OpJumpIfFalse => {
                return jumpInstruction("OP_JUMP_IF_FALSE", 1, c, offset);
            },
            .OpReturn => {
                return simpleInstruction("OP_RETURN", offset);
            },
            .OpConstant => {
                return constantInstruction("OP_CONSTANT", c, offset);
            },
            .OpNil => {
                return simpleInstruction("OP_NIL", offset);
            },
            .OpTrue => {
                return simpleInstruction("OP_TRUE", offset);
            },
            .OpFalse => {
                return simpleInstruction("OP_FALSE", offset);
            },
            .OpPop => {
                return simpleInstruction("OP_POP", offset);
            },
            .OpGetLocal => {
                return byteInstruction("OP_GET_LOCAL", c, offset);
            },
            .OpSetLocal => {
                return byteInstruction("OP_SET_LOCAL", c, offset);
            },
            .OpGetGlobal => {
                return constantInstruction("OP_GET_GLOBAL", c, offset);
            },
            .OpDefineGlobal => {
                return constantInstruction("OP_DEFINE_GLOBAL", c, offset);
            },
            .OpSetGlobal => {
                return constantInstruction("OP_SET_GLOBAL", c, offset);
            },
            .OpEqual => {
                return simpleInstruction("OP_EQUAL", offset);
            },
            .OpGreater => {
                return simpleInstruction("OP_GREATER", offset);
            },
            .OpLess => {
                return simpleInstruction("OP_LESS", offset);
            },
            .OpNot => {
                return simpleInstruction("OP_NOT", offset);
            },
            .OpNegate => {
                return simpleInstruction("OP_NEGATE", offset);
            },
            .OpAdd => {
                return simpleInstruction("OP_ADD", offset);
            },
            .OpSubtract => {
                return simpleInstruction("OP_SUBTRACT", offset);
            },
            .OpMultiply => {
                return simpleInstruction("OP_MULTIPLY", offset);
            },
            .OpDivide => {
                return simpleInstruction("OP_DIVIDE", offset);
            },
            // else => {
            //     std.debug.print("Unknown opcode {}\n", .{instruction});
            //     return offset + 1;
            // },
        }
    } else {
        std.debug.print("No code\n", .{});
    }

    return offset;
}

fn simpleInstruction(name: []const u8, offset: usize) usize {
    std.debug.print("{s}\n", .{name});
    return offset + 1;
}

fn byteInstruction(name: []const u8, c: *chunk.Chunk, offset: usize) usize {
    if (c.code) |code| {
        const slot: u8 = code[offset + 1];
        std.debug.print("{s} {d:4}\n", .{ name, slot });
    }
    return offset + 2;
}

fn jumpInstruction(name: []const u8, sign: i32, c: *chunk.Chunk, offset: usize) usize {
    if (c.code) |code| {
        var jump: u16 = @intCast(@as(u16, code[offset + 1]) << 8);
        jump |= @intCast(code[offset + 2]);
        std.debug.print("{s} {d:4} -> {d}\n", .{ name, offset, offset + 3 + @as(u16, @intCast(sign)) * jump });
    }
    return offset + 3;
}

fn constantInstruction(name: []const u8, c: *chunk.Chunk, offset: usize) usize {
    if (c.code) |code| {
        const constant: u8 = code[offset + 1];
        std.debug.print("{s} {d:4} '", .{ name, constant });
        // if (c.constants.values) |vals| {
        printValue(c.constants.items[constant]);
        // }
        std.debug.print("'\n", .{});
    }
    return offset + 2;
}

pub fn printValue(val: value.Value) void {
    switch (val) {
        .number => |n| {
            std.debug.print("{d}", .{n});
        },
        .bool => |b| {
            std.debug.print("{}", .{b});
        },
        .nil => {
            std.debug.print("nil", .{});
        },
        .string => |s| {
            std.debug.print("{s}", .{s});
        },
    }
}
