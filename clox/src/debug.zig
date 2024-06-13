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
            .OpReturn => {
                return simpleInstruction("OP_RETURN", offset);
            },
            .OpConstant => {
                return constantInstruction("OP_CONSTANT", c, offset);
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

fn constantInstruction(name: []const u8, c: *chunk.Chunk, offset: usize) usize {
    if (c.code) |code| {
        const constant: u8 = code[offset + 1];
        std.debug.print("{s} {d:4} '", .{ name, constant });
        if (c.constants.values) |vals| {
            printValue(vals[constant]);
        }
        std.debug.print("'\n", .{});
    }
    return offset + 2;
}

pub fn printValue(val: value.Value) void {
    std.debug.print("{d}", .{val});
}
