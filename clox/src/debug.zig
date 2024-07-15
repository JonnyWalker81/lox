const std = @import("std");
const chunk = @import("chunk.zig");
const value = @import("value.zig");

pub fn disassembleChunk(c: chunk.Chunk, name: []const u8) void {
    std.debug.print("== {s} ==\n", .{name});

    var offset: usize = 0;
    while (offset < c.count) {
        offset = disassembleInstruction(c, offset);
    }
    std.debug.print("\n\n", .{});
}

pub fn disassembleInstruction(c: chunk.Chunk, offset: usize) usize {
    std.debug.print("{d:0>4} ", .{offset});

    if (offset > 0) {
        if (c.lines.items[offset] == c.lines.items[offset - 1]) {
            std.debug.print("   | ", .{});
        } else {
            std.debug.print("{d:4} ", .{c.lines.items[offset]});
        }
    } else {
        std.debug.print("{d:4} ", .{c.lines.items[offset]});
    }

    // if (c.code) |code| {
    const instruction: chunk.OpCode = @enumFromInt(c.code.items[offset]);
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
        .OpLoop => {
            return jumpInstruction("OP_LOOP", -1, c, offset);
        },
        .OpCall => {
            return byteInstruction("OP_CALL", c, offset);
        },
        .OpClosure => {
            var o = offset + 1;
            const constant: u8 = c.code.items[o];
            o += 1;
            std.debug.print("{s:16} {d:4} ", .{ "OP_CLOSURE", constant });
            printValue(c.constants.items[constant]);
            std.debug.print("\n", .{});

            const ff = c.constants.items[constant];
            const func = ff.asObject().asFunction();
            for (0..func.upvalueCount) |_| {
                const isLocal: u8 = c.code.items[o];
                o += 1;
                const index: u8 = c.code.items[o];
                o += 1;
                std.debug.print("{d:4}      |                     {s} {d}\n", .{ offset - 2, if (isLocal == 1) "local" else "upvalue", index });
            }
            return o;
        },
        .OpCloseUpvalue => {
            return simpleInstruction("OP_CLOSE_UPVALUE", offset);
        },
        .OpReturn => {
            return simpleInstruction("OP_RETURN", offset);
        },
        .OpClass => {
            return constantInstruction("OP_CLASS", c, offset);
        },
        .OpMethod => {
            return constantInstruction("OP_METHOD", c, offset);
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
        .OpGetUpvalue => {
            return byteInstruction("OP_GET_UPVALUE", c, offset);
        },
        .OpSetUpvalue => {
            return byteInstruction("OP_SET_UPVALUE", c, offset);
        },
        .OpGetProperty => {
            return constantInstruction("OP_GET_PROPERTY", c, offset);
        },
        .OpSetProperty => {
            return constantInstruction("OP_SET_PROPERTY", c, offset);
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
    // } else {
    //     std.debug.print("No code\n", .{});
    // }

    return offset;
}

fn simpleInstruction(name: []const u8, offset: usize) usize {
    std.debug.print("{s}\n", .{name});
    return offset + 1;
}

fn byteInstruction(name: []const u8, c: chunk.Chunk, offset: usize) usize {
    // if (c.code) |code| {
    const slot: u8 = c.code.items[offset + 1];
    std.debug.print("{s} {d:4}\n", .{ name, slot });
    // }
    return offset + 2;
}

fn jumpInstruction(name: []const u8, sign: i32, c: chunk.Chunk, offset: usize) usize {
    // if (c.code) |code| {
    var jump: u16 = @intCast(@as(u16, c.code.items[offset + 1]) << 8);
    jump |= @intCast(c.code.items[offset + 2]);
    const loc = @as(i32, @intCast(offset)) + 3 + sign * @as(i32, @intCast(jump));
    std.debug.print("{s} {d:4} -> {d}\n", .{ name, offset, loc });
    // }
    return offset + 3;
}

fn constantInstruction(name: []const u8, c: chunk.Chunk, offset: usize) usize {
    // if (c.code) |code| {
    const constant: u8 = c.code.items[offset + 1];
    std.debug.print("{s} {d:4} '", .{ name, constant });
    // if (c.constants.values) |vals| {
    printValue(c.constants.items[constant]);
    // }
    std.debug.print("'\n", .{});
    // }
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
        .obj => |o| {
            printObject(o);
        },
    }
}

pub fn printObject(obj: *value.Obj) void {
    switch (obj.type) {
        .string => {
            const s = obj.asString();
            std.debug.print("{s}", .{s.bytes});
        },
        .function => {
            if (obj.asFunction().name) |name| {
                std.debug.print("<fn {s}>", .{name});
            } else {
                std.debug.print("<script>", .{});
            }
        },
        .native => |_| {
            std.debug.print("<native fn>", .{});
        },
        .closure => {
            if (obj.asClosure().function.name) |name| {
                std.debug.print("<fn {s}>", .{name});
            } else {
                std.debug.print("<script>", .{});
            }
        },
        .upvalue => |_| {
            std.debug.print("upvalue", .{});
        },
        .class => {
            const c = obj.asClass();
            std.debug.print("{s}", .{c.name.bytes});
        },
        .instance => {
            const i = obj.asInstance();
            std.debug.print("{s} instance", .{i.class.name.bytes});
        },
        .boundMethod => {
            const bm = obj.asBoundMethod();
            if (bm.method.function.name) |name| {
                std.debug.print("<fn {s}>", .{name.bytes});
            } else {
                std.debug.print("<script>", .{});
            }
        },
    }
}
