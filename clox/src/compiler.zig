const std = @import("std");
const scanner = @import("scanner.zig");

pub const Compiler = struct {
    arena: std.heap.ArenaAllocator,

    pub fn init(allocator: std.mem.Allocator) Compiler {
        return Compiler{
            .arena = std.heap.ArenaAllocator.init(allocator),
        };
    }

    pub fn compile(self: *Compiler, source: []const u8) !void {
        var s = scanner.Scanner.init(self.arena.allocator(), source);
        var line: i32 = -1;
        while (true) {
            const token = try s.scanToken();
            if (token.line != line) {
                std.debug.print("{d:4} ", .{token.line});
                line = @intCast(token.line);
            } else {
                std.debug.print("     | ", .{});
            }
            std.debug.print("'{s}'\n", .{token.type});
            if (token.type == .eof) {
                break;
            }
        }
    }
};
