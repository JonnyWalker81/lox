const std = @import("std");
const scanner = @import("scanner.zig");

pub const Lox = struct {
    const Self = @This();

    const inner = struct {
        pub var hadError: bool = false;
    };

    pub fn runFile(allocator: std.mem.Allocator, path: []const u8) !void {
        const file = try std.fs.cwd().openFile(path, .{});
        defer file.close();

        var source: [4096]u8 = undefined;
        _ = try file.readAll(&source);
        try run(allocator, &source);

        if (inner.hadError) {
            std.process.exit(65);
        }
    }

    pub fn runPrompt(allocator: std.mem.Allocator) !void {
        const stdin = std.io.getStdIn().reader();
        var br = std.io.bufferedReader(stdin);
        const stdout = std.io.getStdOut().writer();
        const r = br.reader();

        while (true) {
            try stdout.print("> ", .{});
            var line_buf: [1024]u8 = undefined;
            const line = try r.readUntilDelimiterOrEof(&line_buf, '\n');
            if (line) |input| {
                try run(allocator, input);
                inner.hadError = false;
            }
        }
    }

    fn run(allocator: std.mem.Allocator, source: []const u8) !void {
        var scan = scanner.Scanner.init(allocator, source);
        const tokens = try scan.scanTokens();

        for (tokens) |token| {
            std.debug.print("{s}\n", .{token});
        }
    }

    pub fn err(line: usize, message: []const u8) void {
        report(line, "", message);
    }

    pub fn report(line: usize, where: []const u8, message: []const u8) void {
        std.debug.print("[line {d}] Error {s}: {s}\n", .{ line, where, message });
        const hadError = true;
        _ = hadError;
    }
};
