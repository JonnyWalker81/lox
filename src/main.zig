const std = @import("std");

pub fn main() !void {
    var args = std.process.args();
    _ = args.skip();

    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    var arena = std.heap.ArenaAllocator.init(gpa.allocator());
    defer arena.deinit();

    if (args.len > 2) {
        try std.debug.print("Usage: jlox [script]", .{});
        return std.os.exit(64);
    } else if (args.len == 2) {
        try runFile(arena, args[1]);
    } else {
        try runPrompt(arena);
    }

    // Prints to stderr (it's a shortcut based on `std.io.getStdErr()`)main
    // std.debug.print("All your {s} are belong to us.\n", .{"codebase"});

    // stdout is for the actual output of your application, for example if you
    // are implementing gzip, then only the compressed bytes should be sent to
    // stdout, not any debugging messages.
    // const stdout_file = std.io.getStdOut().writer();
    // var bw = std.io.bufferedWriter(stdout_file);
    // const stdout = bw.writer();

    // try stdout.print("Run `zig build test` to run the tests.\n", .{});

    // try bw.flush(); // don't forget to flush!
}

const Lox = struct {
    const Self = @This();

    hadError: bool = false,
    fn runFile(arena: *std.heap.ArenaAllocator, path: []const u8) !void {
        const self = @This();
        const file = try std.fs.cwd().openFile(path, .{});
        defer file.close();

        const source = try file.readAll(arena.allocator);
        try run(arena, source);

        if (self.hadError) {
            std.os.exit(65);
        }
    }

    fn runPrompt(arena: *std.heap.ArenaAllocator) !void {
        const self = @This();
        const stdin = std.io.getStdIn().reader();
        var br = std.io.bufferedReader(stdin);
        const stdout = std.io.getStdOut().writer();

        while (true) {
            try stdout.print("> ", .{});
            const line = try br.readLine(arena.allocator);
            if (line == null) {
                break;
            }
            try run(arena, line);
            self.hadError = false;
        }
    }

    fn run(arena: *std.heap.ArenaAllocator, source: []const u8) !void {
        const scanner = Scanner.init(arena, source);
        const tokens = try scanner.scanTokens();

        for (tokens) |token| {
            try std.debug.print("{s}\n", .{token});
        }
    }

    fn err(line: u32, message: []const u8) void {
        report(line, "", message);
    }

    fn report(line: u32, where: []const u8, message: []const u8) void {
        std.debug.print("[line {d}] Error {s}: {s}\n", .{ line, where, message });
        const hadError = true;
        _ = hadError;
    }
};
