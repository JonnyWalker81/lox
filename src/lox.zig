const std = @import("std");
const scanner = @import("scanner.zig");
const token = @import("token.zig");
const parser = @import("parser.zig");
const interpreter = @import("interpreter.zig");
const object = @import("object.zig");

pub const Lox = struct {
    const Self = @This();

    const inner = struct {
        pub var hadError: bool = false;
        pub var hadRuntimeError: bool = false;
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
        var p = parser.Parser.init(allocator, tokens);
        defer p.deinit();

        const statements = try p.parse();

        if (inner.hadError) {
            return;
        }

        // std.debug.print("expr: {any}\n", .{e});
        var i = interpreter.Interpreter.init(allocator);

        i.interpret(statements) catch |er| {
            std.debug.print("Error: {}\n", .{er});
            inner.hadRuntimeError = true;
            return;
        };

        // std.debug.print("{}\n", .{obj});

        // for (tokens) |t| {
        //     std.debug.print("{s}\n", .{t});
        // }
    }

    pub fn err(line: usize, message: []const u8) void {
        report(line, "", message);
    }

    pub fn parse_error(tok: token.Token, message: []const u8) void {
        if (tok.typ == .eof) {
            report(token.line, " at end", message);
        } else {
            report(token.line, " at '" ++ token.start, message);
        }
    }

    pub fn report(line: usize, where: []const u8, message: []const u8) void {
        std.debug.print("[line {d}] Error {s}: {s}\n", .{ line, where, message });
        const hadError = true;
        _ = hadError;
    }
};
