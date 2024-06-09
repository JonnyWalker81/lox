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
        defer scan.deinit();
        const tokens = try scan.scanTokens();
        var p = parser.Parser.init(allocator, tokens);
        defer p.deinit();

        const statements = try p.parse();

        if (inner.hadError) {
            return;
        }

        // std.debug.print("expr: {any}\n", .{e});
        var i = interpreter.Interpreter.init(allocator);
        defer i.deinit();

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

    pub fn parse_error(allocator: std.mem.Allocator, tok: token.Token, message: []const u8) void {
        if (tok.typ == .eof) {
            report(tok.line, " at end", message);
        } else {
            const lexeme = std.fmt.allocPrint(allocator, " at '{}", .{tok.typ}) catch unreachable;
            report(tok.line, lexeme, message);
        }
    }

    pub fn report(line: usize, where: []const u8, message: []const u8) void {
        std.debug.print("[line {d}] Error {s}: {s}\n", .{ line, where, message });
        const hadError = true;
        _ = hadError;
    }
};

const test_allocator = std.testing.allocator;
test "test variable statement" {
    var arena = std.heap.ArenaAllocator.init(test_allocator);
    defer arena.deinit();

    const source = "var a = 1; var b = 2; print a + b;";
    try Lox.run(arena.allocator(), source);
}

test "test variable assignment" {
    var arena = std.heap.ArenaAllocator.init(test_allocator);
    defer arena.deinit();

    const source = "var a = 1; print a = 2;";
    try Lox.run(arena.allocator(), source);
}

test "test block statements" {
    var arena = std.heap.ArenaAllocator.init(test_allocator);
    defer arena.deinit();

    const source =
        \\var a = "global a";
        \\var b = "global b";
        \\var c = "global c";
        \\{
        \\  var a = "outer a";
        \\  var b = "outer b";
        \\  {
        \\    var a = "inner a";
        \\    print a;
        \\    print b;
        \\    print c;
        \\  }
        \\  print a;
        \\  print b;
        \\  print c;
        \\}
        \\print a;
        \\print b;
        \\print c;
        \\
    ;
    try Lox.run(arena.allocator(), source);
}

test "test while loop" {
    var arena = std.heap.ArenaAllocator.init(test_allocator);
    defer arena.deinit();
    const source =
        \\var a = 0;
        \\var temp = 0;
        \\
        \\for (var b = 1; a < 1000; b = temp + b) {
        \\  print a;
        \\  temp = a;
        \\  a = b;
        \\}
    ;

    // const source =
    //     \\for (var i = 0; i < 2; i = i + 1) {
    //     \\  print i;
    //     \\}
    // ;

    try Lox.run(arena.allocator(), source);
}
