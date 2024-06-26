const std = @import("std");
const scanner = @import("scanner.zig");
const token = @import("token.zig");
const parser = @import("parser.zig");
const interpreter = @import("interpreter.zig");
const object = @import("object.zig");
const resolver = @import("resolver.zig");

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

        var i = interpreter.Interpreter.init(allocator);
        defer i.deinit();

        var r = resolver.Resolver.init(allocator, &i);
        defer r.deinit();

        try r.resolveStatements(statements);

        if (inner.hadError) {
            return;
        }

        // std.debug.print("expr: {any}\n", .{e});

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

test "test function" {
    var arena = std.heap.ArenaAllocator.init(test_allocator);
    defer arena.deinit();

    const source =
        \\ fun sayHi(first, last) {
        \\  print "Hi, " + first + " " + last + "!";
        \\}
        \\
        \\sayHi("Dear", "Reader");
    ;

    try Lox.run(arena.allocator(), source);
}

test "test return statement" {
    var arena = std.heap.ArenaAllocator.init(test_allocator);
    defer arena.deinit();

    const source =
        \\ fun foo() {
        \\  return 1;
        \\}
        \\
        \\print foo();
    ;

    try Lox.run(arena.allocator(), source);
}

test "test fib" {
    var arena = std.heap.ArenaAllocator.init(test_allocator);
    defer arena.deinit();

    const source =
        \\fun fib(n) {
        \\  if (n <= 1) return n;
        \\  return fib(n - 2) + fib(n - 1);
        \\}
        \\
        \\for (var i = 0; i < 20; i = i + 1) {
        \\  print fib(i);
        \\}
    ;

    try Lox.run(arena.allocator(), source);
}

test "test make counter" {
    var arena = std.heap.ArenaAllocator.init(test_allocator);
    defer arena.deinit();

    const source =
        \\fun makeCounter() {
        \\  var i = 0;
        \\  fun count() {
        \\    i = i + 1;
        \\    print i;
        \\  }
        \\  return count;
        \\}
        \\
        \\var counter = makeCounter();
        \\counter();
        \\counter();
    ;

    try Lox.run(arena.allocator(), source);
}

test "test variable already declared" {
    var arena = std.heap.ArenaAllocator.init(test_allocator);
    defer arena.deinit();

    const source =
        \\fun bad() {
        \\  var a = 1;
        \\  var a = 2;
        \\}
    ;

    try Lox.run(arena.allocator(), source);
}

test "test class name" {
    var arena = std.heap.ArenaAllocator.init(test_allocator);
    defer arena.deinit();

    const source =
        \\class DevonshireCream {
        \\  serveOn() {
        \\    return "Sconess";
        \\  }
        \\}
        \\
        \\print DevonshireCream;
    ;

    try Lox.run(arena.allocator(), source);
}

test "test class instance simple" {
    var arena = std.heap.ArenaAllocator.init(test_allocator);
    defer arena.deinit();

    const source =
        \\class Bagel {}
        \\var bagel = Bagel();
        \\print bagel;
    ;

    try Lox.run(arena.allocator(), source);
}

test "test class instance method" {
    var arena = std.heap.ArenaAllocator.init(test_allocator);
    defer arena.deinit();

    const source =
        \\class Bacon {
        \\  eat() {
        \\    print "Crunch crunch cruch!";
        \\  }
        \\}
        \\
        \\Bacon().eat();
    ;

    try Lox.run(arena.allocator(), source);
}

test "test class this" {
    var arena = std.heap.ArenaAllocator.init(test_allocator);
    defer arena.deinit();

    const source =
        \\class Cake {
        \\  taste() {
        \\   var adjective = "delicious";
        \\   print "The " + this.flavor + " cake is " + adjective + "!";
        \\  }
        \\}
        \\
        \\var cake = Cake();
        \\cake.flavor = "German chocolate";
        \\cake.taste(); // Prints "The German chocolate cake is delicious!".
    ;

    try Lox.run(arena.allocator(), source);
}

test "test class init" {
    var arena = std.heap.ArenaAllocator.init(test_allocator);
    defer arena.deinit();

    const source =
        \\class Foo {
        \\  init() {
        \\    print this;
        \\  }
        \\}
        \\
        \\var foo = Foo();
        \\print foo.init();
    ;

    try Lox.run(arena.allocator(), source);
}

test "test basic inheritance" {
    var arena = std.heap.ArenaAllocator.init(test_allocator);
    defer arena.deinit();

    const source =
        \\class Doughnut {
        \\  cook() {
        \\    print "Fry until golden brown.";
        \\  }
        \\}
        \\
        \\class BostonCream < Doughnut {}
        \\
        \\BostonCream().cook();
    ;

    try Lox.run(arena.allocator(), source);
}

test "test super class" {
    var arena = std.heap.ArenaAllocator.init(test_allocator);
    defer arena.deinit();

    const source =
        \\class A {
        \\  method() {
        \\    print "A method";
        \\  }
        \\}
        \\
        \\class B < A {
        \\  method() {
        \\    print "B method";
        \\  }
        \\
        \\  test() {
        \\    super.method();
        \\  }
        \\}
        \\
        \\class C < B {}
        \\
        \\C().test();
    ;

    try Lox.run(arena.allocator(), source);
}

test "test fib(40)" {
    var arena = std.heap.ArenaAllocator.init(test_allocator);
    defer arena.deinit();

    const source =
        \\        fun fib(n) {
        \\  if (n < 2) return n;
        \\  return fib(n - 1) + fib(n - 2);
        \\}
        \\
        \\var before = clock();
        \\print fib(10);
        \\var after = clock();
        \\print after - before;
    ;

    try Lox.run(arena.allocator(), source);
}
