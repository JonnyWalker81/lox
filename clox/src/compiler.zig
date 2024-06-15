const std = @import("std");
const scanner = @import("scanner.zig");
const chunk = @import("chunk.zig");

const Precedence = enum {
    none,
    assignment, // =
    @"or", // or
    @"and", // and
    equality, // == !=
    comparison, // < > <= >=
    term, // + -
    factor, // * /
    unary, // ! -
    call, // . () []
    primary,
};

const Parser = struct {
    const Self = @This();

    current: scanner.Token = scanner.Token.init(),
    previous: scanner.Token = scanner.Token.init(),
    hadError: bool = false,
    panicMode: bool = false,

    pub fn init() Parser {
        return .{};
    }

    pub fn parse(self: *Self, source: []const u8) !void {
        _ = self;
        _ = source;
    }
};

pub const Compiler = struct {
    const Self = @This();

    arena: std.heap.ArenaAllocator,
    parser: Parser,
    compilingChunk: *chunk.Chunk = undefined,
    scnr: scanner.Scanner = undefined,

    pub fn init(allocator: std.mem.Allocator) Compiler {
        const parser = Parser.init();
        return Compiler{
            .arena = std.heap.ArenaAllocator.init(allocator),
            .parser = parser,
        };
    }

    pub fn deinit(self: *Compiler) void {
        self.arena.deinit();
    }

    pub fn compile(self: *Compiler, source: []const u8) !*chunk.Chunk {
        self.scnr = scanner.Scanner.init(self.arena.allocator(), source);
        const c = chunk.Chunk.init(self.arena.allocator());
        self.compilingChunk = c;
        defer self.endCompiler() catch unreachable;
        _ = try self.advance();
        try self.expression();
        _ = try self.consume(.eof, "Expect end of expression.");
        // var line: i32 = -1;
        // while (true) {
        //     const token = try s.scanToken();
        //     if (token.line != line) {
        //         std.debug.print("{d:4} ", .{token.line});
        //         line = @intCast(token.line);
        //     } else {
        //         std.debug.print("     | ", .{});
        //     }
        //     std.debug.print("'{s}'\n", .{token.type});
        //     if (token.type == .eof) {
        //         break;
        //     }
        // }

        if (self.parser.hadError) {
            return scanner.CompilerError.InvalidSyntax;
        }

        return c;
    }

    fn advance(self: *Self) !void {
        self.parser.previous = self.parser.current;

        while (true) {
            self.parser.current = try self.scnr.scanToken();
            if (self.parser.current.type != .@"error") {
                break;
            }

            const msg = try std.fmt.allocPrint(self.arena.allocator(), "{s}", .{self.parser.current.type});
            self.errorAtCurrent(msg);
        }
    }

    fn consume(self: *Self, typ: scanner.TokenType, message: []const u8) !void {
        if (@intFromEnum(self.parser.current.type) == @intFromEnum(typ)) {
            _ = try self.advance();
            return;
        }

        self.errorAtCurrent(message);
    }

    fn emitByte(self: *Self, byte: u8) !void {
        try self.currentChunk().writeChunk(byte, self.parser.previous.line);
    }

    fn emitBytes(self: *Self, byte1: u8, byte2: u8) !void {
        try self.emitByte(byte1);
        try self.emitByte(byte2);
    }

    fn endCompiler(self: *Self) !void {
        try self.emitReturn();
    }

    fn binary(self: *Self) !void {
        const operatorType = self.parser.previous.type;
        // const rule = getRule(operatorType);
        // try self.parsePrecedence(@enumFromInt(@intFromEnum(rule.precedence) + 1));

        switch (operatorType) {
            .plus => try self.emitByte(@intFromEnum(.OpAdd)),
            .minus => try self.emitByte(@intFromEnum(.OpSubtract)),
            .star => try self.emitByte(@intFromEnum(.OpMultiply)),
            .slash => try self.emitByte(@intFromEnum(.OpDivide)),
            _ => return,
        }
    }

    fn grouping(self: *Self) !void {
        try self.expression();
        _ = try self.consume(.rightParen, "Expect ')' after expression.");
    }

    fn number(self: *Self) !void {
        // const value = std.fmt.parseFloat(self.parser.previous.start, self.parser.previous.length);
        // if (value == null) {
        //     self.errorAtCurrent("Invalid number.");
        // }

        const value = self.parser.previous.number();

        try self.emitConstant(value);
    }

    fn unary(self: *Self) !void {
        const operatorType = self.parser.previous.type;
        try self.parsePrecedence(.unary);

        try self.expression();

        switch (operatorType) {
            .minus => try self.emitByte(@intFromEnum(.OpNegate)),
            else => return,
        }
    }

    fn parsePrecedence(self: *Self, precedence: Precedence) !void {
        _ = self;
        _ = precedence;
    }

    fn expression(self: *Self) !void {
        try self.parsePrecedence(.assignment);
    }

    fn emitReturn(self: *Self) !void {
        try self.emitByte(@intFromEnum(chunk.OpCode.OpReturn));
    }

    fn makeConstant(self: *Self, value: chunk.Value) !u8 {
        const constant = try self.currentChunk().addConstant(value);
        if (constant > u8.max) {
            self.err("Too many constants in one chunk.");
            return 0;
        }

        return constant;
    }

    fn emitConstant(self: *Self, constant: u8) !void {
        try self.emitBytes(@intFromEnum(.OpConstant), constant);
    }

    fn currentChunk(self: *Self) *chunk.Chunk {
        return self.compilingChunk;
    }

    fn errorAtCurrent(self: *Self, message: []const u8) void {
        self.errorAt(self.parser.current, message);
    }

    fn err(self: *Self, message: []const u8) void {
        self.errorAt(self.parser.previous, message);
    }

    fn errorAt(self: *Self, token: scanner.Token, message: []const u8) void {
        if (self.parser.panicMode) {
            return;
        }

        self.parser.panicMode = true;
        std.debug.print("[line {d}] Error", .{token.line});

        if (token.type == .eof) {
            std.debug.print(" at end", .{});
        } else if (token.type == .@"error") {
            // Nothing.
        } else {
            std.debug.print(" at '{s}'", .{token.type});
        }

        std.debug.print(": {s}\n", .{message});
        self.parser.hadError = true;
    }
};
