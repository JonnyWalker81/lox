const std = @import("std");
const scanner = @import("scanner.zig");
const chunk = @import("chunk.zig");
const build_options = @import("build_options");
const debug = @import("debug.zig");
const value = @import("value.zig");
const vm = @import("vm.zig");

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

const ParseFn = *const fn (*Compiler, bool) anyerror!void;

const ParseRule = struct {
    prefixFn: ?ParseFn,
    infixFn: ?ParseFn,
    precedence: Precedence,
};

const rules = [_]ParseRule{
    .{ .prefixFn = Compiler.grouping, .infixFn = null, .precedence = .none }, // .left_paren
    .{ .prefixFn = null, .infixFn = null, .precedence = .none }, // .left_paren
    .{ .prefixFn = null, .infixFn = null, .precedence = .none }, // .left_brace
    .{ .prefixFn = null, .infixFn = null, .precedence = .none }, // .right_brace
    .{ .prefixFn = null, .infixFn = null, .precedence = .none }, // .comma
    .{ .prefixFn = null, .infixFn = null, .precedence = .none }, // .dot
    .{ .prefixFn = Compiler.unary, .infixFn = Compiler.binary, .precedence = .term }, // .minus
    .{ .prefixFn = null, .infixFn = Compiler.binary, .precedence = .term }, // .plus
    .{ .prefixFn = null, .infixFn = null, .precedence = .none }, // .semicolon
    .{ .prefixFn = null, .infixFn = Compiler.binary, .precedence = .factor }, // .slash
    .{ .prefixFn = null, .infixFn = Compiler.binary, .precedence = .factor }, // .star
    .{ .prefixFn = Compiler.unary, .infixFn = null, .precedence = .none }, // .bang
    .{ .prefixFn = null, .infixFn = Compiler.binary, .precedence = .equality }, // .bang_equal
    .{ .prefixFn = null, .infixFn = null, .precedence = .none }, // .equal
    .{ .prefixFn = null, .infixFn = Compiler.binary, .precedence = .equality }, // .equal_equal
    .{ .prefixFn = null, .infixFn = Compiler.binary, .precedence = .comparison }, // .greater
    .{ .prefixFn = null, .infixFn = Compiler.binary, .precedence = .comparison }, // .greater_equal
    .{ .prefixFn = null, .infixFn = Compiler.binary, .precedence = .comparison }, // .less
    .{ .prefixFn = null, .infixFn = Compiler.binary, .precedence = .comparison }, // .less_equal
    .{ .prefixFn = Compiler.variable, .infixFn = null, .precedence = .none }, // .identifier
    .{ .prefixFn = Compiler.string, .infixFn = null, .precedence = .none }, // .string
    .{ .prefixFn = Compiler.number, .infixFn = null, .precedence = .none }, // .number
    .{ .prefixFn = null, .infixFn = null, .precedence = .none }, // .and
    .{ .prefixFn = null, .infixFn = null, .precedence = .none }, // .class
    .{ .prefixFn = null, .infixFn = null, .precedence = .none }, // .else
    .{ .prefixFn = Compiler.literal, .infixFn = null, .precedence = .none }, // .false
    .{ .prefixFn = null, .infixFn = null, .precedence = .none }, // .for
    .{ .prefixFn = null, .infixFn = null, .precedence = .none }, // .fun
    .{ .prefixFn = null, .infixFn = null, .precedence = .none }, // .if
    .{ .prefixFn = Compiler.literal, .infixFn = null, .precedence = .none }, // .nil
    .{ .prefixFn = null, .infixFn = null, .precedence = .none }, // .or
    .{ .prefixFn = null, .infixFn = null, .precedence = .none }, // .print
    .{ .prefixFn = null, .infixFn = null, .precedence = .none }, // .return
    .{ .prefixFn = null, .infixFn = null, .precedence = .none }, // .super
    .{ .prefixFn = null, .infixFn = null, .precedence = .none }, // .this
    .{ .prefixFn = Compiler.literal, .infixFn = null, .precedence = .none }, // .true
    .{ .prefixFn = null, .infixFn = null, .precedence = .none }, // .var
    .{ .prefixFn = null, .infixFn = null, .precedence = .none }, // .while
    .{ .prefixFn = null, .infixFn = null, .precedence = .none }, // .error
    .{ .prefixFn = null, .infixFn = null, .precedence = .none }, // .eof
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
    // v: *vm.VM = undefined,

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
        // self.v = v;
        const c = chunk.Chunk.init(self.arena.allocator());
        self.compilingChunk = c;
        defer self.endCompiler() catch unreachable;
        _ = try self.advance();
        while (!try self.match(scanner.TokenType.eof)) {
            try self.declaration();
        }
        // try self.expression();
        // _ = try self.consume(.eof, "Expect end of expression.");
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

    fn consume(self: *Self, typ: usize, message: []const u8) !void {
        if (@intFromEnum(self.parser.current.type) == typ) {
            _ = try self.advance();
            return;
        }

        self.errorAtCurrent(message);
    }

    fn check(self: *Self, typ: scanner.TokenType) bool {
        return @intFromEnum(self.parser.current.type) == @intFromEnum(typ);
    }

    fn match(self: *Self, typ: scanner.TokenType) !bool {
        if (!self.check(typ)) {
            return false;
        }

        _ = try self.advance();
        return true;
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

        if (build_options.debug_print_code and !self.parser.hadError) {
            debug.disassembleChunk(self.currentChunk(), "code");
        }
    }

    fn binary(self: *Self, _: bool) !void {
        const operatorType = self.parser.previous.type;
        const rule = try self.getRule(operatorType);
        try self.parsePrecedence(@enumFromInt(@intFromEnum(rule.precedence) + 1));

        switch (operatorType) {
            .bang_equal => try self.emitBytes(@intFromEnum(chunk.OpCode.OpEqual), @intFromEnum(chunk.OpCode.OpNot)),
            .equal_equal => try self.emitByte(@intFromEnum(chunk.OpCode.OpEqual)),
            .greater => try self.emitByte(@intFromEnum(chunk.OpCode.OpGreater)),
            .greater_equal => try self.emitBytes(@intFromEnum(chunk.OpCode.OpLess), @intFromEnum(chunk.OpCode.OpNot)),
            .less => try self.emitByte(@intFromEnum(chunk.OpCode.OpLess)),
            .less_equal => try self.emitBytes(@intFromEnum(chunk.OpCode.OpGreater), @intFromEnum(chunk.OpCode.OpNot)),
            .plus => try self.emitByte(@intFromEnum(chunk.OpCode.OpAdd)),
            .minus => try self.emitByte(@intFromEnum(chunk.OpCode.OpSubtract)),
            .star => try self.emitByte(@intFromEnum(chunk.OpCode.OpMultiply)),
            .slash => try self.emitByte(@intFromEnum(chunk.OpCode.OpDivide)),
            else => return,
        }
    }

    fn literal(self: *Self, _: bool) !void {
        switch (self.parser.previous.type) {
            .false => try self.emitByte(@intFromEnum(chunk.OpCode.OpFalse)),
            .nil => try self.emitByte(@intFromEnum(chunk.OpCode.OpNil)),
            .true => try self.emitByte(@intFromEnum(chunk.OpCode.OpTrue)),
            else => return,
        }
    }

    fn grouping(self: *Self, _: bool) !void {
        try self.expression();
        _ = try self.consume(@intFromEnum(scanner.TokenType.right_paren), "Expect ')' after expression.");
    }

    fn number(self: *Self, _: bool) !void {
        // const value = std.fmt.parseFloat(self.parser.previous.start, self.parser.previous.length);
        // if (value == null) {
        //     self.errorAtCurrent("Invalid number.");
        // }

        const val = self.parser.previous.number();

        const numVal: value.Value = .{ .number = val };

        try self.emitConstant(numVal);
    }

    fn string(self: *Self, _: bool) !void {
        const prevStart = self.parser.previous.start + 1;
        const prevLength = self.parser.previous.length - 2;
        const strVal: value.Value = .{ .string = self.scnr.source[prevStart .. prevStart + prevLength] };
        // const s = try std.fmt.allocPrint(self.arena.allocator(), "{s}", .{strVal.string});
        // try self.v.strings.put(s, void{});
        try self.emitConstant(strVal);
    }

    fn namedVariable(self: *Self, name: scanner.Token, canAssign: bool) !void {
        const constant = try self.identifierConstant(name);
        if (canAssign and try self.match(.equal)) {
            try self.expression();
            try self.emitBytes(@intFromEnum(chunk.OpCode.OpSetGlobal), constant);
        } else {
            try self.emitBytes(@intFromEnum(chunk.OpCode.OpGetGlobal), constant);
        }
    }

    fn variable(self: *Self, canAssign: bool) !void {
        try self.namedVariable(self.parser.previous, canAssign);
    }

    fn unary(self: *Self, _: bool) !void {
        const operatorType = self.parser.previous.type;
        try self.parsePrecedence(.unary);

        switch (operatorType) {
            .bang => try self.emitByte(@intFromEnum(chunk.OpCode.OpNot)),
            .minus => try self.emitByte(@intFromEnum(chunk.OpCode.OpNegate)),
            else => return,
        }
    }

    fn parsePrecedence(self: *Self, precedence: Precedence) !void {
        try self.advance();
        var rule = try self.getRule(self.parser.previous.type);
        const prefixRule = rule.prefixFn;

        if (prefixRule == null) {
            std.debug.print("No rule found for {s}\n", .{self.parser.current.type});
            self.err("Expect expression.");
            return;
        }

        const canAssign = @intFromEnum(precedence) <= @intFromEnum(Precedence.assignment);
        try prefixRule.?(self, canAssign);

        rule = try self.getRule(self.parser.current.type);

        while (@intFromEnum(precedence) <= @intFromEnum(rule.precedence)) {
            try self.advance();
            const infixRule = try self.getRule(self.parser.previous.type);
            try infixRule.infixFn.?(self, canAssign);
            rule = try self.getRule(self.parser.current.type);
        }

        if (canAssign and try self.match(.equal)) {
            self.err("Invalid assignment target.");
        }
    }

    fn identifierConstant(self: *Self, name: scanner.Token) !u8 {
        const strVal = try std.fmt.allocPrint(self.arena.allocator(), "{s}", .{self.scnr.source[name.start .. name.start + name.length]});
        const constant = try self.makeConstant(.{ .string = strVal });
        return constant;
    }

    fn parseVariable(self: *Self, errorMessage: []const u8) !u8 {
        _ = try self.consume(@intFromEnum(scanner.TokenType.identifier), errorMessage);
        return try self.identifierConstant(self.parser.previous);
    }

    fn defineVariable(self: *Self, global: u8) !void {
        try self.emitBytes(@intFromEnum(chunk.OpCode.OpDefineGlobal), global);
    }

    fn getRule(self: *Self, typ: scanner.TokenType) !ParseRule {
        _ = self;
        return rules[@intFromEnum(typ)];
    }

    fn expression(self: *Self) !void {
        try self.parsePrecedence(.assignment);
    }

    fn varDeclaration(self: *Self) !void {
        const global = try self.parseVariable("Expect variable name.");

        if (try self.match(.equal)) {
            try self.expression();
        } else {
            try self.emitByte(@intFromEnum(chunk.OpCode.OpNil));
        }

        _ = try self.consume(@intFromEnum(scanner.TokenType.semicolon), "Expect ';' after variable declaration.");

        try self.defineVariable(global);
    }

    fn expressionStatement(self: *Self) !void {
        try self.expression();
        _ = try self.consume(@intFromEnum(scanner.TokenType.semicolon), "Expect ';' after expression.");
        try self.emitByte(@intFromEnum(chunk.OpCode.OpPop));
    }

    fn printStatement(self: *Self) !void {
        try self.expression();
        _ = try self.consume(@intFromEnum(scanner.TokenType.semicolon), "Expect ';' after value.");
        try self.emitByte(@intFromEnum(chunk.OpCode.OpPrint));
    }

    fn synchronize(self: *Self) !void {
        self.parser.panicMode = false;

        while (self.parser.current.type != .eof) {
            if (self.parser.previous.type == .semicolon) {
                return;
            }

            switch (self.parser.current.type) {
                .class, .fun, .@"var", .@"for", .@"if", .@"while", .print, .@"return" => return,
                else => {},
            }

            _ = try self.advance();
        }
    }

    fn declaration(self: *Self) !void {
        if (try self.match(.@"var")) {
            try self.varDeclaration();
        } else {
            try self.statement();
        }

        if (self.parser.panicMode) {
            try self.synchronize();
        }
    }

    fn statement(self: *Self) !void {
        if (try self.match(.print)) {
            try self.printStatement();
        } else {
            try self.expressionStatement();
        }
    }

    fn emitReturn(self: *Self) !void {
        try self.emitByte(@intFromEnum(chunk.OpCode.OpReturn));
    }

    fn makeConstant(self: *Self, val: value.Value) !u8 {
        const constant = try self.currentChunk().addConstant(val);
        if (constant > std.math.maxInt(u8)) {
            self.err("Too many constants in one chunk.");
            return 0;
        }

        return @intCast(constant);
    }

    fn emitConstant(self: *Self, constant: value.Value) !void {
        try self.emitBytes(@intFromEnum(chunk.OpCode.OpConstant), try self.makeConstant(constant));
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
