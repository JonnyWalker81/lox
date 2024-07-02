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
    .{ .prefixFn = Compiler.grouping, .infixFn = Compiler.call, .precedence = .call }, // .left_paren
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
    .{ .prefixFn = null, .infixFn = Compiler.and_, .precedence = .@"and" }, // .and
    .{ .prefixFn = null, .infixFn = null, .precedence = .none }, // .class
    .{ .prefixFn = null, .infixFn = null, .precedence = .none }, // .else
    .{ .prefixFn = Compiler.literal, .infixFn = null, .precedence = .none }, // .false
    .{ .prefixFn = null, .infixFn = null, .precedence = .none }, // .for
    .{ .prefixFn = null, .infixFn = null, .precedence = .none }, // .fun
    .{ .prefixFn = null, .infixFn = null, .precedence = .none }, // .if
    .{ .prefixFn = Compiler.literal, .infixFn = null, .precedence = .none }, // .nil
    .{ .prefixFn = null, .infixFn = Compiler.or_, .precedence = .@"or" }, // .or
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

    pub fn init(allocator: std.mem.Allocator) *Parser {
        const p = allocator.create(Self) catch unreachable;
        p.* = .{};
        return p;
    }

    pub fn parse(self: *Self, source: []const u8) !void {
        _ = self;
        _ = source;
    }
};

pub const Local = struct {
    name: scanner.Token,
    depth: i32,
};

const LocalsCount = std.math.maxInt(u8) + 1;
const UpvaluesCount = std.math.maxInt(u8) + 1;

pub const Upvalue = struct {
    index: u8,
    isLocal: bool,
};

const FunctionType = enum {
    function,
    script,
};

pub const Compiler = struct {
    const Self = @This();

    arena: std.heap.ArenaAllocator,
    parser: *Parser,
    compilingChunk: *chunk.Chunk = undefined,
    scnr: *scanner.Scanner = undefined,
    function: value.Value = undefined,
    funcType: FunctionType,
    locals: [LocalsCount]Local = undefined,
    localCount: usize = 0,
    upvalues: [UpvaluesCount]Upvalue = undefined,
    scopeDepth: i32 = 0,
    enclosing: ?*Compiler = null,
    // v: *vm.VM = undefined,

    pub fn init(allocator: std.mem.Allocator) Compiler {
        var u: [UpvaluesCount]Upvalue = undefined;
        @memset(&u, undefined);
        const parser = Parser.init(allocator);
        var c = Compiler{
            .arena = std.heap.ArenaAllocator.init(allocator),
            .parser = parser,
            .funcType = FunctionType.script,
            .function = .{ .function = value.Function.init(allocator) },
            .upvalues = u,
        };

        // std.debug.print("Compiler init: {any}\n", .{c.function.functionValue()});

        const local = &c.locals[c.localCount];
        c.localCount += 1;
        local.depth = 0;
        local.name.start = 0;
        local.name.length = 0;

        return c;
    }

    pub fn initWithEnclosing(allocator: std.mem.Allocator, enclosing: *Compiler, typ: FunctionType) Compiler {
        var c = Compiler.init(allocator);
        c.parser = enclosing.parser;
        c.scnr = enclosing.scnr;
        c.enclosing = enclosing;
        c.funcType = typ;

        if (typ != .script) {
            const s = c.scnr.source[c.parser.previous.start .. c.parser.previous.start + c.parser.previous.length];
            c.function.function.name = std.fmt.allocPrint(allocator, "{s}", .{s}) catch unreachable;
        }

        return c;
    }

    pub fn deinit(self: *Compiler) void {
        self.arena.deinit();
    }

    pub fn compile(self: *Self, source: []const u8) !value.Value {
        self.scnr = scanner.Scanner.init(self.arena.allocator(), source);
        // self.v = v;
        const c = chunk.Chunk.init(self.arena.allocator());
        self.compilingChunk = c;
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

        const f = try self.endCompiler();

        return f;
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

    fn emitLoop(self: *Self, loopStart: u16) !void {
        try self.emitByte(@intFromEnum(chunk.OpCode.OpLoop));

        const offset: u16 = @intCast(self.currentChunk().count - loopStart + 2);
        if (offset > std.math.maxInt(u16)) {
            self.err("Loop body too large.");
        }

        try self.emitByte(@intCast((offset >> 8) & 0xff));
        try self.emitByte(@intCast(offset & 0xff));
    }

    fn emitJump(self: *Self, instruction: u8) !u16 {
        try self.emitByte(instruction);
        try self.emitByte(0xff);
        try self.emitByte(0xff);
        return @intCast(self.currentChunk().count - 2);
    }

    fn endCompiler(self: *Self) !value.Value {
        try self.emitReturn();

        const f = self.function;

        if (build_options.debug_print_code and !self.parser.hadError) {
            debug.disassembleChunk(self.currentChunk(), if (f.functionValue().name.len > 0) "code" else "<script>");
        }

        // std.debug.print("End compiler: {any}\n", .{func.functionValue()});

        return f;
    }

    fn beginScope(self: *Self) !void {
        self.scopeDepth += 1;
    }

    fn endScope(self: *Self) !void {
        self.scopeDepth -= 1;

        while (self.localCount > 0 and self.locals[self.localCount - 1].depth > self.scopeDepth) {
            try self.emitByte(@intFromEnum(chunk.OpCode.OpPop));
            self.localCount -= 1;
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

    fn call(self: *Self, _: bool) !void {
        const argCount = try self.argumentList();
        try self.emitBytes(@intFromEnum(chunk.OpCode.OpCall), argCount);
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

    fn or_(self: *Self, _: bool) !void {
        const elseJump = try self.emitJump(@intFromEnum(chunk.OpCode.OpJumpIfFalse));
        const endJump = try self.emitJump(@intFromEnum(chunk.OpCode.OpJump));

        try self.patchJump(elseJump);
        try self.emitByte(@intFromEnum(chunk.OpCode.OpPop));

        try self.parsePrecedence(.@"or");
        try self.patchJump(endJump);
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
        var constant = try self.resolveLocal(name);
        var getOp: chunk.OpCode = undefined;
        var setOp: chunk.OpCode = undefined;
        const index = try self.resolveUpvalue(name);
        if (index != -1) {
            std.debug.print("Constant index (namedVariable): {d}\n", .{constant});
            std.debug.print("Upvalue index (namedVariable): {d}\n", .{index});
        }

        if (constant != -1) {
            getOp = chunk.OpCode.OpGetLocal;
            setOp = chunk.OpCode.OpSetLocal;
        } else if (index != -1) {
            constant = @intCast(index);
            getOp = chunk.OpCode.OpGetUpvalue;
            setOp = chunk.OpCode.OpSetUpvalue;
        } else {
            constant = try self.identifierConstant(name);
            getOp = chunk.OpCode.OpGetGlobal;
            setOp = chunk.OpCode.OpSetGlobal;
        }

        if (canAssign and try self.match(.equal)) {
            try self.expression();
            try self.emitBytes(@intFromEnum(setOp), @intCast(constant));
        } else {
            try self.emitBytes(@intFromEnum(getOp), @intCast(constant));
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

    fn addLocal(self: *Self, name: scanner.Token) !void {
        if (self.localCount == LocalsCount) {
            self.err("Too many local variables in function.");
            return;
        }

        const local = &self.locals[self.localCount];
        local.name = name;
        local.depth = -1;
        self.localCount += 1;
    }

    fn declareVariable(self: *Self) !void {
        if (self.scopeDepth == 0) {
            return;
        }

        const name = self.parser.previous;
        var i: i32 = @intCast(self.localCount);
        while (i >= 0) : (i -= 1) {
            const local = &self.locals[@intCast(i)];
            if (local.depth != -1 and local.depth < self.scopeDepth) {
                break;
            }

            if (self.identifiersEqual(name, local.name)) {
                self.err("Variable with this name already declared in this scope.");
            }
        }

        try self.addLocal(name);
    }

    fn identifiersEqual(self: *Self, a: scanner.Token, b: scanner.Token) bool {
        const aStr = self.scnr.source[a.start .. a.start + a.length];
        const bStr = self.scnr.source[b.start .. b.start + b.length];
        std.debug.print("Comparing: {s} {s}\n", .{ aStr, bStr });
        return a.length == b.length and
            std.mem.eql(u8, aStr, bStr);
    }

    fn resolveLocal(self: *Self, name: scanner.Token) !i32 {
        std.debug.print("Resolving local...{d}\n", .{self.localCount});
        var i: i32 = @intCast(self.localCount);
        while (i >= 0) : (i -= 1) {
            std.debug.print("Resolving local: {d}\n", .{i});
            const local = &self.locals[@intCast(i)];
            // if (local.depth != -1 and local.depth < self.scopeDepth) {
            //     return -1;
            // }

            if (self.identifiersEqual(name, local.name)) {
                if (local.depth == -1) {
                    self.err("Cannot read local variable in its own initializer.");
                }
                std.debug.print("end resolving Local index: {d}\n", .{i});
                return @intCast(i);
            }
        }

        std.debug.print("end Resolving local (-1)...\n", .{});
        return -1;
    }

    fn addUpvalue(self: *Self, index: i32, isLocal: bool) !i32 {
        const upvalueCount = self.function.functionValue().upvalueCount;
        // var upvalue = self.function.functionValue().upvalues;
        for (0..upvalueCount) |i| {
            const upvalue = &self.upvalues[i];
            if (upvalue.index == index and upvalue.isLocal == isLocal) {
                std.debug.print("Upvalue index (return): {d}\n", .{index});
                return @intCast(i);
            }
        }

        if (upvalueCount == std.math.maxInt(u8) + 1) {
            self.err("Too many closure variables in function.");
            return 0;
        }

        // upvalue = value.Upvalue.init(self.arena.allocator());
        std.debug.print("Upvalue index: {d} -> {d}\n", .{ upvalueCount, index });
        self.upvalues[upvalueCount].index = @intCast(index);
        self.upvalues[upvalueCount].isLocal = isLocal;
        std.debug.print("Upvalue index (return): {any}\n", .{self.upvalues[0]});
        // upvalue.next = self.function.functionValue().upvalues;
        // self.function.functionValue().upvalues = upvalue;

        self.function.function.incrementUpvalueCount();
        return self.function.functionValue().upvalueCount - 1;
    }

    fn resolveUpvalue(self: *Self, name: scanner.Token) !i32 {
        if (self.enclosing == null) {
            return -1;
        }

        const local = try self.enclosing.?.resolveLocal(name);
        if (local != -1) {
            // self.enclosing.locals[@intCast(local)].isCaptured = true;
            std.debug.print("Upvalue index (local): {d}\n", .{local});
            return try self.addUpvalue(@intCast(local), true);
        }

        const upvalue = try self.enclosing.?.resolveUpvalue(name);
        if (upvalue != -1) {
            std.debug.print("Upvalue index (upvalue) (enclosing): {d}\n", .{upvalue});
            return try self.addUpvalue(@intCast(upvalue), false);
        }

        return -1;
    }

    fn parseVariable(self: *Self, errorMessage: []const u8) !u8 {
        _ = try self.consume(@intFromEnum(scanner.TokenType.identifier), errorMessage);

        try self.declareVariable();
        if (self.scopeDepth > 0) {
            return 0;
        }

        return try self.identifierConstant(self.parser.previous);
    }

    fn markInitialized(self: *Self) !void {
        if (self.scopeDepth == 0) {
            return;
        }

        self.locals[self.localCount - 1].depth = self.scopeDepth;
    }

    fn defineVariable(self: *Self, global: u8) !void {
        if (self.scopeDepth > 0) {
            try self.markInitialized();
            return;
        }
        try self.emitBytes(@intFromEnum(chunk.OpCode.OpDefineGlobal), global);
    }

    fn argumentList(self: *Self) !u8 {
        var argCount: u8 = 0;
        if (!self.check(.right_paren)) {
            try self.expression();
            if (argCount == std.math.maxInt(u8)) {
                self.err("Cannot have more than 255 arguments.");
            }
            argCount += 1;

            while (try self.match(.comma)) {
                try self.expression();
                if (argCount == std.math.maxInt(u8)) {
                    self.err("Cannot have more than 255 arguments.");
                }
                argCount += 1;
            }
        }

        _ = try self.consume(@intFromEnum(scanner.TokenType.right_paren), "Expect ')' after arguments.");
        return argCount;
    }

    fn and_(self: *Self, _: bool) !void {
        const endJump = try self.emitJump(@intFromEnum(chunk.OpCode.OpJumpIfFalse));
        try self.emitByte(@intFromEnum(chunk.OpCode.OpPop));
        try self.parsePrecedence(.@"and");
        try self.patchJump(endJump);
    }

    fn getRule(self: *Self, typ: scanner.TokenType) !ParseRule {
        _ = self;
        return rules[@intFromEnum(typ)];
    }

    fn expression(self: *Self) !void {
        try self.parsePrecedence(.assignment);
    }

    fn block(self: *Self) !void {
        while (!self.check(.right_brace) and !self.check(.eof)) {
            try self.declaration();
        }

        _ = try self.consume(@intFromEnum(scanner.TokenType.right_brace), "Expect '}' after block.");
    }

    fn func(self: *Self, typ: FunctionType) !void {
        var compiler = Compiler.initWithEnclosing(self.arena.allocator(), self, typ);
        try compiler.beginScope();

        _ = try compiler.consume(@intFromEnum(scanner.TokenType.left_paren), "Expect '(' after function name.");

        if (!compiler.check(.right_paren)) {
            compiler.function.function.incrementArity();
            if (compiler.function.functionValue().arity > std.math.maxInt(u8)) {
                self.err("Cannot have more than 255 parameters.");
            }

            var paramConstant = try compiler.parseVariable("Expect parameter name.");
            try compiler.defineVariable(paramConstant);

            while (try compiler.match(.comma)) {
                compiler.function.function.incrementArity();
                if (compiler.function.functionValue().arity > std.math.maxInt(u8)) {
                    self.err("Cannot have more than 255 parameters.");
                }

                paramConstant = try compiler.parseVariable("Expect parameter name.");
                try compiler.defineVariable(paramConstant);
            }
        }

        _ = try compiler.consume(@intFromEnum(scanner.TokenType.right_paren), "Expect ')' after parameters.");

        _ = try compiler.consume(@intFromEnum(scanner.TokenType.left_brace), "Expect '{' before function body.");
        try compiler.block();

        const f = try compiler.endCompiler();
        try self.emitBytes(@intFromEnum(chunk.OpCode.OpClosure), try self.makeConstant(f));

        std.debug.print("Upvalues: {any}\n", .{compiler.upvalues[0]});
        for (0..f.functionValue().upvalueCount) |i| {
            const isLocal: u8 = if (compiler.upvalues[i].isLocal) 1 else 0;
            std.debug.print("Upvalue is local: {d}\n", .{isLocal});
            try self.emitByte(isLocal);
            try self.emitByte(@intCast(compiler.upvalues[i].index));
        }
    }

    fn funDeclaration(self: *Self) !void {
        const global = try self.parseVariable("Expect function name.");
        try self.markInitialized();
        try self.func(.function);
        try self.defineVariable(global);
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

    fn forStatement(self: *Self) !void {
        try self.beginScope();

        _ = try self.consume(@intFromEnum(scanner.TokenType.left_paren), "Expect '(' after 'for'.");
        if (try self.match(.semicolon)) {
            // No initializer.
        } else if (try self.match(.@"var")) {
            try self.varDeclaration();
        } else {
            try self.expressionStatement();
        }

        var loopStart: u16 = @intCast(self.currentChunk().count);
        var exitJump: i16 = -1;
        if (!try self.match(.semicolon)) {
            try self.expression();
            _ = try self.consume(@intFromEnum(scanner.TokenType.semicolon), "Expect ';' after loop condition.");

            exitJump = @intCast(try self.emitJump(@intFromEnum(chunk.OpCode.OpJumpIfFalse)));
            try self.emitByte(@intFromEnum(chunk.OpCode.OpPop));
        }

        // _ = try self.consume(@intFromEnum(scanner.TokenType.right_paren), "Expect ')' after for clauses.");

        if (!try self.match(.right_paren)) {
            const bodyJump = try self.emitJump(@intFromEnum(chunk.OpCode.OpJump));
            const incrementStart: u16 = @intCast(self.currentChunk().count);
            try self.expression();
            try self.emitByte(@intFromEnum(chunk.OpCode.OpPop));
            _ = try self.consume(@intFromEnum(scanner.TokenType.right_paren), "Expect ')' after for clauses.");

            try self.emitLoop(loopStart);
            loopStart = incrementStart;
            try self.patchJump(bodyJump);
        }

        try self.statement();

        try self.emitLoop(loopStart);

        if (exitJump != -1) {
            try self.patchJump(@intCast(exitJump));
            try self.emitByte(@intFromEnum(chunk.OpCode.OpPop));
        }

        try self.endScope();
    }

    fn ifStatement(self: *Self) !void {
        _ = try self.consume(@intFromEnum(scanner.TokenType.left_paren), "Expect '(' after 'if'.");
        try self.expression();
        _ = try self.consume(@intFromEnum(scanner.TokenType.right_paren), "Expect ')' after condition.");

        const thenJump = try self.emitJump(@intFromEnum(chunk.OpCode.OpJumpIfFalse));
        try self.emitByte(@intFromEnum(chunk.OpCode.OpPop));
        try self.statement();

        const elseJump = try self.emitJump(@intFromEnum(chunk.OpCode.OpJump));

        // try self.patchJump(thenJump);

        // if (try self.match(.@"else")) {
        //     try self.statement();
        // }

        try self.patchJump(thenJump);
        try self.emitByte(@intFromEnum(chunk.OpCode.OpPop));

        if (try self.match(.@"else")) {
            try self.statement();
        }

        try self.patchJump(elseJump);
    }

    fn printStatement(self: *Self) !void {
        try self.expression();
        _ = try self.consume(@intFromEnum(scanner.TokenType.semicolon), "Expect ';' after value.");
        try self.emitByte(@intFromEnum(chunk.OpCode.OpPrint));
    }

    fn returnStatement(self: *Self) !void {
        if (self.funcType == .script) {
            self.err("Cannot return from top-level code.");
        }

        if (try self.match(.semicolon)) {
            try self.emitReturn();
        } else {
            try self.expression();
            _ = try self.consume(@intFromEnum(scanner.TokenType.semicolon), "Expect ';' after return value.");
            try self.emitByte(@intFromEnum(chunk.OpCode.OpReturn));
        }
    }

    fn whileStatement(self: *Self) !void {
        const loopStart: u16 = @intCast(self.currentChunk().count);

        _ = try self.consume(@intFromEnum(scanner.TokenType.left_paren), "Expect '(' after 'while'.");
        try self.expression();
        _ = try self.consume(@intFromEnum(scanner.TokenType.right_paren), "Expect ')' after condition.");

        const exitJump = try self.emitJump(@intFromEnum(chunk.OpCode.OpJumpIfFalse));

        try self.emitByte(@intFromEnum(chunk.OpCode.OpPop));
        try self.statement();

        try self.emitLoop(loopStart);
        try self.patchJump(exitJump);
        try self.emitByte(@intFromEnum(chunk.OpCode.OpPop));
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

    fn declaration(self: *Self) anyerror!void {
        if (try self.match(.fun)) {
            try self.funDeclaration();
        } else if (try self.match(.@"var")) {
            try self.varDeclaration();
        } else {
            try self.statement();
        }

        if (self.parser.panicMode) {
            try self.synchronize();
        }
    }

    fn statement(self: *Self) anyerror!void {
        if (try self.match(.print)) {
            try self.printStatement();
        } else if (try self.match(.@"for")) {
            try self.forStatement();
        } else if (try self.match(.@"if")) {
            try self.ifStatement();
        } else if (try self.match(.@"return")) {
            try self.returnStatement();
        } else if (try self.match(.@"while")) {
            try self.whileStatement();
        } else if (try self.match(.left_brace)) {
            try self.beginScope();
            try self.block();
            try self.endScope();
        } else {
            try self.expressionStatement();
        }
    }

    fn emitReturn(self: *Self) !void {
        try self.emitByte(@intFromEnum(chunk.OpCode.OpNil));
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

    fn patchJump(self: *Self, offset: u16) !void {
        const jump: u16 = @intCast(self.currentChunk().count - offset - 2);

        if (jump > std.math.maxInt(u16)) {
            self.err("Too much code to jump over.");
        }

        if (self.currentChunk().code) |code| {
            code[offset] = @intCast((jump >> 8) & 0xff);
            code[offset + 1] = @intCast(jump & 0xff);
        }
    }

    fn currentChunk(self: *Self) *chunk.Chunk {
        return self.function.functionValue().chnk;
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
