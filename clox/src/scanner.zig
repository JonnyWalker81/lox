const std = @import("std");

pub const CompilerError = error{
    UnexpectedToken,
    UnterminatedString,
};

pub const TokenType = union(enum) {
    const Self = @This();

    left_paren,
    right_paren,
    left_brace,
    right_brace,
    comma,
    dot,
    minus,
    plus,
    semicolon,
    slash,
    star,
    bang,
    bang_equal,
    equal,
    equal_equal,
    greater,
    greater_equal,
    less,
    less_equal,
    identifier: []const u8,
    string: []const u8,
    number: f64,
    @"and",
    class,
    @"else",
    false,
    @"for",
    fun,
    @"if",
    nil,
    @"or",
    print,
    @"return",
    super,
    this,
    true,
    @"var",
    @"while",
    @"error",
    eof,

    pub fn format(
        self: Self,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;

        switch (self) {
            .left_paren => try writer.print("(", .{}),
            .right_paren => try writer.print(")", .{}),
            .left_brace => try writer.print("{{", .{}),
            .right_brace => try writer.print("}}", .{}),
            .comma => try writer.print(",", .{}),
            .dot => try writer.print(".", .{}),
            .minus => try writer.print("-", .{}),
            .plus => try writer.print("+", .{}),
            .semicolon => try writer.print(";", .{}),
            .slash => try writer.print("/", .{}),
            .star => try writer.print("*", .{}),
            .bang => try writer.print("!", .{}),
            .bang_equal => try writer.print("!=", .{}),
            .equal => try writer.print("=", .{}),
            .equal_equal => try writer.print("==", .{}),
            .greater => try writer.print(">", .{}),
            .greater_equal => try writer.print(">=", .{}),
            .less => try writer.print("<", .{}),
            .less_equal => try writer.print("<=", .{}),
            .identifier => |i| try writer.print("{s}", .{i}),
            .string => |s| try writer.print("\"{s}\"", .{s}),
            .number => |n| try writer.print("{d}", .{n}),
            .@"and" => try writer.print("and", .{}),
            .class => try writer.print("class", .{}),
            .@"else" => try writer.print("else", .{}),
            .false => try writer.print("false", .{}),
            .@"for" => try writer.print("for", .{}),
            .fun => try writer.print("fun", .{}),
            .@"if" => try writer.print("if", .{}),
            .nil => try writer.print("nil", .{}),
            .@"or" => try writer.print("or", .{}),
            .print => try writer.print("print", .{}),
            .@"return" => try writer.print("return", .{}),
            .super => try writer.print("super", .{}),
            .this => try writer.print("this", .{}),
            .true => try writer.print("true", .{}),
            .@"var" => try writer.print("var", .{}),
            .@"while" => try writer.print("while", .{}),
            .@"error" => try writer.print("error", .{}),
            .eof => try writer.print("eof", .{}),
        }
    }
};

pub const Token = struct {
    type: TokenType,
    start: usize,
    length: usize,
    line: usize,
};

pub const Scanner = struct {
    const Self = @This();

    arena: std.heap.ArenaAllocator,
    start: usize = 0,
    current: usize = 0,
    line: usize = 1,
    source: []const u8,

    pub fn init(allocator: std.mem.Allocator, source: []const u8) Scanner {
        return Scanner{
            .arena = std.heap.ArenaAllocator.init(allocator),
            .source = source,
        };
    }

    pub fn scanToken(self: *Self) !Token {
        self.skipWhitespace();

        self.start = self.current;

        if (self.isAtEnd()) {
            return self.makeToken(.eof);
        }

        const c = self.advance();

        switch (c) {
            '(' => return self.makeToken(.left_paren),
            ')' => return self.makeToken(.right_paren),
            '{' => return self.makeToken(.left_brace),
            '}' => return self.makeToken(.right_brace),
            ';' => return self.makeToken(.semicolon),
            ',' => return self.makeToken(.comma),
            '.' => return self.makeToken(.dot),
            '-' => return self.makeToken(.minus),
            '+' => return self.makeToken(.plus),
            '/' => return self.makeToken(.slash),
            '*' => return self.makeToken(.star),
            '!' => {
                if (self.match('=')) {
                    return self.makeToken(.bang_equal);
                } else {
                    return self.makeToken(.bang);
                }
            },
            '=' => {
                if (self.match('=')) {
                    return self.makeToken(.equal_equal);
                } else {
                    return self.makeToken(.equal);
                }
            },
            '<' => {
                if (self.match('=')) {
                    return self.makeToken(.less_equal);
                } else {
                    return self.makeToken(.less);
                }
            },
            '>' => {
                if (self.match('=')) {
                    return self.makeToken(.greater_equal);
                } else {
                    return self.makeToken(.greater);
                }
            },
            '"' => {
                return try self.string();
            },
            else => {
                if (isDigit(c)) {
                    return try self.number();
                } else if (isAlpha(c)) {
                    return try self.identifier();
                } else {
                    std.debug.print("else: {c}\n", .{c});
                }
            },
        }

        std.debug.print("unexpected token: {c}\n", .{c});
        return CompilerError.UnexpectedToken;
    }

    fn isDigit(c: u8) bool {
        return c >= '0' and c <= '9';
    }

    fn isAlpha(c: u8) bool {
        return (c >= 'a' and c <= 'z') or (c >= 'A' and c <= 'Z') or c == '_';
    }

    fn isAtEnd(self: *Self) bool {
        return self.current >= self.source.len;
    }

    fn advance(self: *Self) u8 {
        self.current += 1;
        return self.source[self.current - 1];
    }

    fn peek(self: *Self) u8 {
        if (self.isAtEnd()) {
            return 0;
        }

        return self.source[self.current];
    }

    fn peekNext(self: *Self) u8 {
        if (self.isAtEnd()) {
            return 0;
        }

        return self.source[self.current + 1];
    }

    fn match(self: *Self, expected: u8) bool {
        if (self.isAtEnd()) {
            return false;
        }

        if (self.source[self.current] != expected) {
            return false;
        }

        self.current += 1;
        return true;
    }

    fn makeToken(self: *Self, typ: TokenType) Token {
        return .{
            .type = typ,
            .start = self.start,
            .length = self.current - self.start,
            .line = self.line,
        };
    }

    fn skipWhitespace(self: *Self) void {
        while (true) {
            const c = self.peek();
            switch (c) {
                ' ', '\r', '\t' => _ = self.advance(),
                '\n' => {
                    self.line += 1;
                    _ = self.advance();
                },
                '/' => {
                    if (self.peekNext() == '/') {
                        while (self.peek() != '\n' and !self.isAtEnd()) {
                            _ = self.advance();
                        }
                    } else {
                        break;
                    }
                },
                else => break,
            }
        }
    }

    fn string(self: *Self) !Token {
        while (self.peek() != '"' and !self.isAtEnd()) {
            if (self.peek() == '\n') {
                self.line += 1;
            }
            _ = self.advance();
        }

        if (self.isAtEnd()) {
            return CompilerError.UnterminatedString;
        }

        _ = self.advance();

        return self.makeToken(.{
            .string = self.source[self.start + 1 .. self.current - 1],
        });
    }

    fn number(self: *Self) !Token {
        while (isDigit(self.peek())) {
            _ = self.advance();
        }

        if (self.peek() == '.' and isDigit(self.peekNext())) {
            _ = self.advance();

            while (isDigit(self.peek())) {
                _ = self.advance();
            }
        }

        return self.makeToken(.{
            .number = try std.fmt.parseFloat(f64, self.source[self.start..self.current]),
        });
    }

    fn identifier(self: *Self) !Token {
        while (isAlpha(self.peek()) or isDigit(self.peek())) {
            _ = self.advance();
        }

        const text = self.source[self.start..self.current];

        return self.makeToken(self.identifierType(text));
    }

    fn identifierType(self: *Self, text: []const u8) TokenType {
        const c = self.source[self.start];
        switch (c) {
            'a' => return self.checkKeyword(1, 2, "nd", .@"and", text),
            'c' => return self.checkKeyword(1, 4, "lass", .class, text),
            'e' => return self.checkKeyword(1, 3, "lse", .@"else", text),
            'f' => {
                if (self.current - self.start > 1) {
                    const c2 = self.source[self.start + 1];
                    switch (c2) {
                        'a' => return self.checkKeyword(2, 3, "lse", .false, text),
                        'o' => return self.checkKeyword(2, 1, "r", .@"for", text),
                        'u' => return self.checkKeyword(2, 1, "n", .fun, text),
                        else => return .{ .identifier = text },
                    }
                }
            },
            'i' => return self.checkKeyword(1, 1, "f", .@"if", text),
            'n' => return self.checkKeyword(1, 2, "il", .nil, text),
            'o' => return self.checkKeyword(1, 1, "r", .@"or", text),
            'p' => return self.checkKeyword(1, 4, "rint", .print, text),
            'r' => return self.checkKeyword(1, 5, "eturn", .@"return", text),
            's' => return self.checkKeyword(1, 4, "uper", .super, text),
            't' => {
                if (self.current - self.start > 1) {
                    const c2 = self.source[self.start + 1];
                    switch (c2) {
                        'h' => return self.checkKeyword(2, 2, "is", .this, text),
                        'r' => return self.checkKeyword(2, 2, "ue", .true, text),
                        else => return .{ .identifier = text },
                    }
                }
            },
            'v' => return self.checkKeyword(1, 2, "ar", .@"var", text),
            'w' => return self.checkKeyword(1, 4, "hile", .@"while", text),
            else => return .{ .identifier = text },
        }

        return .{ .identifier = text };
    }

    fn checkKeyword(self: *Self, start: usize, length: usize, rest: []const u8, typ: TokenType, text: []const u8) TokenType {
        const s = self.source[self.start + start .. self.start + start + length];
        // std.debug.print("s: {s}\n", .{s});
        // std.debug.print("rest: {s}\n", .{rest});
        // std.debug.print("current: {d} -> {c}\n", .{ self.current, self.source[self.current] });
        // std.debug.print("left: {d}\n", .{self.current - self.start});
        // std.debug.print("right: {d}\n", .{start + length});
        if (self.current - self.start == start + length and std.mem.eql(u8, s, rest)) {
            // std.debug.print("matched\n", .{});
            return typ;
        }

        return .{ .identifier = text };
    }
};
