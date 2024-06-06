const std = @import("std");
const token = @import("token.zig");
const lox = @import("lox.zig");

pub const Scanner = struct {
    const Self = @This();

    source: []const u8,
    tokens: std.ArrayList(token.Token),
    start: usize = 0,
    current: usize = 0,
    line: usize = 1,

    pub fn init(allocator: std.mem.Allocator, source: []const u8) Scanner {
        return Scanner{
            .source = source,
            .tokens = std.ArrayList(token.Token).init(allocator),
        };
    }

    pub fn deinit(self: *Self) void {
        self.tokens.deinit();
    }

    pub fn scanTokens(self: *Self) ![]token.Token {
        while (!self.isAtEnd()) {
            self.start = self.current;
            _ = try self.scanToken();
        }

        try self.tokens.append(.{
            .typ = .eof,
            .line = self.line,
        });

        return self.tokens.toOwnedSlice();
    }

    fn scanToken(self: *Self) !void {
        const c = self.advance();
        switch (c) {
            '(' => try self.addToken(.left_paren),
            ')' => try self.addToken(.right_paren),
            '{' => try self.addToken(.left_brace),
            '}' => try self.addToken(.right_brace),
            ',' => try self.addToken(.comma),
            '.' => try self.addToken(.dot),
            '-' => try self.addToken(.minus),
            '+' => try self.addToken(.plus),
            ';' => try self.addToken(.semicolon),
            '*' => try self.addToken(.star),
            '!' => if (self.match('=')) {
                try self.addToken(.bang_equal);
            } else {
                try self.addToken(.bang);
            },
            '=' => if (self.match('=')) {
                try self.addToken(.equal_equal);
            } else {
                try self.addToken(.equal);
            },
            '<' => if (self.match('=')) {
                try self.addToken(.less_equal);
            } else {
                try self.addToken(.less);
            },
            '>' => if (self.match('=')) {
                try self.addToken(.greater_equal);
            } else {
                try self.addToken(.greater);
            },
            '/' => if (self.match('/')) {
                while (!self.isAtEnd() and self.source[self.current] != '\n') {
                    _ = self.advance();
                }
            } else {
                try self.addToken(.slash);
            },
            ' ', '\r', '\t' => {},
            '\n' => self.line += 1,
            '"' => {
                try self.string();
            },
            else => {
                if (isDigit(c)) {
                    try self.number();
                } else if (isAlpha(c)) {
                    try self.identifier();
                } else {
                    lox.Lox.err(self.line, "Unexpected character.");
                }
            },
        }
    }

    fn identifier(self: *Self) !void {
        while (isAlphaNumeric(self.peek())) {
            _ = self.advance();
        }

        const text = self.source[self.start..self.current];
        const typ = token.keyword(text);
        if (typ) |t| {
            try self.addToken(t);
        } else {
            try self.addToken(.{ .identifier = text });
        }
    }

    fn number(self: *Self) !void {
        while (isDigit(self.peek())) {
            _ = self.advance();
        }

        if (self.peek() == '.' and isDigit(self.peekNext())) {
            _ = self.advance();

            while (isDigit(self.peek())) {
                _ = self.advance();
            }
        }

        const value = try std.fmt.parseFloat(f64, self.source[self.start..self.current]);
        try self.addToken(.{ .number = value });
    }

    fn string(self: *Self) !void {
        while (!self.isAtEnd() and self.peek() != '"') {
            if (self.peek() == '\n') {
                self.line += 1;
            }
            _ = self.advance();
        }

        if (self.isAtEnd()) {
            lox.Lox.err(self.line, "Unterminated string.");
            return;
        }

        _ = self.advance();

        const value = self.source[self.start + 1 .. self.current - 1];
        try self.addToken(.{ .string = value });
    }

    fn advance(self: *Self) u8 {
        defer self.current += 1;
        return self.source[self.current];
    }

    fn addToken(self: *Self, typ: token.TokenType) !void {
        try self.tokens.append(.{
            .typ = typ,
            .line = self.line,
        });
    }

    fn isAtEnd(self: *Self) bool {
        return self.current >= self.source.len;
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

    fn peek(self: *Self) u8 {
        if (self.isAtEnd()) {
            return '\x00';
        }

        return self.source[self.current];
    }

    fn peekNext(self: *Self) u8 {
        if (self.current + 1 >= self.source.len) {
            return '\x00';
        }

        return self.source[self.current + 1];
    }

    fn isAlpha(c: u8) bool {
        return (c >= 'a' and c <= 'z') or (c >= 'A' and c <= 'Z') or c == '_';
    }

    fn isAlphaNumeric(c: u8) bool {
        return isAlpha(c) or isDigit(c);
    }

    fn isDigit(c: u8) bool {
        return c >= '0' and c <= '9';
    }
};
