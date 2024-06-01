const std = @import("std");
const token = @import("token.zig");

const Scanner = struct {
    const Self = @This();

    source: []const u8,
    tokens: std.ArrayList(token.Token),
    start: usize = 0,
    current: usize = 0,
    line: usize = 1,

    pub fn init(allocator: std.mem.Allocator, source: []const u8) !void {
        return Scanner{
            .source = source,
            .tokens = std.ArrayList(token.Token).init(allocator),
        };
    }

    pub fn scanTokens(self: *Self) ![]token.Token {
        while (!self.isAtEnd()) {
            self.start = self.current;
            self.scanToken();
        }

        try self.tokens.append(.{
            .typ = .eof,
            .length = self.line,
        });

        return self.tokens.toOwnedSlice();
    }

    fn scanToken(self: *Self) !void {
        const c = self.advance();
        switch (c) {
            '(' => self.addToken(.leftParen),
            ')' => self.addToken(.rightParen),
            '{' => self.addToken(.leftBrace),
            '}' => self.addToken(.rightBrace),
            ',' => self.addToken(.comma),
            '.' => self.addToken(.dot),
            '-' => self.addToken(.minus),
            '+' => self.addToken(.plus),
            ';' => self.addToken(.semicolon),
            '*' => self.addToken(.star),
            else => {
                return std.debug.print("Unexpected character: '{}'\n", .{c});
            },
        }
    }

    fn advance(self: *Self) u8 {
        defer self.current += 1;
        return self.source[self.current];
    }

    fn addToken(self: *Self, typ: token.TokenType) !void {
        try self.tokens.append(.{
            .typ = typ,
            .length = self.line,
        });
    }

    fn isAtEnd(self: *Self) bool {
        return self.current >= self.source.len;
    }
};
