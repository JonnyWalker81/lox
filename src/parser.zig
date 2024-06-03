const std = @import("std");
const token = @import("token.zig");
const ast = @import("ast.zig");
const lox = @import("lox.zig");

const ParseErrors = error{
    ParseError,
    ExpectedExpression,
};

pub const Parser = struct {
    const Self = @This();

    arena: std.heap.ArenaAllocator,
    tokens: []token.Token,
    current: usize = 0,

    pub fn init(allocator: std.mem.Allocator, tokens: []token.Token) Parser {
        const arena = std.heap.ArenaAllocator.init(allocator);
        return Parser{
            .arena = arena,
            .tokens = tokens,
        };
    }

    pub fn deinit(self: *Self) void {
        self.arena.deinit();
    }

    pub fn parse(self: *Self) !?*ast.Expression {
        return self.expression() catch {
            return null;
        };
    }

    pub fn expression(self: *Self) anyerror!*ast.Expression {
        return try self.equality();
    }

    fn equality(self: *Self) anyerror!*ast.Expression {
        var expr = try self.comparison();

        while (self.match(.{ @tagName(token.TokenType.bang_equal), @tagName(token.TokenType.equal_equal) })) {
            const op = self.previous();
            const right = try self.comparison();
            const left = expr;
            expr = try self.arena.allocator().create(ast.Expression);
            expr.* = .{
                .binary = .{
                    .left = left,
                    .operator = op,
                    .right = right,
                },
            };
        }

        return expr;
    }

    fn comparison(self: *Self) anyerror!*ast.Expression {
        var expr = try self.term();

        while (self.match(.{
            @tagName(token.TokenType.greater),
            @tagName(token.TokenType.greater_equal),
            @tagName(token.TokenType.less),
            @tagName(token.TokenType.less_equal),
        })) {
            const op = self.previous();
            const left = expr;
            const right = try self.term();
            expr = try self.arena.allocator().create(ast.Expression);
            expr.* = .{
                .binary = .{
                    .left = left,
                    .operator = op,
                    .right = right,
                },
            };
        }

        return expr;
    }

    fn term(self: *Self) !*ast.Expression {
        var expr = try self.factor();

        std.log.warn("factor result: {}\n", .{expr});

        while (self.match(.{
            @tagName(token.TokenType.minus),
            @tagName(token.TokenType.plus),
        })) {
            const op = self.previous();
            const right = try self.factor();
            const left = expr;
            expr = try self.arena.allocator().create(ast.Expression);
            expr.* = .{
                .binary = .{
                    .left = left,
                    .operator = op,
                    .right = right,
                },
            };
            std.log.warn("Term: {} {} {}\n", .{ expr.binary.left, expr.binary.operator, expr.binary.right });
            // std.log.warn("Term: {}\n", .{expr.binary.left});
        }

        return expr;
    }

    fn factor(self: *Self) !*ast.Expression {
        var expr = try self.unary();

        while (self.match(.{ @tagName(token.TokenType.slash), @tagName(token.TokenType.star) })) {
            const op = self.previous();
            const left = expr;
            const right = try self.unary();
            expr = try self.arena.allocator().create(ast.Expression);
            expr.* = .{
                .binary = .{
                    .left = left,
                    .operator = op,
                    .right = right,
                },
            };
        }

        return expr;
    }

    fn unary(self: *Self) !*ast.Expression {
        if (self.match(.{ @tagName(token.TokenType.bang), @tagName(token.TokenType.minus) })) {
            const op = self.previous();
            const right = try self.unary();
            const expr = try self.arena.allocator().create(ast.Expression);
            expr.* = .{
                .unary = .{
                    .operator = op,
                    .right = right,
                },
            };
            return expr;
        }

        return self.primary();
    }

    fn primary(self: *Self) !*ast.Expression {
        if (self.match(.{@tagName(token.TokenType.false)})) {
            const expr = try self.arena.allocator().create(ast.Expression);
            const literal = try self.arena.allocator().create(ast.Expression);
            literal.* = .{
                .boolean = false,
            };
            expr.* = .{
                .literal = literal,
            };
            return expr;
        }

        if (self.match(.{@tagName(token.TokenType.true)})) {
            const expr = try self.arena.allocator().create(ast.Expression);
            const literal = try self.arena.allocator().create(ast.Expression);
            literal.* = .{
                .boolean = true,
            };
            expr.* = .{
                .literal = literal,
            };
            return expr;
        }

        if (self.match(.{@tagName(token.TokenType.nil)})) {
            const expr = try self.arena.allocator().create(ast.Expression);
            const literal = try self.arena.allocator().create(ast.Expression);
            literal.* = .nil;
            expr.* = .{
                .literal = literal,
            };
            return expr;
        }

        if (self.match(.{ @tagName(token.TokenType.number), @tagName(token.TokenType.string) })) {
            // std.log.warn("Literal: {}\n", .{self.previous().typ});
            const expr = try self.arena.allocator().create(ast.Expression);
            const literal = try self.arena.allocator().create(ast.Expression);
            if (self.previous().typ == token.TokenType.number) {
                literal.* = .{
                    .number = self.previous().typ.number,
                };
            } else {
                literal.* = .{
                    .string = self.previous().typ.string,
                };
            }
            expr.* = .{
                .literal = literal,
            };
            std.log.warn("returning Literal: {}\n", .{expr.literal});
            return expr;
        }

        if (self.match(.{@tagName(token.TokenType.left_paren)})) {
            const expr = try self.expression();
            _ = try self.consume(@tagName(token.TokenType.right_paren), "Expect ')' after expression.");
            return expr;
        }

        return ParseErrors.ExpectedExpression;
    }

    fn match(self: *Self, types: anytype) bool {
        inline for (types) |t| {
            if (self.check(t)) {
                _ = self.advance();
                return true;
            }
        }

        return false;
    }

    fn consume(self: *Self, typ: []const u8, message: []const u8) !token.Token {
        if (self.check(typ)) {
            return self.advance();
        }

        lox.Lox.err(self.peek().line, message);
        return ParseErrors.ParseError;
    }

    fn check(self: *Self, typ: []const u8) bool {
        if (self.is_at_end()) {
            return false;
        }

        return std.mem.eql(u8, @tagName(self.peek().typ), typ);
    }

    fn advance(self: *Self) token.Token {
        if (!self.is_at_end()) {
            self.current += 1;
        }

        return self.previous();
    }

    fn is_at_end(self: *Self) bool {
        return self.peek().typ == token.TokenType.eof;
    }

    fn peek(self: *Self) token.Token {
        return self.tokens[self.current];
    }

    fn previous(self: *Self) token.Token {
        return self.tokens[self.current - 1];
    }

    fn err(tok: token.Token, message: []const u8) !void {
        lox.Lox.err(tok, message);
        return ParseErrors.ParseError;
    }

    fn synchronize(self: *Self) void {
        self.advance();

        while (!self.is_at_end()) {
            if (self.previous().typ == token.TokenType.semicolon) {
                return;
            }

            switch (self.peek().typ) {
                .class,
                .fun,
                .@"var",
                .@"for",
                .@"if",
                .@"while",
                .print,
                .@"return",
                => return,
                else => {},
            }

            self.advance();
        }
    }
};
