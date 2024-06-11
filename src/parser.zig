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

    pub fn parse(self: *Self) ![]*ast.Statement {
        var statements = std.ArrayList(*ast.Statement).init(self.arena.allocator());
        while (!self.is_at_end()) {
            _ = try statements.append(try self.declaration());
        }

        return statements.toOwnedSlice();
    }

    pub fn expression(self: *Self) anyerror!*ast.Expression {
        return try self.assignment();
    }

    fn declaration(self: *Self) anyerror!*ast.Statement {
        if (self.match(.{@tagName(token.TokenType.class)})) {
            return self.classDeclaration();
        }

        if (self.match(.{@tagName(token.TokenType.fun)})) {
            const fun = try self.arena.allocator().create(ast.Statement);
            fun.* = .{
                .function = try self.function("function"),
            };
            return fun;
        }

        if (self.match(.{@tagName(token.TokenType.@"var")})) {
            return self.varDeclaration() catch {
                self.synchronize();
                return ParseErrors.ParseError;
            };
        }

        return self.statement() catch {
            self.synchronize();
            return ParseErrors.ParseError;
        };
    }

    pub fn classDeclaration(self: *Self) !*ast.Statement {
        const name = try self.consume(@tagName(token.TokenType.identifier), "Expect class name.");
        _ = try self.consume(@tagName(token.TokenType.left_brace), "Expect '{' before class body.");

        // var superclass: ?*ast.Expression = null;
        // if (self.match(.{@tagName(token.TokenType.less)})) {
        //     _ = try self.consume(@tagName(token.TokenType.identifier), "Expect superclass name.");
        //     superclass = try self.arena.allocator().create(ast.Expression);
        //     superclass.* = .{
        //         .variable = self.previous(),
        //     };
        // }

        // _ = try self.consume(@tagName(token.TokenType.left_brace), "Expect '{' before class body.");

        var methods = std.ArrayList(*ast.FunctionStatement).init(self.arena.allocator());
        while (!self.check(@tagName(token.TokenType.right_brace)) and !self.is_at_end()) {
            _ = try methods.append(try self.function("method"));
        }

        _ = try self.consume(@tagName(token.TokenType.right_brace), "Expect '}' after class body.");

        const stmt = try self.arena.allocator().create(ast.Statement);
        const class = try self.arena.allocator().create(ast.ClassStatement);
        class.* = .{
            .name = name,
            // .superclass = superclass,
            .methods = try methods.toOwnedSlice(),
        };

        stmt.* = .{
            .classStmt = class,
        };

        return stmt;
    }

    pub fn statement(self: *Self) anyerror!*ast.Statement {
        if (self.match(.{@tagName(token.TokenType.@"for")})) {
            return try self.forStatement();
        }

        if (self.match(.{@tagName(token.TokenType.@"if")})) {
            return try self.ifStatement();
        }

        if (self.match(.{@tagName(token.TokenType.print)})) {
            return try self.printStatement();
        }

        if (self.match(.{@tagName(token.TokenType.@"return")})) {
            return try self.returnStatement();
        }

        if (self.match(.{@tagName(token.TokenType.@"while")})) {
            return try self.whileStatement();
        }

        if (self.match(.{@tagName(token.TokenType.left_brace)})) {
            const stmts = try self.block();
            const stmt = try self.arena.allocator().create(ast.Statement);
            stmt.* = .{
                .block = stmts,
            };
            return stmt;
        }

        return try self.expressionStatement();
    }

    fn forStatement(self: *Self) !*ast.Statement {
        _ = try self.consume(@tagName(token.TokenType.left_paren), "Expect '(' after 'for'.");
        var initializer: ?*ast.Statement = null;
        if (self.match(.{@tagName(token.TokenType.semicolon)})) {
            initializer = null;
        } else if (self.match(.{@tagName(token.TokenType.@"var")})) {
            initializer = try self.varDeclaration();
        } else {
            initializer = try self.expressionStatement();
        }

        var condition: ?*ast.Expression = null;
        if (!self.check(@tagName(token.TokenType.semicolon))) {
            condition = try self.expression();
        }
        _ = try self.consume(@tagName(token.TokenType.semicolon), "Expect ';' after loop condition.");

        var increment: ?*ast.Expression = null;
        if (!self.check(@tagName(token.TokenType.right_paren))) {
            increment = try self.expression();
        }
        _ = try self.consume(@tagName(token.TokenType.right_paren), "Expect ')' after for clauses.");

        const bodyStmt = try self.statement();
        const bodyBlock: ?*ast.Statement = try self.arena.allocator().create(ast.Statement);
        if (increment) |inc| {
            // const b = body;
            const stmt = try self.arena.allocator().create(ast.Statement);

            stmt.* = .{ .expressionStatement = inc };
            // body = try self.arena.allocator().create(ast.Statement);
            var stmts = std.ArrayList(*ast.Statement).init(self.arena.allocator());
            try stmts.append(bodyStmt);
            try stmts.append(stmt);
            bodyBlock.?.* = .{
                .block = try stmts.toOwnedSlice(),
            };
        }

        if (condition == null) {
            // std.log.warn("Condition is null, while true", .{});
            condition = try self.arena.allocator().create(ast.Expression);
            const condLit = try self.arena.allocator().create(ast.Expression);
            condLit.* = .{ .boolean = true };
            condition.?.* = .{
                .literal = condLit,
            };
        }

        // const stmt = try self.arena.allocator().create(ast.Statement);
        const whileStmt = try self.arena.allocator().create(ast.WhileStatement);

        // const b = body;
        whileStmt.* = .{
            .condition = condition.?,
            .body = bodyBlock.?,
        };

        const bodyWhile: ?*ast.Statement = try self.arena.allocator().create(ast.Statement);
        bodyWhile.?.* = .{
            .whileStmt = whileStmt,
        };

        // stmt.* = .{ .whileStmt = whileStmt };
        var bodyFinal: ?*ast.Statement = null;
        if (initializer) |in| {
            bodyFinal = try self.arena.allocator().create(ast.Statement);
            // const bbb = try self.arena.allocator().create(ast.Statement);
            // bbb. = &[_]*ast.Statement{ initializer.?, bb };
            var stmts = std.ArrayList(*ast.Statement).init(self.arena.allocator());
            try stmts.append(in);
            try stmts.append(bodyWhile.?);
            bodyFinal.?.* = .{
                .block = try stmts.toOwnedSlice(),
            };
        }

        // std.log.warn("For statement (while loop): {s} -- {any}\n", .{ stmt.whileStmt.condition, stmt.whileStmt.body });

        if (bodyFinal) |bf| {
            return bf;
        } else {
            return bodyWhile.?;
        }
    }

    fn whileStatement(self: *Self) !*ast.Statement {
        _ = try self.consume(@tagName(token.TokenType.left_paren), "Expect '(' after 'while'.");
        const condition = try self.expression();
        _ = try self.consume(@tagName(token.TokenType.right_paren), "Expect ')' after condition.");
        const body = try self.statement();

        const stmt = try self.arena.allocator().create(ast.Statement);
        const whileStmt = try self.arena.allocator().create(ast.WhileStatement);
        whileStmt.* = .{
            .condition = condition,
            .body = body,
        };
        stmt.* = .{
            .whileStmt = whileStmt,
        };

        return stmt;
    }

    fn ifStatement(self: *Self) !*ast.Statement {
        _ = try self.consume(@tagName(token.TokenType.left_paren), "Expect '(' after 'if'.");
        const condition = try self.expression();

        _ = try self.consume(@tagName(token.TokenType.right_paren), "Expect ')' after if condition.");
        const thenBranch = try self.statement();
        var elseBranch: ?*ast.Statement = null;
        if (self.match(.{@tagName(token.TokenType.@"else")})) {
            elseBranch = try self.statement();
        }

        const stmt = try self.arena.allocator().create(ast.Statement);
        const ifStmt = try self.arena.allocator().create(ast.IfStatement);
        ifStmt.* = .{
            .condition = condition,
            .thenBranch = thenBranch,
            .elseBranch = elseBranch,
        };
        stmt.* = .{
            .ifStmt = ifStmt,
        };

        return stmt;
    }

    fn printStatement(self: *Self) !*ast.Statement {
        const value = try self.expression();
        // std.log.warn("Print statement: {}\n", .{value.variable.typ});
        _ = try self.consume(@tagName(token.TokenType.semicolon), "Expect ';' after value.");
        const stmt = try self.arena.allocator().create(ast.Statement);
        stmt.* = .{
            .print = value,
        };

        return stmt;
    }

    fn returnStatement(self: *Self) !*ast.Statement {
        const keyword = self.previous();
        var expr: ?*ast.Expression = null;
        if (!self.check(@tagName(token.TokenType.semicolon))) {
            expr = try self.expression();
        }
        _ = try self.consume(@tagName(token.TokenType.semicolon), "Expect ';' after return value.");
        const stmt = try self.arena.allocator().create(ast.Statement);
        const returnStmt = try self.arena.allocator().create(ast.ReturnStatement);
        returnStmt.* = .{
            .keyword = keyword,
            .expr = expr,
        };

        stmt.* = .{
            .returnStmt = returnStmt,
        };

        return stmt;
    }

    fn varDeclaration(self: *Self) !*ast.Statement {
        const name = try self.consume(@tagName(token.TokenType.identifier), "Expect variable name.");
        var initializer: *ast.Expression = undefined;
        if (self.match(.{@tagName(token.TokenType.equal)})) {
            initializer = try self.expression();
        }

        _ = try self.consume(@tagName(token.TokenType.semicolon), "Expect ';' after variable declaration.");
        const stmt = try self.arena.allocator().create(ast.Statement);
        const v = try self.arena.allocator().create(ast.Variable);
        v.* = .{
            .name = name,
            .initializer = initializer,
        };
        stmt.* = .{
            .variable = v,
        };
        // std.log.warn("Var declaration: {any}\n", .{stmt});

        return stmt;
    }

    fn expressionStatement(self: *Self) !*ast.Statement {
        const value = try self.expression();
        _ = try self.consume(@tagName(token.TokenType.semicolon), "Expect ';' after value.");
        const stmt = try self.arena.allocator().create(ast.Statement);
        stmt.* = .{
            .expressionStatement = value,
        };

        return stmt;
    }

    fn function(self: *Self, kind: []const u8) !*ast.FunctionStatement {
        var errStr = try std.fmt.allocPrint(self.arena.allocator(), "Expect {s} name", .{kind});
        const name = try self.consume(@tagName(token.TokenType.identifier), errStr);
        errStr = try std.fmt.allocPrint(self.arena.allocator(), "Expect '(' after {s} name", .{kind});
        _ = try self.consume(@tagName(token.TokenType.left_paren), errStr);
        var parameters = std.ArrayList(token.Token).init(self.arena.allocator());
        if (!self.check(@tagName(token.TokenType.right_paren))) {
            _ = try parameters.append(try self.consume(@tagName(token.TokenType.identifier), "Expect parameter name."));
            while (self.match(.{@tagName(token.TokenType.comma)})) {
                if (parameters.items.len >= 255) {
                    lox.Lox.err(self.peek().line, "Cannot have more than 255 parameters.");
                }
                _ = try parameters.append(try self.consume(@tagName(token.TokenType.identifier), "Expect parameter name."));
            }
        }
        _ = try self.consume(@tagName(token.TokenType.right_paren), "Expect ')' after parameters.");

        errStr = try std.fmt.allocPrint(self.arena.allocator(), "Expect '{{' before {s} body", .{kind});
        _ = try self.consume(@tagName(token.TokenType.left_brace), errStr);
        const body = try self.block();
        const f = try self.arena.allocator().create(ast.FunctionStatement);
        f.* = .{
            .name = name,
            .parameters = try parameters.toOwnedSlice(),
            .body = body,
        };

        return f;
    }

    fn block(self: *Self) ![]*ast.Statement {
        var statements = std.ArrayList(*ast.Statement).init(self.arena.allocator());
        while (!self.check(@tagName(token.TokenType.right_brace)) and !self.is_at_end()) {
            _ = try statements.append(try self.declaration());
        }
        _ = try self.consume(@tagName(token.TokenType.right_brace), "Expect '}' after block.");
        return statements.toOwnedSlice();
    }

    fn assignment(self: *Self) anyerror!*ast.Expression {
        // std.log.warn("Assignment\n", .{});
        const expr = try self.logicalOr();

        if (self.match(.{@tagName(token.TokenType.equal)})) {
            const equals = self.previous();
            const value = try self.assignment();

            if (expr.* == ast.Expression.variable) {
                const variable = expr.variable;
                const assign = try self.arena.allocator().create(ast.Expression);
                assign.* = .{
                    .assignment = .{
                        .name = variable,
                        .value = value,
                    },
                };
                // std.log.warn("Assignment: {s}\n", .{assign});
                return assign;
            } else if (expr.* == ast.Expression.get) {
                const get = expr.get;
                const assign = try self.arena.allocator().create(ast.Expression);
                assign.* = .{
                    .set = .{
                        .object = get.object,
                        .name = get.name,
                        .value = value,
                    },
                };
                return assign;
            }

            try self.err(equals, "Invalid assignment target.");
        }

        return expr;
    }

    fn logicalOr(self: *Self) anyerror!*ast.Expression {
        var expr = try self.logicalAnd();
        while (self.match(.{@tagName(token.TokenType.@"or")})) {
            const op = self.previous();
            const right = try self.logicalAnd();
            const left = expr;
            expr = try self.arena.allocator().create(ast.Expression);
            expr.* = .{
                .logical = .{
                    .left = left,
                    .operator = op,
                    .right = right,
                },
            };
        }
        return expr;
    }

    fn logicalAnd(self: *Self) anyerror!*ast.Expression {
        var expr = try self.equality();
        while (self.match(.{@tagName(token.TokenType.@"and")})) {
            const op = self.previous();
            const right = try self.equality();
            const left = expr;
            expr = try self.arena.allocator().create(ast.Expression);
            expr.* = .{
                .logical = .{
                    .left = left,
                    .operator = op,
                    .right = right,
                },
            };
        }
        return expr;
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

        // std.log.warn("factor result: {}\n", .{expr});

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
            // std.log.warn("Term: {} {} {}\n", .{ expr.binary.left, expr.binary.operator, expr.binary.right });
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

        return self.call();
    }

    fn finishCall(self: *Self, callee: *ast.Expression) !*ast.Expression {
        var arguments = std.ArrayList(*ast.Expression).init(self.arena.allocator());
        if (!self.check(@tagName(token.TokenType.right_paren))) {
            _ = try arguments.append(try self.expression());
            while (self.match(.{@tagName(token.TokenType.comma)})) {
                if (arguments.items.len >= 255) {
                    lox.Lox.err(self.peek().line, "Cannot have more than 255 arguments.");
                }
                _ = try arguments.append(try self.expression());
            }
        }
        const paren = try self.consume(@tagName(token.TokenType.right_paren), "Expect ')' after arguments.");
        const expr = try self.arena.allocator().create(ast.Expression);
        expr.* = .{
            .call = .{
                .callee = callee,
                .paren = paren,
                .arguments = try arguments.toOwnedSlice(),
            },
        };

        return expr;
    }

    fn call(self: *Self) !*ast.Expression {
        var expr = try self.primary();

        while (true) {
            if (self.match(.{@tagName(token.TokenType.left_paren)})) {
                expr = try self.finishCall(expr);
            } else if (self.match(.{@tagName(token.TokenType.dot)})) {
                const name = try self.consume(@tagName(token.TokenType.identifier), "Expect property name after '.'.");
                const expr2 = try self.arena.allocator().create(ast.Expression);
                expr2.* = .{
                    .get = .{
                        .object = expr,
                        .name = name,
                    },
                };
                expr = expr2;
            } else {
                break;
            }
        }
        return expr;
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
            // std.log.warn("returning Literal: {}\n", .{expr.literal});
            return expr;
        }

        if (self.match(.{@tagName(token.TokenType.identifier)})) {
            const expr = try self.arena.allocator().create(ast.Expression);
            expr.* = .{
                .variable = self.previous(),
            };
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

    fn err(self: *Self, tok: token.Token, message: []const u8) !void {
        lox.Lox.parse_error(self.arena.allocator(), tok, message);
        return ParseErrors.ParseError;
    }

    fn synchronize(self: *Self) void {
        _ = self.advance();

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

            _ = self.advance();
        }
    }
};
