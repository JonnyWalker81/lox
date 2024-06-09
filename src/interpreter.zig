const std = @import("std");
const ast = @import("ast.zig");
const object = @import("object.zig");
const env = @import("environment.zig");
const lox = @import("lox.zig");
const callable = @import("callable.zig");

const InterpreterErrors = error{ UnexpectedExpression, UnexpectedStatement, FunctionArityMismatch };

pub const Interpreter = struct {
    const Self = @This();

    arena: std.heap.ArenaAllocator,
    environment: *env.Environment,
    globals: *env.Environment,

    pub fn init(allocator: std.mem.Allocator) Interpreter {
        const globals = env.Environment.init(allocator);
        const clockCallable = allocator.create(callable.Callable) catch unreachable;
        const clock = allocator.create(callable.Clock) catch unreachable;

        clock.* = callable.Clock.init(allocator);

        clockCallable.* = callable.Callable.init(clock);

        const clockObj = allocator.create(object.Object) catch unreachable;
        clockObj.* = .{ .callable = clockCallable };

        globals.define("clock", clockObj) catch unreachable;

        return Interpreter{
            .arena = std.heap.ArenaAllocator.init(allocator),
            .environment = globals,
            .globals = globals,
        };
    }

    pub fn deinit(self: *Self) void {
        self.arena.deinit();
        self.environment.deinit();
    }

    pub fn interpret(self: *Self, statements: []*ast.Statement) !void {
        for (statements) |stmt| {
            try self.execute(stmt);
        }
    }

    fn execute(self: *Self, stmt: *ast.Statement) !void {
        switch (stmt.*) {
            .expressionStatement => |_| {
                return self.evalExpressionStatement(stmt);
            },
            .print => |_| {
                return self.evalPrintStatement(stmt);
            },
            .variable => |v| {
                return self.evalVariableStatement(v);
            },
            .block => |b| {
                const e = env.Environment.initWithEnclosing(
                    self.arena.allocator(),
                    self.environment,
                );
                return self.evalBlock(b, e);
            },
            .ifStmt => |i| {
                return self.evalIfStatement(i);
            },
            .whileStmt => |w| {
                return self.evalWhileStatement(w);
            },
        }

        return InterpreterErrors.UnexpectedStatement;
    }

    fn evalWhileStatement(self: *Self, stmt: *ast.WhileStatement) anyerror!void {
        // std.log.warn("While statement: {any}&[_]*ast.Statement{ initializer.?, bb }&[_]*ast.Statement{ initializer.?, bb }&[_]*ast.Statement{ initializer.?, bb }\n", .{stmt});
        while (isTruthy(try self.evaluate(stmt.condition))) {
            try self.execute(stmt.body);
        }
    }

    fn evalIfStatement(self: *Self, stmt: *ast.IfStatement) !void {
        const condition = try self.evaluate(stmt.condition);
        if (isTruthy(condition)) {
            try self.execute(stmt.thenBranch);
        } else if (stmt.elseBranch) |e| {
            try self.execute(e);
        }
    }

    fn evalBlock(self: *Self, block: []const *ast.Statement, environment: *env.Environment) !void {
        const previous = self.environment;
        defer self.environment = previous;

        // std.log.warn("Statements: {any}", .{block});
        self.environment = environment;
        for (block) |s| {
            try self.execute(s);
        }
    }

    fn evalLiteral(self: *Self, expr: *ast.Expression) !*object.Object {
        return try self.toObject(expr);
    }

    fn evalGrouping(self: *Self, expr: *const ast.GroupingExpr) anyerror!*object.Object {
        return try self.evaluate(expr.expression);
    }

    fn evalUnary(self: *Self, expr: *const ast.UnaryExpr) anyerror!*object.Object {
        const right = try self.evaluate(expr.right);
        const obj = try self.arena.allocator().create(object.Object);
        switch (expr.operator.typ) {
            .minus => {
                switch (right.*) {
                    .number => |n| {
                        obj.* = .{ .number = -n };
                    },
                    else => {
                        return InterpreterErrors.UnexpectedExpression;
                    },
                }
            },
            .bang => {
                obj.* = .{
                    .boolean = !isTruthy(right),
                };
            },
            else => {
                return InterpreterErrors.UnexpectedExpression;
            },
        }

        return obj;
    }

    fn evalBinary(self: *Self, expr: *const ast.BinaryExpr) anyerror!*object.Object {
        const left = try self.evaluate(expr.left);
        const right = try self.evaluate(expr.right);
        const obj = try self.arena.allocator().create(object.Object);
        switch (expr.operator.typ) {
            .plus => {
                if (left.* == .number and right.* == .number) {
                    obj.* = .{ .number = left.numberValue() + right.numberValue() };
                } else if (left.* == .string and right.* == .string) {
                    const str = try std.fmt.allocPrint(self.arena.allocator(), "{s}{s}", .{ left.stringValue(), right.stringValue() });
                    obj.* = .{ .string = str };
                } else {
                    return InterpreterErrors.UnexpectedExpression;
                }
            },
            .minus => {
                if (left.* == .number and right.* == .number) {
                    obj.* = .{ .number = left.numberValue() - right.numberValue() };
                }
            },
            .star => {
                if (left.* == .number and right.* == .number) {
                    obj.* = .{ .number = left.numberValue() * right.numberValue() };
                } else {
                    return InterpreterErrors.UnexpectedExpression;
                }
            },
            .slash => {
                if (left.* == .number and right.* == .number) {
                    obj.* = .{ .number = left.numberValue() / right.numberValue() };
                } else {
                    return InterpreterErrors.UnexpectedExpression;
                }
            },
            .greater => {
                if (left.* == .number and right.* == .number) {
                    obj.* = .{ .boolean = left.numberValue() > right.numberValue() };
                } else {
                    return InterpreterErrors.UnexpectedExpression;
                }
            },
            .greater_equal => {
                if (left.* == .number and right.* == .number) {
                    obj.* = .{ .boolean = left.numberValue() >= right.numberValue() };
                } else {
                    return InterpreterErrors.UnexpectedExpression;
                }
            },
            .less => {
                if (left.* == .number and right.* == .number) {
                    obj.* = .{ .boolean = left.numberValue() < right.numberValue() };
                } else {
                    return InterpreterErrors.UnexpectedExpression;
                }
            },
            .less_equal => {
                if (left.* == .number and right.* == .number) {
                    obj.* = .{ .boolean = left.numberValue() <= right.numberValue() };
                } else {
                    return InterpreterErrors.UnexpectedExpression;
                }
            },
            .equal_equal => {
                obj.* = .{ .boolean = isEqual(left, right) };
            },
            .bang_equal => {
                obj.* = .{ .boolean = !isEqual(left, right) };
            },
            else => {
                return InterpreterErrors.UnexpectedExpression;
            },
        }

        return obj;
    }

    fn evaluate(self: *Self, expr: *ast.Expression) anyerror!*object.Object {
        switch (expr.*) {
            .literal => |l| {
                return try self.evalLiteral(l);
            },
            .grouping => |g| {
                return try self.evalGrouping(&g);
            },
            .unary => |u| {
                return try self.evalUnary(&u);
            },
            .binary => |b| {
                return try self.evalBinary(&b);
            },
            .variable => |v| {
                const name = try std.fmt.allocPrint(self.arena.allocator(), "{}", .{v.typ});
                return self.environment.get(name);
            },
            .assignment => |a| {
                return try self.evalAssign(&a);
            },
            .logical => |l| {
                return try self.evalLogical(&l);
            },
            .call => |c| {
                return try self.evalCall(&c);
            },
            else => {
                std.log.warn("Unexpected expression: {}\n", .{expr});
                std.log.warn("Unexpected expression: {s}\n", .{@tagName(expr.*)});
                return InterpreterErrors.UnexpectedExpression;
            },
        }
    }

    fn evalCall(self: *Self, expr: *const ast.Call) anyerror!*object.Object {
        const callee = try self.evaluate(expr.callee);
        var arguments = std.ArrayList(*object.Object).init(self.arena.allocator());
        for (expr.arguments) |arg| {
            const obj = try self.evaluate(arg);
            try arguments.append(obj);
        }
        const function = callee.callable;
        if (arguments.items.len != function.arity()) {
            lox.Lox.err(expr.paren.line, "Cannot have more than 255 arguments.");
            return InterpreterErrors.FunctionArityMismatch;
        }

        return function.call(self, try arguments.toOwnedSlice());
    }

    fn evalLogical(self: *Self, expr: *const ast.Logical) anyerror!*object.Object {
        const left = try self.evaluate(expr.left);
        if (expr.operator.typ == .@"or") {
            if (isTruthy(left)) {
                return left;
            }
        } else {
            if (!isTruthy(left)) {
                return left;
            }
        }
        return self.evaluate(expr.right);
    }

    fn evalExpressionStatement(self: *Self, stmt: *ast.Statement) !void {
        _ = try self.evaluate(stmt.expressionStatement);
    }

    fn evalPrintStatement(self: *Self, stmt: *ast.Statement) !void {
        // std.log.warn("Evaluating print statement: {}\n", .{stmt});
        const obj = try self.evaluate(stmt.print);
        std.debug.print("{}\n", .{obj});
        // std.log.warn("{}\n", .{obj});
    }

    fn evalVariableStatement(self: *Self, stmt: *ast.Variable) !void {
        var value: *object.Object = undefined;
        if (stmt.initializer) |i| {
            value = try self.evaluate(i);
        }

        const name = try std.fmt.allocPrint(self.arena.allocator(), "{}", .{stmt.name.typ});
        try self.environment.define(name, value);
    }

    fn evalAssign(self: *Self, expr: *const ast.Assignment) !*object.Object {
        const value = try self.evaluate(expr.value);
        const name = try std.fmt.allocPrint(self.arena.allocator(), "{}", .{expr.name.typ});
        try self.environment.assign(name, value);
        return value;
    }

    fn toObject(self: *Self, expr: *ast.Expression) !*object.Object {
        const obj = try self.arena.allocator().create(object.Object);
        switch (expr.*) {
            .number => |n| {
                obj.* = .{ .number = n };
            },
            .string => |s| {
                obj.* = .{ .string = s };
            },
            .boolean => |b| {
                obj.* = .{ .boolean = b };
            },
            else => {
                obj.* = .nil;
            },
        }

        return obj;
    }

    fn isTruthy(obj: *object.Object) bool {
        switch (obj.*) {
            .boolean => |b| {
                return b;
            },
            .nil => {
                return false;
            },
            else => {
                return true;
            },
        }
    }

    fn isEqual(left: *object.Object, right: *object.Object) bool {
        if (left.* == .nil and right.* == .nil) {
            return true;
        }

        if (left.* == .nil) {
            return false;
        }

        if (left.* == .boolean and right.* == .boolean) {
            return left.booleanValue() == right.booleanValue();
        }

        if (left.* == .number and right.* == .number) {
            return left.numberValue() == right.numberValue();
        }

        if (left.* == .string and right.* == .string) {
            return std.mem.eql(u8, left.stringValue(), right.stringValue());
        }

        return false;
    }
};
