const std = @import("std");
const ast = @import("ast.zig");
const object = @import("object.zig");
const env = @import("environment.zig");

const InterpreterErrors = error{
    UnexpectedExpression,
    UnexpectedStatement,
};

pub const Interpreter = struct {
    const Self = @This();

    arena: std.heap.ArenaAllocator,
    environment: env.Environment,

    pub fn init(allocator: std.mem.Allocator) Interpreter {
        return Interpreter{
            .arena = std.heap.ArenaAllocator.init(allocator),
            .environment = env.Environment.init(allocator),
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
            .variable => |_| {
                return self.evalVariableStatement(stmt);
            },
        }

        return InterpreterErrors.UnexpectedStatement;
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
            else => {
                return InterpreterErrors.UnexpectedExpression;
            },
        }
    }

    fn evalExpressionStatement(self: *Self, stmt: *ast.Statement) !void {
        _ = try self.evaluate(stmt.expressionStatement);
    }

    fn evalPrintStatement(self: *Self, stmt: *ast.Statement) !void {
        const obj = try self.evaluate(stmt.print);
        std.debug.print("{}\n", .{obj});
        std.log.warn("{}\n", .{obj});
    }

    fn evalVariableStatement(self: *Self, stmt: *ast.Statement) !void {
        var value: *object.Object = undefined;
        if (stmt.variable.initializer) |i| {
            value = try self.evaluate(i);
        }

        const name = try std.fmt.allocPrint(self.arena.allocator(), "{}", .{stmt.variable.name.typ});
        try self.environment.define(name, value);
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
