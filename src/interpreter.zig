const std = @import("std");
const ast = @import("ast.zig");
const object = @import("object.zig");
const env = @import("environment.zig");
const lox = @import("lox.zig");
const callable = @import("callable.zig");
const token = @import("token.zig");

const InterpreterErrors = error{ UnexpectedExpression, UnexpectedStatement, FunctionArityMismatch };

pub const Interpreter = struct {
    const Self = @This();

    arena: std.heap.ArenaAllocator,
    environment: *env.Environment,
    globals: *env.Environment,
    locals: std.AutoHashMap(*ast.Expression, usize),

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
            .locals = std.AutoHashMap(*ast.Expression, usize).init(allocator),
        };
    }

    pub fn deinit(self: *Self) void {
        self.arena.deinit();
        self.environment.deinit();
    }

    pub fn interpret(self: *Self, statements: []*ast.Statement) !void {
        for (statements) |stmt| {
            _ = try self.execute(stmt);
        }
    }

    fn execute(self: *Self, stmt: *ast.Statement) anyerror!*object.Object {
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

                // const ReturnValue = struct {
                //     value: ?*object.Object = null,
                // };

                // var payload = ReturnValue{ .value = null };
                // var opts = .{ .error_payload = &payload };

                const v = self.executeBlock(b, e);

                return v;
            },
            .ifStmt => |i| {
                return self.evalIfStatement(i);
            },
            .whileStmt => |w| {
                return self.evalWhileStatement(w);
            },
            .function => |f| {
                return self.evalFunction(f);
            },
            .returnStmt => |r| {
                return self.evalReturn(r);
            },
        }
    }

    pub fn resolve(self: *Self, expr: *ast.Expression, depth: usize) !void {
        try self.locals.put(expr, depth);
    }

    fn evalReturn(self: *Self, r: *ast.ReturnStatement) anyerror!*object.Object {
        // std.debug.print("Evaluating return statement: {}\n", .{r});
        if (r.expr) |v| {
            const stmt = try self.arena.allocator().create(object.Object);
            const obj = try self.evaluate(v);

            stmt.* = .{ .returnValue = obj };
            return stmt;
        } else {
            return self.nullObj();
        }
    }

    fn evalFunction(self: *Self, f: *ast.FunctionStatement) anyerror!*object.Object {
        const funcCallable = try self.arena.allocator().create(callable.Callable);
        const func = try self.arena.allocator().create(callable.LoxFunction);

        func.* = callable.LoxFunction.init(self.arena.allocator(), f, self.environment);

        funcCallable.* = callable.Callable.init(func);

        const funcObj = try self.arena.allocator().create(object.Object);
        funcObj.* = .{ .callable = funcCallable };

        const funcName = try std.fmt.allocPrint(self.arena.allocator(), "{s}", .{f.name.typ});
        try self.environment.define(funcName, funcObj);

        return self.nullObj();
    }

    fn evalWhileStatement(self: *Self, stmt: *ast.WhileStatement) anyerror!*object.Object {
        // std.log.warn("While statement: {any}&[_]*ast.Statement{ initializer.?, bb }&[_]*ast.Statement{ initializer.?, bb }&[_]*ast.Statement{ initializer.?, bb }\n", .{stmt});
        while (isTruthy(try self.evaluate(stmt.condition))) {
            const obj = try self.execute(stmt.body);
            if (obj.* == .returnValue) {
                return obj;
            }
        }

        return self.nullObj();
    }

    fn evalIfStatement(self: *Self, stmt: *ast.IfStatement) !*object.Object {
        const condition = try self.evaluate(stmt.condition);
        if (isTruthy(condition)) {
            const obj = try self.execute(stmt.thenBranch);
            if (obj.* == .returnValue) {
                // std.debug.print("Returning from if statement: {any}\n", .{obj});
                return obj;
            }
        } else if (stmt.elseBranch) |e| {
            const obj = try self.execute(e);
            if (obj.* == .returnValue) {
                return obj;
            }
        }

        return self.nullObj();
    }

    pub fn executeBlock(self: *Self, block: []const *ast.Statement, environment: *env.Environment) anyerror!*object.Object {
        const previous = self.environment;
        defer self.environment = previous;

        // std.log.warn("Statements: {any}", .{block});
        self.environment = environment;
        for (block) |s| {
            const obj = try self.execute(s);
            if (obj.* == .returnValue) {
                return obj;
            }
        }

        return self.nullObj();
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
                if (left.isNumber() and right.isNumber()) {
                    obj.* = .{ .number = left.numberValue() + right.numberValue() };
                } else if (left.isString() and right.isString()) {
                    const str = try std.fmt.allocPrint(self.arena.allocator(), "{s}{s}", .{ left.stringValue(), right.stringValue() });
                    obj.* = .{ .string = str };
                } else {
                    // std.debug.print("plus...{s}, {s} => {s} {}\n", .{ left, right, @tagName(left.*), right.numberValue() });
                    return InterpreterErrors.UnexpectedExpression;
                }
            },
            .minus => {
                if (left.isNumber() and right.isNumber()) {
                    obj.* = .{ .number = left.numberValue() - right.numberValue() };
                }
            },
            .star => {
                if (left.isNumber() and right.isNumber()) {
                    obj.* = .{ .number = left.numberValue() * right.numberValue() };
                } else {
                    return InterpreterErrors.UnexpectedExpression;
                }
            },
            .slash => {
                if (left.isNumber() and right.isNumber()) {
                    obj.* = .{ .number = left.numberValue() / right.numberValue() };
                } else {
                    return InterpreterErrors.UnexpectedExpression;
                }
            },
            .greater => {
                if (left.isNumber() and right.isNumber()) {
                    obj.* = .{ .boolean = left.numberValue() > right.numberValue() };
                } else {
                    return InterpreterErrors.UnexpectedExpression;
                }
            },
            .greater_equal => {
                if (left.isNumber() and right.isNumber()) {
                    obj.* = .{ .boolean = left.numberValue() >= right.numberValue() };
                } else {
                    return InterpreterErrors.UnexpectedExpression;
                }
            },
            .less => {
                if (left.isNumber() and right.isNumber()) {
                    obj.* = .{ .boolean = left.numberValue() < right.numberValue() };
                } else {
                    return InterpreterErrors.UnexpectedExpression;
                }
            },
            .less_equal => {
                if (left.isNumber() and right.isNumber()) {
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
                // const name = try std.fmt.allocPrint(self.arena.allocator(), "{}", .{v.typ});
                // std.debug.print("Evaluating variable: {s}\n", .{name});
                // const obj = self.environment.get(name);
                // std.debug.print("Variable object: {any}\n", .{obj});
                return self.lookupVariable(v, expr);
            },
            .assignment => |a| {
                return try self.evalAssign(expr, &a);
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

    fn lookupVariable(self: *Self, name: token.Token, expr: *ast.Expression) !*object.Object {
        const depth = self.locals.get(expr);
        const varName = try std.fmt.allocPrint(self.arena.allocator(), "{}", .{name.typ});
        if (depth) |d| {
            return self.environment.getAt(d, varName);
        } else {
            return self.globals.get(varName);
        }
    }

    fn evalCall(self: *Self, expr: *const ast.Call) anyerror!*object.Object {
        // std.debug.print("Evaluating call: {any}\n", .{expr});
        const callee = try self.evaluate(expr.callee);
        var arguments = std.ArrayList(*object.Object).init(self.arena.allocator());
        for (expr.arguments) |arg| {
            const obj = try self.evaluate(arg);
            try arguments.append(obj);
        }
        const function = callee.callableValue();
        if (arguments.items.len != function.arity()) {
            lox.Lox.err(expr.paren.line, "Cannot have more than 255 arguments.");
            return InterpreterErrors.FunctionArityMismatch;
        }

        if (try function.call(self, try arguments.toOwnedSlice())) |o| {
            // std.debug.print("Function call returned: {any}\n", .{o});
            return o;
        } else {
            const nil = try self.arena.allocator().create(object.Object);
            nil.* = .nil;
            return nil;
        }
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

    fn evalExpressionStatement(self: *Self, stmt: *ast.Statement) !*object.Object {
        _ = try self.evaluate(stmt.expressionStatement);
        return self.nullObj();
    }

    fn evalPrintStatement(self: *Self, stmt: *ast.Statement) !*object.Object {
        // std.log.warn("Evaluating print statement: {}\n", .{stmt});
        const obj = try self.evaluate(stmt.print);
        std.debug.print("{}\n", .{obj});
        // std.log.warn("{}\n", .{obj});
        return self.nullObj();
    }

    fn evalVariableStatement(self: *Self, stmt: *ast.Variable) !*object.Object {
        var value: *object.Object = undefined;
        if (stmt.initializer) |i| {
            value = try self.evaluate(i);
        }

        const name = try std.fmt.allocPrint(self.arena.allocator(), "{}", .{stmt.name.typ});
        try self.environment.define(name, value);
        return self.nullObj();
    }

    fn evalAssign(self: *Self, expr: *ast.Expression, a: *const ast.Assignment) !*object.Object {
        const value = try self.evaluate(a.value);
        const name = try std.fmt.allocPrint(self.arena.allocator(), "{}", .{a.name.typ});
        const distance = self.locals.get(expr);
        if (distance) |d| {
            try self.environment.assignAt(d, name, value);
        } else {
            try self.globals.assign(name, value);
        }

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

        if (left.isBoolean() and right.isBoolean()) {
            return left.booleanValue() == right.booleanValue();
        }

        if (left.isNumber() and right.isNumber()) {
            return left.numberValue() == right.numberValue();
        }

        if (left.isString() and right.isString()) {
            return std.mem.eql(u8, left.stringValue(), right.stringValue());
        }

        return false;
    }

    fn nullObj(self: *Self) !*object.Object {
        const nil = try self.arena.allocator().create(object.Object);
        nil.* = .nil;
        return nil;
    }
};
