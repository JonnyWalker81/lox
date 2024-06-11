const std = @import("std");
const interpreter = @import("interpreter.zig");
const ast = @import("ast.zig");
const lox = @import("lox.zig");
const token = @import("token.zig");

const FunctionType = enum {
    none,
    function,
    // INITIALIZER,
    // METHOD,
};

pub const Resolver = struct {
    const Self = @This();

    allocator: std.mem.Allocator,
    interpreter: *interpreter.Interpreter,
    scopes: std.ArrayList(std.StringHashMap(bool)),
    currentFunction: FunctionType = .none,

    pub fn init(allocator: std.mem.Allocator, i: *interpreter.Interpreter) Self {
        return Self{
            .allocator = allocator,
            .interpreter = i,
            .scopes = std.ArrayList(std.StringHashMap(bool)).init(allocator),
        };
    }

    pub fn deinit(self: *Self) void {
        self.scopes.deinit();
    }

    pub fn resolveStatements(self: *Self, stmts: []const *ast.Statement) anyerror!void {
        for (stmts) |s| {
            try self.resolveStatement(s);
        }
    }

    fn resolveStatement(self: *Self, stmt: *const ast.Statement) anyerror!void {
        switch (stmt.*) {
            .variable => |v| {
                try self.resolveVariableStmt(v);
            },
            .function => |f| {
                try self.resolveFunction(f, .function);
            },
            .expressionStatement => |e| {
                try self.resolveExpression(e);
            },
            .ifStmt => |i| {
                try self.resolveIf(i);
            },
            .print => |p| {
                try self.resolvePrint(p);
            },
            .returnStmt => |r| {
                try self.resolveReturn(r);
            },
            .whileStmt => |w| {
                try self.resolveWhile(w);
            },
            .block => |b| {
                try self.resolveBlock(b);
            },
            .classStmt => |c| {
                try self.resolveClassStmt(c);
            },
        }
    }

    fn resolveExpression(self: *Self, expr: *ast.Expression) anyerror!void {
        switch (expr.*) {
            .variable => |v| {
                try self.resolveVariableExpr(expr, v);
            },
            .assignment => |a| {
                try self.resolveAssignment(expr, &a);
            },
            .binary => |b| {
                try self.resolveBinaryExpr(&b);
            },
            .unary => |u| {
                try self.resolveUnaryExpr(&u);
            },
            .grouping => |g| {
                try self.resolveGroupingExpr(&g);
            },
            .call => |c| {
                try self.resolveCallExpr(&c);
            },
            .logical => |l| {
                try self.resolveLogicalExpr(&l);
            },
            else => {},
        }
    }

    fn resolveClassStmt(self: *Self, c: *ast.ClassStatement) !void {
        try self.declare(c.name);
        try self.define(c.name);

        // const enclosingClass = self.currentClass;
        // self.currentClass = .class;
        // try self.beginScope();
        // try self.scopes.items[self.scopes.items.len - 1].put("this", true);

        // for (c.methods) |m| {
        //     var declaration = .method;
        //     if (m.name.lexeme == "init") {
        //         declaration = .initializer;
        //     }
        //     try self.resolveFunction(m, declaration);
        // }

        // try self.endScope();
        // self.currentClass = enclosingClass;
    }

    fn resolveBlock(self: *Self, block: []const *ast.Statement) !void {
        try self.beginScope();
        try self.resolveStatements(block);
        try self.endScope();
    }

    fn resolveFunction(self: *Self, f: *ast.FunctionStatement, typ: FunctionType) !void {
        try self.declare(f.name);
        try self.define(f.name);

        const enclosingFunction = self.currentFunction;
        self.currentFunction = typ;
        try self.beginScope();
        for (f.parameters) |p| {
            try self.declare(p);
            try self.define(p);
        }
        try self.resolveStatements(f.body);
        try self.endScope();
        self.currentFunction = enclosingFunction;
    }

    fn resolveIf(self: *Self, i: *ast.IfStatement) !void {
        try self.resolveExpression(i.condition);
        try self.resolveStatement(i.thenBranch);
        if (i.elseBranch) |e| {
            try self.resolveStatement(e);
        }
    }

    fn resolvePrint(self: *Self, p: *ast.Expression) !void {
        try self.resolveExpression(p);
    }

    fn resolveReturn(self: *Self, r: *ast.ReturnStatement) !void {
        if (self.currentFunction == .none) {
            lox.Lox.err(r.keyword.line, "Cannot return from top-level code.");
        }

        if (r.expr) |v| {
            try self.resolveExpression(v);
        }
    }

    fn resolveWhile(self: *Self, w: *ast.WhileStatement) !void {
        try self.resolveExpression(w.condition);
        try self.resolveStatement(w.body);
    }

    fn resolveVariableStmt(self: *Self, v: *ast.Variable) !void {
        try self.declare(v.name);
        if (v.initializer) |i| {
            try self.resolveExpression(i);
        }
        try self.define(v.name);
    }

    fn resolveAssignment(self: *Self, expr: *ast.Expression, a: *const ast.Assignment) !void {
        try self.resolveExpression(a.value);
        try self.resolveLocal(expr, a.name);
    }

    fn resolveVariableExpr(self: *Self, expr: *ast.Expression, v: token.Token) !void {
        if (self.scopes.items.len > 0) {
            const scope = &self.scopes.items[self.scopes.items.len - 1];
            if (scope.get(v.toString(self.allocator))) |b| {
                if (!b) {
                    lox.Lox.err(v.line, "Cannot read local variable in its own initializer.");
                }
            }
        }

        try self.resolveLocal(expr, v);
    }

    fn resolveBinaryExpr(self: *Self, b: *const ast.BinaryExpr) !void {
        try self.resolveExpression(b.left);
        try self.resolveExpression(b.right);
    }

    fn resolveCallExpr(self: *Self, c: *const ast.Call) !void {
        try self.resolveExpression(c.callee);
        for (c.arguments) |a| {
            try self.resolveExpression(a);
        }
    }

    fn resolveGroupingExpr(self: *Self, g: *const ast.GroupingExpr) !void {
        try self.resolveExpression(g.expression);
    }

    fn resolveLogicalExpr(self: *Self, l: *const ast.Logical) !void {
        try self.resolveExpression(l.left);
        try self.resolveExpression(l.right);
    }

    fn resolveUnaryExpr(self: *Self, u: *const ast.UnaryExpr) !void {
        try self.resolveExpression(u.right);
    }

    fn resolveLocal(self: *Self, expr: *ast.Expression, name: token.Token) !void {
        if (self.scopes.items.len == 0) {
            return;
        }

        var i: i64 = @intCast(self.scopes.items.len - 1);
        while (i >= 0) : (i -= 1) {
            const idx: usize = @intCast(i);
            if (self.scopes.items[idx].contains(name.toString(self.allocator))) {
                try self.interpreter.resolve(expr, self.scopes.items.len - 1 - idx);
                return;
            }
        }
    }

    fn beginScope(self: *Self) !void {
        try self.scopes.append(std.StringHashMap(bool).init(self.allocator));
    }

    fn endScope(self: *Self) !void {
        _ = self.scopes.pop();
    }

    fn declare(self: *Self, name: token.Token) !void {
        if (self.scopes.items.len == 0) {
            return;
        }

        const nameStr = name.toString(self.allocator);
        var scope = &self.scopes.items[self.scopes.items.len - 1];
        if (scope.contains(nameStr)) {
            return lox.Lox.err(name.line, "Variable with this name already declared in this scope.");
        }
        try scope.put(nameStr, false);
    }

    fn define(self: *Self, name: token.Token) !void {
        if (self.scopes.items.len == 0) {
            return;
        }

        const nameStr = name.toString(self.allocator);
        var scope = &self.scopes.items[self.scopes.items.len - 1];
        try scope.put(nameStr, true);
    }
};
