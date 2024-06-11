const std = @import("std");
const token = @import("token.zig");

pub const BinaryExpr = struct {
    left: *Expression,
    operator: token.Token,
    right: *Expression,
};

pub const UnaryExpr = struct {
    operator: token.Token,
    right: *Expression,
};

pub const GroupingExpr = struct {
    expression: *Expression,
};

pub const Assignment = struct {
    name: token.Token,
    value: *Expression,
};

pub const Logical = struct {
    left: *Expression,
    operator: token.Token,
    right: *Expression,
};

pub const Call = struct {
    callee: *Expression,
    paren: token.Token,
    arguments: []const *Expression,
};

pub const Function = struct {
    name: token.Token,
    parameters: []const token.Token,
    body: []const *Statement,
};

pub const GetExpr = struct {
    object: *Expression,
    name: token.Token,
};

pub const SetExpr = struct {
    object: *Expression,
    name: token.Token,
    value: *Expression,
};

pub const Expression = union(enum) {
    const Self = @This();

    assignment: Assignment,
    variable: token.Token,
    literal: *Expression,
    binary: BinaryExpr,
    unary: UnaryExpr,
    grouping: GroupingExpr,
    string: []const u8,
    number: f64,
    boolean: bool,
    logical: Logical,
    call: Call,
    function: Function,
    get: GetExpr,
    set: SetExpr,
    nil: void,

    pub fn format(
        self: Self,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;

        switch (self) {
            .assignment => |a| {
                try writer.print("{s} = {s}", .{ a.name, a.value });
            },
            .variable => |i| {
                try writer.print("{s}", .{i.typ});
            },
            .literal => |l| {
                try writer.print("{s}", .{l});
            },
            .binary => |b| {
                try parenthesize(writer, b.operator.typ, .{ b.left, b.right });
            },
            .unary => |u| {
                try parenthesize(writer, u.operator.typ, .{u.right});
            },
            .grouping => |g| {
                try parenthesize(writer, "group", .{g.expression});
            },
            .string => |s| {
                try writer.print("{s}", .{s});
            },
            .number => |n| {
                try writer.print("{d}", .{n});
            },
            .boolean => |b| {
                try writer.print("{s}", .{if (b) "true" else "false"});
            },
            .logical => |l| {
                try parenthesize(writer, l.operator.typ, .{ l.left, l.right });
            },
            .call => |c| {
                try parenthesize(writer, c.callee, .{c.arguments});
            },
            .function => |f| {
                try writer.print("fn {s}(", .{f.name});
                for (f.parameters, 0..) |p, i| {
                    if (i != 0) {
                        try writer.print(", ", .{});
                    }
                    try writer.print("{s}", .{p});
                }
                try writer.print(") {any}", .{f.body});
            },
            .get => |g| {
                try writer.print("{s}.{s}", .{ g.object, g.name });
            },
            .set => |s| {
                try writer.print("{s}.{s} = {s}", .{ s.object, s.name, s.value });
            },
            .nil => {},
        }
    }

    fn parenthesize(
        writer: anytype,
        name: anytype,
        expressions: anytype,
    ) !void {
        // _ = expressions;
        try writer.print("({s}", .{name});
        inline for (expressions) |e| {
            try writer.print(" ", .{});
            try writer.print("{s}", .{e});
        }
        try writer.print(")", .{});
    }
};

pub const Variable = struct {
    name: token.Token,
    initializer: ?*Expression,
};

pub const IfStatement = struct {
    condition: *Expression,
    thenBranch: *Statement,
    elseBranch: ?*Statement,
};

pub const WhileStatement = struct {
    condition: *Expression,
    body: *Statement,
};

pub const FunctionStatement = struct {
    name: token.Token,
    parameters: []const token.Token,
    body: []const *Statement,
};

pub const ReturnStatement = struct {
    keyword: token.Token,
    expr: ?*Expression,
};

pub const ClassStatement = struct {
    name: token.Token,
    methods: []const *FunctionStatement,
};

pub const Statement = union(enum) {
    expressionStatement: *Expression,
    print: *Expression,
    variable: *Variable,
    block: []const *Statement,
    ifStmt: *IfStatement,
    whileStmt: *WhileStatement,
    function: *FunctionStatement,
    returnStmt: *ReturnStatement,
    classStmt: *ClassStatement,
};

const test_allocator = std.testing.allocator;
test "test pretty print" {
    const expr: *Expression = try test_allocator.create(Expression);
    defer test_allocator.destroy(expr);

    const unary: *Expression = try test_allocator.create(Expression);
    defer test_allocator.destroy(unary);

    const literal: *Expression = try test_allocator.create(Expression);
    defer test_allocator.destroy(literal);

    literal.* = .{ .number = 123 };
    unary.* = .{
        .unary = .{
            .operator = .{ .typ = token.TokenType.minus, .line = 1 },
            .right = literal,
        },
    };
    const literalF: *Expression = try test_allocator.create(Expression);
    defer test_allocator.destroy(literalF);
    literalF.* = .{ .number = 45.67 };

    const right: *Expression = try test_allocator.create(Expression);
    defer test_allocator.destroy(right);
    right.* = .{ .literal = literalF };

    const group: *Expression = try test_allocator.create(Expression);
    defer test_allocator.destroy(group);
    group.* = .{ .grouping = .{ .expression = right } };

    expr.* = .{
        .binary = .{
            .left = unary,
            .operator = .{ .typ = token.TokenType.star, .line = 1 },
            .right = group,
        },
    };

    const expected = "(* (- 123) (group 45.67))";
    std.log.warn("{s}", .{expected});

    const actual = try std.fmt.allocPrint(test_allocator, "{s}", .{expr});
    std.log.warn("{s}", .{actual});
    defer test_allocator.free(actual);
    try std.testing.expectEqualSlices(u8, expected, actual);
}
