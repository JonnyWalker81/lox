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

pub const Expression = union(enum) {
    const Self = @This();

    variable: token.Token,
    literal: *Expression,
    binary: BinaryExpr,
    unary: UnaryExpr,
    grouping: GroupingExpr,
    string: []const u8,
    number: f64,
    boolean: bool,
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
            .variable => |i| {
                try writer.print("{s}", .{i});
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
    initializer: *Expression,
};

pub const Statement = union(enum) {
    expressionStatement: *Expression,
    print: *Expression,
    variable: *Variable,
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
