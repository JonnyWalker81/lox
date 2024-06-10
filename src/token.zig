const std = @import("std");

pub const TokenType = union(enum) {
    const Self = @This();

    left_paren,
    right_paren,
    left_brace,
    right_brace,
    comma,
    dot,
    minus,
    plus,
    semicolon,
    slash,
    star,
    bang,
    bang_equal,
    equal,
    equal_equal,
    greater,
    greater_equal,
    less,
    less_equal,
    identifier: []const u8,
    string: []const u8,
    number: f64,
    @"and",
    class,
    @"else",
    false,
    fun,
    @"for",
    @"if",
    nil,
    @"or",
    print,
    @"return",
    super,
    this,
    true,
    @"var",
    @"while",
    eof,

    pub fn format(
        self: Self,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;

        switch (self) {
            .left_paren => try writer.print("(", .{}),
            .right_paren => try writer.print(")", .{}),
            .left_brace => try writer.print("{{", .{}),
            .right_brace => try writer.print("}}", .{}),
            .comma => try writer.print(",", .{}),
            .dot => try writer.print(".", .{}),
            .minus => try writer.print("-", .{}),
            .plus => try writer.print("+", .{}),
            .semicolon => try writer.print(";", .{}),
            .slash => try writer.print("/", .{}),
            .star => try writer.print("*", .{}),
            .bang => try writer.print("!", .{}),
            .bang_equal => try writer.print("!=", .{}),
            .equal => try writer.print("=", .{}),
            .equal_equal => try writer.print("==", .{}),
            .greater => try writer.print(">", .{}),
            .greater_equal => try writer.print(">=", .{}),
            .less => try writer.print("<", .{}),
            .less_equal => try writer.print("<=", .{}),
            .identifier => try writer.print("{s}", .{self.identifier}),
            .string => try writer.print("{s}", .{self.string}),
            .number => try writer.print("{d}", .{self.number}),
            .@"and" => try writer.print("and", .{}),
            .class => try writer.print("class", .{}),
            .@"else" => try writer.print("else", .{}),
            .false => try writer.print("false", .{}),
            .fun => try writer.print("fun", .{}),
            .@"for" => try writer.print("for", .{}),
            .@"if" => try writer.print("if", .{}),
            .nil => try writer.print("nil", .{}),
            .@"or" => try writer.print("or", .{}),
            .print => try writer.print("print", .{}),
            .@"return" => try writer.print("return", .{}),
            .super => try writer.print("super", .{}),
            .this => try writer.print("this", .{}),
            .true => try writer.print("true", .{}),
            .@"var" => try writer.print("var", .{}),
            .@"while" => try writer.print("while", .{}),
            .eof => try writer.print("eof", .{}),
        }
    }
};

pub fn keyword(
    name: []const u8,
) ?TokenType {
    const map = std.ComptimeStringMap(TokenType, .{
        .{ "and", .@"and" },
        .{ "class", .class },
        .{ "else", .@"else" },
        .{ "false", .false },
        .{ "true", .true },
        .{ "for", .@"for" },
        .{ "fun", .fun },
        .{ "if", .@"if" },
        .{ "nil", .nil },
        .{ "or", .@"or" },
        .{ "print", .print },
        .{ "return", .@"return" },
        .{ "super", .super },
        .{ "this", .this },
        .{ "var", .@"var" },
        .{ "while", .@"while" },
    });
    return map.get(name);
}

pub const Token = struct {
    const Self = @This();

    typ: TokenType,
    line: usize,

    pub fn init(typ: TokenType, line: usize) Token {
        return Token{
            .typ = typ,
            .line = line,
        };
    }

    pub fn format(
        self: Self,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;

        try writer.print("token {s}: line - {d}", .{ self.typ, self.line });
        // try writer.print("{s}", .{self.typ});
    }

    pub fn toString(self: *const Self, allocator: std.mem.Allocator) []const u8 {
        return std.fmt.allocPrint(allocator, "{s}", .{self.typ}) catch unreachable;
    }
};
