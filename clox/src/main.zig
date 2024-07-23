const std = @import("std");
const chunk = @import("chunk.zig");
const build_options = @import("build_options");
const vm = @import("vm.zig");

pub fn main() !void {
    const argLen = argCount();
    var args = std.process.args();
    _ = args.skip();

    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    var arena = std.heap.ArenaAllocator.init(gpa.allocator());
    defer arena.deinit();

    var v = vm.VM.init(gpa.allocator());
    v.setup();
    defer v.deinit();

    if (argLen == 1) {
        try repl(&v);
        return std.process.exit(64);
    } else if (argLen == 2) {
        const script = args.next();
        try runFile(gpa.allocator(), script.?, &v);
    } else {
        std.debug.print("Usage: clox [path]\n", .{});
    }
}

pub fn runFile(allocator: std.mem.Allocator, path: []const u8, v: *vm.VM) !void {
    std.debug.print("Running {s}\n", .{path});
    const file = try std.fs.cwd().openFile(path, .{});
    defer file.close();

    const stat = try file.stat();
    const source = try file.readToEndAlloc(allocator, stat.size);
    defer allocator.free(source);

    _ = v.interpret(source) catch |err| {
        std.debug.print("error: {}\n", .{err});
        std.process.exit(65);
    };
}

fn repl(v: *vm.VM) !void {
    const stdin = std.io.getStdIn().reader();
    var br = std.io.bufferedReader(stdin);
    const stdout = std.io.getStdOut().writer();
    const r = br.reader();

    while (true) {
        try stdout.print("> ", .{});
        var line_buf: [1024]u8 = undefined;
        const line = try r.readUntilDelimiterOrEof(&line_buf, '\n');
        if (line) |input| {
            _ = try v.interpret(input);
        }
        std.debug.print("\n", .{});
    }
}

fn argCount() usize {
    var args = std.process.args();
    var count: usize = 0;

    while (args.next()) |_| {
        count += 1;
    }

    return count;
}
