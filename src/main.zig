const std = @import("std");
const lox = @import("lox.zig");

pub fn main() !void {
    const argLen = argCount();
    var args = std.process.args();
    _ = args.skip();

    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    var arena = std.heap.ArenaAllocator.init(gpa.allocator());
    defer arena.deinit();

    if (argLen > 2) {
        std.debug.print("Usage: jlox [script]", .{});
        return std.process.exit(64);
    } else if (argLen == 2) {
        const script = args.next();
        try lox.Lox.runFile(arena.allocator(), script.?);
    } else {
        try lox.Lox.runPrompt(arena.allocator());
    }

    // Prints to stderr (it's a shortcut based on `std.io.getStdErr()`)main
    // std.debug.print("All your {s} are belong to us.\n", .{"codebase"});

    // stdout is for the actual output of your application, for example if you
    // are implementing gzip, then only the compressed bytes should be sent to
    // stdout, not any debugging messages.
    // const stdout_file = std.io.getStdOut().writer();
    // var bw = std.io.bufferedWriter(stdout_file);
    // const stdout = bw.writer();

    // try stdout.print("Run `zig build test` to run the tests.\n", .{});

    // try bw.flush(); // don't forget to flush!
}

fn argCount() usize {
    var args = std.process.args();
    var count: usize = 0;

    while (args.next()) |_| {
        count += 1;
    }

    return count;
}
