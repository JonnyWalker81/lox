const std = @import("std");
const io = std.io;
const Regex = @import("regex").Regex;
const Captures = @import("regex").Captures;

pub const ExpectedOutput = struct {
    const Self = @This();

    line: usize,
    output: []const u8,

    pub fn init(allocator: std.mem.Allocator, line: usize, output: []const u8) !Self {
        const c = try std.fmt.allocPrint(allocator, "{s}", .{output});
        return .{
            .line = line,
            .output = c,
        };
    }
};

pub const Test = struct {
    const Self = @This();

    allocator: std.mem.Allocator,
    path: []const u8,
    baseDir: std.fs.Dir,
    expectedOutput: std.ArrayList(ExpectedOutput),
    expectedErrors: std.ArrayList([]const u8),
    expectedRuntimeError: ?[]const u8,
    runtimeErrorLine: usize = 0,
    expectedExitCode: i32 = 0,
    failures: std.ArrayList([]const u8),
    expectations: usize = 0,

    expectedOutputPattern: Regex,
    expectedErrorPattern: Regex,
    errorLinePattern: Regex,
    expectedRuntimeErrorPattern: Regex,
    syntaxErrorPattern: Regex,
    stackTracePattern: Regex,
    nonTestPattern: Regex,

    pub fn init(allocator: std.mem.Allocator, d: std.fs.Dir, path: []const u8) !Self {
        const expectedOutputPattern = try Regex.compile(allocator,
            \\.*// expect: ?(.*)
        );

        const expectedErrorPattern = try Regex.compile(allocator,
            \\.*// (Error.*)
        );

        const errorLinePattern = try Regex.compile(allocator,
            \\.*// \[((java|c) )?line (\d+)\] (Error.*)
        );

        const expectedRuntimeErrorPattern = try Regex.compile(allocator,
            \\.*// expect runtime error: (.*)
        );

        const syntaxErrorPattern = try Regex.compile(allocator,
            \\.*\[.*line (\d+)\] (Error.+)
        );

        const stackTracePattern = try Regex.compile(allocator,
            \\.*\[line (\d+)\].*
        );

        const nonTestPattern = try Regex.compile(allocator,
            \\.*// nontest
        );

        return .{
            .allocator = allocator,
            .path = path,
            .baseDir = d,
            .expectedOutput = std.ArrayList(ExpectedOutput).init(allocator),
            .expectedErrors = std.ArrayList([]const u8).init(allocator),
            .expectedRuntimeError = null,
            .failures = std.ArrayList([]const u8).init(allocator),
            .expectedOutputPattern = expectedOutputPattern,
            .expectedErrorPattern = expectedErrorPattern,
            .errorLinePattern = errorLinePattern,
            .expectedRuntimeErrorPattern = expectedRuntimeErrorPattern,
            .syntaxErrorPattern = syntaxErrorPattern,
            .stackTracePattern = stackTracePattern,
            .nonTestPattern = nonTestPattern,
        };
    }

    pub fn deinit(self: *Self) void {
        self.expectedOutput.deinit();
        self.expectedErrors.deinit();
        self.failures.deinit();
    }

    pub fn parse(self: *Self) !bool {
        var file = try self.baseDir.openFile(self.path, .{});
        defer file.close();
        var buf_reader = io.bufferedReader(file.reader());
        var in_stream = buf_reader.reader();
        var buf: [1024]u8 = undefined;

        const stdout = std.io.getStdOut().writer();
        var lineNum: usize = 1;
        while (try in_stream.readUntilDelimiterOrEof(&buf, '\n')) |line| {
            defer {
                lineNum += 1;
            }

            if (try self.nonTestPattern.match(line)) {
                try stdout.print("Skipping non-test: '{s}'\n", .{self.path});
                return false;
            }

            if (try self.expectedOutputPattern.captures(line)) |caps| {
                if (caps.sliceAt(1)) |c| {
                    // try stdout.print("Expected output: '{s}'\n", .{c});
                    try self.expectedOutput.append(try ExpectedOutput.init(self.allocator, lineNum, c));
                    self.expectations += 1;
                }
                continue;
            }

            if (try self.expectedErrorPattern.captures(line)) |caps| {
                if (caps.sliceAt(1)) |c| {
                    // try stdout.print("-- found Expected error: '{s}'\n", .{line});
                    const err = try std.fmt.allocPrint(self.allocator, "[{d}] {s}", .{ lineNum, c });
                    try self.expectedErrors.append(err);
                    self.expectedExitCode = 65;
                    self.expectations += 1;
                }
                continue;
            }

            if (try self.errorLinePattern.captures(line)) |caps| {
                if (caps.len() < 3) {
                    continue;
                }
                const language = caps.sliceAt(2) orelse null;
                if (language == null or std.mem.eql(u8, language.?, "c")) {
                    const m3 = caps.sliceAt(3) orelse "";
                    const m4 = caps.sliceAt(4) orelse "";
                    const err = try std.fmt.allocPrint(self.allocator, "[{s}] {s}", .{ m3, m4 });
                    try self.expectedErrors.append(err);
                    self.expectedExitCode = 65;
                    self.expectations += 1;
                }
                continue;
            }

            if (try self.expectedRuntimeErrorPattern.captures(line)) |caps| {
                if (caps.sliceAt(1)) |c| {
                    self.runtimeErrorLine = lineNum;
                    self.expectedRuntimeError = try self.allocator.dupe(u8, c);
                    self.expectedExitCode = 70;
                    self.expectations += 1;
                }
                continue;
            }

            if (self.expectedErrors.items.len > 0 and self.expectedRuntimeError != null) {
                try stdout.print("TEST ERROR", .{});
                try stdout.print("Cannot expect both errors and runtime error: '{s}'\n", .{self.path});
                return false;
            }
        }

        return true;
    }

    pub fn run(self: *Self) !std.ArrayList([]const u8) {
        const fullPath = try std.fmt.allocPrint(self.allocator, "../test/{s}", .{self.path});
        const argv = [_][]const u8{ "./zig-out/bin/clox", fullPath };

        const proc = try std.process.Child.run(.{
            .allocator = self.allocator,
            .argv = &argv,
        });

        // const stdout = std.io.getStdOut().writer();
        // if (proc.stdout.len > 0) {
        //     try stdout.print("{s}\n", .{proc.stdout});
        // }
        // if (proc.stderr.len > 0) {
        //     try stdout.print("output -- {s} --end output\n", .{proc.stderr});
        // }
        var iter = std.mem.split(u8, proc.stdout, "\n");
        var lines = std.ArrayList([]const u8).init(self.allocator);
        defer lines.deinit();
        while (iter.next()) |l| {
            try lines.append(l);
        }

        iter = std.mem.split(u8, proc.stderr, "\n");
        var errorLines = std.ArrayList([]const u8).init(self.allocator);
        defer errorLines.deinit();
        while (iter.next()) |l| {
            if (l.len == 0) {
                continue;
            }
            try errorLines.append(l);
        }

        if (self.expectedRuntimeError) |_| {
            try self.validateRuntimeError(errorLines.items);
        } else {
            try self.validateCompileErrors(errorLines.items);
        }

        const exitCode: i32 = switch (proc.term) {
            .Exited => proc.term.Exited,
            .Signal => @intCast(proc.term.Signal),
            else => 0,
        };

        // const stdout = std.io.getStdOut().writer();
        // try stdout.print("Exit code: {d}\n", .{exitCode});

        try self.validateExitCode(exitCode, errorLines.items);
        try self.validateOutput(lines.items);

        return self.failures;
    }

    pub fn validateOutput(self: *Self, lines: []const []const u8) !void {
        var index: usize = 0;
        const stdout = std.io.getStdOut().writer();
        while (index < lines.len) : (index += 1) {
            const line = lines[index];
            if (line.len == 0 and index == lines.len - 1) {
                continue;
            }

            if (index >= self.expectedOutput.items.len) {
                try self.fail(try std.fmt.allocPrint(self.allocator, "Got output '{d}' when none was expected.", .{line}), null);
                continue;
            }

            const expected = self.expectedOutput.items[index];
            if (!std.mem.eql(u8, expected.output, line)) {
                try stdout.print("Expected: '{s}', Got: '{s}'\n", .{ expected.output, line });
                try self.fail(try std.fmt.allocPrint(self.allocator, "Expected output '{s}' and got '{s}'.", .{ expected.output, line }), null);
            }
        }

        while (index < self.expectedOutput.items.len) {
            const expected = self.expectedOutput.items[index];
            try self.fail(try std.fmt.allocPrint(self.allocator, "Missing expected output '{s}' on line {d}.", .{ expected.output, expected.line }), null);
            index += 1;
        }
    }

    pub fn validateRuntimeError(self: *Self, errorLines: []const []const u8) !void {
        const stdout = std.io.getStdOut().writer();
        if (errorLines.len < 2) {
            try self.fail("Expected runtime error and got none.", null);
            try self.fail(try std.fmt.allocPrint(self.allocator, "Expected runtime error '{s}' and got none.", .{self.expectedRuntimeError.?}), null);
            return;
        }

        if (!std.mem.eql(u8, errorLines[0], self.expectedRuntimeError.?)) {
            try stdout.print("Expected runtime error: '{s}'\n", .{self.expectedRuntimeError.?});
            try self.fail(try std.fmt.allocPrint(self.allocator, "Expected runtime error '{s}' and got:", .{self.expectedRuntimeError.?}), null);
            try self.fail(errorLines[0], null);
        }

        const stackLines = errorLines[1..];
        var captures: ?Captures = null;
        for (stackLines, 0..) |line, i| {
            if (line.len == 0 and i == stackLines.len - 1) {
                continue;
            }
            if (try self.stackTracePattern.captures(line)) |c| {
                captures = c;
                break;
            }
        }

        if (captures) |c| {
            const stackLine = c.sliceAt(1) orelse null;
            if (stackLine) |sl| {
                const line = try std.fmt.parseInt(i32, sl, 10);
                if (line != self.runtimeErrorLine) {
                    try stdout.print("--Expected runtime error on line {d} but was on line {d}\n", .{ self.runtimeErrorLine, line });
                    try self.fail(try std.fmt.allocPrint(self.allocator, "Expected runtime error on line {d} but was on line {d}", .{ self.runtimeErrorLine, line }), null);
                }
            }
        } else {
            try self.fail("Expected stack trace and got:", stackLines);
        }
    }

    pub fn validateCompileErrors(self: *Self, errorLines: []const []const u8) !void {
        // const stdout = std.io.getStdOut().writer();
        var foundErrors = std.ArrayList([]const u8).init(self.allocator);
        var unexpectedCount: usize = 0;

        for (errorLines, 0..) |line, i| {
            if (line.len == 0 and i == errorLines.len - 1) {
                break;
            }

            if (try self.syntaxErrorPattern.captures(line)) |caps| {
                // try stdout.print("Syntax error: '{s}'\n", .{line});
                // try stdout.print("Expected errorr: '{any}'\n", .{self.expectedErrors});
                const l = caps.sliceAt(1) orelse "";
                // std.debug.print("l: '{s}'\n", .{l});
                const e = caps.sliceAt(2) orelse "";
                const err = try std.fmt.allocPrint(self.allocator, "[{s}] {s}", .{ l, e });
                // std.debug.print("err: '{s}'\n", .{err});
                if (contains(self.expectedErrors.items, err)) {
                    // try stdout.print("Found expected error: '{s}'\n", .{err});
                    try foundErrors.append(err);
                } else {
                    if (unexpectedCount < 10) {
                        try self.fail("Unexpected error:", null);
                        try self.fail(line, null);
                    }
                    unexpectedCount += 1;
                }
            } else if (line.len > 0) {
                // try stdout.print("Unexpected error line: '{s}'\n", .{line});
                if (unexpectedCount < 10) {
                    try self.fail("Unexpected output on stderr: ", null);
                    try self.fail(line, null);
                }
                unexpectedCount += 1;
            }
        }

        if (unexpectedCount > 10) {
            try self.fail(try std.fmt.allocPrint(self.allocator, "(truncated {d}i more...)", .{unexpectedCount - 10}), null);
        }

        // std.debug.print("expectedErrors: '{s}'\n", .{self.expectedErrors.items});
        for (self.expectedErrors.items) |expected| {
            if (!contains(foundErrors.items, expected)) {
                try self.fail(try std.fmt.allocPrint(self.allocator, "Missing expected error '{s}'", .{expected}), null);
            }
        }
    }

    pub fn validateExitCode(self: *Self, exitCode: i32, errorLines: []const []const u8) !void {
        if (exitCode == self.expectedExitCode) {
            return;
        }

        try self.fail(try std.fmt.allocPrint(self.allocator, "Expected exit code {d} and got {d}", .{ self.expectedExitCode, exitCode }), errorLines);
    }

    fn fail(self: *Self, message: []const u8, lines: ?[]const []const u8) !void {
        std.debug.print("message: '{s}'\n", .{message});
        try self.failures.append(message);
        if (lines) |lns| {
            for (lns) |line| {
                try self.failures.append(line);
            }
        }
    }

    fn contains(arr: []const []const u8, item: []const u8) bool {
        for (arr) |i| {
            if (std.mem.eql(u8, i, item)) {
                return true;
            }
        }

        return false;
    }
};

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    var arena = std.heap.ArenaAllocator.init(gpa.allocator());
    defer arena.deinit();

    const stdout = std.io.getStdOut().writer();
    const cwd = try std.fs.cwd().realpathAlloc(arena.allocator(), ".");
    try stdout.print("Current directory: '{s}'\n", .{cwd});
    const d = try std.fs.cwd().openDir("../test", .{ .iterate = true });
    const p = try d.realpathAlloc(arena.allocator(), ".");
    try stdout.print("Current directory: '{s}'\n", .{p});
    try stdout.print("Hello, world!\n", .{});

    // on success, we own the output streams
    // defer alloc.free(proc.stdout);
    // defer alloc.free(proc.stderr);

    var count: usize = 0;
    var iter = try d.walk(arena.allocator());
    while (try iter.next()) |entry| {
        if (std.mem.indexOf(u8, entry.path, "benchmark")) |_| {
            continue;
        }

        if (entry.kind == .file) {
            try stdout.print("Entry: '{s}: {s}'\n", .{ entry.basename, entry.path });
            // if (entry.kind == .file) {
            // const file = try d.readFileAlloc(arena.allocator(), entry.path, 1024 * 1024);
            // try stdout.print("File size: {d}\n", .{file.len});
            // std.debug.print("File contents: '{s}'\n", .{file});
            // the command to run
            var t = try Test.init(arena.allocator(), d, entry.path);
            defer t.deinit();

            if (!try t.parse()) {
                continue;
            }

            const failures = try t.run();

            if (failures.items.len > 0) {
                try stdout.print("Test failed: '{s}'\n", .{entry.path});
                for (failures.items) |failure| {
                    try stdout.print("{s}\n", .{failure});
                }
                break;
            } else {
                try stdout.print("Test passed: '{s}'\n", .{entry.path});
            }

            // const fullPath = try std.fmt.allocPrint(arena.allocator(), "../test/{s}", .{entry.path});
            // const argv = [_][]const u8{ "./zig-out/bin/clox", fullPath };

            // const proc = try std.process.Child.run(.{
            //     .allocator = arena.allocator(),
            //     .argv = &argv,
            // });

            // if (proc.stdout.len > 0) {
            //     try stdout.print("{s}\n", .{proc.stdout});
            // } else if (proc.stderr.len > 0) {
            //     try stdout.print("{s}\n", .{proc.stderr});
            // }
            count += 1;
            // if (count == 80) {
            //     break;
            // }
        }
    }

    try stdout.print("Ran {d} tests\n", .{count});
}
