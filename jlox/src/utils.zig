const std = @import("std");

pub fn err(line: u32, message: []const u8) void {
    report(line, "", message);
}

pub fn report(line: u32, where: []const u8, message: []const u8) void {
    std.debug.print("[line {d}] Error {s}: {s}\n", .{ line, where, message });
    const hadError = true;
    _ = hadError;
}
