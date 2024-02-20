const std = @import("std");

// TODO: add stdin support
pub fn shell(cmd: []const u8, allocator: std.mem.Allocator) !std.process.Child.RunResult {
    const proc = try std.process.Child.run(.{
        .allocator = allocator,
        .argv = &[_][]const u8{ "bash", "--norc", "--noprofile", "-c", cmd },
        .cwd = "/tmp",
    });

    errdefer {
        allocator.free(proc.stdout);
        allocator.free(proc.stderr);
    }

    return proc;
}
