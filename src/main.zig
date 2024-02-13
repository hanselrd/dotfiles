const builtin = @import("builtin");
const cova = @import("cova");
const std = @import("std");

const Cli = @import("cli.zig").Cli;
const profile = @import("profile.zig");
const role = @import("role.zig");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    const allocator = gpa.allocator();

    const x = role.Role{ .system = .bootstrap };
    const y = role.Role{ .user = .bootstrap };
    const xx = profile.Profile{ .system = .nixos, .user = .full };
    const yy = profile.ProfileUnion{ .system = .nixos };

    var string = std.ArrayList(u8).init(allocator);
    defer string.deinit();
    try std.json.stringify(x, .{}, string.writer());
    try std.json.stringify(y, .{}, string.writer());
    try std.json.stringify(xx, .{}, string.writer());
    try std.json.stringify(yy, .{}, string.writer());

    std.debug.print("JSON= {s}\n", .{string.items});

    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    var cli = try Cli.init(alloc);
    defer cli.deinit();

    std.log.info("[START] Processing", .{});

    // if (cli.root_cmd.matchSubCmd("home-manager")) |home_manager_cmd| {
    //     const opts = try home_manager_cmd.getOpts(.{});
    //     const profile_val = opts.get("profile").?.val;
    //     const profile_str = profile_val.getAs([]const u8) catch "opt not set";
    //     std.log.info("Home Manager Cmd: profile={s}", .{profile_str});
    // }

    std.log.info("[END] Processing", .{});
}

test {
    std.testing.refAllDecls(@This());
}
