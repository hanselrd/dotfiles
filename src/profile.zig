const std = @import("std");

const Mode = @import("mode.zig").Mode;

pub const Profile = struct {
    system: ProfileEnum(.system),
    user: ProfileEnum(.user),

    const Self = @This();

    pub fn name(self: Self, allocator: std.mem.Allocator) ![]const u8 {
        return std.fmt.allocPrint(allocator, "{s}-{s}", .{ @tagName(self.system), @tagName(self.user) });
    }

    pub fn jsonStringify(self: Self, out: anytype) !void {
        var buf: [50]u8 = undefined;
        var fba = std.heap.FixedBufferAllocator.init(&buf);
        return out.write(try self.name(fba.allocator()));
    }
};

pub const ProfileUnion = union(Mode) {
    system: ProfileEnum(.system),
    user: ProfileEnum(.user),

    const Self = @This();

    pub fn name(self: Self) []const u8 {
        return switch (self) {
            inline else => |p| @tagName(p),
        };
    }

    pub fn jsonStringify(self: Self, out: anytype) !void {
        return out.write(self.name());
    }
};

pub fn ProfileEnum(comptime m: Mode) type {
    return switch (m) {
        .system => enum {
            nixos,
            macos,
            @"linux-systemd",
            linux,
            wsl,
        },

        .user => enum {
            base,
            standard,
            minimal,
            full,
        },
    };
}
