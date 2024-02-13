const std = @import("std");

const Mode = @import("mode.zig").Mode;
const profile = @import("profile.zig");

pub const ProfileType = enum {
    @"home-manager",
    darwin,
    nixos,
};

pub fn ProfileEnum(comptime pt: ProfileType) type {
    return switch (pt) {
        .@"home-manager" => blk: {
            const system_profiles = [_]profile.ProfileEnum(.system){ .@"linux-systemd", .linux, .wsl };
            var fields: [system_profiles.len * std.meta.fields(profile.ProfileEnum(.user)).len]std.builtin.Type.EnumField = undefined;
            var index = 0;
            for (system_profiles) |system_profile| {
                const system_profile_name = @tagName(system_profile);
                for (std.meta.fieldNames(profile.ProfileEnum(.user))) |user_profile_name| {
                    fields[index] = std.builtin.Type.EnumField{
                        .name = system_profile_name ++ "-" ++ user_profile_name,
                        .value = index,
                    };
                    index += 1;
                }
            }
            break :blk @Type(std.builtin.Type{
                .Enum = std.builtin.Type.Enum{
                    .tag_type = u8,
                    .fields = &fields,
                    .decls = &.{},
                    .is_exhaustive = true,
                },
            });
        },

        .darwin => enum {},

        .nixos => enum {},
    };
}
