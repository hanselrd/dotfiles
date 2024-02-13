const builtin = @import("builtin");
const cova = @import("cova");
const std = @import("std");

const profiles = @import("profiles.zig");

const CommandT = cova.Command.Custom(.{
    .global_help_prefix = "Dotfiles CLI",
    .val_config = .{
        .custom_types = &.{
            std.log.Level,
            profiles.ProfileEnum(.@"home-manager"),
        },
    },
});
const OptionT = CommandT.OptionT;
const ValueT = CommandT.ValueT;

const setup_cmd: CommandT = .{
    .name = "dotfiles-cli",
    .description = "Dotfiles CLI",
    .opts = &.{
        OptionT{
            .name = "dryrun",
            .description = "Run without affecting the system.",
            .short_name = 'd',
            .long_name = "dryrun",
            .val = ValueT.ofType(bool, .{
                .name = "dryrun_flag",
            }),
        },
        OptionT{
            .name = "log-level",
            .description = "Log level",
            .short_name = 'l',
            .long_name = "log-level",
            .val = ValueT.ofType(std.log.Level, .{
                .name = "log_level_enum",
                .default_val = switch (builtin.mode) {
                    .Debug => .debug,
                    else => .info,
                },
            }),
        },
    },
    .sub_cmds = &.{
        CommandT{
            .name = "home-manager",
            .description = "Home Manager",
            .opts = &.{
                OptionT{
                    .name = "profile",
                    .description = "Home Manager profile",
                    .short_name = 'p',
                    .long_name = "profile",
                    .val = ValueT.ofType(profiles.ProfileEnum(.@"home-manager"), .{
                        .name = "profile_enum",
                        .default_val = .@"linux-base",
                    }),
                },
            },
            .sub_cmds = &.{
                CommandT{
                    .name = "bootstrap",
                    .description = "Bootstrap",
                },
                CommandT{
                    .name = "eject",
                    .description = "Eject",
                },
            },
        },
        CommandT{
            .name = "homeage",
            .description = "Homeage",
            .sub_cmds = &.{
                CommandT{
                    .name = "key",
                    .description = "Key",
                },
                CommandT{
                    .name = "secret",
                    .description = "Secret",
                },
            },
        },
        CommandT{
            .name = "environment",
            .description = "Environment",
        },
        CommandT{
            .name = "template",
            .description = "Template",
        },
        CommandT{
            .name = "docker-compose",
            .description = "Docker Compose",
        },
    },
};

pub const Cli = struct {
    allocator: std.mem.Allocator,
    root_cmd: *const CommandT,

    const Self = @This();

    pub fn init(allocator: std.mem.Allocator) !Self {
        const root_cmd = try allocator.create(CommandT);
        errdefer allocator.destroy(root_cmd);

        root_cmd.* = try setup_cmd.init(allocator, .{});
        errdefer root_cmd.deinit();

        var args_iter = try cova.ArgIteratorGeneric.init(allocator);
        defer args_iter.deinit();

        const stderr = std.io.getStdErr().writer();

        cova.parseArgs(&args_iter, CommandT, root_cmd, stderr, .{}) catch |err| switch (err) {
            error.UsageHelpCalled,
            error.TooManyValues,
            error.UnrecognizedArgument,
            error.UnexpectedArgument,
            error.CouldNotParseOption,
            => {},
            else => return err,
        };

        if (builtin.mode == .Debug) {
            try cova.utils.displayCmdInfo(CommandT, root_cmd, allocator, stderr);
        }

        return Self{
            .allocator = allocator,
            .root_cmd = root_cmd,
        };
    }

    pub fn deinit(self: *Self) void {
        self.root_cmd.deinit();
        self.allocator.destroy(self.root_cmd);
    }
};
