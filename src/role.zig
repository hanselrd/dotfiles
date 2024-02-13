const std = @import("std");

const Mode = @import("mode.zig").Mode;

pub const Role = union(Mode) {
    system: RoleEnum(.system),
    user: RoleEnum(.user),

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

pub fn RoleEnum(comptime m: Mode) type {
    return switch (m) {
        .system => enum {
            bootstrap,
            boot,
            i18n,
            kernel,
            monitoring,
            motd,
            networking,
            nix,
            openssh,
            shell,
            time,
            user,
            virtualization,
            x11,
        },

        .user => enum {
            bootstrap,
            alacritty,
            bash,
            bat,
            browser,
            development,
            @"development.cpp",
            @"development.dhall",
            @"development.elixir",
            @"development.go",
            @"development.haskell",
            @"development.java",
            @"development.lua",
            @"development.nickel",
            @"development.nix",
            @"development.nodejs",
            @"development.purescript",
            @"development.python",
            @"development.rust",
            @"development.shell",
            @"development.zig",
            docker,
            editor,
            eza,
            fzf,
            git,
            homeage,
            htop,
            neovim,
            nix,
            pager,
            ripgrep,
            scripts,
            shell,
            ssh,
            starship,
            terminal,
            theme,
            tmux,
            vscode,
            zsh,
            zzz,
        },
    };
}
