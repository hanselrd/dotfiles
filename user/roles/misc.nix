{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.roles.user.misc;
in
{
  options = {
    roles.user.misc = {
      enable = lib.mkEnableOption "roles.user.misc";
    };
  };

  config = lib.mkIf cfg.enable {
    home.packages =
      with pkgs;
      lib.flatten [
        (lib.optionals
          (
            lib.profiles.isUserBase
            || lib.profiles.isUserMinimal
            || lib.profiles.isUserStandard
            || lib.profiles.isUserFull
          )
          [
            age
            coreutils
            cpio
            curl
            diffutils
            dmidecode
            file
            findutils
            gawk
            gnugrep
            gnused
            gnutar
            gzip
            hexxy
            killall
            lm_sensors
            lsb-release
            miller
            pfetch
            pqrs
            procps
            rsync
            ssh-to-age
            strace
            sysstat
            tree
            unzip
            wget
            xan
            xar
            xxd
            zstd
          ]
        )
        (lib.optionals
          (lib.profiles.isUserMinimal || lib.profiles.isUserStandard || lib.profiles.isUserFull)
          [
            (lib.hiPrio stress)
            hyperfine
            speedtest-cli
            tshark
          ]
        )
        (lib.optionals (lib.profiles.isUserStandard || lib.profiles.isUserFull) [
          # ansible
          # ventoy
          android-tools
          atool
          fd
          ffmpeg
          figlet
          flock
          fortune
          graphviz
          httpie
          imagemagick
          lolcat
          nmap
          restic
          sshfs
          stow
          tlp
          tokei
          udisks
          wireguard-tools
          wtfutil
          yt-dlp
          zlib
        ])
        (lib.optionals lib.profiles.isUserFull [
          # renderdoc
          arandr
          dbeaver-bin
          libreoffice-fresh
          mesa
          mesa-demos
          vulkan-tools
          zoom-us
        ])
      ];
  };
}
