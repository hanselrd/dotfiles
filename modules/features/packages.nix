{ self, lib, ... }:
let
  packages = rec {
    base =
      pkgs: with pkgs; [
        age
        agenix
        coreutils
        cpio
        curl
        diffutils
        dmidecode
        file
        findutils
        gawk
        gnugrep
        gnumake
        gnupatch
        gnused
        gnutar
        gzip
        hexxy
        jq
        killall
        less
        lm_sensors
        lsb-release
        man-db
        ncdu
        nix
        nix-diff
        objconv
        pfetch
        pkg-config
        procps
        rsync
        ssh-to-age
        strace
        sysstat
        tree
        tzdata
        universal-ctags
        unzip
        wget
        which
        xxd
        zlib
        zstd
      ];
    standard =
      pkgs:
      with pkgs;
      (base pkgs)
      ++ [
        # ventoy
        (lib.hiPrio stress)
        android-tools
        ansible
        atool
        cowsay
        fd
        ffmpeg
        figlet
        flock
        fortune
        graphviz
        httpie
        hyperfine
        imagemagick
        lolcat
        nmap
        restic
        speedtest-cli
        sshfs
        stow
        tlp
        tokei
        tshark
        udisks
        wireguard-tools
        yt-dlp
      ];
    all =
      pkgs:
      with pkgs;
      (standard pkgs)
      ++ [
        arandr
        bottles
        brave
        dbeaver-bin
        heroic
        libreoffice-fresh
        lutris
        mesa
        mesa-demos
        protonup-qt
        renderdoc
        vulkan-tools
        wineWowPackages.stableFull
        winetricks
        zoom-us
      ];
  };
in
{
  modules = self.lib.eachModule (
    module:
    lib.mapAttrs' (
      name: value:
      lib.nameValuePair "${name}-package" (
        { pkgs, ... }:
        lib.mergeAttrsList [
          (lib.optionalAttrs (lib.elem module [
            "nixos"
            "darwin"
          ]) { environment.systemPackages = value pkgs; })
          (lib.optionalAttrs (module == "home") { home.packages = value pkgs; })
        ]
      )
    ) packages
  );
}
