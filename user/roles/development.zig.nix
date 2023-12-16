{
  config,
  lib,
  pkgs,
  ...
}: let
  cfg = config.roles.user.development.zig;
in {
  options = {
    roles.user.development.zig = {
      enable = lib.mkEnableOption "roles.user.development.zig";
    };
  };

  config = lib.mkIf cfg.enable {
    home.packages = with pkgs; [
      # (pkgs.zig.overrideAttrs (prevAttrs: {
      #   version = "unstable";
      #   src = pkgs.fetchFromGitHub {
      #     inherit (prevAttrs.src) owner repo;
      #     rev = "779b8e2598521de45934c085ebece852bf5039de";
      #     hash = "sha256-oAk4uSpFY2Y2S88l67aQHJVeqeIIEKy8hlfmWXlmyuk=";
      #   };
      #   nativeBuildInputs =
      #     (lib.remove llvmPackages.llvm.dev prevAttrs.nativeBuildInputs)
      #     ++ [llvmPackages_17.llvm.dev];
      #   buildInputs =
      #     (lib.subtractLists (with llvmPackages; [
      #         libclang
      #         lld
      #         llvm
      #       ])
      #       prevAttrs.buildInputs)
      #     ++ (with llvmPackages_17; [
      #       libclang
      #       lld
      #       llvm
      #     ]);
      # }))
      zig
    ];
  };
}
