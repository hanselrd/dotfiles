{
  modules.nixos.fuse-program = {
    programs.fuse = {
      enable = true;
      userAllowOther = true;
    };
  };
}
