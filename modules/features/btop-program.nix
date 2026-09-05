{ self, lib, ... }: {
  modules.home.btop-program = {
    programs.btop = {
      enable = true;
      settings = {
        clock_format = lib.replaceStrings [ "%-" ] [ "%" ] self.lib.timeFormat;
        update_ms = 3000;
      };
    };
  };
}
