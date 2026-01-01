{ pkgs, ... }:
{
  home.packages = with pkgs; [ lldb ];

  home.file.".lldbinit" = {
    text = ''
      # TODO: settings
    '';
  };
}
