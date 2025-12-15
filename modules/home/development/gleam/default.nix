{
  config,
  lib,
  pkgs,
  ...
}:
{
  home.packages = with pkgs; [
    erlang
    gleam
    rebar3
  ];
}
