{
  modules.nixos.scx-service = {
    services.scx = {
      enable = true;
      scheduler = "scx_lavd";
    };
  };
}
