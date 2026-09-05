{
  modules.nixos.udisks2-service = {
    services.udisks2 = {
      enable = true;
      mountOnMedia = true;
    };
  };
}
