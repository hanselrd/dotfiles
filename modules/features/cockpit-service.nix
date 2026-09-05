{
  modules.nixos.cockpit-service = {
    services.cockpit = {
      enable = true;
      openFirewall = true;
    };
  };
}
