{ self, lib, ... }: {
  lib = rec {
    modules = [
      "nixos"
      "darwin"
      "home"
    ];

    eachModule = lib.genAttrs modules;
  };

  tests = {
    test-eachModule = {
      expr = self.lib.eachModule (_: { });
      expected = {
        nixos = { };
        darwin = { };
        home = { };
      };
    };
    test-eachModule-test = {
      expr = self.lib.eachModule (_: {
        test = { };
      });
      expected = {
        nixos.test = { };
        darwin.test = { };
        home.test = { };
      };
    };
  };
}
