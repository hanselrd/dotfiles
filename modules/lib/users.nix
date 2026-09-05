{ self, lib, ... }:
let
  mkUser =
    args@{
      username ? "delacruz",
      name ? "Hansel De La Cruz",
      email ? "18725263+hanselrd@users.noreply.github.com",
    }:
    args // { inherit username name email; };
in
{
  lib = rec {
    users = rec {
      root = mkUser {
        username = "root";
        name = "root";
      };
      delacruz = mkUser { };
      hansel-delacruz = mkUser { username = "hansel.delacruz"; };
      delacruz-work = mkUser { email = lib.fileContents (self.outPath + "/secrets/work-email"); };
      hansel-delacruz-work = mkUser {
        inherit (hansel-delacruz) username;
        inherit (delacruz-work) email;
      };
    };

    eachUser =
      f:
      lib.listToAttrs (
        lib.concatMap (
          tag: lib.mapAttrsToList (name: value: lib.nameValuePair "${tag}-${name}" value) (f users.${tag})
        ) (lib.attrNames users)
      );
  };

  tests = {
    test-eachUser = {
      expr = self.lib.eachUser (_: { });
      expected = { };
    };
    test-eachUser-test = {
      expr = self.lib.eachUser (_: {
        test = { };
      });
      expected = {
        root-test = { };
        delacruz-test = { };
        hansel-delacruz-test = { };
        delacruz-work-test = { };
        hansel-delacruz-work-test = { };
      };
    };
    test-eachUser-test0-test1 = {
      expr = self.lib.eachUser (_: {
        test0 = { };
        test1 = { };
      });
      expected = {
        root-test0 = { };
        root-test1 = { };
        delacruz-test0 = { };
        delacruz-test1 = { };
        hansel-delacruz-test0 = { };
        hansel-delacruz-test1 = { };
        delacruz-work-test0 = { };
        delacruz-work-test1 = { };
        hansel-delacruz-work-test0 = { };
        hansel-delacruz-work-test1 = { };
      };
    };
  };
}
