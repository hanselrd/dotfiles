{ exec, ... }:
let
  shell =
    cmd:
    exec [
      "bash"
      "-c"
      cmd
    ];
  builtin = name: args: shell "nix run .#builtins -- ${name} ${builtins.concatStringsSep " " args}";

  assertMsg = pred: msg: pred || builtins.throw msg;
in
{
  getRandomString =
    length:
    builtin "random-string" [
      "--length"
      (builtins.toString length)
    ];

  getDevicePartition =
    path:
    builtin "device-partition" [
      "--path"
      path
    ];

  getDevice =
    path:
    builtin "device" [
      "--path"
      path
    ];

  decryptSecret =
    identity: secret:
    assert assertMsg (builtins.isPath secret)
      "The file to decrypt must be given as a path to prevent impurity";
    builtin "decrypt-secret" [
      "--identity"
      identity
      "--secret"
      secret
    ];
}
