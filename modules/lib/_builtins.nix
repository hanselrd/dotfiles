{ exec, ... }:
let
  builtin =
    name: args:
    exec (
      [
        "nix"
        "run"
        ".#builtins"
        "--"
        name
      ]
      ++ args
    );

  assertMsg = pred: msg: pred || builtins.throw msg;
in
{
  getRandomString = length: builtin "random-string" [ (builtins.toString length) ];

  getDevicePartition = path: builtin "device-partition" [ path ];

  getDevice = path: builtin "device" [ path ];

  decryptSecret =
    identity: secret:
    assert assertMsg (builtins.isPath secret)
      "The file to decrypt must be given as a path to prevent impurity";
    builtin "decrypt-secret" [
      identity
      secret
    ];
}
