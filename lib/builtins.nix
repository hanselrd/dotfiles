{ exec, ... }:
let
  shell =
    cmd:
    exec [
      "env"
      "bash"
      "-c"
      cmd
    ];
  builtin =
    name: args: shell <| "nix run .#builtins -- ${name} " + builtins.concatStringsSep " " args;

  assertMsg = pred: msg: pred || builtins.throw msg;

  TMPDIR = "\${XDG_RUNTIME_DIR:-\${TMPDIR:-/tmp}}/nix-$(id -u)";
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
    exec [
      "env"
      "bash"
      "-c"
      "umask 077 && mkdir -p \"${TMPDIR}\" && TMP=$(mktemp -p \"${TMPDIR}\") && age -d -i ${identity} -o \"$TMP\" ${secret} && echo \"$TMP\""
    ];
}
