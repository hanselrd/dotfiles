{ exec, ... }:
let
  assertMsg = pred: msg: pred || builtins.throw msg;

  TMPDIR = "\${XDG_RUNTIME_DIR:-\${TMPDIR:-/tmp}}/nix-$(id -u)";
in
rec {
  getDevicePartition =
    path:
    exec [
      "env"
      "bash"
      "-c"
      "findmnt -no source -T ${path} | sed -e 's/\\n$//' -e 's/.*/\"&\"/'"
    ];

  getDevice =
    path:
    exec [
      "env"
      "bash"
      "-c"
      "lsblk -dpno pkname ${getDevicePartition path} | sed -e 's/\\n$//' -e 's/.*/\"&\"/'"
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
