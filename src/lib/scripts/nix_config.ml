open Genspio.EDSL

let main =
  let conf =
    [
      "experimental-features = nix-command flakes pipe-operators";
      "show-trace = true";
    ]
  in
  setenv ~var:(str "NIX_CONFIG") (str (String.concat "\n" conf))
