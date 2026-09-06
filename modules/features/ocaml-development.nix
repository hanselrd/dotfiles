{
  modules.home.ocaml-development = { pkgs, ... }: {
    home.packages = with pkgs; [
      dune
      gcc
      ocaml
      ocamlPackages.ocaml-lsp
      ocamlPackages.odoc
      ocamlPackages.utop
      ocamlformat
      opam
    ];
  };
}
