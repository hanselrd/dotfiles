{
  modules.home.ocaml-development = { pkgs, ... }: {
    home.packages = with pkgs; [
      dune
      ocamlPackages_latest.ocaml
      ocamlPackages_latest.ocaml-lsp
      ocamlPackages_latest.ocamlformat
      ocamlPackages_latest.utop
      opam
    ];
  };
}
