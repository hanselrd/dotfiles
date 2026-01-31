{ ... }:
let
  excludes = [ "ancestry/*" ];
in
{
  projectRootFile = "flake.nix";

  programs.nixfmt.enable = true;
  programs.nixfmt.width = 100;
  programs.nixfmt.indent = 2;
  programs.nixfmt.strict = true;
  settings.formatter.nixfmt.excludes = excludes;
  settings.formatter.nixfmt.includes = [ "*.nix" ];

  programs.deadnix.enable = true;
  settings.formatter.deadnix.excludes = excludes;
  settings.formatter.deadnix.includes = [ "*.nix" ];
  settings.formatter.deadnix.options = [ "-W" ];

  programs.shfmt.enable = true;
  programs.shfmt.indent_size = 2;
  programs.shfmt.simplify = true;
  settings.formatter.shfmt.excludes = excludes;
  settings.formatter.shfmt.includes = [ "*.sh" ];
  settings.formatter.shfmt.options = [ "-sr" ];

  programs.stylua.enable = true;
  programs.stylua.settings.column_width = 100;
  programs.stylua.settings.indent_type = "Spaces";
  programs.stylua.settings.indent_width = 2;
  programs.stylua.settings.quote_style = "AutoPreferDouble";
  programs.stylua.settings.call_parentheses = "Always";
  programs.stylua.settings.collapse_simple_statement = "Never";
  settings.formatter.stylua.excludes = excludes;
  settings.formatter.stylua.includes = [ "*.lua" ];

  programs.jsonfmt.enable = true;
  settings.formatter.jsonfmt.excludes = excludes;
  settings.formatter.jsonfmt.includes = [
    "*.json"
    "*.jsonc"
  ];

  programs.ormolu.enable = true;
  programs.ormolu.ghcOpts = [
    "BangPatterns"
    "DuplicateRecordFields"
    "FlexibleContexts"
    "GeneralizedNewtypeDeriving"
    "LambdaCase"
    "OverloadedRecordDot"
    "OverloadedStrings"
    "PatternSynonyms"
    "QuasiQuotes"
    "TemplateHaskell"
    "TypeApplications"
  ];
  settings.formatter.ormolu.excludes = excludes;
  settings.formatter.ormolu.includes = [ "*.hs" ];

  programs.cabal-fmt.enable = true;
  settings.formatter.cabal-fmt.excludes = excludes;
  settings.formatter.cabal-fmt.includes = [ "*.cabal" ];

  programs.goimports.enable = true;
  settings.formatter.goimports.excludes = excludes;
  settings.formatter.goimports.includes = [ "*.go" ];
  settings.formatter.goimports.options = [
    "-local"
    "github.com/hanselrd"
  ];

  programs.gofumpt.enable = true;
  programs.gofumpt.extra = true;
  settings.formatter.gofumpt.excludes = excludes;
  settings.formatter.gofumpt.includes = [ "*.go" ];

  programs.golines.enable = true;
  programs.golines.maxLength = 100;
  settings.formatter.golines.excludes = excludes;
  settings.formatter.golines.includes = [ "*.go" ];

  programs.dockerfmt.enable = true;
  settings.formatter.dockerfmt.excludes = excludes;
  settings.formatter.dockerfmt.includes = [ "Dockerfile" ];
  settings.formatter.dockerfmt.options = [
    "-i"
    "2"
    "-s"
  ];
}
