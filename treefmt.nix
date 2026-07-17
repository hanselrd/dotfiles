{ ... }: {
  projectRootFile = "flake.nix";

  settings.walk = "filesystem";
  settings.excludes = [
    ".git/**"
    "ancestry/**"
  ];

  programs.nixfmt.enable = true;
  programs.nixfmt.width = 100;
  programs.nixfmt.indent = 2;
  programs.nixfmt.strict = true;
  settings.formatter.nixfmt.includes = [ "*.nix" ];

  programs.deadnix.enable = true;
  settings.formatter.deadnix.includes = [ "*.nix" ];
  settings.formatter.deadnix.options = [ "-W" ];

  programs.shfmt.enable = true;
  programs.shfmt.indent_size = 2;
  programs.shfmt.simplify = true;
  settings.formatter.shfmt.includes = [ "*.sh" ];
  settings.formatter.shfmt.options = [ "-sr" ];

  programs.stylua.enable = true;
  programs.stylua.settings.column_width = 100;
  programs.stylua.settings.indent_type = "Spaces";
  programs.stylua.settings.indent_width = 2;
  programs.stylua.settings.quote_style = "AutoPreferDouble";
  programs.stylua.settings.call_parentheses = "Always";
  programs.stylua.settings.collapse_simple_statement = "Never";
  settings.formatter.stylua.includes = [ "*.lua" ];

  programs.jsonfmt.enable = true;
  settings.formatter.jsonfmt.includes = [
    "*.json"
    "*.jsonc"
  ];

  programs.fourmolu.enable = true;
  programs.fourmolu.ghcOpts = [
    "BangPatterns"
    "CPP"
    "DuplicateRecordFields"
    "ExtendedDefaultRules"
    "FlexibleContexts"
    "GeneralizedNewtypeDeriving"
    "LambdaCase"
    "NamedFieldPuns"
    "OverloadedRecordDot"
    "OverloadedStrings"
    "PatternSynonyms"
    "QuasiQuotes"
    "RecordWildCards"
    "TemplateHaskell"
    "TypeApplications"
  ];
  settings.formatter.fourmolu.includes = [ "*.hs" ];
  settings.formatter.fourmolu.options = [
    "--indentation"
    "2"
    "--column-limit"
    "100"
    "--import-export-style"
    "leading"
    "--indent-wheres"
    "true"
    "--record-brace-space"
    "true"
    "--haddock-style"
    "single-line"
    "--let-style"
    "inline"
    "--sort-constraints"
    "true"
    "--sort-derived-classes"
    "true"
    "--sort-deriving-clauses"
    "true"
    "--trailing-section-operators"
    "false"
  ];

  programs.cabal-fmt.enable = true;
  settings.formatter.cabal-fmt.includes = [ "*.cabal" ];

  programs.goimports.enable = true;
  settings.formatter.goimports.includes = [ "*.go" ];
  settings.formatter.goimports.options = [
    "-local"
    "github.com/hanselrd"
  ];

  programs.gofumpt.enable = true;
  programs.gofumpt.extra = true;
  settings.formatter.gofumpt.includes = [ "*.go" ];

  programs.golines.enable = true;
  programs.golines.maxLength = 100;
  settings.formatter.golines.includes = [ "*.go" ];

  programs.dockerfmt.enable = true;
  settings.formatter.dockerfmt.includes = [ "Dockerfile" ];
  settings.formatter.dockerfmt.options = [
    "-i"
    "2"
    "-s"
  ];
}
