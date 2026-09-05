{
  modules.home.kotlin-development = { pkgs, ... }: {
    home.packages = with pkgs; [
      gradle
      kotlin
      kotlin-language-server
    ];
  };
}
