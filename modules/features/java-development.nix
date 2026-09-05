{
  modules.home.java-development = { pkgs, ... }: {
    programs.java.enable = true;

    home.packages = with pkgs; [
      jdt-language-server
      maven
    ];
  };
}
