{ self, lib, ... }: {
  modules.home.claude-code-program = {
    programs.claude-code = {
      enable = true;
      settings = {
        env = {
          CLAUDE_CODE_DISABLE_NONESSENTIAL_TRAFFIC = "1";
          CLAUDE_CODE_EXPERIMENTAL_AGENT_TEAMS = "1";
        };
        theme = "dark";
      };
    };

    home.sessionVariables = {
      CLAUDE_CODE_OAUTH_TOKEN = lib.fileContents (self.outPath + "/secrets/claude-code-oauth-token");
    };
  };
}
