{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.features.dev;
in
{
  config = lib.mkIf cfg.enable {
    programs.git = {
      enable = true;
      package = pkgs.gitFull;
      lfs.enable = true;

      settings = {
        user = {
          email = "jameekim@nodaplife.me";
          name = "Jamee Kim";
        };
        init.defaultBranch = "main";
        alias = {
          st = "status";
          staus = "status";
          stauts = "status";
        };
      };

      ignores = [
        ".nvim.lua"
        ".lazy.lua"
        ".tmux.conf"
        ".tmuxp.yaml"
        ".tmuxp.yml"
      ];
    };

    # TODO: Move `gitui` config.
  };
}
