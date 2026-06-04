# Settings related to using LLM.
{
  config,
  lib,
  pkgs,
  ...
}:
let
  devCfg = config.features.dev;
  cfg = config.features.dev.llm;
in
{
  config = lib.mkIf (devCfg.enable && cfg.enable) {
    programs.opencode = {
      enable = true;
      # TODO: Go through permissions.
      # TODO: Read access to external paths like cargo src.
      settings = lib.importJSON ./opencode/opencode.json;
      tui = lib.importJSON ./opencode/tui.json;
    };
  };
}
