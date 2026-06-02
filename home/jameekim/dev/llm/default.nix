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
      settings = lib.importJSON ./opencode/opencode.json;
      tui = lib.importJSON ./opencode/tui.json;
    };
  };
}
