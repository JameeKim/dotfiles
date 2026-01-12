# Settings related to developing game assets.
{
  config,
  lib,
  pkgs,
  ...
}:
let
  devCfg = config.features.dev;
  cfg = config.features.dev.game-assets;
in
{
  config = lib.mkIf (devCfg.enable && cfg.enable) {
    home.packages = with pkgs; [ aseprite ];
  };
}
