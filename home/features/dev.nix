{ lib, ... }:
{
  options.features.dev = {
    enable = lib.mkEnableOption "settings for development environment";

    game-assets = {
      enable = lib.mkEnableOption "programs for developing game assets";
    };

    llm = {
      enable = lib.mkEnableOption "programs for using LLM";
    };
  };
}
