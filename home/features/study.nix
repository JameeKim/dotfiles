{ lib, ... }:
{
  options.features.study = {
    enable = lib.mkEnableOption "settings for studying";
  };
}
