{ lib, ... }:
{
  options.features.desktop = {
    enable = lib.mkEnableOption "settings for desktop environment";
  };
}
