{ lib, ... }:
{
  options.features.gaming = {
    enable = lib.mkEnableOption "settings for gaming";
  };
}
