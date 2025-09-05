{ lib, ... }:
{
  options.features.dev = {
    enable = lib.mkEnableOption "settings for development environment";
  };
}
