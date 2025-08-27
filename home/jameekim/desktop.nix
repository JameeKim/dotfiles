{ config, lib, ... }:
let
  cfg = config.features.desktop;
in
{
  config = lib.mkIf cfg.enable {
    # TODO: desktop environment
  };
}
