{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.targets.archLinux;
in
{
  config = lib.mkIf cfg.enable {
    # Set `nix` to latest verion.
    nixpkgs.overlays = [ (import ./overlay.nix) ];
    xdg.configFile."nixpkgs/overlays/arch-linux-nix-latest.nix".source = ./overlay.nix;
    nix.package = lib.mkIf (config.nix.settings != { } || config.nix.extraOptions != "") (
      lib.mkDefault pkgs.nix
    );
  };
}
