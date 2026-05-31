# home-manager settings for host `z790-eos`.
{ ... }:
{
  imports = [
    ./gpu.nix
  ];

  targets.archLinux.enable = true;
}
