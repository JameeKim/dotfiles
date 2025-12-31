# home-manager settings for host `z790-eos`.
{ ... }:
{
  imports = [
    ./gpu
  ];

  targets.archLinux.enable = true;
}
