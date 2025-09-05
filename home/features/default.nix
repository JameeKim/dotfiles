# home-manager modules for feature sets.
# This provides shared names to be used in user configurations.
{ ... }:
{
  imports = [
    ./dev.nix
    ./desktop.nix
    ./gaming.nix
  ];
}
