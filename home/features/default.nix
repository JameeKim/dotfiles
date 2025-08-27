# home-manager modules for feature sets.
# This provides shared names to be used in user configurations.
{ ... }:
{
  imports = [
    ./desktop.nix
    ./gaming.nix
  ];
}
