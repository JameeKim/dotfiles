# Main home-manager module for EndeavourOS systems.
{ pkgs, ... }:
{
  targets.genericLinux.enable = true;
  nix.package = pkgs.nixVersions.latest; # Match with Arch repo
  systemd.user.systemctlPath = "/usr/bin/systemctl";
  xdg.mime.enable = false;
}
