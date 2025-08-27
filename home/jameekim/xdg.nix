{ config, lib, ... }:
{
  home.preferXdgDirectories = true;
  nix.settings = lib.mkIf config.nix.enable {
    use-xdg-base-directories = true;
  };

  xdg.enable = true;
  xdg.mime.enable = false; # TODO: xdg-mime
}
