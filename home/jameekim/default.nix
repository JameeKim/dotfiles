{ ... }:
{
  imports = [
    ./lib.nix
    ./shell
    ./dev
    ./desktop
    ./gaming.nix
    ./study.nix
    ./nix.nix
    ./xdg.nix
  ];

  home = {
    username = "jameekim";
    homeDirectory = "/home/jameekim";
    stateVersion = "25.05";
  };

  programs.home-manager.enable = true;

  systemd.user = {
    enable = false;
    # TODO: systemd
  };

  # TODO: xdg-terminal-exec
  # TODO: xdg-mime
  # TODO: desktop entries and autostart
  # TODO: wayland sessions (nixGL probably needed)
}
