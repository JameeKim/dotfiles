{ ... }:
{
  imports = [ ./home ];

  home = {
    username = "jameekim";
    homeDirectory = "/home/jameekim";
    stateVersion = "25.05";
    preferXdgDirectories = true;
  };

  nix.settings = {
    use-xdg-base-directories = true;
    experimental-features = [
      "nix-command"
      "flakes"
    ];
  };

  programs.home-manager.enable = true;

  systemd.user = {
    enable = false;
  };
}
