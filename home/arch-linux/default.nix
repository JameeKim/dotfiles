# home-manager module for settings specifically tailored for Arch Linux systems.
# Intended to be used in place of `targets.genericLinux`, not with it.
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
  imports = [ ./nix ];

  options.targets.archLinux = {
    enable = lib.mkEnableOption "settings for Arch Linux";
  };

  config = lib.mkIf cfg.enable {
    assertions = [
      (lib.hm.assertions.assertPlatform "targets.archLinux" pkgs lib.platforms.linux)
    ];

    # `systemctl` should be already installed.
    systemd.user.systemctlPath = "/usr/bin/systemctl";

    # `man` is already properly configured.
    programs.man.enable = lib.mkDefault false;

    # `bash` is already installed with completion enabled.
    programs.bash = {
      package = lib.mkDefault null;
      enableCompletion = lib.mkDefault false;
    };

    # TODO: copy `targets/generic-linux.nix` from home-manager modules.
  };
}
