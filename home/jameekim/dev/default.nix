# Settings related to programming.
{ config, lib, ... }:
let
  cfg = config.features.dev;
in
{
  # TODO: Make these settings conditional to `cfg.enable`.
  imports = [
    # cargo
    (
      { config, ... }:
      {
        # Prepend cargo bin directory to `$PATH`.
        home.sessionPath = [ "${config.home.homeDirectory}/.cargo/bin" ];
      }
    )

    # asdf
    (
      { config, ... }:
      {
        # Set env vars for `asdf`.
        home.sessionVariables = {
          ASDF_CONFIG_FILE = "${config.xdg.configHome}/asdf/asdfrc";
          ASDF_DIR = "${config.xdg.dataHome}/asdf";
          ASDF_DATA_DIR = "${config.xdg.stateHome}/asdf";
        };
        # Add `asdf` programs to `$PATH`.
        home.sessionPath = [ "${config.home.sessionVariables.ASDF_DATA_DIR}/shims" ];
      }
    )

    # android
    (
      { config, ... }:
      {
        # Set path for Android SDK.
        home.sessionVariables.ANDROID_HOME = "${config.xdg.dataHome}/Android/Sdk";
      }
    )

    ./git.nix
    ./game-assets.nix
  ];

  config = lib.mkIf cfg.enable {
    # TODO: Dev env settings.
  };
}
