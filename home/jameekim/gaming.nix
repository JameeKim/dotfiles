# Settings related to gaming.
{ config, lib, ... }:
let
  cfg = config.features.gaming;
in
{
  config =
    let
      # Environment variables.
      env = {
        # Force using Xbox controller button scheme for Nintento Switch controllers.
        SDL_GAMECONTROLLER_USE_BUTTON_LABELS = 0;
      };
    in
    lib.mkIf cfg.enable {
      home.sessionVariables = env;
      systemd.user.sessionVariables = env;
    };
}
