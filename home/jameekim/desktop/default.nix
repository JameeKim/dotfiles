# Settings related to desktop environment.
{ config, lib, ... }:
let
  cfg = config.features.desktop;
in
{
  config = lib.mkIf cfg.enable {
    # Automatically start GUI session if in 1st tty.
    programs.bash.initExtra =
      lib.mkOrder 9999 # bash
        ''
          if [ "$TERM" = "linux" ] \
            && [ "$XDG_SESSION_TYPE" = "tty" ] \
            && [ "$XDG_VTNR" = "1" ]
          then
            if uwsm check may-start 1 && uwsm select ; then
              systemd-cat -t uwsm_start uwsm start default
            fi
          fi
        '';

    # Symlink niri config files.
    xdg.configFile."niri".source = config.lib.file.mkDotfilesSymlink ./niri;

    # TODO: desktop environment
  };
}
