{ config, lib, ... }:
{
  imports = [
    # ssh-agent
    (
      { ... }:
      {
        # Set `$SSH_AUTH_SOCK`.
        home.sessionVariablesExtra = # bash
          ''
            if [ -z "$SSH_AUTH_SOCK" ]; then
              export SSH_AUTH_SOCK="$XDG_RUNTIME_DIR/ssh-agent.socket"
            fi
          '';
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

    # w3m
    (
      { config, ... }:
      {
        # Tell `w3m` to store files in `$XDG_CONFIG_HOME`.
        home.sessionVariables.W3M_DIR = "${config.xdg.configHome}/w3m";
      }
    )

    # cargo
    (
      { config, ... }:
      {
        # Prepend cargo bin directory to `$PATH`.
        home.sessionPath = [ "${config.home.homeDirectory}/.cargo/bin" ];
      }
    )

    # man
    (
      { ... }:
      {
        # Use Neovim as man pager.
        home.sessionVariables.MANPAGER = "nvim +Man!";
      }
    )

    # gui session
    (
      { config, lib, ... }:
      {
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
      }
    )

    ./tty-colors.nix
  ];

  # Prepend local bin directory to "$PATH".
  # This overrides other executables if same names exist.
  home.sessionPath = lib.mkBefore [ "${config.home.homeDirectory}/.local/bin" ];

  home.shellAliases = {
    # `grep`
    grep = "grep --color=auto";

    # `headsetcontrol`
    hsc = "headsetcontrol";
    hsl = "hsc -l 0";

    # Use single instance for `kitty`.
    kitty = "kitty -1";

    # `ls`
    ls = "ls --color=auto -Fh";
    lv = "ls -v";
    la = "ls -vA";
    ll = "ls -vAl";

    # `tmux`
    tn = "tmux new -As";
  };

  programs.bash = {
    enable = true;

    initExtra = # bash
      ''
        # Search in command history with up/down arrow keys.
        bind '"\e[A":history-search-backward'
        bind '"\e[B":history-search-forward'

        # Make <C-l> execute `clear`.
        bind -x '"\C-l":clear'

        # Set prompt.
        PS1='[\u@\h \w]\$ '
      '';
  };

  programs.jq = {
    enable = true;
    colors = {
      null = "0;35";
      false = "0;35";
      true = "0;35";
      numbers = "0;35";
      strings = "0;93";
      arrays = "0;39";
      objects = "0;39";
      objectKeys = "0;33";
    };
  };
}
