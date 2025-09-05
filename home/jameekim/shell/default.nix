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

    # w3m
    (
      { config, ... }:
      {
        # Tell `w3m` to store files in `$XDG_CONFIG_HOME`.
        home.sessionVariables.W3M_DIR = "${config.xdg.configHome}/w3m";
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

    ./tty-colors.nix
  ];

  # Prepend local bin directory to "$PATH".
  # This overrides executables from other paths if same names exist.
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

  # TODO: Add direnv.

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
