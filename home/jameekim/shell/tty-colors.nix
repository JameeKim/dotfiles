# Set colors for the tty.
{ ... }:
{
  programs.bash.initExtra =
    # bash
    ''
      if [ "$TERM" = "linux" ] ; then
        echo -en "\e]P0282828" # dark black
        echo -en "\e]P1f92672" # dark red
        echo -en "\e]P2a6e22e" # dark green
        echo -en "\e]P3cf8822" # dark yellow
        echo -en "\e]P44186f8" # dark blue
        echo -en "\e]P5ae81ff" # dark purple
        echo -en "\e]P6507874" # dark cyan
        echo -en "\e]P7a9b7c6" # dark white
        echo -en "\e]P8808080" # bright black
        echo -en "\e]P9f92672" # bright red
        echo -en "\e]PAa6e22e" # bright green
        echo -en "\e]PBe6db74" # bright yellow
        echo -en "\e]PC4186f8" # bright blue
        echo -en "\e]PDae81ff" # bright purple
        echo -en "\e]PE66d9ef" # bright cyan
        echo -en "\e]PFe3e1e4" # bright white
      fi
    '';
}
