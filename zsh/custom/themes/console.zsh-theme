if [[ $UID -eq 0 ]] ; then
    UCLR=red
else
    UCLR=green
fi

PROMPT=$' %{$fg_bold[white]%}[%*] [%{$fg_bold[$UCLR]%}%n%{$fg_bold[white]%}] [%{$fg_bold[yellow]%}%~%{$fg_bold[white]%}]%{$reset_color%} $(git_prompt_info)\n -> %{$fg_bold[white]%}λ%{$reset_color%} '

ZSH_THEME_GIT_PROMPT_PREFIX="%{$fg_bold[white]%}[git:%{$fg_bold[cyan]%}"
ZSH_THEME_GIT_PROMPT_SUFFIX="%{$fg_bold[white]%}]%{$reset_color%}"
ZSH_THEME_GIT_PROMPT_CLEAN=""
ZSH_THEME_GIT_PROMPT_DIRTY="%{$fg_bold[red]%}*"
