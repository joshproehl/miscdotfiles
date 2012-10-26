function zle-keymap-select {
  VIMODE="${${KEYMAP/vicmd/:}/(main|viins)/}"
  zle reset-prompt
}

zle -N zle-keymap-select

PROMPT='[%{$fg[magenta]%}$(rbenv version-name)%{$reset_color%}][%{$fg[blue]%}%~%{$reset_color%}] $(git_prompt_info)%{$reset_color%}%{$fg_bold[red]%} ${VIMODE}➜ %{$reset_color%} '
RPROMPT='[%{$fg[green]%}%T %D%{$reset_color%}]'
ZSH_THEME_GIT_PROMPT_PREFIX="%{$fg[blue]%}|%{$fg[red]%}"
ZSH_THEME_GIT_PROMPT_SUFFIX="%{$fg[blue]%}|"
ZSH_THEME_GIT_PROMPT_DIRTY="%{$fg[yellow]%} ✗✗✗"
ZSH_THEME_GIT_PROMPT_CLEAN="%{$fg[white]%} ✔"
