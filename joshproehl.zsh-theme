function zle-keymap-select {
  # Keeping as example for possible later changes:
  # VIM_PROMPT="%{$fg_bold[yellow]%} [% NORMAL]%  %{$reset_color%}"
  # VIMODE="${${KEYMAP/vicmd/$VIM_PROMPT}/(main|viins)/}"
  VIMODE="${${KEYMAP/vicmd/:}/(main|viins)/}"
  zle reset-prompt
}

zle -N zle-keymap-select

# cribbed VERY heavily from the agnoster theme at https://gist.github.com/agnoster/3712874, because they like BASH a LOT more than I do. :-)

SLT="\ue0b2"
SGT="\ue0b0"

PLUSMINUS="\u00b1"
BRANCH="\ue0a0"
DETACHED="\u27a6"

CURRENT_BG='NONE'
PRIMARY_FG=black

# Begin a segment
# Takes two arguments, background and foreground. Both can be omitted,
# rendering default background/foreground.
prompt_segment() {
  local bg fg
  [[ -n $1 ]] && bg="%K{$1}" || bg="%k"
  [[ -n $2 ]] && fg="%F{$2}" || fg="%f"
  if [[ $CURRENT_BG != 'NONE' && $1 != $CURRENT_BG ]]; then
    print -n "%{$bg%F{$CURRENT_BG}%}$SGT%{$fg%}"
  else
    print -n "%{$bg%}%{$fg%}"
  fi
  CURRENT_BG=$1
  [[ -n $3 ]] && print -n $3
}

# End the prompt, closing any open segments
prompt_end() {
  if [[ -n $CURRENT_BG ]]; then
    print -n "%{%k%F{$CURRENT_BG}%}$SGT"
  else
    print -n "%{%k%}"
  fi
  print -n "%{%f%}"
  CURRENT_BG=''
}

# Git: branch/detached head, dirty status
prompt_git() {
  local color ref
  is_dirty() {
    test -n "$(git status --porcelain --ignore-submodules)"
  }
  ref="$vcs_info_msg_0_"
  if [[ -n "$ref" ]]; then
    if is_dirty; then
      color=yellow
      ref="${ref} $PLUSMINUS"
    else
      color=green
      ref="${ref} "
    fi
    if [[ "${ref/.../}" == "$ref" ]]; then
      ref="$BRANCH $ref"
    else
      ref="$DETACHED ${ref/.../}"
    fi
    prompt_segment $color $PRIMARY_FG
    print -Pn " $ref"
  fi
}



# If we're not connected to the local computer, as our normal user,
# then display the username@host portion of the prompt
prompt_userhost() {
  local user=`whoami`

  if [[ "$user" != "$DEFAULT_USER" || -n "$SSH_CONNECTION" ]]; then
   prompt_segment "red" "white" "$user@%m"
  fi
}

# Only show the rbenv status if we're not using the "global" version
prompt_rbenv() {
  if [[ `rbenv version-origin` != "/home/joshproehl/.rbenv/version" ]]; then
    prompt_segment "white" "red" "$(rbenv version-name)"
  fi
}

# Helper function which just shows our current path in the normal ZSH way
prompt_path() {
  prompt_segment "black" "cyan" " %~ "
}

prompt_vimode() {
 prompt_segment "green" "white" "${VIMODE}"
}

# Build the entire prompt.
# Note: Turns out this is necessary for the CURRENT_BG variable scope.
prompt_main() {
  CURRENT_BG='NONE'
  prompt_userhost
  #prompt_rbenv
  prompt_path
  prompt_git
  prompt_vimode
  prompt_end
}

prompt_theme_precmd() {
  vcs_info
  PROMPT='%{%f%b%k%}$(prompt_main) '
}

prompt_agnoster_setup() {
  autoload -Uz add-zsh-hook
  autoload -Uz vcs_info

  prompt_opts=(cr subst percent)

  add-zsh-hook precmd prompt_theme_precmd

  zstyle ':vcs_info:*' enable git
  zstyle ':vcs_info:*' check-for-changes false
  zstyle ':vcs_info:git*' formats '%b'
  zstyle ':vcs_info:git*' actionformats '%b (%a)'
}

prompt_agnoster_setup "$@"

RPROMPT='[%{$fg[green]%}%T %D%{$reset_color%}]'
