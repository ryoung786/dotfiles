if [ -f ~/.bash_aliases ]; then
	. ~/.bash_aliases
fi

# OSX annoyingly prints a long msg about switching to ZSH on startup
export BASH_SILENCE_DEPRECATION_WARNING=1

# utility function for finding out what process is listening on a given port
onport() {
    lsof -i :$1
}
export -f onport

# iex history between sessions
export ERL_AFLAGS="-kernel shell_history enabled"

# megaphone golang
export GOPROXY=https://gomodproxy.spotify.net
export GOPRIVATE=github.com/panoplymedia
export GONOSUMDB=ghe.spotify.net
export GO111MODULE=on
export GPG_TTY=$(tty)

export EDITOR="emacsclient -t -a ''"
export SPOTIFY_DOMAIN="guc3.spotify.net"

# asdf
. /usr/local/opt/asdf/asdf.sh
. /usr/local/opt/asdf/etc/bash_completion.d/asdf.bash


################################################################################
#### BASH-GIT-PROMPT and VTERM config
################################################################################

# bash-git-prompt
export GIT_PS1_SHOWDIRTYSTATE=1
export GIT_PS1_SHOWSTASHSTATE=1
export GIT_PS1_SHOWUNTRACKEDFILES=1
export GIT_PS1_SHOWUPSTREAM=verbose # or auto to omit counts
export GIT_PS1_SHOWCOLORHINTS=1
export PROMPT_COMMAND='__git_ps1 "\h:\w" "\\\$ "'
if [ -f $(brew --prefix)/etc/bash_completion ]; then
  . $(brew --prefix)/etc/bash_completion
fi

if [ -f "$(brew --prefix)/opt/bash-git-prompt/share/gitprompt.sh" ]; then
  __GIT_PROMPT_DIR="$(brew --prefix)/opt/bash-git-prompt/share"
  source "$(brew --prefix)/opt/bash-git-prompt/share/gitprompt.sh"
fi

# vterm
if [[ "$INSIDE_EMACS" = 'vterm' ]] \
    && [[ -n ${EMACS_VTERM_PATH} ]] \
    && [[ -f ${EMACS_VTERM_PATH}/etc/emacs-vterm-bash.sh ]]; then
        # holy toledo this is a mess, but it works
        # The bash-git-prompt sets a PROMPT_COMMDAND that looks for git info
        # in the dir and then sets the PS1.
        # vterm also relies on setting the PROMPT_COMMAND in its config, so I had to
        # create my own config and comment out the PROMPT_COMMAND in there

        # To keep prompt end detection working, I had to set the PROMPT_COMMAND below
        # to reference the PROMPT_COMMAND set by bash-git-prompt, then, because bash-git-prompt
        # mucks with the PS1 inside its PROMPT_COMMAND, set the PS1 explicitly again.
        # Finally, append the echo host:pwd bit so detection does indeed work in vterm

        # NOTE: if you aren't using bash-git-prompt, then replace the two lines below with
        # source ${EMACS_VTERM_PATH}/etc/emacs-vterm-bash.sh
        # as suggested by the vterm readme docs
        source ~/.emacs.d/vterm-bash.sh
        PROMPT_COMMAND="${PROMPT_COMMAND} ; "'PS1="${PS1}"'"'"'\[$(vterm_prompt_end)\]'"'"'; echo -ne "\033]0;${HOSTNAME}:${PWD}\007"'
fi
