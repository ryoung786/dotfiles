export XDG_CONFIG_HOME="$HOME/.config"
export XDG_DATA_HOME="$XDG_CONFIG_HOME/.local/share"
export XDG_CACHE_HOME="$XDG_CONFIG_HOME/.cache"
export ZDOTDIR="$XDG_CONFIG_HOME/zsh"

export EDITOR='emacs -nw -Q'

# for GPG signing of git commits
export GPG_TTY=$(tty)

export PATH="/opt/homebrew/bin:/usr/bin:/bin:/usr/sbin:/sbin:/usr/local/bin"

F=~/.zshenv_private
test -f $F && source $F
