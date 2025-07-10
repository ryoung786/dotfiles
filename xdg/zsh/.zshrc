#### Oh-my-zsh

# Path to your oh-my-zsh installation.
# XDG_CONFIG_HOME is defined in ~/.zshenv
export ZSH="$XDG_CONFIG_HOME/zsh/oh-my-zsh"
plugins=(git mise direnv brew mix-fast colored-man-pages zsh-autosuggestions zsh-syntax-highlighting)
source $ZSH/oh-my-zsh.sh

#### End Oh-my-zsh

export STARSHIP_CONFIG=~/.config/starship/starship.toml
eval "$(starship init zsh)"

# User configuration

# Each iterm has its own history
unsetopt inc_append_history
unsetopt share_history

alias -g tree='tree -C'

export ERL_AFLAGS="-kernel shell_history enabled"
export BAT_THEME="ansi"

F=~/.zsh_private
test -f $F && source $F
