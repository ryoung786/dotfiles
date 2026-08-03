autoload -Uz compinit && compinit
autoload -Uz select-word-style && select-word-style bash

source /opt/homebrew/share/zsh-autosuggestions/zsh-autosuggestions.zsh
source /opt/homebrew/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

export STARSHIP_CONFIG=~/.config/starship/starship.toml
eval "$(starship init zsh)"

eval "$(~/.local/bin/mise activate zsh)"

# Each iterm has its own history
unsetopt inc_append_history
unsetopt share_history

alias tree='tree -C'

F=~/.zsh_private
test -f $F && source $F
