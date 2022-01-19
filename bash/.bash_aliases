alias ec="emacsclient -t -a ''"
alias ldiff="git diff HEAD~ HEAD"
alias jfmt="mvn com.coveo:fmt-maven-plugin:format"
alias loadenv="set -a; source .env; set +a"
alias k="kubectl"

alias ls='ls -G'
alias ll='ls -lG'

alias servedir='python -m SimpleHTTPServer'
alias gitconfigspotify='git config --add include.path "~/.gitconfig-spotify"'

alias my_ip='dig +short myip.opendns.com @resolver1.opendns.com'

# start/stop postgres
alias pgup='pg_ctl -D /usr/local/var/postgres start'
alias pgdown='pg_ctl -D /usr/local/var/postgres stop'