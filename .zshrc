autoload -U colors && colors

PROMPT="
$fg[blue]%n$reset_color@$fg[cyan]%m$fg[yellow] %~
$fg[green]>$reset_color "

alias l='ls'
alias ll='ls -laG'

function server () {
    if [ $1 ] 
    then
	local port="$1"
    else
	local port="8000"
    fi
    open "http://localhost:$port" && python -m SimpleHTTPServer "$port"
}

export PATH="$PATH:$HOME/.rvm/bin" # Add RVM to PATH for scripting
