# .zshrc file
#
# Concepts:
#
#    1) .zshrc is the *non-login* config for zsh, run in scripts and after
#        first connection.
#    2) .zprofile is the *login* config for zsh, launched upon first connection.
#    3) .zprofile imports .zshrc, but not vice versa.
#    4) .zshrc imports .zshrc_custom, which can be used to override
#        variables specified here.
#           
# When using GNU screen:
#
#    1) .zprofile is loaded the first time you login, and should be used
#       only for paths and environmental settings

#    2) .zshrc is loaded in each subsequent screen, and should be used for
#       aliases and things like writing to .bash_eternal_history (see below)
#
# For more information, see here:
# http://en.wikipedia.org/wiki/Bash#Startup_scripts
#
# When zsh starts, it executes the commands in a variety of different scripts.
#
#  1) It first reads and executes commands from the file /etc/zshenv, 
#     if that file exists. This is done for every instance of zsh, whether 
#     it's a login shell or not.
#
#  2) Next, it reads and executes commands from /etc/zprofile, if that 
#     file exists.
#
#  3) After reading the global files, it looks for ~/.zshenv and sources 
#     it if it exists.
#
#  4) It then looks for ~/.zprofile and sources it if it exists.
#
#  5) After that, for interactive shells, it will source ~/.zshrc if it exists.
#
#  6) Finally, before presenting the prompt for the first time, it will 
#     source ~/.zlogin if it exists.




# -----------------------------------
# -- 1.1) Set up umask permissions --
# -----------------------------------
#  The following incantation allows easy group modification of files.
#  See here: http://en.wikipedia.org/wiki/Umask
#
#     umask 002 allows only you to write (but the group to read) any new
#     files that you create.
#
#     umask 022 allows both you and the group to write to any new files
#     which you make.
#
#  In general we want umask 022 on the server and umask 002 on local
#  machines.
#
#  The command 'id' gives the info we need to distinguish these cases.
#
#     $ id -gn  #gives group name
#     $ id -un  #gives user name
#     $ id -u   #gives user ID
#
#  So: if the group name is the same as the username OR the user id is not
#  greater than 99 (i.e. not root or a privileged user), then we are on a
#  local machine (check for yourself), so we set umask 002.
#
#  Conversely, if the default group name is *different* from the username
#  AND the user id is greater than 99, we're on the server, and set umask
#  022 for easy collaborative editing.
if [ "`id -gn`" = "`id -un`" -a `id -u` -gt 99 ]; then
	umask 002
else
	umask 022
fi


# Make prompt informative
PS1="%F{blue}[%n@%m:%~]$%f "


## -----------------------
## -- 2) Set up aliases --
## -----------------------

# 2.1) Safety
alias rm="rm -i"
alias mv="mv -i"
alias cp="cp -i"
set -o noclobber

# 2.2) Listing, directories, and motion
alias ll="ls -walrtFG"
alias la="ls -wA"
alias l="ls -wCF"
alias dir='ls -wG --format=vertical'
alias vdir='ls -wG --format=long'
alias m='less'
alias ..='cd ..'
alias ...='cd ..;cd ..'
alias md='mkdir'
alias cl='clear'
alias du='du -ch --max-depth=1'
alias treeacl='tree -A -C -L 2'

# 2.3) Text and editor commands
alias emacs='emacs -nw'     # No X11 windows
alias eqq='emacs -nw -Q' # No config and no X11
export EDITOR='emacs -nw'
export VISUAL='emacs -nw' 

# 2.4) grep options
export GREP_OPTIONS='--color=auto'
export GREP_COLOR='1;31' # green for matches

# 2.5) sort options
# Ensures cross-platform sorting behavior of GNU sort.
# http://www.gnu.org/software/coreutils/faq/coreutils-faq.html#Sort-does-not-sort-in-normal-order_0021
unset LANG
export LC_ALL=POSIX


# ## ------------------------------
# ## -- 3) User-customized code  --
# ## ------------------------------

# ## Define any user-specific variables you want here.
source ~/.zshrc_custom


export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh" # This loads nvm
[[ -r $NVM_DIR/bash_completion ]] && \. $NVM_DIR/bash_completion