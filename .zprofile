# Description: This file is loaded before the shell starts in login mode.
#              This file is loaded before .zshrc.

# Configure Homebrew
eval "$(/opt/homebrew/bin/brew shellenv)"


# Configure PATH
#  - These are line by line so that you can kill one without affecting the others.
#  - Lowest priority first, highest priority last.
export PATH=$PATH
export PATH=$HOME/bin:$PATH
export PATH=/usr/bin:$PATH
export PATH=/usr/local/bin:$PATH
export PATH=/usr/local/sbin:$PATH

