## -----------------------
## -- 1) Import .bashrc --
## -----------------------

# Factor out all repeated profile initialization into .zshrc
#  - All non-login shell parameters go there
#  - All declarations repeated for each screen session go there
if [ -f ~/.bashrc ]; then
   source ~/.zshrc
fi

# Configure PATH
#  - These are line by line so that you can kill one without affecting the others.
#  - Lowest priority first, highest priority last.
export PATH=$PATH
export PATH=$HOME/bin:$PATH
export PATH=/usr/bin:$PATH
export PATH=/usr/local/bin:$PATH
export PATH=/usr/local/sbin:$PATH
export PATH="$HOME/.cask/bin:$PATH"  # emacs cask

# virtualenv wrapper
export WORKON_HOME=$HOME/.virtualenvs
source /usr/local/bin/virtualenvwrapper.sh