# Login-shell environment; loaded before .zshrc.

# Keep the first occurrence of each PATH entry, including in nested shells.
typeset -U path PATH
for dotfiles_bin in "$HOME/bin" /usr/local/bin /usr/local/sbin; do
  if [[ -d "$dotfiles_bin" ]]; then
    path=("$dotfiles_bin" "${path[@]}")
  fi
done
unset dotfiles_bin

# Let the native Homebrew installation take precedence over system binaries.
if [[ $(uname -m) == arm64 && -x /opt/homebrew/bin/brew ]]; then
  eval "$(/opt/homebrew/bin/brew shellenv)"
elif [[ -x /usr/local/bin/brew ]]; then
  eval "$(/usr/local/bin/brew shellenv)"
fi
