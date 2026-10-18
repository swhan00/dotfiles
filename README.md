# dotfiles

Zsh and Emacs configuration for macOS. Move any existing configuration files
aside before creating the symlinks below.

```sh
git clone git@github.com:swhan00/dotfiles.git "$HOME/dotfiles"
ln -s "$HOME/dotfiles/.zprofile" "$HOME/.zprofile"
ln -s "$HOME/dotfiles/.zshrc" "$HOME/.zshrc"
ln -s "$HOME/dotfiles/.zshrc_custom" "$HOME/.zshrc_custom"
ln -s "$HOME/dotfiles/.emacs.d" "$HOME/.emacs.d"
```

- `.zprofile` configures login-shell PATH entries and Homebrew on Apple Silicon
  or Intel Macs.
- `.zshrc` contains shared interactive-shell behavior and loads `.zshrc_custom`.
- `.zshrc_custom` contains locale settings and optional tools. Python uses pyenv
  with pyenv-virtualenv when available; Node.js uses nvm. Tool paths are added
  only when installed, and repeated PATH entries are removed.
- `~/.zshrc_local` is an optional final override for machine-specific settings
  and credentials. It is ignored by Git and is never committed here.

The default locale is `ko_KR.UTF-8`. Override `LANG` or individual `LC_*`
categories in `~/.zshrc_local` if needed; use `LC_ALL=C sort` for bytewise sorting.
