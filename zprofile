# vim: ft=zsh
export XDG_CACHE_HOME=$HOME/.cache
export XDG_CONFIG_HOME=$HOME/.config
export XDG_DATA_HOME=$HOME/.local/share

export BROWSER=/usr/bin/xdg-open
export GNUPGHOME="$XDG_CONFIG_HOME"/gnupg
export LS_COLORS="$(vivid generate nord)"
export MANWIDTH=80
export PAGER="less -R"
export RCRC="$XDG_CONFIG_HOME"/rcm/rcrc

# Prevents alacritty from screwing up on multiple monitors
export WINIT_X11_SCALE_FACTOR=1
