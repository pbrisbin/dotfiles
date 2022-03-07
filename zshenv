export XDG_CACHE_HOME=$HOME/.cache
export XDG_CONFIG_HOME=$HOME/.config
export XDG_DATA_HOME=$HOME/.local/share

export BROWSER=browser
export GNUPGHOME="$XDG_CONFIG_HOME"/gnupg
export GPG_TTY="$(tty)"
export MANWIDTH=80
export RCRC="$XDG_CONFIG_HOME"/rcm/rcrc

# Prevents alacritty from screwing up on multiple monitors
export WINIT_X11_SCALE_FACTOR=1

HISTSIZE=500000
SAVEHIST=$HISTSIZE

cdpath=(
  ~
  ~/code
  ~/code/freckle
  ~/code/pbrisbin
  ~/code/restyled-io/
  $cdpath
)
fpath=(
  ~/.local/share/zsh/site-functions
  $fpath
)
path=(
  ~/.local/bin
  $(ruby -r rubygems -e "puts Gem.user_dir")/bin
  $path
)
