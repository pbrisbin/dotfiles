export XDG_CACHE_HOME=$HOME/.cache
export XDG_CONFIG_HOME=$HOME/.config
export XDG_DATA_HOME=$HOME/.local/share

export BROWSER=browser
export GNUPGHOME="$XDG_CONFIG_HOME"/gnupg
export GPG_TTY="$(tty)"
export FZF_DEFAULT_COMMAND='
  ( git ls-tree -r --name-only HEAD ||
    find . -path "*/\.*" -prune -o -type f -print -o -type l -print |
    sed s/^..//
  ) 2>/dev/null
'
export MANWIDTH=80

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
