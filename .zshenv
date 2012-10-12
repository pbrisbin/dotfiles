if [[ "$OSTYPE" == darwin* ]]; then
  export BROWSER='open'
else
  # fix the incorrect source order on Arch
  emulate sh -c 'source /etc/profile'
  export BROWSER='chromium'
fi

export EDITOR='vim'
export VISUAL='vim'
export PAGER='less'

typeset -gU fpath cdpath manpath path

fpath=(
  /usr/local/share/zsh-completions
  $fpath
)

cdpath=(
  "$HOME/Code"
  $cdpath
)

manpath=(
  /usr/local/share/man
  /usr/share/man
  $manpath
)

path=(
  "$HOME/.bin"
  "$HOME/.cabal/bin"
  "$HOME/Code/bin"

  /usr/local/{bin,sbin}
  /usr/{bin,sbin}
  /{bin,sbin}
  $path
)

# if we have brew, add gnu coreutils to path
if [[ "$OSTYPE" == darwin* ]] && (( $+commands[brew] )); then
  path=( "$(brew --prefix coreutils)/libexec/gnubin" $path )
fi

# likely user gem home
gem_home="$HOME/.gem/ruby/1.9.1"

if [[ -d "$gem_home" ]]; then
  export GEM_HOME="$gem_home"
  path=( "$GEM_HOME/bin" $path )
fi

if (( $+commands[albumart.php] )); then
  export AWS_LIB="$HOME/Code/php/albumart/lib"
  export AWS_CERT_FILE="$HOME/.aws/cert-67RVMJTXXBDL4ZZOYSYBI3A7ZP56N3XD.pem"
  export AWS_PRIVATE_KEY_FILE="$HOME/.aws/pk-67RVMJTXXBDL4ZZOYSYBI3A7ZP56N3XD.pem"
fi

if (( $+commands[dmenu] )); then
  # dmenu-xft required
  export DMENU_OPTIONS='-i -fn Verdana-8 -nb #303030 -nf #909090 -sb #909090 -sf #303030'
fi

if (( $+commands[mpc] )); then
  export MPD_HOST=192.168.0.5
  export MPD_PORT=6600
fi

# TODO: rescue nonexistence
source "$HOME/.screen/screen.sh"
source "$HOME/.aws_keys"
source "$HOME/.secrets"

# start x if appropriate
[[ $TTY == /dev/tty1 ]] \
  && (( $UID ))         \
  && [[ -z $DISPLAY ]]  \
  && exec startx
