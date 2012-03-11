set -o vi

ZSH=$HOME/.oh-my-zsh

ZSH_THEME="zhann"

plugins=(git gem archlinux bundler rails rake sprunge ssh-agent vagrant vi-mode)

source $ZSH/oh-my-zsh.sh

zstyle :omz:plugins:ssh-agent id_rsa id_rsa.github id_rsa.ideeli

_is_linux() {
  [[ "$(uname -s)" =~ Linux\|GNU ]]
}

_is_arch() {
  [[ -f /etc/arch-release ]]
}

_is_root() {
  [[ $UID -eq 0 ]]
}

_is_x_running() {
  [[ -n "$DISPLAY" ]]
}

_have() {
  which $1 &>/dev/null
}

# aliases {{{
alias ls='ls -h --group-directories-first --color=auto'
alias grep='grep --color=auto'
alias myip='printf "%s\n" "$(curl --silent http://tnx.nl/ip)"'
alias path='echo -e "${PATH//:/\n}"'

alias apptree='tree -I "dist|config|static|pandoc|tmp"'

if _have mpc; then
  alias addall='mpc --no-status clear && mpc listall | mpc --no-status add && mpc play'
  alias n='mpc next'
  alias p='mpc prev'
fi

_have albumbler && alias a='albumbler'

if _have ossvol; then
  alias u='ossvol -i 3'
  alias d='ossvol -d 3'
  alias m='ossvol -t'
fi

if _have colortail; then
  alias tailirc='/usr/bin/colortail -q -k /etc/colortail/conf.irc'
  alias colortail='colortail -q -k /etc/colortail/conf.messages'
fi

if [[ -b '/dev/sr0' ]]; then
  alias eject='eject -T /dev/sr0'
  alias mountdvd='sudo mount -t iso9660 -o ro /dev/sr0 /media/dvd/'
fi

[[ -f "$HOME/.xmonad/xmonad.hs" ]] && alias checkmonad='(cd ~/.xmonad && ghci -ilib xmonad.hs)'

if _have mplayer; then
  alias playiso='mplayer dvd://1 -dvd-device'
  alias playdvd='mplayer dvdnav:// /dev/sr0'
  alias playcda='mplayer cdda:// -cdrom-device /dev/sr0 -cache 10000'
fi

if _have ghc-pkg; then
  alias gc='ghc-pkg check'
  alias gl='ghc-pkg list'
  alias gu='ghc-pkg unregister'
fi

alias updatehtpc='curl "http://htpc:8080/xbmcCmds/xbmcHttp?command=ExecBuiltIn&parameter=XBMC.updatelibrary(video)"'

# }}}

# functions {{{
ghc-pkg-clean() {
  _have ghc-pkg || return 1

  while read -r pkg; do
    echo "attempting to unregister $pkg..."
    ghc-pkg $* unregister $pkg
  done < <(ghc-pkg $* check |& sed '/^There are problems in package \([^:]*\):$/!d; s//\1/')
}

ghc-pkg-reset() {
  read -p 'Are you sure (yes/no)? ' ans

  if [[ "$ans" == 'yes' ]]; then
    rm -rf "$HOME/.cabal/packages"/*/*
    rm -rf "$HOME/.cabal/bin"/*
    rm -rf "$HOME/.ghc"
  fi
}

# start or stop services need for ideeli development
ideeli() {
  case "$1" in
    up)
      sudo /etc/rc.d/mysqld start
      sudo /etc/rc.d/mongodb start
      sudo /etc/rc.d/memcached --port 11211 start
      sudo /etc/rc.d/memcached --port 11212 start
      sudo /etc/rc.d/riak start
      sudo /etc/rc.d/activemq start
      ;;
    down)
      sudo /etc/rc.d/activemq stop
      sudo /etc/rc.d/riak stop
      sudo /etc/rc.d/memcached --port 11211 stop
      sudo /etc/rc.d/memcached --port 11212 stop
      sudo /etc/rc.d/mongodb stop
      sudo /etc/rc.d/mysqld stop
      ;;
  esac
}

# update haskell documentation and publish it to my server
hdocs() {
  _have cabal || return 1

  local  name="${PWD##*/}"
  local  here="dist/doc/html/$name"
  local there="$HOME/Code/haskell/devsite/static/docs/haskell/$name"

  # update
  cabal haddock \
    --html-location='http://hackage.haskell.org/packages/archive/$pkg/latest/doc/html' \
    --hyperlink-source "$@" || return 1

  # publish
  rm -rf "$there"
  cp -r "$here" "$there"
}

# update ruby documentation and publish it to my server
rdocs() {
  _have rdoc || return 1

  local name="${PWD##*/}"
  local there="$HOME/Code/haskell/devsite/static/docs/ruby/$name"

  # update
  rdoc --title="$name" "$@"

  # publish
  rm -rf "$there"
  cp -r doc "$there"
}

# combine pdfs into one using ghostscript
combinepdf() {
  _have gs       || return 1
  [[ $# -ge 2 ]] || return 1

  local out="$1"; shift

  gs -dBATCH -dNOPAUSE -q -sDEVICE=pdfwrite -sOutputFile="$out" "$@"
}

# rip a dvd with handbrake
hbrip() {
  _have HandBrakeCLI || return 1
  [[ -n "$1" ]]      || return 1

  local name="$1" out drop="$HOME/Movies"; shift
  [[ -d "$drop" ]] || mkdir -p "$drop"

  out="$drop/$name.mp4"

  echo "rip /dev/sr0 --> $out"
  HandBrakeCLI -Z iPad "$@" -i /dev/sr0 -o "$out" 2>/dev/null
  echo
}

# convert media to ipad format with handbrake
hbconvert() {
  _have HandBrakeCLI || return 1
  [[ -n "$1" ]]      || return 1

  local in="$1" out drop="$HOME/Movies/converted"; shift
  [[ -d "$drop" ]] || mkdir -p "$drop"

  out="$drop/$(basename "${in%.*}").mp4"

  echo "convert $in --> $out"
  HandBrakeCLI -Z iPad "$@" -i "$in" -o "$out" 2>/dev/null
  echo
}

# set an ad-hoc GUI timer 
timer() {
  _is_x_running || return 1
  _have zenity  || return 1

  local N="${1:-5m}"; shift

  (sleep $N && zenity --info --title="Time's Up" --text="${*:-DING}") &
  echo "timer set for $N"
}

runsql() {
  if _is_linux; then
    _have psql || return 1
    psql -U pbrisbin pbrisbin;
  else
    _have mysql || return 1
    mysql -urails -pdev ideeli_development;
  fi
}

newcomments() {
  if _is_linux; then
    runsql << EOF
select
  id,
  "threadId",
  "timeStamp",
  "userEmail",
  substring("content", 1, 60)
from "SqlComment"
order by "timeStamp" asc;
EOF
  fi
}

# }}}

unset -f _is_linux
unset -f _is_arch
unset -f _is_root
unset -f _is_x_running
unset -f _have

if [[ $(tty) = /dev/tty1 ]] && ! _is_root && ! _is_x_running; then
  exec startx
fi
