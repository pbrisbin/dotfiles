zstyle ':omz:editor'            keymap         'vi'
zstyle ':omz:*:*'               color          'yes'
zstyle ':omz:terminal'          auto-title     'yes'
zstyle ':omz:*:*'               case-sensitive 'no'
zstyle ':omz:editor'            dot-expansion  'no'
zstyle ':omz:load'              plugin         'git'       \
                                               'rails'     \
                                               'ruby'      \
                                               'ssh-agent'
zstyle ':omz:plugins:ssh-agent' identities     'id_rsa'          \
                                               'id_rsa.pbrisbin' \
                                               'id_rsa.github'   \
                                               'id_rsa.ideeli'

zstyle ':omz:prompt' theme 'pbrisbin'

source "$HOME/.oh-my-zsh/init.zsh"

# aliases {{{
alias grep='grep --color=auto'
alias myip='printf "%s\n" "$(curl --silent http://tnx.nl/ip)"'
alias path='echo -e "${PATH//:/\n}"'
alias apptree='tree -I "dist|config|static|pandoc|tmp"'

if (( $+commands[mpc] )); then
  alias n='mpc next'
  alias p='mpc prev'

  if (( $+commands[albumbler] )); then
    alias a='albumbler'
  fi
fi

if [[ -b '/dev/sr0' ]]; then
  alias eject='eject -T /dev/sr0'
  alias mountdvd='sudo mount -t iso9660 -o ro /dev/sr0 /media/dvd/'
fi

if (( $+commands[mplayer] )); then
  alias playiso='mplayer dvd://1 -dvd-device'
  alias playdvd='mplayer dvdnav:// /dev/sr0'
  alias playcda='mplayer cdda:// -cdrom-device /dev/sr0 -cache 10000'
fi

alias updatehtpc='curl "http://htpc:8080/xbmcCmds/xbmcHttp?command=ExecBuiltIn&parameter=XBMC.updatelibrary(video)"'

# }}}

# functions {{{
ghc-pkg-clean() {
  (( $+commands[ghc-pkg] )) || return 1

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

ideeli() {
  case "$1" in
    up)
      sudo /etc/rc.d/mysqld start
      sudo /etc/rc.d/mongodb start
      sudo /etc/rc.d/memcached start
      sudo /etc/rc.d/memcached_secondary start
      sudo /etc/rc.d/riak start
      sudo /etc/rc.d/activemq start
      ;;
    down)
      sudo /etc/rc.d/activemq stop
      sudo /etc/rc.d/riak stop
      sudo /etc/rc.d/memcached_secondary stop
      sudo /etc/rc.d/memcached stop
      sudo /etc/rc.d/mongodb stop
      sudo /etc/rc.d/mysqld stop
      ;;
  esac
}

hdocs() {
  (( $+commands[cabal] )) || return 1

  local  name="${PWD##*/}"
  local  here="dist/doc/html/$name"
  local there="$HOME/Code/haskell/devsite/static/docs/haskell/$name"

  cabal haddock \
    --html-location='http://hackage.haskell.org/packages/archive/$pkg/latest/doc/html' \
    --hyperlink-source "$@" || return 1

  rm -rf "$there"
  cp -r "$here" "$there"
}

# update ruby documentation and publish it to my server
rdocs() {
  (( $+commands[rdoc] )) || return 1

  local name="${PWD##*/}"
  local there="$HOME/Code/haskell/devsite/static/docs/ruby/$name"

  rdoc --title="$name" "$@"

  rm -rf "$there"
  cp -r doc "$there"
}

combinepdf() {
  (( $+commands[gs] )) || return 1

  if [[ $# -ge 2 ]]; then
    local out="$1"; shift

    gs -dBATCH -dNOPAUSE -q -sDEVICE=pdfwrite -sOutputFile="$out" "$@"
  fi
}

hbrip() {
  (( $+commands[HandBrakeCLI] )) || return 1
  [[ -n "$1" ]] || return 1

  local name="$1" out drop="$HOME/Movies"; shift
  [[ -d "$drop" ]] || mkdir -p "$drop"

  out="$drop/$name.mp4"

  echo "rip /dev/sr0 --> $out"
  HandBrakeCLI -Z iPad "$@" -i /dev/sr0 -o "$out" 2>/dev/null
  echo
}

hbconvert() {
  (( $+commands[HandBrakeCLI] )) || return 1
  [[ -n "$1" ]] || return 1

  local in="$1" out drop="$HOME/Movies/converted"; shift
  [[ -d "$drop" ]] || mkdir -p "$drop"

  out="$drop/$(basename "${in%.*}").mp4"

  echo "convert $in --> $out"
  HandBrakeCLI -Z iPad "$@" -i "$in" -o "$out" 2>/dev/null
  echo
}

timer() {
  (( $+commands[zenity] )) || return 1

  if [[ -n $DISPLAY ]]; then
    local N="${1:-5m}"; shift

    (sleep $N && zenity --info --title="Time's Up" --text="${*:-DING}") &
    echo "timer set for $N"
  fi
}

newcomments() {
  if (( $+commands[psql] )); then
    psql -U pbrisbin pbrisbin << EOF
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

if [[ $(tty) == /dev/tty1 ]] && (( $UID )) && [[ -z $DISPLAY ]]; then
  exec startx
fi
