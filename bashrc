# ~/.bashrc
#
# pbrisbin 2010
#
# an attempt at a monolithic/portable bashrc
#
###

# get out if non-interactive
[[ -z "$PS1" ]] && return

### General options {{{

# is $1 installed?
_have() { which "$1" &>/dev/null; }

[[ -f $HOME/.dircolors ]] && eval $(/bin/dircolors -b $HOME/.dircolors)

if [[ -f /etc/bash_completion ]]; then
  source /etc/bash_completion
  _have sudo && complete -cf sudo
fi

# bash > 4 has ./** support
[[ ${BASH_VERSINFO[0]} -ge 4 ]] && shopt -s globstar

shopt -s checkwinsize
shopt -s extglob

# list of apps to be tried in order
xbrowsers='browser:chromium:google-chrome:firefox'
browsers='elinks:lynx:links'
editors='vim:vi'

# }}}

### Overall conditionals/functions {{{

_islinux=false
[[ "$(uname -s)" =~ Linux|GNU|GNU/* ]] && _islinux=true

_isarch=false
[[ -f /etc/arch-release ]] && _isarch=true

_isxrunning=false
[[ -n "$DISPLAY" ]] && _isxrunning=true

_isroot=false
[[ $UID -eq 0 ]] && _isroot=true

# set $EDITOR
_set_editor() {
  local IFS=':' editor

  for editor in $editors; do
    editor="$(which $editor 2>/dev/null)"

    if [[ -x "$editor" ]]; then
      export EDITOR="$editor"
      export VISUAL="$EDITOR"
      break
    fi
  done
}

# set $BROWSER
_set_browser() {
  local IFS=':' _browsers="$*" browser

  for browser in $_browsers; do
    browser="$(which $browser 2>/dev/null)"

    if [[ -x "$browser" ]]; then
      export BROWSER="$browser"
      break
    fi
  done
}

# add directories to $PATH
_add_to_path() {
  local path

  for path in "$@"; do
    [[ -d "$path" ]] && [[ ! ":${PATH}:" =~ :${path}: ]] && export PATH=${path}:$PATH
  done
}

# ssh-agent stuff
#_ssh_env="$HOME/.ssh/environment"
#
#_start_agent() {
#  _have ssh-agent || return 1
#
#  ssh-agent | sed 's/^echo/#echo/g' > "$_ssh_env"
#
#  chmod 600 "$_ssh_env"
#  . "$_ssh_env" >/dev/null
#
#  ssh-add
#}
#
#if [[ -f "$_ssh_env" ]]; then
#  . "$_ssh_env" >/dev/null
#  if ! ps "$SSH_AGENT_PID" | grep 'ssh-agent$'; then
#    _start_agent
#  fi
#else
#  _start_agent
#fi

# }}}

### Bash exports {{{

# set path
_add_to_path "$HOME/.bin" "$HOME/Code/bin" '/opt/android-sdk/tools'

# set browser
$_isxrunning && _set_browser "$xbrowsers" || _set_browser "$browsers"

# set editor
_set_editor

# custom ip var
[[ -f "$HOME/.myip" ]] && export MYIP=$(cat "$HOME/.myip")

# custom log directory
[[ -d "$HOME/.logs" ]] && export LOGS="$HOME/.logs" || export LOGS='/tmp'

# screen tricks
if [[ -d "$HOME/.screen/configs" ]]; then
  export SCREEN_CONF_DIR="$HOME/.screen/configs"
  export SCREEN_CONF='main'
fi

# standard
export HISTSIZE=1000000
export HISTFILESIZE=1000000
export HISTTIMEFORMAT='%Y/%m/%d %H:%M '
export HISTIGNORE='&:ls:ll:la:cd:exit:clear:history'
export LANG=en_US.UTF-8
export LC_ALL=en_US.utf8

# less
if _have less; then
  export PAGER=less

  # these look terrible on a mac or in console
  if $_islinux && [[ "$TERM" != 'linux' ]]; then
    export LESS_TERMCAP_mb=$'\E[01;31m'       # begin blinking
    export LESS_TERMCAP_md=$'\E[01;38;5;74m'  # begin bold
    export LESS_TERMCAP_me=$'\E[0m'           # end mode
    export LESS_TERMCAP_se=$'\E[0m'           # end standout-mode
    export LESS_TERMCAP_so=$'\E[38;5;246m'    # begin standout-mode - info box
    export LESS_TERMCAP_ue=$'\E[0m'           # end underline
    export LESS_TERMCAP_us=$'\E[04;38;5;146m' # begin underline
  fi
fi

export MPD_HOST=192.168.0.5
export MPD_PORT=6600

# }}}

### Bash aliases {{{

# ssh
alias blue='ssh patrick@blue'
alias howie='ssh patrick@howard'
alias htpc='ssh xbmc@htpc'
alias susie='ssh patrick@susan'

# only for linux
if $_islinux; then
  alias dir='dir --color'
  alias ls='ls -h --group-directories-first --color=auto'
else
  alias ls='ls -h'
fi

# standard
alias la='ls -la'
alias ll='ls -l'

alias ..='cd ..'
alias ...='cd ../..'
alias cd..='cd ..'
alias cd...='cd ../..'

alias df='df -h'
alias grep='grep --color=auto'
alias mkdir='mkdir -p'
alias myip='lynx -dump http://tnx.nl/ip'
alias nocomment='grep -Ev "^\s*(#|$)"'
alias path='echo -e "${PATH//:/\n}"'
alias rawcode='grep -cv ^#\|^$'
alias sizeof='du -sh'
alias xmodocs='$BROWSER "http://www.eng.uwaterloo.ca/~aavogt/xmonad/docs/"'

# only if we have mpc
if _have mpc; then
  alias np='mpc --format "  %title% by %artist% #[%album%#]" | head -n1'

  alias add='mpc add'
  alias addall='mpc --no-status clear && mpc ls | mpc --no-status add && mpc --no-status play; np'

  alias P='mpc --no-status toggle; np'
  alias n='mpc --no-status next; np'
  alias p='mpc --no-status prev; np'
  alias s='mpc --no-status stop'
fi

if _have ossvol; then
  alias u='ossvol -i 3'
  alias d='ossvol -d 3'
  alias m='ossvol -t'
fi

# only if we have a screen_conf
if [[ -d "$SCREEN_CONF_DIR" ]]; then
  alias clean='SCREEN_CONF=clean screen -S clean -D -R clean'

  if _have rtorrent; then
    alias rtorrent='SCREEN_CONF=rtorrent screen -S rtorrent -R -D rtorrent'
    alias gtorrent='SCREEN_CONF=gtorrent screen -S gtorrent -R -D gtorrent'
  fi

  _have irssi && alias irssi='SCREEN_CONF=irssi screen -S irssi -R -D irssi'
fi

if _have colortail; then
  alias tailirc='/usr/bin/colortail -q -k /etc/colortail/conf.irc'
  alias colortail='colortail -q -k /etc/colortail/conf.messages'
fi

# need certain apps
_have mytime    && alias t='mytime'
_have curlpaste && alias pastie='curlpaste -s ghost -n pbrisbin'
_have hoogle    && alias hoogle='hoogle --color=true --n=10'
_have tree      && alias lt='tree'
_have x11vnc    && alias vncup='x11vnc -nopw -ncache 10 -display :0 -localhost'
_have xprop     && alias xp='xprop | grep "^WM_WINDOW_ROLE\|^WM_CLASS\|^WM_NAME"'

# only if we have a disc drive
[[ -b '/dev/sr0' ]] && alias eject='eject -T /dev/sr0'

# only if we have xmonad
[[ -f "$HOME/.xmonad/xmonad.hs" ]] && alias checkmonad='(cd ~/.xmonad && ghci -ilib xmonad.hs)'

# only for mplayer
if _have mplayer; then
  alias playiso='mplayer dvd://1 -dvd-device'
  alias playdvd='mplayer dvdnav:// /dev/sr0'
  alias playcda='mplayer cdda:// -cdrom-device /dev/sr0 -cache 10000'

  alias wers='mplayer -playlist http://wers.org/wers.asx'
fi

# only for rdesktop
if _have rdesktop; then
  alias rdp='rdesktop -K -g 1280x1040'
  alias rdpat='rdesktop -K -g 1280x1040 -d GREENBEACON -u PBrisbin -p -'
  alias rdfaf='rdesktop -K -g 1280x1040 -d FAF -u pbrisbin www.faf.com -p -'
  alias rdste='rdesktop -K -g 1280x1040 -d STEINER -u pbrisbin -p -'
fi

# we're on ARCH
if $_isarch; then

  # we're not root
  if ! $_isroot; then

    # pacman-color is installed
    if _have pacman-color; then
      alias pacman='sudo pacman-color'

      alias pacsearch='pacman-color -Ss'
      alias pacin='sudo pacman-color -S'
      alias pacout='sudo pacman-color -R'
      alias pacup='sudo pacman-color -Syu'
      alias pacorphans='sudo pacman-color -Rs $(/usr/bin/pacman -Qtdq)'
      alias paccorrupt='sudo find /var/cache/pacman/pkg -name '\''*.part.*'\'''
      alias pactesting='pacman-color -Q $(/usr/bin/pacman -Sql {community-,}testing) 2>/dev/null'

    # pacman-color not installed
    else
      alias pacman='sudo pacman'

      alias pacsearch='/usr/bin/pacman -Ss'
      alias pacin='pacman -S'
      alias pacout='pacman -R'
      alias pacup='pacman -Syu'
      alias pacorphans='pacman -Rs $(/usr/bin/pacman -Qtdq)'
      alias paccorrupt='sudo find /var/cache/pacman/pkg -name '\''*.part.*'\'''
      alias pactesting='/usr/bin/pacman -Q $(/usr/bin/pacman -Sql {community-,}testing) 2>/dev/null'
    fi

  # we are root
  else
      alias pacman &>/dev/null && unalias pacman

      alias pacsearch='pacman -Ss'
      alias pacin='pacman -S'
      alias pacout='pacman -R'
      alias pacup='pacman -Syu'
      alias pacorphans='pacman -Rs $(pacman -Qtdq)'
      alias paccorrupt='find /var/cache/pacman/pkg -name '\''*.part.*'\'''
      alias pactesting='pacman -Q $(pacman -Sql {community-,}testing) 2>/dev/null'
  fi
fi

# }}}

### Bash functions {{{

# combine pdfs into one using ghostscript
combinepdf() {
  _have gs || return 1

  local out="$1"; shift

  gs -dBATCH -dNOPAUSE -q -sDEVICE=pdfwrite -sOutputFile="$out" "$@"
}

# add by artist to mpc 
addartist() {
  _have mpc || return 1

  mpc search artist "$*" | mpc add &>/dev/null
  mpc play
}

# make a thumb %20 the size of a pic 
thumbit() {
  _have mogrify || return 1

  for pic in "$@"; do
    case "$pic" in
      *.jpg)  thumb="${pic/.jpg/-thumb.jpg}"   ;;
      *.jpeg) thumb="${pic/.jpeg/-thumb.jpeg}" ;;
      *.png)  thumb="${pic/.png/-thumb.png}"   ;;
      *.bmp)  thumb="${pic/.bmp/-thumb.bmp}"   ;;
    esac

    [[ -z "$thumb" ]] && return 1

    cp "$pic" "$thumb" && mogrify -resize 20% "$thumb"
  done
}

# convert any media for transfer to my ipad
iconvert() {
  local drop="$HOME/Movies/converted" infile outfile hb

  hb="$(which HandBrakeCLI 2>/dev/null)"
  [[ -z "$hb" ]] && return 1

  [[ -d "$drop" ]] || mkdir -p "$drop"

  # allow quality override
  if [[ "$1" = '-q' ]]; then
    hb="$hb -q $2"
    shift 2
  fi

  for infile in "$@"; do
    # handle /dev/sr0 input
    if [[ -b "$infile" ]]; then
      outfile="$drop/movie.mp4"
    else
      outfile="$drop/$(basename "${infile%.*}").mp4"
    fi

    echo "convert $infile --> $outfile"
    $hb -Z iPad -i "$infile" -o "$outfile" 2>/dev/null
    echo
  done
}

# share a file out of my public dropbox
#dbshare() {
#  local db="$HOME/Dropbox/Public" uid='472835' file url
#
#  file="${1/$db/}"
#
#  [[ -f "$db/$file" ]] || return 1
#
#  url="http://dl.dropbox.com/u/$uid/$file"
#
#  echo -n "$url" | xlip 2>/dev/null
#
#  echo "$url"
#}

# simple spellchecker, uses /usr/share/dict/words
spellcheck() {
  [[ -f /usr/share/dict/words ]] || return 1

  for word in "$@"; do
    if grep -Fqx "$word" /usr/share/dict/words; then
      echo -e "\e[1;32m$word\e[0m" # green
    else
      echo -e "\e[1;31m$word\e[0m" # red
    fi
  done
}

# go to google for anything
google() {
  [[ -z "$BROWSER" ]] && return 1

  local term="${*:-$(xclip -o)}"

  $BROWSER "http://www.google.com/search?q=${term// /+}" &>/dev/null &
}

# go to google for a definition 
#define() {
#  _have lynx || return 1
#
#  local lang charset tmp
#
#  lang="${LANG%%_*}"
#  charset="${LANG##*.}"
#  tmp='/tmp/define'
#  
#  lynx -accept_all_cookies \
#       -dump \
#       -hiddenlinks=ignore \
#       -nonumbers \
#       -assume_charset="$charset" \
#       -display_charset="$charset" \
#       "http://www.google.com/search?hl=$lang&q=define%3A+$1&btnG=Google+Search" | grep -m 5 -C 2 -A 5 -w "*" > "$tmp"
#
#  if [[ ! -s "$tmp" ]]; then
#    echo -e "No definition found.\n"
#  else
#    echo -e "$(grep -v Search "$tmp" | sed "s/$1/\\\e[1;32m&\\\e[0m/g")\n"
#  fi
#
#  rm -f "$tmp"
#}

#
# man readlink
#
# accepts a relative path and returns an absolute 
#rel2abs() {
#  local file dir
#
#  file="$(basename "$1")"
#  dir="$(dirname "$1")"
#
#  pushd "${dir:-./}" &>/dev/null || return 1
#  echo "$PWD/$file"
#  popd &>/dev/null
#}

# grep by paragraph 
grepp() { perl -00ne "print if /$1/i" < "$2"; }

# pull a single file out of a .tar.gz, stops on first match
# useful for .PKGINFO files in .pkg.tar.gz files
pullout() {
  $_islinux || return 1

  [[ "$2" =~ .tar.gz$|.tgz$ ]] || return 1

  gunzip < "$2" | bsdtar -qxf - "$1"
}

# recursively 'fix' dir/file perm
fix() {
  local dir

  for dir in "$@"; do
    find "$dir" -type d -exec chmod 755 {} \; 
    find "$dir" -type f -exec chmod 644 {} \;
  done
}

# manage services 
service() {
  $_isarch || return 1
  
  sudo /etc/rc.d/$1 $2
}

# print docs to default printer in reverse page order 
printr() {
  _have enscript || return 1

  local file

  # files on commandline
  if [[ $# -ne 0 ]]; then
    for file in "$@"; do
      enscript -p - "$file" | psselect -r | lp
    done
  # try stdin
  else
    local IFS=$'\n'

    (while read -r; do
      echo "$REPLY"
    done) | enscript -p - | psselect -r | lp
  fi
}

# print a webpage to pdf
web2pdf() {
  $_isxrunning || return 1
  _have wkhtmltopdf || return 1

  local file="$1"; shift

  for url in "$@"; do
    wget -O - "$url" | wkhtmltopdf --no-background --page-size A4 --margin-top 20 --margin-bottom 20 --margin-left 24 - "$file"
  done
}

# print a webpage from CLI
web2paper() {
  $_isxrunning || return 1
  _have wkhtmltopdf || return 1

  local tmp='/tmp/webpage.pdf'

  for url in "$@"; do
    wget -O - "$url" | wkhtmltopdf --no-background --page-size A4 --margin-top 20 --margin-bottom 20 --margin-left 24 - "$tmp"
    enscript -p - "$tmp" | psselect -r | lp && rm /tmp/webpage.pdf
  done
}

# set an ad-hoc GUI timer 
timer() {
  $_isxrunning || return 1
  _have zenity || return 1

  local N=$1; shift

  (sleep $N && zenity --info --title="Time's Up" --text="${*:-DING}") &
  echo "timer set for $N"
}

# auto send an attachment from CLI 
send() {
  _have mutt || return 1

  echo 'Auto-sent from linux. Please see attached.' | mutt -s 'File Attached' -a "$1" "$2"
}

# simple calculator 
calc() {
  if _have bc; then
    echo "scale=3; $*" | bc -l
  else
    awk "BEGIN { print $* }"
  fi
}

# run a bash script in 'debug' mode
debug() {
  local script="$1"; shift

  if _have "$script"; then
    PS4='+$LINENO:$FUNCNAME: ' bash -x "$script" "$@"
  fi
}

# go to a directory or file's parent
goto() { [[ -d "$1" ]] && cd "$1" || cd "$(dirname "$1")"; }

# copy and follow
cpf() { cp "$@" && goto "$_"; }

# move and follow
mvf() { mv "$@" && goto "$_"; }

# print the url to a manpage
webman() { echo "http://unixhelp.ed.ac.uk/CGI/man-cgi?$1"; }

# }}}

### Titlebar and Prompt {{{
# order matters here

# set the default titlebar
PROMPT_COMMAND='echo -ne "\0033]0;${HOSTNAME} ${PWD/$HOME/~}\007"'

# set a crazy-ass ssh titlebar -- uptime broken on mac os x
#[[ -n "$SSH_TTY" ]] && PROMPT_COMMAND='echo -ne "\033]0;$USER@$HOSTNAME$(who -m | sed -e "s%^.* \(pts/[0-9]*\).*(\(.*\))%[\1] (\2)%g")[$(uptime | sed -e "s/.*: \([^,]*\).*/\1/" -e "s/ //g")/$(ps aux | wc -l)]\007"'

# nothing in console
[[ $TERM == 'linux' ]] && PROMPT_COMMAND=''

export PROMPT_COMMAND

# set colors for PS1
nrm='\[\e[0m\]'         # normal
wht='\[\e[1;37m\]'      # bold white

# root is bold red, user is bold blue
$_isroot && bld='\[\e[1;31m\]' || bld='\[\e[1;34m\]'

# change prompt behavior in screen -- why does this never work?!
[[ -n "$STY" ]] && _screen='\[\ek\e\\\]\[\ek\w\e\\\]' || _screen=''

#export PS1="$bld//$wht\h$bld/$wht\$?$bld/$wht\w/ $nrm"
export PS1="$_screen$bld//$wht\h$bld/$wht\$?$bld/$wht\w/ $nrm"
export PS2="$bld// $nrm"

# fallback prompt
#PS1='[ \A ][ \w ]\n> '
#PS2='>'

# }}}

### Starting X {{{

# auto startx if on tty1 and logout if/when X ends
if [[ $(tty) = /dev/tty1 ]] && ! $_isroot && ! $_isxrunning; then
  _set_browser "$xbrowsers"

  startx | tee "$LOGS/X.log"
  logout
fi

# }}}
