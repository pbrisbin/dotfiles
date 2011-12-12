# ~/.bashrc
#
# pbrisbin 2010
#
# an attempt at a monolithic/portable bashrc
#
###

# get out if non-interactive
[[ $- != *i* ]] && return

### General options {{{

# is $1 installed?
_have() { which "$1" &>/dev/null; }

if [[ -f "$HOME/.lscolors" ]] && [[ $(tput colors) == "256" ]]; then
  # https://github.com/trapd00r/LS_COLORS
  _have dircolors && eval $( dircolors -b $HOME/.lscolors )
fi

if [[ -f /etc/bash_completion ]]; then
  . /etc/bash_completion
  _have sudo && complete -cf sudo
fi

# macports path
if [[ -f /opt/local/etc/bash_completion ]]; then
  . /opt/local/etc/bash_completion
  _have sudo && complete -cf sudo
fi

# bash 4 features
if [[ ${BASH_VERSINFO[0]} -ge 4 ]]; then
  shopt -s globstar autocd dirspell 
fi

shopt -s cdspell extglob histverify no_empty_cmd_completion checkwinsize

# should've done this a long time ago
set -o vi

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

  for path; do
    [[ -d "$path" ]] && [[ ! ":${PATH}:" =~ :${path}: ]] && export PATH=${path}:$PATH
  done
}

# source a file if readable
_source () {
  local file="$1"
  [[ -r "$file" ]] || return 1
  . "$file"
}

# ssh-agent stuff
_ssh_env="$HOME/.ssh/environment"

_start_agent() {
  [[ -d "$HOME/.ssh" ]] || return 1
  _have ssh-agent       || return 1

  local key keyfile

  ssh-agent | sed 's/^echo/#echo/g' > "$_ssh_env"

  chmod 600 "$_ssh_env"
  . "$_ssh_env" >/dev/null

  for key in id_rsa id_rsa.pbrisbin id_rsa.github; do
    keyfile="$HOME/.ssh/$key"
    if [[ -r "$keyfile" ]]; then
      ssh-add "$keyfile"
    fi
  done
}

if [[ -f "$_ssh_env" ]]; then
  . "$_ssh_env" >/dev/null
  if ! ps "$SSH_AGENT_PID" | grep -q 'ssh-agent$'; then
    _start_agent
  fi
else
  _start_agent
fi

# }}}

### Bash exports {{{

# set path
_add_to_path "$HOME/.bin" "$HOME/Code/bin" "$HOME/.cabal/bin" "$HOME/.rvm/bin"

# some custom paths used on mac os x
_add_to_path /opt/local/libexec/gnubin \
             /opt/local/bin            \
             /opt/local/sbin           \
             /usr/local/mongodb/bin

# set browser
$_isxrunning && _set_browser "$xbrowsers" || _set_browser "$browsers"

# set editor
_set_editor

# custom ip var
[[ -f "$HOME/.myip" ]] && export MYIP=$(cat "$HOME/.myip")

# custom log directory
[[ -d "$HOME/.logs" ]] && export LOGS="$HOME/.logs" || export LOGS='/tmp'

# screen tricks
_source "$HOME/.screen/bashrc.screen"

# raw AWS keys stored and exported in separate file
_source "$HOME/.aws_keys"

# albumart.php
if _have albumart.php; then
  export AWS_LIB="$HOME/Code/php/albumart/lib"
  export AWS_CERT_FILE="$HOME/.aws/cert-67RVMJTXXBDL4ZZOYSYBI3A7ZP56N3XD.pem"
  export AWS_PRIVATE_KEY_FILE="$HOME/.aws/pk-67RVMJTXXBDL4ZZOYSYBI3A7ZP56N3XD.pem"
fi

# dmenu options
if _have dmenu; then
  # dmenu-xft required
  export DMENU_OPTIONS='-i -fn Verdana-8 -nb #303030 -nf #909090 -sb #909090 -sf #303030'
fi

# standard in linux
if $_islinux; then
  export LANG=en_US.UTF-8
  export LC_ALL=en_US.utf8
fi

HISTIGNORE="&:ls:[bf]g:exit:reset:clear:cd*"
HISTCONTROL="ignoreboth:erasedups"
HISTSIZE=1000000
HISTFILESIZE=1000000
export ${!HIST@}

# less
if _have less; then
  export PAGER=less

  LESS=-R # use -X to avoid sending terminal initialization
  LESS_TERMCAP_mb=$'\e[01;31m'
  LESS_TERMCAP_md=$'\e[01;31m'
  LESS_TERMCAP_me=$'\e[0m'
  LESS_TERMCAP_se=$'\e[0m'
  LESS_TERMCAP_so=$'\e[01;44;33m'
  LESS_TERMCAP_ue=$'\e[0m'
  LESS_TERMCAP_us=$'\e[01;32m'
  export ${!LESS@}
fi

# REE GC optimizations
RUBY_HEAP_MIN_SLOTS=2400000
RUBY_HEAP_SLOTS_INCREMENT=100000
RUBY_HEAP_SLOTS_GROWTH_FACTOR=1
RUBY_GC_MALLOC_LIMIT=40000000
export ${!RUBY_@}

if _have mpc; then
  export MPD_HOST=192.168.0.5
  export MPD_PORT=6600
fi

_source "$HOME/.rvm/scripts/rvm"

# }}}

### Bash aliases {{{

alias ls='ls -h --group-directories-first --color=auto'
alias grep='grep --color=auto'
alias myip='printf "%s\n" "$(curl --silent http://tnx.nl/ip)"'
alias path='echo -e "${PATH//:/\n}"'

# a good overview of a yesod application
alias apptree='tree -I "dist|config|tmp|pandoc"'

# only if we have mpc
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

# only if we have a disc drive
if [[ -b '/dev/sr0' ]]; then
  alias eject='eject -T /dev/sr0'
  alias mountdvd='sudo mount -t iso9660 -o ro /dev/sr0 /media/dvd/'
fi

# only if we have xmonad
[[ -f "$HOME/.xmonad/xmonad.hs" ]] && alias checkmonad='(cd ~/.xmonad && ghci -ilib xmonad.hs)'

# only for mplayer
if _have mplayer; then
  alias playiso='mplayer dvd://1 -dvd-device'
  alias playdvd='mplayer dvdnav:// /dev/sr0'
  alias playcda='mplayer cdda:// -cdrom-device /dev/sr0 -cache 10000'
fi

# pacman aliases
if $_isarch; then
  if ! $_isroot; then
    _have pacman-color && alias pacman='sudo pacman-color' || alias pacman='sudo pacman'
  else
    _have pacman-color && alias pacman='pacman-color'
  fi

  alias pacorphans='pacman -Rs $(pacman -Qtdq)'
  alias paccorrupt='sudo find /var/cache/pacman/pkg -name '\''*.part.*'\''' # sudo so we can quickly add -delete
  alias pactesting='pacman -Q $(pacman -Sql {community-,multilib-,}testing) 2>/dev/null'
fi

# ghc aliases
if _have ghc-pkg; then
  alias gc='ghc-pkg check'
  alias gl='ghc-pkg list'
  alias gu='ghc-pkg unregister'
fi

# }}}

### Bash functions {{{

# demolish any --user installed cabal packages.
cabalwipe() {
  rm -rf "$HOME/.cabal/packages"/*/*
  rm -rf "$HOME/.cabal/bin"/*
  rm -rf "$HOME/.ghc"
}

ideeliup() {
  $_isarch || return 1

  sudo /etc/rc.d/mysql start
  sudo /etc/rc.d/mongodb start
  sudo /etc/rc.d/memcached --port 11211 start
  sudo /etc/rc.d/memcached --port 11212 start
  sudo /etc/rc.d/riak start
  sudo /etc/rc.d/activemq start

  rvm use ree
}

ideelidown() {
  $_isarch || return 1

  sudo /etc/rc.d/activemq stop
  sudo /etc/rc.d/riak stop
  sudo /etc/rc.d/memcached --port 11211 stop
  sudo /etc/rc.d/memcached --port 11212 stop
  sudo /etc/rc.d/mongodb stop
  sudo /etc/rc.d/mysql stop

  rvm use ruby-1.9.3
}

# run a rails test suite via the parallel_tests gem
runtests() {
  local suite="${1:-(unit|functional|integration)}"
  local suffix="${1:-parallel_trunk}"

  rake DBSUFFIX="$suffix" \
    clean                 \
    tmp:clear             \
    parallel:drop         \
    parallel:create       \
    parallel:migrate      \
    parallel:test[^"'$suite'"] --trace
}

# share via sprunge
sprunge() {
  local url

  _have curl || return 1
  read -r url < <(curl -sF 'sprunge=<-' 'http://sprunge.us' < "${1:-/dev/stdin}")

  _have xclip && echo -n "$url" | xclip
  echo $url
}

# startup a synergy client
synergyc() {
  _have synergyc || return 1

  local host="${1:-blue}"
  /usr/bin/synergyc -f "$host" &>"$LOGS/synergy.log" &
}

runsql() {
  if $_islinux; then
    _have psql || return 1
    psql -U pbrisbin pbrisbin;
  else
    _have mysql || return 1
    mysql -urails -pdev ideeli_development;
  fi
}

newcomments() {
  if $_islinux; then
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

# http://pbrisbin.com/posts/notes
noteit() {
  _have mutt || return 1

  local subject="$*" message

  [[ -z "$subject" ]] && { echo 'no subject.'; return 1; }

  echo -e "^D to save, ^C to cancel\nNote:\n"

  message="$(cat)"

  [[ -z "$message" ]] && { echo 'no message.'; return 1; }

  # send message
  echo "$message" | mutt -s "Note - $subject" -- pbrisbin@gmail.com

  echo -e "\nnoted.\n"
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

# simple spellchecker, uses /usr/share/dict/words
spellcheck() {
  [[ -f /usr/share/dict/words ]] || return 1

  for word; do
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
define() {
  _have w3m     || return 1
  _have mplayer || return 1

  local word="$*"

  w3m -dump "http://www.google.ca/search?q=define%20${word// /_}" | awk '/^     1./,/^        More info >>/'
  mplayer "http://ssl.gstatic.com/dictionary/static/sounds/de/0/${word// /_}.mp3" &>/dev/null
}

# grep by paragraph 
grepp() { perl -00ne "print if /$1/i" < "$2"; }

# pull a single file out of an achive, stops on first match. useful for
# .PKGINFO files in .pkg.tar.[gx]z files.
pullout() {
  _have bsdtar || return 1

  local opt

  case "$2" in
    *gz) opt='-qxzf' ;;
    *xz) opt='-qxJf' ;;
    *)   return 1    ;;
  esac

  bsdtar $opt "$2" "$1"
}

# recursively 'fix' dir/file perm
fix() {
  local dir

  for dir; do
    find "$dir" -type d -exec chmod 755 {} \; 
    find "$dir" -type f -exec chmod 644 {} \;
  done
}

# print docs to default printer in reverse page order 
printr() {
  _have enscript || return 1

  # stdin?
  if [[ -z "$*" ]]; then
    cat | enscript -p - | psselect -r | lp
    return 0
  fi

  local file

  for file; do
    enscript -p - "$file" | psselect -r | lp
  done
}

# set an ad-hoc GUI timer 
timer() {
  $_isxrunning || return 1
  _have zenity || return 1

  local N="${1:-5m}"; shift

  (sleep $N && zenity --info --title="Time's Up" --text="${*:-DING}") &
  echo "timer set for $N"
}

# send an attachment from CLI 
send() {
  _have mutt    || return 1
  [[ -f "$1" ]] || return 1
  [[ -z "$2" ]] || return 1

  echo 'Please see attached.' | mutt -s "File: $1" -a "$1" -- "$2"
}

# run a bash script in 'debug' mode
debug() {
  local script="$1"; shift

  if _have "$script"; then
    PS4='+$LINENO:$FUNCNAME: ' bash -x "$script" "$@"
  fi
}

# }}}

### Titlebar and Prompt {{{
#
# /[0]/[1]/[2]/[3]/
#
# 0: % batt remaining (if BAT0 exists)
# 1: host name
# 2: last exit status
# 3: git/svn summary or current directory
#
###

# colors setup {{{

# element colors
batt_color=WHITE
batt_med_color=YELLOW
batt_low_color=RED
host_color=WHITE
dir_color=WHITE
retval_color=WHITE
retval_nonzero_color=MAGENTA
sep_color=BLUE
root_sep_color=RED

# vcs colors
init_vcs_color=white
clean_vcs_color=white
modified_vcs_color=magenta
added_vcs_color=green
addmoded_vcs_color=yellow
untracked_vcs_color=cyan
op_vcs_color=magenta
detached_vcs_color=red
hex_vcs_color=white

# term color codes
black='\['`tput sgr0; tput setaf 0`'\]'
red='\['`tput sgr0; tput setaf 1`'\]'
green='\['`tput sgr0; tput setaf 2`'\]'
yellow='\['`tput sgr0; tput setaf 3`'\]'
blue='\['`tput sgr0; tput setaf 4`'\]'
magenta='\['`tput sgr0; tput setaf 5`'\]'
cyan='\['`tput sgr0; tput setaf 6`'\]'
white='\['`tput sgr0; tput setaf 7`'\]'

BLACK='\['`tput setaf 0; tput bold`'\]'
RED='\['`tput setaf 1; tput bold`'\]'
GREEN='\['`tput setaf 2; tput bold`'\]'
YELLOW='\['`tput setaf 3; tput bold`'\]'
BLUE='\['`tput setaf 4; tput bold`'\]'
MAGENTA='\['`tput setaf 5; tput bold`'\]'
CYAN='\['`tput setaf 6; tput bold`'\]'
WHITE='\['`tput setaf 7; tput bold`'\]'

colors_reset='\['`tput sgr0`'\]'

# replace symbolic colors names to raw terminfo strings
batt_color=${!batt_color}
batt_med_color=${!batt_med_color}
batt_low_color=${!batt_low_color}
host_color=${!host_color}
dir_color=${!dir_color}
retval_color=${!retval_color}
retval_nonzero_color=${!retval_nonzero_color}
sep_color=${!sep_color}
root_sep_color=${!root_sep_color}

init_vcs_color=${!init_vcs_color}
modified_vcs_color=${!modified_vcs_color}
untracked_vcs_color=${!untracked_vcs_color}
clean_vcs_color=${!clean_vcs_color}
added_vcs_color=${!added_vcs_color}
op_vcs_color=${!op_vcs_color}
addmoded_vcs_color=${!addmoded_vcs_color}
detached_vcs_color=${!detached_vcs_color}
hex_vcs_color=${!hex_vcs_color}

# }}}

_update_title() { # {{{
  case ${TERM} in
    xterm*|rxvt*|Eterm|aterm|kterm|gnome*) echo -en "\033]0;${PWD/$HOME/~}\007" ;;
  esac
}
#}}}

_battery_info() { # {{{
  local bat='/proc/acpi/battery/BAT0/' batt_level _batt_color

  unset batt_info

  if [[ -d "$bat" ]]; then
    eval "$(awk '
      /^remaining capacity: / { rem = $3 }
      /^last full capacity: / { max = $4 }
      END { printf ("batt_level=%d", rem/max*100) }' "$bat"/{info,state})"

      # set color based on level
      if [[ batt_level -le 10 ]]; then
        _batt_color=$batt_low_color
      elif [[ $batt_level -le 30 ]]; then
        _batt_color=$batt_med_color
      else
        _batt_color=$batt_color
      fi

      batt_info="${batt_color}~${_batt_color}${batt_level}${batt_color}%${colors_reset}"
  fi
}
# }}}

# a simplification on http://volnitsky.com/project/git-prompt/ {{{
_parse_git_status() {
  local git_dir file_regex added_files modified_files untracked_files file_list
  local freshness clean init added modified untracked detached
  local op rawhex branch vcs_info status vcs_color

  unset git_info

  git_dir="$(git rev-parse --git-dir 2> /dev/null)"
  
  if [[ -n ${git_dir/./} ]]; then
    file_regex='\([^/ ]*\/\{0,1\}\).*'
    added_files=()
    modified_files=()
    untracked_files=()

    eval "$(
      git status 2>/dev/null |
      sed -n '
      s/^# On branch /branch=/p
      s/^nothing to commi.*/clean=clean/p
      s/^# Initial commi.*/init=init/p

      s/^# Your branch is ahead of .[/[:alnum:]]\+. by [[:digit:]]\+ commit.*/freshness=\" ${WHITE}↑\"/p
      s/^# Your branch is behind .[/[:alnum:]]\+. by [[:digit:]]\+ commit.*/  freshness=\" ${YELLOW}↓\"/p
      s/^# Your branch and .[/[:alnum:]]\+. have diverged.*/freshness=${YELLOW}↕/p

      /^# Changes to be committed:/,/^# [A-Z]/ {
      s/^# Changes to be committed:/added=added;/p
      s/^#	modified:   '"$file_regex"'/    [[ \" ${added_files[*]} \" =~ \" \1 \" ]] || added_files+=(\"\1\")/p
      s/^#	new file:   '"$file_regex"'/    [[ \" ${added_files[*]} \" =~ \" \1 \" ]] || added_files+=(\"\1\")/p
      s/^#	renamed:[^>]*> '"$file_regex"'/ [[ \" ${added_files[*]} \" =~ \" \1 \" ]] || added_files+=(\"\1\")/p
      s/^#	copied:[^>]*> '"$file_regex"'/  [[ \" ${added_files[*]} \" =~ \" \1 \" ]] || added_files+=(\"\1\")/p
      }

      /^# Changed but not updated:/,/^# [A-Z]/ {
      s/^# Changed but not updated:/modified=modified;/p
      s/^#	modified:   '"$file_regex"'/ [[ \" ${modified_files[*]} \" =~ \" \1 \" ]] || modified_files+=(\"\1\")/p
      s/^#	unmerged:   '"$file_regex"'/ [[ \" ${modified_files[*]} \" =~ \" \1 \" ]] || modified_files+=(\"\1\")/p
      }

      /^# Changes not staged for commit:/,/^# [A-Z]/ {
      s/^# Changes not staged for commit:/modified=modified;/p
      s/^#	modified:   '"$file_regex"'/ [[ \" ${modified_files[*]} \" =~ \" \1 \" ]] || modified_files+=(\"\1\")/p
      s/^#	unmerged:   '"$file_regex"'/ [[ \" ${modified_files[*]} \" =~ \" \1 \" ]] || modified_files+=(\"\1\")/p
      }

      /^# Unmerged paths:/,/^[^#]/ {
      s/^# Unmerged paths:/modified=modified;/p
      s/^#	both modified:\s*'"$file_regex"'/ [[ \" ${modified_files[*]} \" =~ \" \1 \" ]] || modified_files+=(\"\1\")/p
      }

      /^# Untracked files:/,/^[^#]/{
      s/^# Untracked files:/untracked=untracked;/p
      s/^#	'"$file_regex"'/ [[ \" ${untracked_files[*]} ${modified_files[*]} ${added_files[*]} \" =~ \" \1 \" ]] || untracked_files+=(\"\1\")/p
      }
      '
    )"

    grep -q "^ref:" $git_dir/HEAD 2>/dev/null || detached=detached

    if [[ -d "$git_dir/.dotest" ]] ;  then
      if [[ -f "$git_dir/.dotest/rebasing" ]]; then
        op="rebase"
      elif [[ -f "$git_dir/.dotest/applying" ]]; then
        op="am"
      else
        op="am/rebase"
      fi
    elif [[ -f "$git_dir/.dotest-merge/interactive" ]]; then
      op="rebase -i"
    elif [[ -d "$git_dir/.dotest-merge" ]]; then
      op="rebase -m"
    elif [[ -f "$git_dir/MERGE_HEAD" ]]; then
      op="merge"
    elif [[ -f "$git_dir/index.lock" ]]; then
      op="locked"
    elif [[ -f "$git_dir/BISECT_LOG" ]]; then
      op="bisect"
    fi

    rawhex=$(git rev-parse HEAD 2>/dev/null)
    rawhex=${rawhex/HEAD/}
    rawhex="$hex_vcs_color${rawhex:0:5}"

    if [[ $init ]]; then 
      vcs_info=${white}init
    else
      if [[ "$detached" ]]; then
        branch="<detached:`git name-rev --name-only HEAD 2>/dev/null`"
      elif [[ "$op" ]]; then
        branch="$op:$branch"
        [[ "$op" == 'merge' ]] && branch+="<--$(git name-rev --name-only $(<$git_dir/MERGE_HEAD))"
      fi

      vcs_info="$branch${white}[$rawhex${white}]$fresshness"
    fi

    status=${op:+op}
    status=${status:-$detached}
    status=${status:-$clean}
    status=${status:-$modified}
    status=${status:-$added}
    status=${status:-$untracked}
    status=${status:-$init}
    eval vcs_color="\${${status}_vcs_color}"

    [[ ${added_files[0]}     ]] && file_list+=" $added_vcs_color${added_files[@]}"
    [[ ${modified_files[0]}  ]] && file_list+=" $modified_vcs_color${modified_files[@]}"
    [[ ${untracked_files[0]} ]] && file_list+=" $untracked_vcs_color${untracked_files[@]}"

    # real git info
    git_info="${vcs_color}${vcs_info}${vcs_color}${file_list}${colors_reset}"
  fi
}

_parse_svn_status() {
  local repo_dir rev branch vcs_info

  unset svn_info # default

  if [[ -d ./.svn  ]]; then
    eval $(sed -n '
      s@^URL[^/]*//@repo_dir=@p
      s/^Revision: /rev=/p
      ' < <(svn info)
    )

    if [[ "$repo_dir" =~ trunk ]]; then
      branch='trunk'
    elif [[ "$repo_dir" =~ branches/(.*) ]]; then
      branch="${BASH_REMATCH[1]}"
    fi

    vcs_info=svn:$branch:r$rev
    svn_info="${clean_vcs_color}${vcs_info}${colors_reset}"
  fi
}

# }}}

prompt_command_function() {
  local retval="$?" _sep_color host_info retval_info ps

  # sep changes colors for root
  $_isroot && _sep_color=$root_sep_color || _sep_color=$sep_color

  _battery_info
  _parse_git_status
  _parse_svn_status

  if [[ -n "$git_info" ]]; then
    # Git
    vcs_info="$git_info"
  elif [[ -n "$svn_info" ]]; then
    # SVN
    vcs_info="$svn_info"
  else
    # directory display
    vcs_info="${dir_color}${PWD/$HOME/~}${colors_reset}"
  fi

  host_info="$host_color${HOSTNAME/.local/}"

  if [[ $retval -eq 0 ]]; then
    retval_info="$retval_color$retval"
  else
    retval_info="$retval_nonzero_color$retval"
  fi

  # add control characters for screen
  [[ -n "$STY" ]] && ps='\[\ek\e\\\]\[\ek\w\e\\\]' || ps=''

  # build that prompt
  ps+="$_sep_color/$batt_info"
  ps+="$_sep_color/$host_info"
  ps+="$_sep_color/$retval_info"
  ps+="$_sep_color/$vcs_info"
  ps+="$_sep_color/ $colors_reset"

  PS1="$ps"
  PS2="$_sep_color// $colors_reset"
  PS3="$_sep_color// $colors_reset"

  _update_title
}

unset PROMPT_COMMAND
PROMPT_COMMAND=prompt_command_function
# }}}

### Starting X {{{

if [[ $(tty) = /dev/tty1 ]] && ! $_isroot && ! $_isxrunning; then
  _set_browser "$xbrowsers"
  exec startx
fi

# }}}
