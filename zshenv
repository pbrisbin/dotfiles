source "$HOME/.secrets"

if [[ -e /usr/share/chruby ]]; then
  source /usr/share/chruby/chruby.sh
  source /usr/share/chruby/auto.sh

  chruby $(cat ~/.ruby-version)
fi

if pgrep "ssh-agent" >/dev/null; then
  source ~/.ssh-env
else
  ssh-agent | grep -Fv 'echo' > ~/.ssh-env
  source ~/.ssh-env
  ssh-add
fi

cdpath=( "$HOME" "$HOME/Code" $cdpath )

path=( "$HOME/.bin" "$HOME/.cabal/bin" $path )

[[ $TTY == /dev/tty1 ]] && [[ -z $DISPLAY ]] && exec startx
