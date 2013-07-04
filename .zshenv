source "$HOME/.secrets"

if [[ -e /usr/share/chruby ]]; then
  source /usr/share/chruby/chruby.sh
  source /usr/share/chruby/auto.sh

  chruby $(cat ~/.ruby-version)
fi

SSHPID=$(ps ax | grep -c "[s]sh-agent")

if (( $SSHPID == 0 )); then
  ssh-agent | sed 's/^echo/#echo/' > ~/.ssh-env
  source ~/.ssh-env
  ssh-add
else
  source ~/.ssh-env
fi

cdpath=( "$HOME/Code" $cdpath )

path=( "$HOME/.bin" "$HOME/.cabal/bin" $path )

[[ $TTY == /dev/tty1 ]] && [[ -z $DISPLAY ]] && exec startx
