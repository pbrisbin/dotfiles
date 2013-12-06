if [[ -e /usr/share/chruby ]]; then
  source /usr/share/chruby/chruby.sh
  source /usr/share/chruby/auto.sh

  chruby $(cat ~/.ruby-version)
fi

cdpath=( "$HOME" "$HOME/Code" $cdpath )

fpath=( "$ZDOTDIR/functions" $fpath )
