if [[ -e /usr/share/chruby ]]; then
  source /usr/share/chruby/chruby.sh
  source /usr/share/chruby/auto.sh

  if [[ -e ./.ruby-version ]]; then
    chruby $(cat ./.ruby-version)
  else
    chruby $(cat ~/.ruby-version)
  fi
fi

cdpath=( "$HOME" "$HOME/Code" $cdpath )

fpath=( "$ZDOTDIR/functions" $fpath )
