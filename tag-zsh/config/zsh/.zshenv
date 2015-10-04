if [[ -e /usr/share/chruby ]]; then
  source /usr/share/chruby/chruby.sh
  source /usr/share/chruby/auto.sh

  if [[ -e ./.ruby-version ]]; then
    chruby $(cat ./.ruby-version)
  else
    chruby $(cat ~/.ruby-version)
  fi
fi

cdpath=( "$HOME" "$HOME/code" $cdpath )

if [[ -e /etc/profile.d/autojump.zsh ]]; then
  source /etc/profile.d/autojump.zsh
fi

fpath=( "$ZDOTDIR/functions" $fpath )

unsetopt nomatch
