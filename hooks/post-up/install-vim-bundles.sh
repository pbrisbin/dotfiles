#!/usr/bin/env sh
if [ ! -d ~/.vim/bundle/vundle ]; then
  mkdir -p ~/.vim/bundle
  git clone https://github.com/gmarik/vundle.git ~/.vim/bundle/vundle
fi

count=$(find ~/.vim/bundle -maxdepth 1 -type d -print | wc -l)

[ $count -eq 1 ] && vim +BundleInstall +wall
