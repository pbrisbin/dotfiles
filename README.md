# Dotfiles

## Usage:

~~~
$ git clone https://github.com/pbrisbin/dotfiles ~/.dotfiles
$ cd ~/.dotfiles
$ rake
~~~

This will symlink the files (or directories) here into the proper places 
under `$HOME`.

Existing files will be backed up; existing symlinks will be clobbered.

## Notes

* The ZSH config relies on (parts of) the grml zsh config. If you get 
  errors loading ZSH, install it with `pacman -S grml-zsh-config`.
