# Dotfiles

All of the configs.

## Installation

This repo uses [rcm][].

[rcm]: https://github.com/thoughtbot/rcm

```
$ aurget -S rcm-git
$ git clone https://github.com/pbrisbin/dotfiles .dotfiles
$ cd .dotfiles
$ RCRC="host-$HOST/config/rcrc" rcup
$ exit
```

## Structure

Files which make sense across all of my machines (screen, git, etc) are 
kept directly at top-level. Files which should only be installed on 
certain machines are kept in the appropriate tag subdirectories. The 
host-specific `rcrc` files then dictate which tags are used where.
