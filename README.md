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

All dotfiles are kept in tag-specific directories. Host-specific rcrc 
files then dictate which machines get which tags.
