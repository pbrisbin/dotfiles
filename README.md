# Dotfiles

All of the configs.

## Installation

* Install [rcm][]
* Clone to `~/.dotfiles`
* View what would be installed:

```
$ RCRC="config/rcrc" lsrc  # See what will happen
$ RCRC="config/rcrc" rcup  # Do it
```

To modify how or what will be installed, see `man 1 rcup`.

[rcm]: https://github.com/mike-burns/rcm

## Structure

Files which make sense across all of my machines (screen, git, etc) are 
kept directly at top-level. Files which should only be installed on 
certain machines are kept in the appropriate tag subdirectories. The 
host-specific `rcrc` files then dictate which tags are used where.
