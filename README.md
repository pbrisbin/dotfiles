# Dotfiles

This repo uses [rcm][]. Also, all dotfiles are kept in tag-specific 
directories. Host-specific rcrc files then dictate which machines get 
which tags. This also allows for easy cherry-picking.

For example, if you want only my vim setup:

```
% git clone https://github.com/pbrisbin/dotfiles .pbrisbin-dotfiles
% rcup -d .pbrisbin-dotfiles -x README.md -t vim
```

These options could be made the default in your own `~/.rcrc`.

See more details with `man 7 rcm`.

[rcm]: https://github.com/thoughtbot/rcm
