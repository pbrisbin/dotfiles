# Dotfiles

Various configuration files for my main system and a rake task to 
install them.

### Features

The features you'll enjoy if you choose to install my setup:

1. A nice vim setup with lots of plugins
2. A nice zsh setup using oh-my-zsh
3. A nice screen setup
4. A nice git setup
5. Pretty threaded htop
6. Pretty ls colors
7. Pretty X colors

### Usage

1. Clone or fork-then-clone the repo
2. Customize as desired
3. Sanity-check the Rakefile
4. `rake install`

### Warning

The `rake install` functionality is new and only marginally tested. It 
should be safe except for one gotcha:

If, for example, it installs `.foo` and you already have a `.foo` it 
will `mv` it to `.foo.backup` before proceeding. This is a Good Thing.

However, If you then run `rake install` again it will again find `.foo` 
and again `mv` it to `.foo.backup`. Unfortunately, it's now overwrote 
your actual backup with the link it previously installed.

Coding around this is complexity I just don't need.
