require 'fileutils'

module Sys
  class << self
    include FileUtils

    def run(cmd)
      log :execute, cmd
      system(cmd) or raise $?
    end

    def install(source, target)
      source = File.expand_path(source)
      target = File.expand_path(target)

      if File.symlink?(target) && File.readlink(target) == source
        log :exists, target
        return
      end

      if File.exists?(target)
        log :backup, target
        mv target, "#{target}.backup"
      end

      log :link, target
      ln_s source, target
    end

    private

    def log(action, message)
      c = {
        backup:  "\e[1;36m",
        execute: "\e[1;33m",
        exists:  "\e[1;34m",
        link:    "\e[1;32m",
      }.fetch(action)

      printf "#{c}%10.10s \e[1;37m%s\e[0m\n", action, message
    end
  end
end

desc "Pulls from origin"
task :pull do
  Sys.run 'git pull origin master'
end

desc "Updates and initializes submodules"
task :submodules do
  Sys.run 'git submodule update --init --recursive'
end

desc "Installs all dotfiles"
task :dotfiles do
  Sys.install '.Xdefaults', '~/.Xdefaults'
  Sys.install '.gitconfig', '~/.gitconfig'
  Sys.install '.gitignore', '~/.gitignore'
  Sys.install '.screenrc' , '~/.screenrc'
  Sys.install '.vim'      , '~/.vim'
  Sys.install '.vim/vimrc', '~/.vimrc'
  Sys.install '.xinitrc'  , '~/.xinitrc'
  Sys.install '.zshenv'   , '~/.zshenv'
  Sys.install '.zshrc'    , '~/.zshrc'
end

desc "Sets up Vundle"
task :vundle do
  unless File.exists?('.vim/bundle/vundle')
    Sys.run 'git clone https://github.com/gmarik/vundle .vim/bundle/vundle'
  end

  Sys.run 'vim +BundleInstall +qall'
end

task install: [:submodules, :dotfiles, :vundle]

task update: [:pull, :submodules, :dotfiles, :vundle]

task default: :install
