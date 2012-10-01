require 'fileutils'

module Dotfiles
  def self.each(&block)
    [
      '.gitconfig',
      '.gitignore',
      '.htoprc',
      '.irbrc',
      '.dir_colors',
      '.Xdefaults',
      '.zshrc',
      '.zshenv',
      '.zlogin',
      '.zpreztorc',
      '.prezto',
      '.screen',
      '.vim',
      '.irssi'
    ].each do |file|
      yield Dotfile.new(file)
    end
  end

  class Dotfile
    include FileUtils

    attr_reader :dotfile
    attr_accessor :source, :target

    def initialize(dotfile)
      @dotfile = dotfile
      @source  = File.join(pwd, dotfile)
      @target  = File.join(ENV['HOME'], dotfile)
    end

    def install!
      puts "--> installing #{dotfile} as #{target}..."
      if File.exists?(target)
        if File.symlink?(target)
          rm target, :verbose => true
        else
          mv target, "#{target}.backup", :verbose => true
        end
      end

      ln_s source, target, :verbose => true
    end
  end
end

desc "updates all submodules"
task :submodules do
  unless system('git submodule update --init --recursive')
    raise 'error initializing submodules'
  end
end

desc "installs all dotfiles into the proper places"
task :install => [:submodules] do
  Dotfiles.each(&:install!)

  vimrc = Dotfiles::Dotfile.new('.vimrc')
  vimrc.source = File.join(ENV['HOME'], '.vim', 'vimrc')
  vimrc.install!
end

desc "pulls latest version from github"
task :pull do
  unless system('git pull origin master')
    raise 'error pulling latest from github'
  end
end

task :update => [:pull, :submodules]

task :default => :install
