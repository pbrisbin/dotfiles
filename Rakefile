def exec(cmd)
  puts cmd
  system cmd or raise $?
end

class Dotfile
  def initialize(source, target = "~/#{source}")
    @source = File.expand_path(source)
    @target = File.expand_path(target)
  end

  def install
    unless installed?
      exec "mv '#{@target}' '#{@target}.backup'" if File.exists?(@target)
      exec "ln -s '#{@source}' '#{@target}'"
    end
  end

  def installed?
    File.symlink?(@target) && File.readlink(@target) == @source
  end
end

desc "Install all the dotfiles"
task :link do
  Dotfile.new('.Xdefaults').install
  Dotfile.new('.gitconfig').install
  Dotfile.new('.gitignore').install
  Dotfile.new('.screenrc').install
  Dotfile.new('.vimrc').install
  Dotfile.new('.xinitrc').install
  Dotfile.new('.zshenv').install
  Dotfile.new('.zshrc').install
end

desc "Install Vundle and vim bundles"
task :vundle do
  exec 'mkdir -p ~/.vim'
  exec 'git clone https://github.com/gmarik/vundle ~/.vim/bundle/vundle'
  exec 'vim +BundleInstall +qall'
end

task install: [:link, :vundle]

task default: :install
