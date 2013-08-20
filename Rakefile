def execute(cmd)
  puts cmd
  system cmd or raise $?
end

def create_link(dotfile)
  source = File.expand_path(dotfile)
  target = File.expand_path("~/.#{dotfile}")

  unless File.symlink?(target) && File.readlink(target) == source
    execute "mv '#{target}' '#{target}.backup'" if File.exists?(target)
    execute "ln -sf '#{source}' '#{target}'"
  end
end

DOTFILES = %w(
  Xdefaults
  gitconfig
  gitignore
  msmtprc
  offlineimap.py
  offlineimaprc
  screenrc
  vimrc
  xinitrc
  zshenv
  zshrc
)

task :default do
  DOTFILES.each(&method(:create_link))

  begin
    execute 'git clone https://github.com/gmarik/vundle ~/.vim/bundle/vundle'
    execute 'vim +BundleInstall +qall'
  rescue
  end
end
