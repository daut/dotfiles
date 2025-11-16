# install brew
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install.sh)"

# create directories
mkdir ~/projects
mkdir ~/.emacs.d

# install brews
brew install git
brew install node
brew install go
brew install siege
brew install jq
brew install httpie
brew install ack
brew install awscli
brew install postgresql
brew install mysql
brew install uchardet # detect file encoding
brew install zsh-autosuggestions
brew install cmake
brew install coreutils # for gnu commands
brew install ripgrep # faster grep
brew install inetutils # for ftp
brew install rust
brew install fish # fish shell

# add taps
brew tap homebrew/cask-versions

# install casks
brew install --cask emacs-plus
brew install --cask zen
brew install --cask hammerspoon
brew install --cask karabiner-elements
brew install --cask docker
brew install --cask ghostty

# install oh-my-zsh
sh -c "$(curl -fsSL https://raw.githubusercontent.com/robbyrussell/oh-my-zsh/master/tools/install.sh)"

# install npm packages
npm i -g n
npm i -g eslint

# setup emacs
ln -s "$(pwd)/.emacs.d/init.el" ~/.emacs.d/init.el
ln -s "$(pwd)/.emacs.d/early-init.el" ~/.emacs.d/early-init.el
ln -s "$(pwd)/.emacs.d/snippets/" ~/.emacs.d/
ln -s "$(pwd)/.emacs.d/themes/emacs.txt" ~/.emacs.d/themes/

# setup ghostty
ln -s "$(pwd)/ghostty" ~/.config/ghostty

# setup fish
which fish | sudo tee -a /etc/shells
chsh -s $(which fish)
