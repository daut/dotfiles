# install brew
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install.sh)"

# create directories
mkdir ~/projects
mkdir ~/.emacs.d

# install brews
brew install git
brew install node
brew install siege
brew install jq
brew install httpie
brew install noti
brew install tig
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
brew install mpv # media player

# add taps
brew tap homebrew/cask-versions

# install casks
brew install --cask anki
brew install --cask iterm2
brew install --cask emacs-plus
brew install --cask firefox
brew install --cask numi
brew install --cask slack
brew install --cask postman
brew install --cask hammerspoon
brew install --cask karabiner-elements
brew install --cask docker
brew install --cask plex

# install oh-my-zsh
sh -c "$(curl -fsSL https://raw.githubusercontent.com/robbyrussell/oh-my-zsh/master/tools/install.sh)"

# install npm packages
npm i -g fnm
npm i -g eslint

# setup emacs
ln -s ~/projects/dotfiles/.emacs.d/init.el ~/.emacs.d/init.el
ln -s ~/projects/dotfiles/.emacs.d/early-init.el ~/.emacs.d/early-init.el
ln -s ~/projects/dotfiles/.emacs.d/snippets/ ~/.emacs.d/
