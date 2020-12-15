# install brew
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install.sh)"

# create projects directory
mkdir ~/projects

# install brews
brew install node
brew install siege
brew install jq
brew install httpie
brew install noti
brew install tig
brew install ack
brew install awscli
brew install git
brew install postgresql
brew install mysql
brew install uchardet

# add taps
brew tap homebrew/cask-versions

# install casks
brew cask install anki
brew cask install iterm2
brew cask install visual-studio-code
brew cask install emacs
brew cask install google-chrome
brew cask install firefox-nightly
brew cask install numi
brew cask install slack
brew cask install postman
brew cask install grammarly
brew cask install zeplin
brew cask install hammerspoon

# install oh-my-zsh
sh -c "$(curl -fsSL https://raw.githubusercontent.com/robbyrussell/oh-my-zsh/master/tools/install.sh)"

# install npm packages
npm i -g n
npm i -g eslint
