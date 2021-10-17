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
brew install zsh-autosuggestions
brew install cmake
brew install coreutils

# add taps
brew tap homebrew/cask-versions

# install casks
brew install --cask anki
brew install --cask iterm2
brew install --cask visual-studio-code
brew install --cask emacs
brew install --cask google-chrome
brew install --cask firefox-nightly
brew install --cask numi
brew install --cask slack
brew install --cask postman
brew install --cask grammarly
brew install --cask zeplin
brew install --cask hammerspoon

# install oh-my-zsh
sh -c "$(curl -fsSL https://raw.githubusercontent.com/robbyrussell/oh-my-zsh/master/tools/install.sh)"

# install npm packages
npm i -g n
npm i -g eslint
