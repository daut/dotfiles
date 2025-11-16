# Overview
Personal system configuration primarily targeting MacOS.

# Usage
How to set it up

## Emacs
`ln -s "$(pwd)/.emacs.d/init.el" ~/.emacs.d/init.el`
`ln -s "$(pwd)/.emacs.d/early-init.el" ~/.emacs.d/early-init.el`
`ln -s "$(pwd)/.emacs.d/snippets/" ~/.emacs.d/`
`ln -s "$(pwd)/.emacs.d/themes/emacs.txt" ~/.emacs.d/themes/`
`ln -s "$(pwd)/ghostty" ~/.config/ghostty`

### How does Emacs.org file work
Configuration for the Emacs is in the Emacs.org code elisp blocks which get tangled and output to `./.emacs.d/init.el` file on every file save. Text outside of the elisp code blocks represents documentation.
