-- Personal keybinding overrides (Omarchy).
-- Symlinked from ~/.dotfiles/hypr/bindings.lua

-- See current bindings and descriptions:
--   omarchy menu keybindings --print

-- Gmail (muscle-memory port from hammerspoon cmd+ctrl+M)
o.bind("SUPER + CTRL + M", "Gmail", "xdg-open https://mail.google.com")

-- Select all (mac Cmd+A muscle memory; terminals get Ctrl+Shift+A instead)
o.bind("SUPER + A", "Select all", "select-all")
