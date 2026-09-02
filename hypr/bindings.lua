-- Personal keybinding overrides (Omarchy).
-- Symlinked from ~/.dotfiles/hypr/bindings.lua

-- See current bindings and descriptions:
--   omarchy menu keybindings --print

-- Gmail (muscle-memory port from hammerspoon cmd+ctrl+M)
o.bind("SUPER + CTRL + M", "Gmail", "xdg-open https://mail.google.com")

-- Select all (mac Cmd+A muscle memory; terminals get Ctrl+Shift+A instead)
o.bind("SUPER + A", "Select all", "select-all")

-- Kill and relaunch the focused application (SUPER+W closes it)
o.bind("SUPER + SHIFT + R", "Restart focused app", "restart-focused-app")

-- Free s-C-, and s-C-. for Emacs; remap the omarchy actions to shift variants
hl.unbind("SUPER + CTRL + comma")
hl.unbind("SUPER + CTRL + PERIOD")
o.bind_toggle("SUPER + CTRL + SHIFT + N", "Toggle silencing notifications", "notification-silencing")
o.bind("SUPER + CTRL + SHIFT + T", "Transcode", "omarchy-transcode")
