#!/usr/bin/env bash
# Set up dotfiles on Omarchy (Arch Linux + Hyprland).
# Idempotent: safe to re-run. macOS setup lives in install.sh.

set -euo pipefail

# This script links config into $HOME. Running it with sudo sets $HOME to
# /root and pollutes the root account instead of your user, so refuse.
if [ "$(id -u)" -eq 0 ]; then
  echo "ERROR: do not run this script with sudo." >&2
  echo "Run it as your normal user; sudo is requested only inside the script." >&2
  exit 1
fi

REPO_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

backup() {
  local path=$1
  if [ -e "$path" ] && [ ! -L "$path" ]; then
    cp "$path" "$path.pre-dotfiles.bak"
    echo "backed up: $path"
  fi
}

link() {
  local target=$1 name=$2
  mkdir -p "$(dirname "$name")"
  backup "$name"
  ln -sfn "$target" "$name"
  echo "linked: $name -> $target"
}

# --- Emacs ------------------------------------------------------------
mkdir -p ~/.emacs.d/themes
link "$REPO_DIR/.emacs.d/init.el" ~/.emacs.d/init.el
link "$REPO_DIR/.emacs.d/early-init.el" ~/.emacs.d/early-init.el
link "$REPO_DIR/.emacs.d/snippets" ~/.emacs.d/snippets
link "$REPO_DIR/.emacs.d/themes/emacs.txt" ~/.emacs.d/themes/emacs.txt
link "$REPO_DIR/.emacs.d/themes/miasma-theme.el" ~/.emacs.d/themes/miasma-theme.el

# --- opencode ---------------------------------------------------------
link "$REPO_DIR/.opencode/AGENTS.md" ~/.config/opencode/AGENTS.md
link "$REPO_DIR/.opencode/agents" ~/.config/opencode/agents
link "$REPO_DIR/.opencode/commands" ~/.config/opencode/commands
link "$REPO_DIR/.opencode/skills" ~/.config/opencode/skills
link "$REPO_DIR/.opencode/opencode.omarchy.json" ~/.config/opencode/opencode.json

# --- ghostty ----------------------------------------------------------
link "$REPO_DIR/ghostty/config-omarchy" ~/.config/ghostty/config

# --- hyprland ---------------------------------------------------------
link "$REPO_DIR/hypr/bindings.lua" ~/.config/hypr/bindings.lua
link "$REPO_DIR/hypr/input.lua" ~/.config/hypr/input.lua
link "$REPO_DIR/bin/select-all" ~/.local/bin/select-all
link "$REPO_DIR/bin/restart-focused-app" ~/.local/bin/restart-focused-app
hyprctl reload
hyprctl configerrors

# --- kanata -----------------------------------------------------------
if ! command -v kanata >/dev/null; then
  echo "installing kanata from AUR (needs sudo)..."
  omarchy pkg aur add kanata
fi
sudo ln -sfn "$REPO_DIR/kanata/kanata-omarchy.kbd" /etc/kanata.kbd
sudo systemctl enable --now kanata

# --- agent skills -------------------------------------------------------
# Hunk ships its generated review skill beside the CLI; the path moves with
# the toolchain (mise node version), so resolve it at install time.
# Re-run this script after Node/hunk upgrades to re-point the link.
if command -v hunk >/dev/null; then
  link "$(dirname "$(hunk skill path)")" ~/.agents/skills/hunk-review
fi

echo
echo "Setup complete. Log out/in if kanata keys misbehave."
