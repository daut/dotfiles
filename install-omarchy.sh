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

# --- herdr ------------------------------------------------------------
# tmux replacement; restores opencode sessions after herdr server restarts
if ! command -v herdr >/dev/null; then
  omarchy pkg add herdr
fi
herdr integration install opencode
link "$REPO_DIR/herdr/config.toml" ~/.config/herdr/config.toml

# --- ghostty ----------------------------------------------------------
link "$REPO_DIR/ghostty/config-omarchy" ~/.config/ghostty/config

# --- hyprland ---------------------------------------------------------
link "$REPO_DIR/hypr/bindings.lua" ~/.config/hypr/bindings.lua
link "$REPO_DIR/hypr/input.lua" ~/.config/hypr/input.lua
link "$REPO_DIR/bin/select-all" ~/.local/bin/select-all
link "$REPO_DIR/bin/restart-focused-app" ~/.local/bin/restart-focused-app
hyprctl reload
hyprctl configerrors

# --- xremap ------------------------------------------------------------
# App-aware key remapper (global emacs-style navigation, except Emacs and
# terminals). Needs the uinput udev rule to run without sudo.
if ! command -v xremap >/dev/null; then
  echo "installing xremap-hypr-bin from AUR (needs sudo)..."
  omarchy pkg aur add xremap-hypr-bin
fi
echo 'KERNEL=="uinput", MODE="0660", GROUP="input", OPTIONS+="static_node=uinput"' | sudo tee /etc/udev/rules.d/99-xremap.rules
sudo udevadm control --reload-rules && sudo udevadm trigger --sysname-match=uinput
link "$REPO_DIR/xremap/xremap.service" ~/.config/systemd/user/xremap.service
systemctl --user daemon-reload
systemctl --user enable --now xremap

# --- agent skills -------------------------------------------------------
# Hunk ships its generated review skill beside the CLI; the path moves with
# the toolchain (mise node version), so resolve it at install time.
# Re-run this script after Node/hunk upgrades to re-point the link.
if command -v hunk >/dev/null; then
  link "$(dirname "$(hunk skill path)")" ~/.agents/skills/hunk-review
fi

# Registry skills (OpenCode) are installed by a shared script, also used by install.sh
"$REPO_DIR/bin/install-skills.sh"

echo
echo "Setup complete. Log out/in if xremap keys misbehave."
