# Overview
Personal system configuration for MacOS and Omarchy (Arch Linux + Hyprland).

# Usage
- MacOS: run `install.sh`
- Omarchy: run `install-omarchy.sh`

Linux-specific files use an `-omarchy` suffix (`ghostty/config-omarchy`, `kanata/kanata-omarchy.kbd`, `.opencode/opencode.omarchy.json`). Shared configs (Emacs, opencode agents/skills, hypr bindings) are symlinked as-is.

## Documentation
Configuration for the Emacs is in the Emacs.org code elisp blocks which get tangled and output to `./.emacs.d/init.el` file on every file save. Text outside of the elisp code blocks represents documentation.
