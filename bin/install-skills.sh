#!/usr/bin/env bash
# Install global agent skills from the skills registry (OpenCode only).
# Idempotent: safe to re-run; re-running also refreshes skills to latest.
# Shared by install.sh (macOS) and install-omarchy.sh (Arch).

set -euo pipefail

if ! command -v npx >/dev/null; then
  echo "WARN: npx not found, skipping global agent skills (install node first)" >&2
  exit 0
fi

npx --yes skills add vuejs-ai/skills -g -s '*' -a opencode -y
npx --yes skills add vercel-labs/skills -g -s find-skills -a opencode -y
npx --yes skills add cloudflare/skills -g -s '*' -a opencode -y
