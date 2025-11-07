#!/usr/bin/env bash
set -euo pipefail

# Log in to GitHub Container Registry (GHCR) for pulling images like ghcr.io/bebbo/amiga-gcc:latest
# Requires a GitHub username and a Personal Access Token (classic) with the scope: read:packages
# Usage examples:
#   GHCR_USERNAME="your-github-user" GHCR_TOKEN="<PAT-with-read:packages>" ./scripts/ghcr_login.sh
#   ./scripts/ghcr_login.sh   # interactive prompts

if ! command -v docker >/dev/null 2>&1; then
  echo "❌ Docker is required to run this script." >&2
  exit 1
fi

USERNAME="${GHCR_USERNAME:-}"
TOKEN="${GHCR_TOKEN:-}"

if [[ -z "$USERNAME" ]]; then
  read -r -p "GitHub username: " USERNAME
fi

if [[ -z "$TOKEN" ]]; then
  echo -n "GitHub PAT (read:packages): "
  # shellcheck disable=SC2162
  read -s TOKEN
  echo
fi

if [[ -z "$USERNAME" || -z "$TOKEN" ]]; then
  echo "❌ Missing credentials. Set GHCR_USERNAME and GHCR_TOKEN or provide them interactively." >&2
  exit 2
fi

# Login using stdin to avoid token in history
printf '%s' "$TOKEN" | docker login ghcr.io -u "$USERNAME" --password-stdin

echo "✅ Logged in to ghcr.io as $USERNAME"
