#!/usr/bin/env bash
set -euo pipefail

# Helper to upload required GitHub Actions secrets using `gh` CLI.
# Usage: export the local files or variables and run this on your machine.
# Examples:
#   export APP_SIGNING_P12=$(./scripts/p12_to_base64.sh ~/Downloads/AppleDistribution.p12)
#   export APP_SIGNING_P12_PASSWORD='p12pass'
#   export SIGN_IDENTITY='Developer ID Application: Your Name (TEAMID)'
#   ./scripts/gh_set_secrets.sh

if ! command -v gh >/dev/null 2>&1; then
  echo "❌ GitHub CLI 'gh' is required. Install from https://cli.github.com and authenticate with 'gh auth login'." >&2
  exit 1
fi

# When running within the repo, we can rely on gh's default repo context.
# If you want to target a different repo, set REPO=owner/name in the environment.
REPO="${REPO:-}"

# List of secrets we expect to set
secrets=(
  APP_SIGNING_P12
  APP_SIGNING_P12_PASSWORD
  INSTALLER_P12
  INSTALLER_P12_PASSWORD
  SIGN_IDENTITY
  INSTALLER_IDENTITY
  APP_STORE_CONNECT_API_KEY_JSON
  APP_STORE_CONNECT_KEY_ID
  APP_STORE_CONNECT_ISSUER_ID
)

missing=()
uploaded=()
for s in "${secrets[@]}"; do
  val="${!s:-}"
  if [[ -z "$val" ]]; then
    missing+=("$s")
    continue
  fi
  echo "➡️  Uploading secret: $s"
  if [[ -n "$REPO" ]]; then
    gh secret set "$s" -R "$REPO" -b "$val" >/dev/null
  else
    gh secret set "$s" -b "$val" >/dev/null
  fi
  uploaded+=("$s")
done

if (( ${#uploaded[@]} > 0 )); then
  echo "✅ Uploaded ${#uploaded[@]} secrets: ${uploaded[*]}"
fi
if (( ${#missing[@]} > 0 )); then
  echo "ℹ️ Skipped ${#missing[@]} secrets (not set in environment): ${missing[*]}"
  echo "   Set any missing values as environment variables and re-run this script."
fi

echo "Done. Verify your repository secrets in GitHub settings."
