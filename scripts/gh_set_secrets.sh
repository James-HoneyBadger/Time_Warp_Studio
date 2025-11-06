#!/usr/bin/env bash
set -euo pipefail

# Helper to upload required GitHub Actions secrets using `gh` CLI.
# Usage: export the local files or variables and run this on your machine.
# Example:
#   export APP_SIGNING_P12_BASE64=$(./scripts/p12_to_base64.sh ~/Downloads/AppleDistribution.p12)
#   export APP_SIGNING_P12_PASSWORD='p12pass'
#   ./scripts/gh_set_secrets.sh

REPO="${REPO:-$(git remote get-url origin | sed -n 's#.*/\(.*\)\.git#\1#p')}"

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

for s in "${secrets[@]}"; do
  val="${!s:-}"
  if [[ -z "$val" ]]; then
    echo "Environment variable $s is empty. Set it before running this script. Skipping $s."
    continue
  fi
  echo "Uploading secret: $s"
  echo -n "$val" | gh secret set "$s" -R "$REPO" --body-file -
done

echo "Done. Verify your repository secrets in GitHub settings."
