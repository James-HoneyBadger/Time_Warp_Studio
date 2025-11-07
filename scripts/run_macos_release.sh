#!/usr/bin/env bash
set -euo pipefail

# Trigger the macOS Release (App Store) workflow and optionally watch it.
# Requires GitHub CLI `gh` authenticated with repo access.
# Usage examples:
#   ./scripts/run_macos_release.sh
#   APP_NAME="Time Warp" VERSION="3.0.0" BUILD_VERSION=1 ./scripts/run_macos_release.sh --watch
#   ./scripts/run_macos_release.sh --app-name "Time Warp" --executable-name time-warp --bundle-id org.honeybadger.timewarp --version 3.0.0 --build-version 1 --watch

if ! command -v gh >/dev/null 2>&1; then
  echo "❌ GitHub CLI 'gh' is required. Install from https://cli.github.com and authenticate with 'gh auth login'." >&2
  exit 1
fi

# Defaults mirror the workflow inputs
APP_NAME="${APP_NAME:-Time Warp}"
EXECUTABLE_NAME="${EXECUTABLE_NAME:-time-warp}"
BUNDLE_ID="${BUNDLE_ID:-org.honeybadger.timewarp}"
VERSION="${VERSION:-3.0.0}"
BUILD_VERSION="${BUILD_VERSION:-1}"
WATCH=false
REF="${REF:-main}"

# Parse flags
while [[ $# -gt 0 ]]; do
  case "$1" in
    --app-name) APP_NAME="$2"; shift 2;;
    --executable-name) EXECUTABLE_NAME="$2"; shift 2;;
    --bundle-id) BUNDLE_ID="$2"; shift 2;;
    --version) VERSION="$2"; shift 2;;
    --build-version) BUILD_VERSION="$2"; shift 2;;
    --ref) REF="$2"; shift 2;;
    --watch) WATCH=true; shift;;
    -h|--help)
      grep -E '^(#|$)' "$0" | sed 's/^# \{0,1\}//'
      exit 0;
      ;;
    *)
      echo "Unknown argument: $1" >&2
      exit 2
      ;;
  esac
done

echo "🚀 Triggering workflow 'macOS Release (App Store)' on ref '$REF'..."

gh workflow run \
  ".github/workflows/macos-release.yml" \
  --ref "$REF" \
  -f app_name="$APP_NAME" \
  -f executable_name="$EXECUTABLE_NAME" \
  -f bundle_id="$BUNDLE_ID" \
  -f version="$VERSION" \
  -f build_version="$BUILD_VERSION" >/dev/null

echo "✅ Workflow dispatch requested."

if $WATCH; then
  echo "⏳ Watching the latest run for this workflow (Ctrl+C to stop)..."
  # This watches the most recent run of the named workflow
  gh run watch --exit-status --workflow "macOS Release (App Store)"
fi
