#!/usr/bin/env bash
set -euo pipefail

# Simple helper: read a local .p12 and print base64 suitable for GitHub secret paste
# Usage: ./scripts/p12_to_base64.sh path/to/cert.p12

P12_PATH="${1:-}"
if [[ -z "$P12_PATH" ]]; then
  echo "Usage: $0 path/to/cert.p12" >&2
  exit 2
fi
if [[ ! -f "$P12_PATH" ]]; then
  echo "File not found: $P12_PATH" >&2
  exit 2
fi

base64 "$P12_PATH" | tr -d '\n'
echo
