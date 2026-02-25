#!/usr/bin/env bash
# setup_github_environments.sh
# Creates the three GitHub Environments used by deploy.yml.
# Requires: gh CLI (https://cli.github.com/) + gh auth login
set -euo pipefail

REPO="James-HoneyBadger/Time_Warp_Studio"

# ── Colours ────────────────────────────────────────────────────
GREEN='\033[0;32m'; YELLOW='\033[1;33m'; NC='\033[0m'

info()  { echo -e "${GREEN}[INFO]${NC}  $*"; }
warn()  { echo -e "${YELLOW}[WARN]${NC}  $*"; }

# ── Preflight ──────────────────────────────────────────────────
if ! command -v gh &>/dev/null; then
  echo "❌  GitHub CLI (gh) is not installed. See https://cli.github.com/"
  exit 1
fi

if ! gh auth status &>/dev/null; then
  echo "❌  Not logged in. Run: gh auth login"
  exit 1
fi

info "Creating GitHub Environments for ${REPO}"
echo ""

# ── Helper: create or update an environment ───────────────────
create_env() {
  local name="$1"
  local payload="$2"
  info "Configuring environment: ${name}"
  gh api \
    --method PUT \
    -H "Accept: application/vnd.github+json" \
    "/repos/${REPO}/environments/${name}" \
    --input - <<< "${payload}"
  echo ""
}

# ── staging (no protection — auto-deploys on every main push) ──
create_env "staging" '{
  "wait_timer": 0,
  "deployment_branch_policy": {
    "protected_branches": false,
    "custom_branch_policies": true
  }
}'

# Add branch policy: only "main" can deploy to staging
gh api \
  --method POST \
  -H "Accept: application/vnd.github+json" \
  "/repos/${REPO}/environments/staging/deployment-branch-policies" \
  --field name="main" \
  --field type="branch" \
  2>/dev/null || warn "staging branch policy may already exist"

# ── production (tag-gated; add reviewers after creation) ──────
# Note: prevent_self_review can only be enabled once at least one
# required reviewer has been added (GitHub API 422 otherwise).
create_env "production" '{
  "wait_timer": 0,
  "deployment_branch_policy": {
    "protected_branches": false,
    "custom_branch_policies": true
  }
}'

# Only version tags (v*) can deploy to production
gh api \
  --method POST \
  -H "Accept: application/vnd.github+json" \
  "/repos/${REPO}/environments/production/deployment-branch-policies" \
  --field name="v*" \
  --field type="tag" \
  2>/dev/null || warn "production tag policy may already exist"

# ── preview (open — all PRs can deploy) ───────────────────────
create_env "preview" '{
  "wait_timer": 0,
  "deployment_branch_policy": null
}'

# ── Done ──────────────────────────────────────────────────────
echo ""
info "All environments configured. Verify at:"
echo "  https://github.com/${REPO}/settings/environments"
echo ""
info "To add a required reviewer + enable prevent_self_review for production:"
echo "  # 1. Find your numeric user ID:"
echo "  gh api /users/<your-github-username> --jq '.id'"
echo ""
echo "  # 2. Apply the reviewer + flag in one call:"
echo "  gh api --method PUT /repos/${REPO}/environments/production \\"
echo "    --field prevent_self_review=true \\"
echo "    --field 'reviewers[][type]=User' \\"
echo "    --field 'reviewers[][id]=<numeric-user-id>'"
