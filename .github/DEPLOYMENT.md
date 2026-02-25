# Time Warp Studio — GitHub Deployments

This document explains the deployment pipeline and how to configure the required GitHub Environments.

---

## Deployment Environments

| Environment | Trigger | Purpose |
|-------------|---------|---------|
| `staging`   | Push to `main` | Validates Docker image in GHCR after every merge |
| `production`| Push of a `v*` tag | Publishes a GitHub Release with binary artifacts |
| `preview`   | Pull request opened/updated | Reports test status back to the PR |

---

## One-time Environment Setup

Run the script below (requires the [GitHub CLI](https://cli.github.com/) and `gh auth login`):

```bash
./Scripts/setup_github_environments.sh
```

Or configure manually in **Settings → Environments**:

### `staging`
- **Required reviewers:** _(none — auto-deploys on merge)_
- **Environment secrets:** _(none required beyond `GITHUB_TOKEN`)_

### `production`
- **Required reviewers:** Add at least one maintainer
- **Wait timer:** 0 minutes (or set a delay for safety)
- **Environment URL:** `https://github.com/James-HoneyBadger/Time_Warp_Studio/releases`

### `preview`
- **Required reviewers:** _(none)_

---

## Workflow: `deploy.yml`

```
Push to main ──► context ──► test ──► build-image ──► deploy-staging
                                  └──► deploy-preview  (PRs only)

Push v* tag  ──► context ──► test ──► build-image ──► deploy-production
                                  └──► build-binaries ─┘
```

### Job descriptions

| Job | Description |
|-----|-------------|
| `context` | Resolves environment name, version string, and release flag |
| `test` | Runs the pytest suite as a pre-deploy gate |
| `build-image` | Builds & pushes the Docker image to GHCR |
| `build-binaries` | Compiles native Linux binaries (releases only) |
| `deploy-staging` | Creates a GitHub Deployment and marks it success/failure |
| `deploy-production` | Creates a GitHub Release, attaches binaries, marks deployment |
| `deploy-preview` | Reports status on PRs via a bot comment |
| `deploy-summary` | Writes a Markdown job summary for easy audit |

---

## Creating a Release

```bash
# Tag and push — the workflow handles the rest
git tag v1.2.0 -m "Release 1.2.0"
git push origin v1.2.0
```

Pre-release versions (e.g. `v1.2.0-beta.1`) are automatically marked as pre-releases in GitHub Releases.

---

## Docker Images (GHCR)

| Tag | When set |
|-----|----------|
| `latest` | Every push to `main` |
| `stable` | Every production (`v*`) release |
| `<version>` | e.g. `v1.2.0` on tag push |
| `staging-<sha>` | Every `main` branch push |
| `pr-<N>` | Pull request builds |

```bash
docker pull ghcr.io/james-honeybadger/time_warp_studio:latest
```
