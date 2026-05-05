# Repository History Rewrite Runbook

## Goal
Optimize clone/fetch performance by flattening repository history and reducing historical object payload while preserving maintainer attribution.

## Chosen Flattening Mode
- **Mode**: single-commit snapshot (maximum pack-size reduction)
- **Why**: this repository contains large historical binary artifacts; flattening to a single root commit yields the largest transfer reduction.
- **Maintainer preservation strategy**: preserve full pre-flatten attribution in `CONTRIBUTORS_HISTORY.md` (generated from full-history shortlog, normalized by email, aliases retained).

## Pre-Cutover Inputs
1. Ensure full history is present (`git fetch --unshallow origin` when required).
2. Generate baseline artifacts and manifest:
   ```bash
   /home/runner/work/SHAFT_ENGINE/SHAFT_ENGINE/scripts/maintenance/flatten-history.sh
   ```
3. Review baseline files in `/tmp/shaft-history-rewrite/`:
   - `pre-maintainers-shortlog.txt`
   - `pre-identities-raw.txt`
   - `pre-largest-blobs.txt`
   - `pre-count-objects.txt`
   - `pre-dotgit-size.txt`

## Retention / Removal Policy
- **Retain**: current `main` tree content needed for build/test/runtime/docs.
- **Remove from history**: all prior commit graph and unreachable historical blobs.
- **Result**: new history starts from one fresh root commit, with maintainer attribution preserved in manifest.

## Execute Local Rewrite
```bash
TARGET_BRANCH=main \
FLATTEN_BRANCH=history/flattened-main \
/home/runner/work/SHAFT_ENGINE/SHAFT_ENGINE/scripts/maintenance/flatten-history.sh --execute
```

> The script auto-configures a local fallback git identity when `user.name`/`user.email` are not set.

## Validation Checklist
- Maintainer manifest exists and is committed: `CONTRIBUTORS_HISTORY.md`
- Object/pack improvement:
  ```bash
  git count-objects -vH
  du -sh .git
  ```
- Clone benchmark (same runner, no local hardlink optimization):
  ```bash
  /usr/bin/time -f '%E real' git clone --no-local <repo-url> /tmp/clone-baseline
  /usr/bin/time -f '%E real' git clone --no-local <rewritten-repo-url> /tmp/clone-rewritten
  ```
- Ref/tag integrity review completed (release tags preserved only if intentionally recreated).
- Example measured results are documented in `docs/REPOSITORY_HISTORY_REWRITE_VALIDATION.md`.

## Maintainer Cutover Procedure (Force Push)
1. Announce maintenance window and temporary push freeze.
2. Create remote backup mirror before cutover:
   ```bash
   git clone --mirror <repo-url> /tmp/repo-backup-mirror.git
   ```
3. Push rewritten branch to a staging remote for verification.
4. After approval, force-update protected default branch (maintainer operation only).
5. Broadcast migration instructions to contributors:
   ```bash
   git fetch --all --prune
   git switch main
   git reset --hard origin/main
   git gc --prune=now
   ```

## Rollback Plan
- Keep backup mirror and pre-flatten backup ref until stability checks complete.
- If rollback is required, force-push backup mirror/ref back to default branch.
- Re-run post-rollback verification (tags, CI pipelines, release automation).
