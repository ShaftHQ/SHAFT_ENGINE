# Repository History Rewrite Validation (Temporary Clone)

This validation was executed in an isolated temporary clone (`/tmp/shaft-history-validation`) using:

```bash
TARGET_BRANCH=copilot/optimize-repository-cloning-performance \
FLATTEN_BRANCH=history/flattened-main \
AUDIT_DIR=/tmp/shaft-history-audit/rewrite-run \
/home/runner/work/SHAFT_ENGINE/SHAFT_ENGINE/scripts/maintenance/flatten-history.sh --execute
```

## Results

| Metric | Before | After (flattened history) |
|---|---:|---:|
| Reachable commit count (`git rev-list --count --all`) | 3010 | 1 |
| `.git` directory size | 410M | 102M |
| Packed object size (`size-pack`) | 407.25 MiB | 101.24 MiB |
| Fresh clone time (`git clone --no-local`) | 8.98s | 1.87s |

## Maintainer Preservation Check

Maintainer coverage is validated against `CONTRIBUTORS_HISTORY.md` generated from full pre-flatten history.

- Pre-flatten normalized emails: **62**
- Manifest emails: **62**
- Missing identities: **0**
- Extra identities: **0**

## Notes

- The flattened branch is a **single root commit** carrying the current tree plus `CONTRIBUTORS_HISTORY.md`.
- For production cutover, default-branch force-push and release-tag strategy must be coordinated with maintainers as documented in `docs/REPOSITORY_HISTORY_REWRITE_RUNBOOK.md`.
