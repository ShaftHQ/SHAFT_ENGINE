#!/usr/bin/env bash
set -euo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$REPO_ROOT"

TARGET_BRANCH="${TARGET_BRANCH:-main}"
FLATTEN_BRANCH="${FLATTEN_BRANCH:-history/flattened-${TARGET_BRANCH}}"
AUDIT_DIR="${AUDIT_DIR:-/tmp/shaft-history-rewrite}"
EXECUTE=false

if [[ "${1:-}" == "--execute" ]]; then
  EXECUTE=true
fi

if ! git rev-parse --git-dir >/dev/null 2>&1; then
  echo "Error: not a git repository." >&2
  exit 1
fi

if [[ -n "$(git status --porcelain)" ]]; then
  echo "Error: working tree must be clean before running history rewrite tooling." >&2
  exit 1
fi

mkdir -p "$AUDIT_DIR"

echo "[1/8] Ensuring full history is available..."
if [[ "$(git rev-parse --is-shallow-repository)" == "true" ]]; then
  git fetch --unshallow origin
fi
git fetch origin "$TARGET_BRANCH:refs/remotes/origin/$TARGET_BRANCH"

BASE_REF="refs/remotes/origin/$TARGET_BRANCH"

echo "[2/8] Capturing pre-rewrite baselines..."
git shortlog -se --all > "$AUDIT_DIR/pre-maintainers-shortlog.txt"
git log --all --format='%aN <%aE>|%cN <%cE>' | tr '|' '\n' | sed '/^$/d' | sort -u > "$AUDIT_DIR/pre-identities-raw.txt"
git count-objects -vH > "$AUDIT_DIR/pre-count-objects.txt"
du -sh .git > "$AUDIT_DIR/pre-dotgit-size.txt"
git rev-list --objects --all | git cat-file --batch-check='%(objecttype) %(objectname) %(objectsize) %(rest)' | awk '$1=="blob"{print $3"\t"$2"\t"$4}' | sort -nr | sed -n '1,100p' > "$AUDIT_DIR/pre-largest-blobs.txt"

echo "[3/8] Building normalized maintainer map..."
AUDIT_DIR="$AUDIT_DIR" python - <<'PY'
from pathlib import Path
import os
import re, collections
audit_dir = Path(os.environ['AUDIT_DIR'])
in_path = audit_dir / 'pre-maintainers-shortlog.txt'
out_path = audit_dir / 'pre-maintainers-normalized.tsv'
rows=[]
for line in in_path.read_text(encoding='utf-8').splitlines():
    m=re.match(r'\s*(\d+)\s+(.+?)\s+<([^>]+)>\s*$', line)
    if m:
        rows.append((int(m.group(1)), m.group(2).strip(), m.group(3).strip().lower()))
by_email=collections.defaultdict(lambda:[0,collections.Counter()])
for c,n,e in rows:
    by_email[e][0]+=c
    by_email[e][1][n]+=c
with out_path.open('w', encoding='utf-8') as f:
    f.write('total_commits\tcanonical_name\temail\taliases\\n')
    for email,(total,names) in sorted(by_email.items(), key=lambda kv:(-kv[1][0], kv[0])):
        canonical=max(names.items(), key=lambda x:(x[1],x[0]))[0]
        aliases='; '.join(f"{n} ({cnt})" for n,cnt in sorted(names.items(), key=lambda x:(-x[1],x[0])))
        f.write(f"{total}\t{canonical}\t{email}\t{aliases}\\n")
PY

echo "[4/8] Building maintainer manifest..."
AUDIT_DIR="$AUDIT_DIR" python - <<'PY'
from pathlib import Path
import os
rows=[]
for line in (Path(os.environ['AUDIT_DIR']) / 'pre-maintainers-normalized.tsv').read_text(encoding='utf-8').splitlines()[1:]:
    if not line.strip():
        continue
    total,name,email,aliases=line.split('\t',3)
    rows.append((int(total),name,email,aliases))
out = Path(os.environ['AUDIT_DIR']) / 'CONTRIBUTORS_HISTORY.md'
with out.open('w', encoding='utf-8') as f:
    f.write('# Maintainer History Manifest (Pre-Flatten Baseline)\\n\\n')
    f.write('Generated from full-history `git shortlog -se --all` before flattening.\\n')
    f.write('Normalization key: lower-cased email. Aliases preserve all observed display names.\\n\\n')
    f.write(f'- Distinct normalized emails: **{len(rows)}**\\n')
    f.write(f'- Total attributed commits in baseline: **{sum(r[0] for r in rows)}**\\n\\n')
    f.write('| Commits | Canonical Name | Email | Observed Aliases |\\n')
    f.write('|---:|---|---|---|\\n')
    for total,name,email,aliases in rows:
        f.write(f"| {total} | {name.replace('|','\\\\|')} | `{email}` | {aliases.replace('|','\\\\|').replace('; ','<br>')} |\\n")
PY

echo "[5/8] Rewrite mode: single-commit snapshot for maximum clone-size reduction."

echo "[6/8] Planned rewrite commands:"
echo "  git switch --orphan '$FLATTEN_BRANCH'"
echo "  git rm -rf ."
echo "  git checkout '$BASE_REF' -- ."
echo "  git add -A"
echo "  git commit -m 'Flatten git history and preserve maintainer manifest'"

echo "[7/8] Baseline artifacts saved in: $AUDIT_DIR"

if [[ "$EXECUTE" != true ]]; then
  cp "$AUDIT_DIR/CONTRIBUTORS_HISTORY.md" "$REPO_ROOT/CONTRIBUTORS_HISTORY.md"
  echo "Dry run complete. Re-run with --execute to perform orphan-branch flattening locally."
  exit 0
fi

echo "[8/8] Executing orphan-branch flattening locally..."
STAMP="$(date -u +%Y%m%dT%H%M%SZ)"
BACKUP_REF="backup/pre-flatten-${TARGET_BRANCH}-${STAMP}"
git branch "$BACKUP_REF" "$BASE_REF"
git switch --orphan "$FLATTEN_BRANCH"
git rm -rf . >/dev/null 2>&1 || true
git checkout "$BASE_REF" -- .
cp "$AUDIT_DIR/CONTRIBUTORS_HISTORY.md" "$REPO_ROOT/CONTRIBUTORS_HISTORY.md"
if [[ -z "$(git config --get user.name || true)" ]]; then
  git config user.name "History Rewrite Bot"
fi
if [[ -z "$(git config --get user.email || true)" ]]; then
  git config user.email "history-rewrite-bot@users.noreply.github.com"
fi
git add -A
git commit -m "Flatten git history and preserve maintainer manifest"

git count-objects -vH > "$AUDIT_DIR/post-count-objects.txt"
du -sh .git > "$AUDIT_DIR/post-dotgit-size.txt"
git shortlog -se --all > "$AUDIT_DIR/post-maintainers-shortlog.txt"

echo "Local flatten branch created: $FLATTEN_BRANCH"
echo "Backup branch created: $BACKUP_REF"
echo "IMPORTANT: force-push cutover must be coordinated by maintainers."
