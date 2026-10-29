# Content-scan preflight (G-META pattern)

License / banned-stack / forbidden-dependency preflights must not trust
path or filename denylists alone.

## Rule

A preflight that claims an environment is clean must:

1. **Content-scan** (`rg`-equivalent, word-boundary) over the real trees that
   can load code or weights: project `src`, virtualenv `site-packages`, model /
   weight dirs, and `custom_nodes` (or the project's equivalents).
2. Expose a **`--self-test`** (or test helper) that **plants** a banned token
   file under a temp tree and expects **FAIL**. A check that cannot fail is
   vacuous ([delegation](delegation.md) `vacuous-check`).
3. Run the same gate on the **live work tree** used for receipts, not a
   sanitized copy.

Path/filename denylists are a fast first pass only. They are not enough.

## Why

Name-only checks miss banned stacks still present under `site-packages` or
`custom_nodes` while reporting PASS. Planted self-tests catch that class of
false PASS before a human review round does.

## Check

- `--self-test` fails when a banned file is planted.
- Live run reports `ok: true` with provenance JSON (or equivalent receipt).
- Reintroducing a banned import / class / weight path ⇒ FAIL.

## Helper

Reusable planted self-test:
[`content_scan_preflight.py`](../content_scan_preflight.py)
(`python3 -m` from repo root, or
`python3 chaos-engine/content_scan_preflight.py --self-test`).

Project gates keep their own token lists; this module only proves the
content-scan + plant pattern.
