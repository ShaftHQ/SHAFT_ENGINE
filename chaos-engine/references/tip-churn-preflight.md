# Tip-churn preflight (batch micro-fixes)

Every tip push on a ChaosEngine PR re-runs PR Gate, Security, Codacy, and (when installer paths match) the fresh-installer matrix on ubuntu and Windows (macOS too with the `ci:installer-macos` label, #6187) for tens of minutes. PR #6158 paid that four times in one afternoon for Bandit, an inventory rehash, a Memory rehash, and a Windows flake skip (epic #6161). This preflight is portable: Codex, Claude, Grok CLI, Gemini, Copilot, and Grok Bot run the same checks; no host-only memory exception.

## Run before every push (#6164)

```bash
python3 .chaos-engine/skills/local-agency/scripts/tip_preflight.py
```

The `git push` guard runs the same checks through `scripts/ci/overlay_pre_push.py`. Each failure is one line; fix it in the same tip.

Checks:
- Bandit B607 on changed Python files (#6165).
- README inventory drift whenever a `chaos-engine/**/*.py` file changes; an added import changes the `python-libraries` table, so refresh with `python3 scripts/ci/validate_chaos_engine_readme.py --write` in the same commit. (repo-only)
- Memory `content_hash` for touched `.memory/memory/**` objects (#6169).

## Tip batching rule

Never push one tip per micro-fix when fresh-installer paths are implicated. Squash inventory, Bandit, Memory-hash, and known-flake fixes into the tip already in flight, or amend before pushing. When only `chaos-engine/**/*.md`, `nosec`, or inventory strings change, prefer the local validators over push-and-babysit. Required CI still runs on GitHub; this rule never skips it.

## macOS installer opt-in (#6187)

PRs run the fresh installer on ubuntu and Windows only. Add `ci:installer-macos` in the `gh pr create` step when a PR touches `chaos-engine/install.sh`, macOS-only installer branches, or `chaos-engine/distributions.json`. Without the label macOS runs post-merge on `main` (never cancelled; a red leg files a `ci-main-red` issue and a revert PR) and daily. PR Gate reads labels live and ignores label events (#6190); adding the label later re-runs the newest PR Gate run automatically (`installer-macos-rerun.yml`, #6208; fork PRs need a manual `gh run rerun <id>`).

## Bandit B607 (#6165)

- B607 flags a subprocess argv0 that is a bare executable name such as `"git"`.
- `# nosec B603` alone is insufficient when the executable is a bare name. Resolve once with `shutil.which()` and pass the absolute path (pattern: `stores.py`, `tool.py`).
- `# nosec B607` is allowed only with a justification comment; never mass-nosec, never disable the rule.
- Codacy `ACTION_REQUIRED` on B607 blocks merge like unit red; see [Codacy ACTION_REQUIRED gate](codacy-action-required-gate.md).

## Memory content_hash (#6169)

- Default write path: `memory save --stdin`.
- When a `.memory/**` body or sidecar changed outside `memory save`, recompute before push: `python3 .chaos-engine/skills/local-agency/scripts/tip_preflight.py --rehash .memory/memory/<type>/<slug>.json`.
- Recipe (mirrors `scripts/ci/validate_agent_setup.py` `memory_content_hash`): sha256 over the sidecar as sorted compact JSON without `content_hash`, a newline, then the LF-normalized body; stored as `sha256:<hex>`.
- The Agent Guidance Gate runs `validate_agent_setup`; a stale hash there is the `memory-content-hash` fingerprint.
- Never hand-author sidecars; `--rehash` rewrites only the stored hash.

## Related

- [CI status economy](ci-status-economy.md) (fingerprints, digest, one status channel)
- [Coach loop](../skills/local-agency/references/coach-loop.md)
- Script: [`tip_preflight.py`](../skills/local-agency/scripts/tip_preflight.py) (also `classify_failure` fingerprints)
