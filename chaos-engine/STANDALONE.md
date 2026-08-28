# Spec: migrate ChaosEngine to ShaftHQ/chaos-engine

Status: specification delivered by ShaftHQ/SHAFT_ENGINE#5225; migration not
implemented as of 2026-08-28. This file is origin-only and is not copied into
adopter payloads. Parent tracker: ShaftHQ/SHAFT_ENGINE#5223. The current
bootstrap still selects the nested `chaos-engine/` source prefix, and no
standalone repository is assumed to exist. Do not create the repository,
rewrite history, or change prefix selection in a documentation-only change.

## Goal

`ShaftHQ/chaos-engine` becomes the public source of the portable harness. The
tree lives at repository root (`install.ps1`, `install.sh`, `bootstrap.py`,
`install.py`, `skills/`, `profiles/portable/`, `references/`, `vendor/`, …).
The public one-liner becomes:

```powershell
irm "https://raw.githubusercontent.com/ShaftHQ/chaos-engine/main/install.ps1" | iex
```

```bash
curl -fsSL "https://raw.githubusercontent.com/ShaftHQ/chaos-engine/main/install.sh" | bash -s -- "https://raw.githubusercontent.com/ShaftHQ/chaos-engine/main/install.sh"
```

Wrappers already derive owner/repository and the sibling `bootstrap.py` from that
URL (SHAFT_ENGINE#5224). The remaining gap is `bootstrap.py` still selecting
GitHub tree entries whose first path part is `chaos-engine/`.

## Current layout

| Location | Role |
| --- | --- |
| `ShaftHQ/SHAFT_ENGINE/chaos-engine/` | Canonical source tree today |
| GitHub tree prefix `chaos-engine/` | `bootstrap.py` download filter and raw path |
| `profiles/portable/` | Public default distribution |
| `profiles/shaft/` | SHAFT-only profile, selected via `installWhen.mavenArtifactIds` |
| `tests/scripts/test_chaos_engine_*.py` | Proof in SHAFT_ENGINE |
| `assets/brand/`, `RESEARCH.md`, this file | Origin-only; omitted from adopter payload |

## Key decisions

1. **End state is a real standalone repository, not a forever-mirror.** User
   requested a direct migration to `ShaftHQ/chaos-engine`. A publish Action that
   leaves SHAFT_ENGINE as the only writable source is rejected as the end state.
2. **History is preserved** with `git filter-repo --path chaos-engine/ --path-rename chaos-engine/:`.
   A fresh orphan branch would drop blame and review context.
3. **SHAFT profile stays in SHAFT_ENGINE.** It is already excluded from the
   portable payload. The public repo ships `profiles/portable/` only.
4. **Shim the old nested URL** for a dated deprecation window so existing
   `.../SHAFT_ENGINE/main/chaos-engine/install.ps1` one-liners keep working.
5. **Bootstrap learns root-or-nested**, then the new one-liner works without a
   second wrapper change.

## Bootstrap prefix (later, PR 1)

Today `download_source` skips every tree entry whose `path.parts[0] != "chaos-engine"`,
then strips that prefix.

Change:

- If the recursive tree contains `skills/chaos-engine/SKILL.md` at repo root,
  treat the repository as the source tree (standalone).
- Else if it contains `chaos-engine/skills/chaos-engine/SKILL.md`, keep today’s
  nested filter (SHAFT_ENGINE shim and any remaining nested hosts).
- Else fail closed: unexpected layout.
- Raw file URLs must use the same prefix the filter selected.
- Origin-only omit list stays `assets/brand/**` plus `RESEARCH.md` and
  `STANDALONE.md` (and later siblings in that omit set).
- Proof: existing nested-tree bootstrap tests stay green; new tests feed a
  root-tree fixture and assert `skills/chaos-engine/SKILL.md` is installed and
  `chaos-engine/` is not required. A mutation that only understands nested
  paths must fail the root fixture.

## Cut over (later, PR 2)

1. Create empty `ShaftHQ/chaos-engine` (MIT, public).
2. From a throwaway clone of SHAFT_ENGINE:
   `git filter-repo --path chaos-engine/ --path-rename chaos-engine/:`
   then push the filtered history to `ShaftHQ/chaos-engine` `main`.
3. Do not force-push SHAFT_ENGINE. Do not rewrite SHAFT_ENGINE history.
4. Confirm HEAD of the new repo has `skills/chaos-engine/SKILL.md` at root and
   no extra wrapping `chaos-engine/` directory.

## Tests and CI (later, PR 3)

Move `tests/scripts/test_chaos_engine_*.py` (and only their dedicated fixtures)
with the project. SHAFT_ENGINE CI either:

- calls the standalone repo’s workflow, or
- vendors a subtree/submodule of the tests until SHAFT-only jobs no longer need
  them.

Do not leave two drifting copies of the same assertion. Portable forbidden-token
tests stay with the portable tree.

## SHAFT_ENGINE shim (later, PR 4)

Keep `chaos-engine/install.ps1` and `install.sh` in SHAFT_ENGINE as wrappers that
still parse the nested URL and still point bootstrap at the nested tree **or**
redirect `--repository ShaftHQ/chaos-engine` once bootstrap understands root
trees. Document a deprecation date. After that date, PR 5 removes the shim and
leaves a short INSTALL note pointing at the standalone one-liner.

SHAFT profile one-liners switch to `ShaftHQ/chaos-engine` when the shim is no
longer the supported path.

## Adopter upgrade

Re-run the one-liner. Bootstrap already upgrades to the resolved commit and
leaves the last verified install in place on a failed download. No silent
conversion of legacy pre-distribution installs (existing rule unchanged).

## Invariants that must survive

- Portable payload still forbids `shaft` / `mohab` / `act-as-mohab` tokens.
- `portable` remains the public default; repository profile install still
  requires `installWhen.mavenArtifactIds` (or an explicit `--distribution`).
- Install target remains the caller’s current working directory.
- Doctor still reports the 40-character commit and required component health.
- Brand masters and RESEARCH stay origin-only.
- Vendor Caveman/Ponytail pins, licenses, and third-party notices move with the
  tree.

## Non-goals of the spec-only change

Creating `ShaftHQ/chaos-engine`, running filter-repo, moving tests, changing
`bootstrap.py` prefix selection, publishing, or rewriting SHAFT_ENGINE history.

## Later PR plan

| Order | Title | Depends on |
| --- | --- | --- |
| 1 | Bootstrap accepts a root source tree or the nested `chaos-engine/` prefix | this spec |
| 2 | Create `ShaftHQ/chaos-engine` with filtered history | 1 |
| 3 | Move ChaosEngine tests/CI to the standalone repo | 2 |
| 4 | Point docs and SHAFT profile at the standalone one-liner; keep nested shim | 2 |
| 5 | Remove the SHAFT_ENGINE nested shim after the deprecation date | 4 |
