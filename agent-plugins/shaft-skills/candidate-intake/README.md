# Online skill-candidate intake

This gate applies before any online skill, script, fixture, or packaging example can enter
the SHAFT harness. Research never installs a candidate into a canonical skill root. Fetch a
pinned source into a disposable quarantine outside the repository, record it in
`candidates.json`, and process the stages in order.

## Stages and required evidence

1. **Provenance and license.** Record the official source, author, immutable commit or
   published version, exact source paths, applicable license/terms, and intended kind of reuse.
2. **Static review.** Run the containment, symlink, executable/script, binary, install-hook,
   dependency, and secret scan without executing the candidate. Record permissions, network
   behavior, scripts, overlap, and every unresolved item.
3. **Quarantine trial.** Use the command shape produced by
   `shaft_skill_candidate_intake.py`: an immutable container image, no network, read-only root,
   dropped capabilities, no new privileges, nonroot user, no credentials, read-only candidate
   and fixture mounts, bounded resources, and only a disposable `/output` mount. Canonical skill
   roots are never mounted. **No host fallback** is allowed when the container contract cannot be
   enforced.
4. **Local evaluation.** Evaluate the smallest relevant SHAFT fixtures and routing cases. Missing
   evidence is unknown, never a pass.
5. **Promotion.** Code or an executable tool can move only through a small, separate adoption PR
   that repeats the evidence, names the source and license, contains focused tests, and receives
   independent review. Tool evidence also pins the exact version plus a SHA-256 digest for every
   policy-required host platform artifact. Promotion requires current source evidence no older
   than the policy freshness window. This intake/report PR adopts no candidate code or tool.

Discovery paths and freshness are explicit evidence. Record every URL used to discover the
candidate, the date its upstream head was checked, that head's full commit SHA, and whether the
record is current, outdated, or retired. Structural validation remains deterministic; run the
optional freshness check when reviewing or refreshing external sources.

## HALT conditions

HALT immediately on unknown provenance or licensing; escaping paths or symlinks; secrets;
unreviewed executables, hooks, dependencies, or network behavior; a request for credentials,
privilege, host network, or canonical-root writes; unavailable containment; evaluation
regression; incomplete evidence; or vendor-specific policy duplication. Every later stage is
`not_run` after HALT, and the report retains the rejection and its reason.

## Decisions

- **adopt code** — all four gates passed and a separate adoption PR is linked.
- **adopt tool** — all four gates passed, supported artifacts are version-and-hash pinned, and a
  separate promotion PR is linked.
- **adopt a pattern** — reimplement a portable idea locally; copy no candidate files.
- **retain a test target** — keep a specification or example only as external conformance input.
- **reject** — record the candidate and the HALT reason; do not silently drop it.

Run `python scripts/ci/shaft_skill_candidate_intake.py` and, when checking upstream currency,
`python scripts/ci/shaft_skill_candidate_intake.py --check-freshness`. Then run
`python -m unittest tests.scripts.test_shaft_skill_candidate_intake -v` after changing the
policy, report, scanner, or promotion rules.
