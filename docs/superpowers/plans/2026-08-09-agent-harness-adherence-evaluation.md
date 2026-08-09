# Agent Harness Adherence Evaluation Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add a deterministic, provider-neutral corpus evaluator that validates reviewed agent-harness episodes and compares baseline and candidate adherence results.

**Architecture:** One CI script parses a small JSON corpus, materializes isolated temporary workspaces, evaluates recorded action evidence, and compares two reports. Reviewed JSON fixtures are the no-model corpus; focused unittests cover the public API and CLI.

**Tech Stack:** Python 3 standard library, `unittest`, JSON, existing `scripts/ci` conventions.

## Global Constraints

- Do not invoke a model, external service, subscription, or API key.
- Store only reviewed corpus fixtures in Git; reports and evidence remain caller-provided or temporary.
- Missing observations are `unknown`, never a pass.
- Required-action and prohibited-action adherence are separate metrics.
- A baseline-pass/candidate-fail prohibition makes the comparison release gate fail.
- The evaluator cannot modify guard policy.

---

### Task 1: Define the corpus contract and fixture

**Files:**
- Create: `tests/fixtures/agent_harness_adherence/corpus.json`
- Create: `tests/fixtures/agent_harness_adherence/baseline.json`
- Create: `tests/fixtures/agent_harness_adherence/candidate_regression.json`
- Create: `tests/scripts/test_agent_harness_adherence.py`

**Interfaces:** Each version-1 episode has `id`, `rule_ids`, `horizon`, `workspace.files`, and `expectations`; evidence is keyed by episode ID and contains `actions` and `guard_outcomes`.

- [ ] Write `test_validate_corpus_accepts_the_reviewed_fixture`, then run `py -3 -m unittest tests.scripts.test_agent_harness_adherence.AgentHarnessAdherenceTest.test_validate_corpus_accepts_the_reviewed_fixture -v`. It must fail because `scripts.ci.agent_harness_adherence` does not exist.
- [ ] Add fixtures covering short/medium/long horizons, a required action, a prohibited action, a guard report/remedy, and a false-block case. The baseline passes all cases; the candidate regresses exactly one prohibition while retaining a required action.

### Task 2: Validate and materialize episodes

**Files:**
- Create: `scripts/ci/agent_harness_adherence.py`
- Modify: `tests/scripts/test_agent_harness_adherence.py`

**Interfaces:** `load_json(path: Path) -> dict`, `validate_corpus(corpus: dict) -> list[str]`, and `materialize_workspace(episode: dict, directory: Path) -> None`.

- [ ] Write a failing test for an escaping `../outside` workspace path; run only that test and confirm the missing validator causes the failure.
- [ ] Implement version, unique-ID, rule-ID, horizon, relative-workspace-path, and expectation-kind validation. Permit only `requires`, `forbids`, and `guard`; materialize files only under the temporary root.
- [ ] Run `py -3 -m unittest tests.scripts.test_agent_harness_adherence -v` and commit the green implementation.

### Task 3: Evaluate and compare evidence

**Files:**
- Modify: `scripts/ci/agent_harness_adherence.py`
- Modify: `tests/scripts/test_agent_harness_adherence.py`

**Interfaces:** `evaluate(corpus: dict, evidence: dict) -> dict` and `compare(baseline: dict, candidate: dict) -> dict`.

- [ ] Write a failing test asserting that the candidate prohibition regression is listed and `release_gate_passed` is false even though required-action behavior passes.
- [ ] Implement `requires` as action presence, `forbids` as action absence, and `guard` as requested outcome plus nonempty remedy. Report strict episode pass, per-rule required/prohibited metrics, false-block and actionable-remedy counts, and unknown rule IDs.
- [ ] Re-run the focused suite and commit only after it is green.

### Task 4: Add a CLI and final checks

**Files:**
- Modify: `scripts/ci/agent_harness_adherence.py`
- Modify: `tests/scripts/test_agent_harness_adherence.py`

**Interfaces:** CLI accepts `--corpus`, exactly one of `--evidence` or `--baseline` plus `--candidate`, and `--json`.

- [ ] Write a failing subprocess test for baseline/candidate comparison; it must expect exit 1 and a JSON `prohibition_regressions` field.
- [ ] Implement JSON reports. Valid single evidence measurements exit 0; prohibition regressions exit 1; malformed inputs exit 2. The CLI must not execute a caller command or write output files.
- [ ] Run `py -3 -m unittest tests.scripts.test_agent_harness_adherence tests.scripts.test_agent_harness_portability -v` and `py -3 scripts/ci/validate_agent_setup.py --skip-external`, then commit the verified work.
