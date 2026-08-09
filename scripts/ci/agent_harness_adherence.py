"""Validate deterministic agent-harness adherence corpus fixtures."""

from __future__ import annotations

import argparse
import json
import tempfile
from pathlib import Path, PurePosixPath, PureWindowsPath

VALID_HORIZONS = {"short", "medium", "long"}
VALID_EXPECTATION_KINDS = {"requires", "forbids", "guard"}
WINDOWS_RESERVED_NAMES = {
    "CON",
    "PRN",
    "AUX",
    "NUL",
    *(f"COM{number}" for number in range(1, 10)),
    *(f"LPT{number}" for number in range(1, 10)),
}


def _is_windows_reserved_name(part: str) -> bool:
    return part.split(".", 1)[0].upper() in WINDOWS_RESERVED_NAMES


def _is_nonempty_string(value: object) -> bool:
    return isinstance(value, str) and bool(value)


def _is_safe_relative_file(path: object) -> bool:
    if not isinstance(path, str) or not path:
        return False
    posix_path = PurePosixPath(path)
    windows_path = PureWindowsPath(path)
    raw_parts = path.replace("\\", "/").split("/")
    return bool(raw_parts) and not (
        posix_path.is_absolute()
        or windows_path.is_absolute()
        or windows_path.drive
        or windows_path.root
        or ".." in posix_path.parts
        or ".." in windows_path.parts
        or any(
            not part
            or part in {".", ".."}
            or ":" in part
            or part.endswith((".", " "))
            or _is_windows_reserved_name(part)
            for part in raw_parts
        )
    )


def validate_corpus(corpus: dict) -> list[str]:
    """Return structural errors for a version-1 adherence corpus."""
    errors: list[str] = []
    schema_version = corpus.get("schema_version")
    if (
        not isinstance(schema_version, int)
        or isinstance(schema_version, bool)
        or schema_version != 1
    ):
        errors.append("schema_version must be 1")

    episodes = corpus.get("episodes")
    if not isinstance(episodes, list) or not episodes:
        return [*errors, "episodes must be a nonempty list"]

    identifiers: set[str] = set()
    for episode in episodes:
        if not isinstance(episode, dict):
            errors.append("episode must be an object")
            continue
        identifier = episode.get("id")
        if not isinstance(identifier, str) or not identifier:
            errors.append("episode id must be a nonempty string")
        elif identifier in identifiers:
            errors.append(f"duplicate episode id: {identifier}")
        else:
            identifiers.add(identifier)
        rule_ids = episode.get("rule_ids")
        if not isinstance(rule_ids, list) or not rule_ids or not all(
            isinstance(rule_id, str) and rule_id for rule_id in rule_ids
        ):
            errors.append(f"{identifier}: rule_ids must be nonempty strings")
        if episode.get("horizon") not in VALID_HORIZONS:
            errors.append(f"{identifier}: horizon must be short, medium, or long")
        workspace = episode.get("workspace")
        files = workspace.get("files") if isinstance(workspace, dict) else None
        if not isinstance(files, dict):
            errors.append(f"{identifier}: workspace.files must be an object")
        elif any(
            not _is_safe_relative_file(path) or not isinstance(contents, str)
            for path, contents in files.items()
        ):
            errors.append(f"{identifier}: escape workspace path or non-string contents")
        expectations = episode.get("expectations")
        if not isinstance(expectations, list) or not expectations:
            errors.append(f"{identifier}: expectations must be a nonempty list")
        elif any(
            not isinstance(expectation, dict)
            or expectation.get("kind") not in VALID_EXPECTATION_KINDS
            or expectation.get("kind") in {"requires", "forbids"}
            and not _is_nonempty_string(expectation.get("action"))
            or expectation.get("kind") == "guard"
            and (
                not _is_nonempty_string(expectation.get("outcome"))
                or not _is_nonempty_string(expectation.get("remedy"))
            )
            for expectation in expectations
        ):
            errors.append(f"{identifier}: expectation kind is invalid")
    return errors


def validate_evidence(corpus: dict, evidence_by_episode: dict) -> list[str]:
    """Return structural errors for evidence bound exactly to ``corpus``."""
    if not isinstance(evidence_by_episode, dict):
        return ["evidence must be an object"]
    corpus_identifiers = {episode["id"] for episode in corpus["episodes"]}
    evidence_identifiers = set(evidence_by_episode)
    errors: list[str] = []
    extra = evidence_identifiers - corpus_identifiers
    if extra:
        errors.append(f"evidence has unknown episode IDs: {', '.join(sorted(extra))}")
    for identifier, episode_evidence in evidence_by_episode.items():
        if not isinstance(episode_evidence, dict):
            errors.append(f"{identifier}: evidence must be an object")
            continue
        actions = episode_evidence.get("actions", [])
        if not isinstance(actions, list) or not all(_is_nonempty_string(action) for action in actions):
            errors.append(f"{identifier}: actions must be a list of nonempty strings")
        guard_outcomes = episode_evidence.get("guard_outcomes", [])
        if not isinstance(guard_outcomes, list) or any(
            not isinstance(outcome, dict)
            or not _is_nonempty_string(outcome.get("outcome"))
            or not _is_nonempty_string(outcome.get("remedy"))
            for outcome in guard_outcomes
        ):
            errors.append(f"{identifier}: guard_outcomes must contain outcome and remedy strings")
    return errors


def materialize_workspace(episode: dict, directory: Path) -> Path:
    """Create and populate a fresh disposable workspace below ``directory``."""
    root = Path(tempfile.mkdtemp(prefix="agent-harness-", dir=directory.resolve())).resolve()
    files = episode["workspace"]["files"]
    for relative_path, contents in files.items():
        if not _is_safe_relative_file(relative_path) or not isinstance(contents, str):
            raise ValueError(f"unsafe workspace file: {relative_path!r}")
        destination = (root / relative_path).resolve()
        try:
            destination.relative_to(root)
        except ValueError as error:
            raise ValueError(f"workspace path escapes root: {relative_path!r}") from error
        destination.parent.mkdir(parents=True, exist_ok=True)
        destination.write_text(contents, encoding="utf-8")
    return root


def _expectation_result(expectation: dict, evidence: dict) -> bool | None:
    if expectation["kind"] == "requires":
        if not isinstance(evidence.get("actions"), list):
            return None
        return expectation.get("action") in evidence.get("actions", [])
    if expectation["kind"] == "forbids":
        if not isinstance(evidence.get("actions"), list):
            return None
        return expectation.get("action") not in evidence.get("actions", [])
    if not isinstance(evidence.get("guard_outcomes"), list):
        return None
    return any(
        outcome.get("outcome") == expectation.get("outcome")
        and outcome.get("remedy") == expectation.get("remedy")
        and _is_nonempty_string(outcome.get("remedy"))
        for outcome in evidence.get("guard_outcomes", [])
        if isinstance(outcome, dict)
    )


def evaluate(corpus: dict, evidence_by_episode: dict) -> dict:
    """Evaluate recorded action evidence against a validated corpus."""
    errors = validate_corpus(corpus)
    if errors:
        raise ValueError("; ".join(errors))
    if not isinstance(evidence_by_episode, dict):
        raise ValueError("evidence must be an object")

    rules: dict[str, dict] = {}
    episodes: dict[str, dict] = {}
    unmeasured_rule_ids: set[str] = set()
    false_block_count = 0
    actionable_remedy_count = 0
    for episode in corpus["episodes"]:
        identifier = episode["id"]
        episode_evidence = evidence_by_episode.get(identifier)
        if not isinstance(episode_evidence, dict):
            episodes[identifier] = {
                "rule_ids": episode["rule_ids"],
                "strict_episode_pass": None,
                "expectations": [],
            }
            unmeasured_rule_ids.update(episode["rule_ids"])
            continue

        expectation_results = []
        guard_outcomes = episode_evidence.get("guard_outcomes")
        guard_outcomes = guard_outcomes if isinstance(guard_outcomes, list) else []
        for expectation in episode["expectations"]:
            passed = _expectation_result(expectation, episode_evidence)
            expectation_results.append({"kind": expectation["kind"], "passed": passed})
            for rule_id in episode["rule_ids"]:
                rule = rules.setdefault(
                    rule_id,
                    {
                        "required_action_adherence": {"passed": 0, "total": 0},
                        "prohibited_action_adherence": {"passed": 0, "total": 0},
                    },
                )
                metric = {
                    "requires": "required_action_adherence",
                    "forbids": "prohibited_action_adherence",
                }.get(expectation["kind"])
                if metric:
                    if passed is not None:
                        rule[metric]["total"] += 1
                        if passed:
                            rule[metric]["passed"] += 1
            if passed is None:
                unmeasured_rule_ids.update(episode["rule_ids"])
        if any(
            expectation["kind"] == "guard" and expectation.get("outcome") == "silent"
            for expectation in episode["expectations"]
        ):
            false_block_count += sum(
                outcome.get("outcome") == "blocks"
                for outcome in guard_outcomes
                if isinstance(outcome, dict)
            )
        actionable_remedy_count += sum(
            bool(outcome.get("remedy")) and outcome.get("remedy") != "none"
            for outcome in guard_outcomes
            if isinstance(outcome, dict)
        )
        episodes[identifier] = {
            "rule_ids": episode["rule_ids"],
            "strict_episode_pass": (
                None
                if any(result["passed"] is None for result in expectation_results)
                else all(result["passed"] for result in expectation_results)
            ),
            "expectations": expectation_results,
        }
    return {
        "episodes": episodes,
        "rules": rules,
        "guard_metrics": {
            "false_block_count": false_block_count,
            "actionable_remedy_count": actionable_remedy_count,
        },
        "unmeasured_rule_ids": sorted(unmeasured_rule_ids),
    }


def compare(baseline: dict, candidate: dict) -> dict:
    """Compare two evaluated reports without hiding prohibition regressions."""
    prohibition_regressions: list[str] = []
    prohibition_regression_rule_ids: set[str] = set()
    comparison_errors: list[str] = []
    if not isinstance(baseline, dict) or not isinstance(candidate, dict):
        comparison_errors.append("both reports must be objects")
        baseline = {}
        candidate = {}
    baseline_episodes = baseline.get("episodes", {})
    candidate_episodes = candidate.get("episodes", {})
    if not isinstance(baseline_episodes, dict) or not isinstance(candidate_episodes, dict):
        comparison_errors.append("both reports must contain an episodes object")
        baseline_episodes = {}
        candidate_episodes = {}
    elif set(baseline_episodes) != set(candidate_episodes):
        comparison_errors.append("reports do not contain the same episode IDs")
    for identifier, baseline_episode in baseline_episodes.items():
        candidate_episode = candidate_episodes.get(identifier)
        if not isinstance(baseline_episode, dict) or not isinstance(candidate_episode, dict):
            comparison_errors.append(f"{identifier}: episode report is invalid")
            continue
        baseline_expectations = baseline_episode.get("expectations", [])
        candidate_expectations = candidate_episode.get("expectations", [])
        if not isinstance(baseline_expectations, list) or not isinstance(candidate_expectations, list):
            comparison_errors.append(f"{identifier}: expectations are invalid")
            continue
        baseline_kinds = [
            expectation.get("kind") if isinstance(expectation, dict) else None
            for expectation in baseline_expectations
        ]
        candidate_kinds = [
            expectation.get("kind") if isinstance(expectation, dict) else None
            for expectation in candidate_expectations
        ]
        if (
            not all(isinstance(expectation, dict) for expectation in baseline_expectations)
            or not all(isinstance(expectation, dict) for expectation in candidate_expectations)
            or any(
                expectation.get("kind") not in VALID_EXPECTATION_KINDS
                or "passed" not in expectation
                or not (
                    isinstance(expectation["passed"], bool)
                    or expectation["passed"] is None
                )
                for expectation in [*baseline_expectations, *candidate_expectations]
            )
        ):
            comparison_errors.append(f"{identifier}: expectation entries are invalid")
            continue
        if baseline_kinds != candidate_kinds:
            comparison_errors.append(f"{identifier}: expectation shapes differ")
            continue
        if any(
            baseline_expectation.get("kind") == "forbids"
            and baseline_expectation.get("passed") is True
            and candidate_expectation.get("kind") == "forbids"
            and candidate_expectation.get("passed") is not True
            for baseline_expectation, candidate_expectation in zip(
                baseline_expectations, candidate_expectations
            )
        ):
            prohibition_regressions.append(identifier)
            rule_ids = baseline_episode.get("rule_ids", [])
            if isinstance(rule_ids, list):
                prohibition_regression_rule_ids.update(
                    rule_id for rule_id in rule_ids if _is_nonempty_string(rule_id)
                )
    baseline_unmeasured = baseline.get("unmeasured_rule_ids", [])
    candidate_unmeasured = candidate.get("unmeasured_rule_ids", [])
    if not all(
        isinstance(unmeasured, list)
        and all(_is_nonempty_string(rule_id) for rule_id in unmeasured)
        for unmeasured in (baseline_unmeasured, candidate_unmeasured)
    ):
        comparison_errors.append("reports have invalid unmeasured_rule_ids")
        unmeasured_rule_ids: list[str] = []
    else:
        unmeasured_rule_ids = sorted(set(baseline_unmeasured) | set(candidate_unmeasured))
        if unmeasured_rule_ids:
            comparison_errors.append("reports have unmeasured rule IDs")
    return {
        "prohibition_regressions": prohibition_regressions,
        "prohibition_regression_rule_ids": sorted(prohibition_regression_rule_ids),
        "comparison_errors": comparison_errors,
        "release_gate_passed": not prohibition_regressions and not comparison_errors,
        "unmeasured_rule_ids": unmeasured_rule_ids,
    }


def load_json(path: Path) -> dict:
    """Read one caller-supplied JSON document without writing any run state."""
    with path.open(encoding="utf-8") as source:
        value = json.load(source)
    if not isinstance(value, dict):
        raise ValueError(f"{path}: expected a JSON object")
    return value


def build_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(
        description=__doc__,
        epilog=(
            "Add only reviewed cases with stable IDs, rule IDs, a horizon, and a minimal "
            "workspace. Freeze a baseline only for reviewed intended behavior; do not update "
            "it merely to pass a candidate. Missing observations are unknown, and comparisons "
            "fail their release gate when rules are unmeasured. Keep cases varied so they test "
            "the harness rule rather than a prompt-specific response."
        ),
    )
    parser.add_argument("--corpus", type=Path, required=True)
    inputs = parser.add_mutually_exclusive_group(required=True)
    inputs.add_argument("--evidence", type=Path)
    inputs.add_argument("--baseline", type=Path)
    parser.add_argument("--candidate", type=Path)
    parser.add_argument("--json", action="store_true", help="emit the JSON report (default)")
    return parser


def main(arguments: list[str] | None = None) -> int:
    parser = build_parser()
    options = parser.parse_args(arguments)
    if options.baseline and not options.candidate:
        parser.error("--candidate is required with --baseline")
    if options.candidate and not options.baseline:
        parser.error("--candidate requires --baseline")
    try:
        corpus = load_json(options.corpus)
        if errors := validate_corpus(corpus):
            raise ValueError("; ".join(errors))
        if options.evidence:
            evidence = load_json(options.evidence)
            if errors := validate_evidence(corpus, evidence):
                raise ValueError("; ".join(errors))
            report = evaluate(corpus, evidence)
            exit_code = 0
        else:
            baseline = load_json(options.baseline)
            candidate = load_json(options.candidate)
            if errors := validate_evidence(corpus, baseline):
                raise ValueError("baseline: " + "; ".join(errors))
            if errors := validate_evidence(corpus, candidate):
                raise ValueError("candidate: " + "; ".join(errors))
            report = compare(
                evaluate(corpus, baseline),
                evaluate(corpus, candidate),
            )
            exit_code = 1 if not report["release_gate_passed"] else 0
    except (OSError, ValueError, json.JSONDecodeError) as error:
        parser.error(str(error))
    print(json.dumps(report, indent=2, sort_keys=True))
    return exit_code


if __name__ == "__main__":
    raise SystemExit(main())
