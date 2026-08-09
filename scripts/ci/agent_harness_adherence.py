"""Validate deterministic agent-harness adherence corpus fixtures."""

from __future__ import annotations

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
    if corpus.get("schema_version") != 1:
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

    rules: dict[str, dict] = {}
    episodes: dict[str, dict] = {}
    unmeasured_rule_ids: set[str] = set()
    false_block_count = 0
    actionable_remedy_count = 0
    for episode in corpus["episodes"]:
        identifier = episode["id"]
        episode_evidence = evidence_by_episode.get(identifier)
        if not isinstance(episode_evidence, dict):
            episodes[identifier] = {"strict_episode_pass": None, "expectations": []}
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
