#!/usr/bin/env python3
"""Compile and validate the canonical SHAFT routing evaluation corpus (#4642)."""

from __future__ import annotations

import argparse
import json
import re
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
HOSTS_RELATIVE = Path("scripts/mcp/agent_hosts.json")
NATIVE_EVIDENCE_LABELS = {"claude": "Claude Code", "codex": "Codex CLI"}
CORPUS_RELATIVE = Path("agent-plugins/shaft-skills/evals/cases.json")
REVIEW_RELATIVE = Path("shaft-skills/quality-review.json")
CLAUDE_RELATIVE = Path("agent-plugins/shaft-skills/evals/claude/evals.json")
CODEX_CASES_RELATIVE = Path("agent-plugins/shaft-skills/evals/codex/cases.jsonl")
CODEX_SCHEMA_RELATIVE = Path("agent-plugins/shaft-skills/evals/codex/output-schema.json")


def _defect(code: str, message: str) -> dict:
    return {"code": code, "message": message}


def _json_text(value: object) -> str:
    return json.dumps(value, indent=2, ensure_ascii=False) + "\n"


def output_schema() -> dict:
    return {
        "$schema": "https://json-schema.org/draft/2020-12/schema",
        "type": "object",
        "properties": {
            "chosen_skill": {
                "type": "string",
                "description": "Canonical name of the one SHAFT specialist selected for the immediate deliverable.",
            }
        },
        "required": ["chosen_skill"],
        "additionalProperties": False,
    }


def validate_corpus(corpus: dict, review: dict) -> list[dict]:  # noqa: MC0001  # Linear schema checks stay auditable together.
    defects: list[dict] = []
    skills = set(review.get("skills", {}))
    cases = corpus.get("cases")
    schema_version = corpus.get("schema_version")
    if type(schema_version) is not int or schema_version != 1:  # pylint: disable=unidiomatic-typecheck  # Exact type rejects bool aliases.
        defects.append(_defect("schema-version", "schema_version must be 1"))
    if corpus.get("package") != "shaft-skills":
        defects.append(_defect("package", "package must be shaft-skills"))
    expected_thresholds = {
        "case_pass_rate": 1.0,  # nosec B105 - routing threshold, not a credential.
        "positive_skill_coverage": 1.0,
    }
    thresholds = corpus.get("thresholds")
    thresholds_match = isinstance(thresholds, dict) and set(thresholds) == set(expected_thresholds)
    if thresholds_match:
        thresholds_match = all(
            type(thresholds[key]) is float and thresholds[key] == value  # pylint: disable=unidiomatic-typecheck  # Exact floats reject bool aliases.
            for key, value in expected_thresholds.items()
        )
    if not thresholds_match:
        defects.append(_defect("threshold", "both routing thresholds must be exactly 1.0"))
    if not isinstance(cases, list):
        return defects + [_defect("cases", "cases must be a list")]

    identifiers: set[str] = set()
    prompts: set[str] = set()
    expected_counts = {skill: 0 for skill in skills}
    covered_confusions: set[tuple[str, str]] = set()
    for index, case in enumerate(cases):
        if not isinstance(case, dict):
            defects.append(_defect("case-shape", f"case {index} must be an object"))
            continue
        identifier = case.get("id")
        prompt = case.get("prompt")
        expected = case.get("expected_skill")
        rejected = case.get("rejected_skills")
        if not isinstance(identifier, str) or not identifier:
            defects.append(_defect("case-id", f"case {index} needs a nonempty string id"))
        elif identifier in identifiers:
            defects.append(_defect("case-id", f"duplicate case id {identifier}"))
        else:
            identifiers.add(identifier)
        if not isinstance(prompt, str) or not prompt.strip():
            defects.append(_defect("case-prompt", f"case {identifier or index} needs a prompt"))
        else:
            normalized_prompt = " ".join(prompt.split()).casefold()
            if normalized_prompt in prompts:
                defects.append(_defect("case-prompt", f"duplicate prompt in {identifier}"))
            prompts.add(normalized_prompt)
            named_skills = [skill for skill in skills if skill.casefold() in normalized_prompt]
            if named_skills:
                defects.append(
                    _defect(
                        "vacuous-prompt",
                        f"{identifier} supplies canonical skill name(s): {', '.join(sorted(named_skills))}",
                    )
                )
        if expected not in skills:
            defects.append(_defect("expected-skill", f"{identifier} has unknown expected skill {expected}"))
        else:
            expected_counts[expected] += 1
        if not isinstance(rejected, list) or any(item not in skills for item in rejected):
            defects.append(_defect("rejected-skill", f"{identifier} has invalid rejected_skills"))
            continue
        if len(rejected) != len(set(rejected)) or expected in rejected:
            defects.append(_defect("rejected-skill", f"{identifier} has duplicate/self rejection"))
        if expected in skills:
            covered_confusions.update((expected, sibling) for sibling in rejected)

    missing_positive = sorted(skill for skill, count in expected_counts.items() if count == 0)
    duplicate_positive = sorted(skill for skill, count in expected_counts.items() if count > 1)
    if missing_positive or duplicate_positive:
        defects.append(
            _defect(
                "positive-coverage",
                f"one case per skill required; missing={missing_positive}, duplicates={duplicate_positive}",
            )
        )
    required_confusions = {
        (skill, sibling)
        for skill, row in review.get("skills", {}).items()
        for sibling in row.get("confusion_with", [])
    }
    missing_confusions = sorted(required_confusions - covered_confusions)
    if missing_confusions:
        defects.append(
            _defect("confusion-coverage", f"missing directed confusion cases: {missing_confusions}")
        )
    return defects


def compile_claude_evals(corpus: dict) -> dict:
    evals = []
    for index, case in enumerate(corpus["cases"], start=1):
        rejected = case["rejected_skills"]
        expectations = [
            f"The structured chosen_skill is exactly {case['expected_skill']}.",
        ]
        if rejected:
            expectations.append(
                "The structured chosen_skill is none of: " + ", ".join(rejected) + "."
            )
        evals.append(
            {
                "id": index,
                "prompt": case["prompt"],
                "expected_output": (
                    "One structured chosen_skill value naming the specialist for the immediate deliverable."
                ),
                "files": [],
                "expectations": expectations,
            }
        )
    return {"skill_name": "shaft-developer", "evals": evals}


def compile_codex_cases(corpus: dict) -> tuple[str, dict]:
    schema = output_schema()
    lines = []
    for case in corpus["cases"]:
        lines.append(
            json.dumps(
                {
                    "case_id": case["id"],
                    "prompt": case["prompt"],
                    "expected": {"chosen_skill": case["expected_skill"]},
                    "rejected_skills": case["rejected_skills"],
                    "output_schema": "output-schema.json",
                },
                separators=(",", ":"),
                ensure_ascii=False,
            )
        )
    return "\n".join(lines) + "\n", schema


def evaluate_results(
    corpus: dict,
    client: str,
    client_version: str,
    records: list[dict],
    *,
    external_blocker: str | None = None,
    client_failure: str | None = None,
) -> dict:
    observed = {
        record.get("case_id"): record.get("chosen_skill")
        for record in records
        if isinstance(record, dict)
    }
    results = []
    for case in corpus["cases"]:
        chosen = observed.get(case["id"])
        if case["id"] in observed:
            verdict = "pass" if chosen == case["expected_skill"] else "fail"
        elif external_blocker:
            verdict = "external_blocker"
        elif client_failure:
            verdict = "client_failure"
        else:
            verdict = "fail"
        results.append(
            {
                "case_id": case["id"],
                "expected_skill": case["expected_skill"],
                "rejected_skills": case["rejected_skills"],
                "observed_skill": chosen,
                "verdict": verdict,
                "detail": (
                    external_blocker
                    if verdict == "external_blocker"
                    else (
                        client_failure
                        if verdict == "client_failure"
                        else "selection assertion evaluated"
                    )
                ),
                "context_budget_warnings": [],
            }
        )
    passed = sum(row["verdict"] == "pass" for row in results)
    failed = sum(row["verdict"] == "fail" for row in results)
    evaluated = passed + failed
    passed_skills = {row["expected_skill"] for row in results if row["verdict"] == "pass"}
    expected_skills = {case["expected_skill"] for case in corpus["cases"]}
    return {
        "schema_version": 1,
        "package": corpus["package"],
        "client": client,
        "client_version": client_version,
        "thresholds": corpus["thresholds"],
        "results": results,
        "summary": {
            "passes": passed,
            "failures": failed,
            "external_blockers": sum(
                row["verdict"] == "external_blocker" for row in results
            ),
            "client_failures": sum(
                row["verdict"] == "client_failure" for row in results
            ),
            "case_pass_rate": passed / evaluated if evaluated else None,
            "positive_skill_coverage": (
                len(passed_skills) / len(expected_skills) if expected_skills else None
            ),
        },
    }


def package_decision(reports: list[dict]) -> dict:
    failures = sum(report["summary"]["failures"] for report in reports)
    warnings = sorted(
        {
            warning
            for report in reports
            for warning in (
                list(report.get("context_budget_warnings", []))
                + [
                    item
                    for result in report["results"]
                    for item in result.get("context_budget_warnings", [])
                ]
            )
        }
    )
    if failures or warnings:
        return {
            "decision": "investigate-split-or-profile",
            "reason": "measured post-compression routing failures or context-budget warnings remain",
            "failures": failures,
            "context_budget_warnings": warnings,
        }
    reports_by_client = {report.get("client"): report for report in reports}
    incomplete_clients = []
    for client in sorted(SUPPORTED_CLIENTS):
        report = reports_by_client.get(client)
        if not report:
            incomplete_clients.append(client)
            continue
        summary = report["summary"]
        thresholds = report.get("thresholds", {})
        complete = (
            summary.get("external_blockers") == 0
            and summary.get("client_failures", 0) == 0
            and summary.get("case_pass_rate") is not None
            and summary.get("positive_skill_coverage") is not None
            and summary["case_pass_rate"] >= thresholds.get("case_pass_rate", 1.0)
            and summary["positive_skill_coverage"]
            >= thresholds.get("positive_skill_coverage", 1.0)
        )
        if not complete:
            incomplete_clients.append(client)
    if incomplete_clients:
        return {
            "decision": "insufficient-evidence",
            "reason": (
                "complete threshold-satisfying authenticated evidence is unavailable for: "
                + ", ".join(incomplete_clients)
            ),
            "failures": 0,
            "context_budget_warnings": [],
            "incomplete_clients": incomplete_clients,
        }
    return {
        "decision": "retain-single-package",
        "reason": "no measured post-compression routing failure or context-budget warning justifies a split",
        "failures": 0,
        "context_budget_warnings": [],
    }


def registry_hosts(root: Path = ROOT) -> list[dict]:
    """Return host records from the canonical registry, or an empty list."""
    path = root / HOSTS_RELATIVE
    if not path.is_file():
        return []
    try:
        payload = json.loads(path.read_text(encoding="utf-8"))
    except (OSError, UnicodeDecodeError, json.JSONDecodeError):
        return []
    hosts = payload.get("hosts") if isinstance(payload, dict) else None
    if not isinstance(hosts, list):
        return []
    return [host for host in hosts if isinstance(host, dict)]


def registry_display_names(root: Path = ROOT) -> list[str]:
    """Display names in registry order."""
    return [host["displayName"] for host in registry_hosts(root) if isinstance(host.get("displayName"), str)]


def native_evidence_clients(root: Path = ROOT) -> set[str]:
    """Native routing clients. Labels stay stable when the registry display name starts with them."""
    by_id = {host.get("id"): host.get("displayName", "") for host in registry_hosts(root)}
    if not by_id:
        return set(NATIVE_EVIDENCE_LABELS.values())
    labels = set()
    for host_id, label in NATIVE_EVIDENCE_LABELS.items():
        display = by_id.get(host_id, "")
        if isinstance(display, str) and (display == label or display.startswith(label)):
            labels.add(label)
        elif isinstance(display, str) and display:
            labels.add(display)
    return labels


SUPPORTED_CLIENTS = native_evidence_clients()


def _coverage_review(review: dict) -> dict:
    """Score the entrypoint and every routed specialist as one routing universe."""
    skills = dict(review.get("skills") or {})
    extra = review.get("routing_reviews")
    if isinstance(extra, dict):
        for name, row in extra.items():
            if isinstance(name, str) and isinstance(row, dict):
                skills.setdefault(name, row)
    return {**review, "skills": skills}


def _product_skill_paths(root: Path) -> list[str]:
    skills = root / "shaft-skills"
    if not skills.is_dir():
        return []
    found = []
    for path in sorted(skills.rglob("SKILL.md")):
        relative = path.relative_to(skills)
        if relative.parts and relative.parts[0] in {"ce-pack", "ce-addons"}:
            continue
        found.append(path.relative_to(root).as_posix())
    return found


def _specialist_domains(root: Path) -> set[str]:
    specialists = root / "shaft-skills/shaft-developer/references/specialists"
    if not specialists.is_dir():
        return set()
    return {path.name for path in specialists.glob("*.md") if path.name != "journey-coverage-map.md"}


def _entrypoint_defects(root: Path) -> list[dict]:
    if not (root / "shaft-skills").is_dir():
        return []
    found = _product_skill_paths(root)
    if found == ["shaft-skills/shaft-developer/SKILL.md"]:
        return []
    return [_defect("entrypoint-count", f"expected shaft-skills/shaft-developer/SKILL.md only, found {found or ['none']}")]


def _specialist_coverage_defects(root: Path, review: dict) -> list[dict]:
    domains = _specialist_domains(root)
    if not domains:
        return []
    routing_reviews = review.get("routing_reviews")
    if not isinstance(routing_reviews, dict):
        return [_defect("routing-reviews", "routing_reviews must map each logical specialist to one reference file")]
    referenced = {
        row.get("reference")
        for row in routing_reviews.values()
        if isinstance(row, dict) and isinstance(row.get("reference"), str)
    }
    defects = []
    missing = sorted(domains - referenced)
    unknown = sorted(item for item in referenced if isinstance(item, str) and item not in domains)
    if missing or unknown:
        defects.append(_defect("specialist-coverage", f"routing review references drifted; missing={missing}, extra={unknown}"))
    routing = root / "shaft-skills/shaft-developer/references/routing.md"
    if not routing.is_file():
        defects.append(_defect("specialist-routing", "routing.md is missing"))
        return defects
    linked = set(re.findall(r"specialists/([a-z0-9-]+\.md)", routing.read_text(encoding="utf-8")))
    if linked != domains:
        defects.append(
            _defect(
                "specialist-routing",
                f"routing.md specialist files drifted; linked={sorted(linked)}, domains={sorted(domains)}",
            )
        )
    return defects


def _client_defects(root: Path, review: dict) -> list[dict]:
    path = root / HOSTS_RELATIVE
    if not path.is_file():
        return []
    expected = registry_display_names(root)
    if not expected or review.get("supported_clients") != expected:
        return [_defect("supported-clients", "supported_clients drifted from scripts/mcp/agent_hosts.json display names")]
    missing_ids = sorted(set(NATIVE_EVIDENCE_LABELS) - {host.get("id") for host in registry_hosts(root)})
    if missing_ids:
        return [_defect("supported-clients", f"agent_hosts.json is missing native evidence hosts: {missing_ids}")]
    return []


def _generated_defects(root: Path, corpus: dict) -> list[dict]:
    defects = []
    for relative, expected in _generated(corpus).items():
        path = root / relative
        try:
            actual = path.read_text(encoding="utf-8")
        except OSError:
            actual = ""
        if actual != expected:
            defects.append(_defect("generated-drift", f"regenerate {relative.as_posix()}"))
    return defects


def _generated(corpus: dict) -> dict[Path, str]:
    codex_cases, schema = compile_codex_cases(corpus)
    return {
        CLAUDE_RELATIVE: _json_text(compile_claude_evals(corpus)),
        CODEX_CASES_RELATIVE: codex_cases,
        CODEX_SCHEMA_RELATIVE: _json_text(schema),
    }


def validate_repository(root: Path = ROOT) -> list[dict]:
    try:
        corpus = json.loads((root / CORPUS_RELATIVE).read_text(encoding="utf-8"))
        review = json.loads((root / REVIEW_RELATIVE).read_text(encoding="utf-8"))
    except (OSError, UnicodeDecodeError, json.JSONDecodeError) as error:
        return [_defect("source", str(error))]
    defects = validate_corpus(corpus, _coverage_review(review))
    defects.extend(_entrypoint_defects(root))
    defects.extend(_specialist_coverage_defects(root, review))
    defects.extend(_client_defects(root, review))
    defects.extend(_generated_defects(root, corpus))
    return defects


def write_generated(root: Path = ROOT) -> None:
    corpus = json.loads((root / CORPUS_RELATIVE).read_text(encoding="utf-8"))
    for relative, content in _generated(corpus).items():
        path = root / relative
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(content, encoding="utf-8", newline="\n")


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--write", action="store_true", help="refresh client-native generated files")
    parser.add_argument("--check", action="store_true", help="validate the single-entrypoint routing corpus")
    arguments = parser.parse_args()
    if arguments.write:
        write_generated()
    defects = validate_repository()
    if defects:
        for defect in defects:
            print(f"{defect['code']}: {defect['message']}")
        return 1
    corpus = json.loads((ROOT / CORPUS_RELATIVE).read_text(encoding="utf-8"))
    print(f"SHAFT routing corpus is valid: {len(corpus['cases'])} cases")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
