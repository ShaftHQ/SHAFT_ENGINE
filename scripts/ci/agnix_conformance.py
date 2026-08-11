#!/usr/bin/env python3
"""Run pinned agnix as supplemental, container-only harness conformance evidence."""

from __future__ import annotations

import argparse
import json
import os
import re
import shutil
import stat
import subprocess  # nosec B404 - fixed Docker argv from a validated tracked contract.
import sys
from pathlib import Path, PurePosixPath
from typing import Callable


ROOT = Path(__file__).resolve().parents[2]
if str(ROOT) not in sys.path:
    sys.path.insert(0, str(ROOT))

from scripts.ci.shaft_skill_candidate_intake import quarantine_command  # noqa: E402


CONTRACT_PATH = Path("scripts/ci/agnix_conformance.json")
PLATFORMS = {"linux-x86_64", "macos-aarch64", "windows-x86_64"}
ARTIFACT_FILENAMES = {
    "linux-x86_64": "agnix-x86_64-unknown-linux-gnu.tar.gz",
    "macos-aarch64": "agnix-aarch64-apple-darwin.tar.gz",
    "windows-x86_64": "agnix-x86_64-pc-windows-msvc.zip",
}
HEX_40 = re.compile(r"[0-9a-f]{40}")
HEX_64 = re.compile(r"[0-9a-f]{64}")


def load_contract(root: Path = ROOT) -> dict:
    """Load the tracked immutable agnix promotion contract."""
    return json.loads((Path(root) / CONTRACT_PATH).read_text(encoding="utf-8"))


def validate_contract(contract: object) -> list[str]:  # noqa: MC0001 - fail-closed schema validation stays linear.
    """Return deterministic defects for one agnix contract."""
    defects: list[str] = []
    if not isinstance(contract, dict):
        return ["contract must be an object"]
    expected_keys = {
        "schema_version",
        "source",
        "image",
        "expected_files_checked",
        "artifacts",
        "staging_paths",
        "allowlisted_findings",
        "evaluation",
    }
    if set(contract) != expected_keys:
        defects.append("contract fields drifted")
    if type(contract.get("schema_version")) is not int or contract.get("schema_version") != 1:
        defects.append("schema_version must be integer 1")

    source = contract.get("source")
    if not isinstance(source, dict) or set(source) != {
        "url",
        "revision",
        "release_tag",
        "release_commit",
        "license",
    }:
        defects.append("source must contain exact provenance fields")
        source = {}
    if not isinstance(source.get("url"), str) or not source.get("url", "").startswith("https://"):
        defects.append("source URL must use HTTPS")
    elif source["url"] != "https://github.com/agent-sh/agnix":
        defects.append("source URL must be the reviewed agnix repository")
    for field in ("revision", "release_commit"):
        if not isinstance(source.get(field), str) or not HEX_40.fullmatch(source.get(field, "")):
            defects.append(f"source {field} must be a full commit SHA")
    if not isinstance(source.get("release_tag"), str) or not re.fullmatch(
        r"v\d+\.\d+\.\d+", source.get("release_tag", "")
    ):
        defects.append("source release_tag must be a semantic release tag")
    if not isinstance(source.get("license"), str) or not source.get("license", "").strip():
        defects.append("source license is required")

    image = contract.get("image")
    if not isinstance(image, dict) or set(image) != {"reference", "id"}:
        defects.append("image must contain an immutable reference and image ID")
        image = {}
    if not isinstance(image.get("id"), str) or not re.fullmatch(
        r"sha256:[0-9a-f]{64}", image.get("id", "")
    ):
        defects.append("image ID must be an immutable SHA-256 digest")
    if image.get("reference") != f'docker.io/library/ubuntu@{image.get("id")}':
        defects.append("image reference must pin the reviewed Ubuntu image by digest")
    if type(contract.get("expected_files_checked")) is not int or contract.get("expected_files_checked", 0) <= 0:
        defects.append("expected_files_checked must be a positive integer")

    artifacts = contract.get("artifacts")
    platforms: list[str] = []
    if not isinstance(artifacts, list) or not artifacts:
        defects.append("artifacts must be a non-empty list")
        artifacts = []
    for index, artifact in enumerate(artifacts):
        if not isinstance(artifact, dict) or set(artifact) != {"platform", "url", "sha256"}:
            defects.append(f"artifact {index} must contain platform, url, and sha256")
            continue
        platforms.append(artifact.get("platform"))
        if not isinstance(artifact.get("url"), str) or not artifact["url"].startswith("https://"):
            defects.append(f"artifact {index} URL must use HTTPS")
        expected_artifact_url = (
            f'{source.get("url")}/releases/download/{source.get("release_tag")}/'
            f'{ARTIFACT_FILENAMES.get(artifact.get("platform"), "")}'
        )
        if isinstance(artifact.get("url"), str) and artifact["url"] != expected_artifact_url:
            defects.append(f"artifact {index} URL must belong to the pinned agnix release")
        if not isinstance(artifact.get("sha256"), str) or not HEX_64.fullmatch(artifact["sha256"]):
            defects.append(f"artifact {index} SHA-256 is invalid")
    if set(platforms) != PLATFORMS or len(platforms) != len(PLATFORMS):
        defects.append("artifacts must cover exactly the three required platforms")

    staging = contract.get("staging_paths")
    if (
        not isinstance(staging, list)
        or not staging
        or not all(
            isinstance(path, str)
            and path
            and "\\" not in path
            and not PurePosixPath(path).is_absolute()
            and ".." not in PurePosixPath(path).parts
            for path in staging
        )
        or len(staging) != len(set(staging))
    ):
        defects.append("staging_paths must be unique non-empty relative paths")

    allowlist = contract.get("allowlisted_findings")
    if not isinstance(allowlist, list) or not allowlist:
        defects.append("allowlisted_findings must be a non-empty exact ledger")
        allowlist = []
    for index, finding in enumerate(allowlist):
        if not isinstance(finding, dict) or set(finding) != {
            "rule",
            "level",
            "path",
            "message",
            "expected_count",
            "reason",
        }:
            defects.append(f"allowlisted finding {index} has invalid fields")
            continue
        if not all(
            isinstance(finding.get(field), str) and finding[field].strip()
            for field in ("rule", "path", "message", "reason")
        ):
            defects.append(f"allowlisted finding {index} must use exact non-empty strings")
        if finding.get("level") != "error":
            defects.append(f"allowlisted finding {index} must be an error")
        if type(finding.get("expected_count")) is not int or finding.get("expected_count", 0) <= 0:
            defects.append(f"allowlisted finding {index} needs a positive exact count")

    evaluation = contract.get("evaluation")
    if not isinstance(evaluation, dict) or set(evaluation) != {
        "corpus_url",
        "corpus_sha256",
        "minimum_precision",
        "minimum_recall",
        "required_rule_prefixes",
    }:
        defects.append("evaluation must contain exact floor fields")
        evaluation = {}
    expected_corpus_url = (
        f'{source.get("url")}/archive/{source.get("revision")}.tar.gz' if source else ""
    )
    if evaluation.get("corpus_url") != expected_corpus_url:
        defects.append("evaluation corpus URL must derive from the pinned agnix revision")
    if not isinstance(evaluation.get("corpus_sha256"), str) or not HEX_64.fullmatch(
        evaluation.get("corpus_sha256", "")
    ):
        defects.append("evaluation corpus SHA-256 is invalid")
    for field in ("minimum_precision", "minimum_recall"):
        value = evaluation.get(field)
        if isinstance(value, bool) or not isinstance(value, (int, float)) or not 0 <= value <= 1:
            defects.append(f"evaluation {field} must be a number from zero through one")
    prefixes = evaluation.get("required_rule_prefixes")
    if not isinstance(prefixes, list) or not prefixes or not all(
        isinstance(prefix, str) and re.fullmatch(r"[A-Z]+-", prefix) for prefix in prefixes
    ) or len(prefixes) != len(set(prefixes)):
        defects.append("evaluation required_rule_prefixes are invalid")
    return defects


def _inside(path: Path, root: Path) -> bool:
    try:
        path.resolve(strict=False).relative_to(root.resolve(strict=False))
        return True
    except ValueError:
        return False


def _is_reparse_point(path: Path) -> bool:
    attributes = getattr(os.lstat(path), "st_file_attributes", 0)
    return bool(attributes & getattr(stat, "FILE_ATTRIBUTE_REPARSE_POINT", 0x0400))


def stage_harness(source_root: Path, destination: Path, contract: dict) -> list[str]:
    """Copy only declared harness inputs into a disposable trial fixture."""
    defects = validate_contract(contract)
    if defects:
        raise ValueError("invalid agnix contract: " + "; ".join(defects))
    source = Path(source_root).resolve(strict=True)
    target = Path(destination).resolve(strict=False)
    if target.exists():
        raise ValueError("fixture destination must not already exist")
    protected = tuple(
        (source / relative).resolve(strict=False)
        for relative in (
            ".agents/skills",
            ".claude/skills",
            ".codex",
            "agent-plugins/shaft-skills",
            "shaft-skills",
        )
    )
    if _inside(target, source) or any(_inside(target, root) or _inside(root, target) for root in protected):
        raise ValueError("fixture destination must be disposable and outside the repository")
    for relative in contract["staging_paths"]:
        source_path = source / relative
        if not source_path.exists():
            raise ValueError(f"declared staging input is missing: {relative}")
        for candidate in (source_path, *source_path.rglob("*")):
            if candidate.is_symlink():
                raise ValueError(f"declared staging input contains a symlink: {candidate}")
            try:
                candidate.resolve(strict=True).relative_to(source)
            except ValueError as error:
                raise ValueError(
                    f"declared staging input resolves outside the source root: {candidate}"
                ) from error
            if _is_reparse_point(candidate):
                raise ValueError(f"declared staging input contains a reparse point: {candidate}")
    target.mkdir(parents=True)
    copied: list[str] = []
    for relative in contract["staging_paths"]:
        source_path = source / relative
        target_path = target / relative
        target_path.parent.mkdir(parents=True, exist_ok=True)
        if source_path.is_dir():
            shutil.copytree(source_path, target_path)
        else:
            shutil.copy2(source_path, target_path)
        copied.append(relative)
    return copied


def _trial_command(candidate_root: Path, fixtures_root: Path, output_root: Path, image: str, argv: list[str]) -> list[str]:
    return quarantine_command(candidate_root, fixtures_root, output_root, image, argv)


def build_trial_command(candidate_root: Path, fixtures_root: Path, output_root: Path, contract: dict) -> list[str]:
    """Build the fixed read-only agnix validation command."""
    defects = validate_contract(contract)
    if defects:
        raise ValueError("invalid agnix contract: " + "; ".join(defects))
    return _trial_command(
        candidate_root,
        fixtures_root,
        output_root,
        contract["image"]["id"],
        ["/usr/bin/env", "DO_NOT_TRACK=1", "/candidate/agnix", "--format", "json", "/fixtures"],
    )


def assess_diagnostics(payload: object, contract: dict) -> dict:
    """Fail on any new error or drift in an exact false-positive fingerprint."""
    if not isinstance(payload, dict) or not isinstance(payload.get("diagnostics"), list):
        return {
            "accepted": False,
            "files_checked": 0,
            "files_checked_mismatch": {
                "expected": contract.get("expected_files_checked"),
                "actual": None,
            },
            "diagnostics": 0,
            "unexpected_errors": [{"reason": "invalid agnix JSON payload"}],
            "allowlist_count_mismatches": [],
            "warnings": [],
        }
    counts = [0] * len(contract["allowlisted_findings"])
    unexpected: list[dict] = []
    warnings: list[dict] = []
    for diagnostic in payload["diagnostics"]:
        if not isinstance(diagnostic, dict):
            unexpected.append({"reason": "non-object diagnostic"})
            continue
        if diagnostic.get("level") not in {"info", "warning", "error"}:
            unexpected.append({"reason": "unknown diagnostic level", "diagnostic": diagnostic})
            continue
        if diagnostic.get("level") == "warning":
            warnings.append(diagnostic)
        if diagnostic.get("level") != "error":
            continue
        normalized_path = str(diagnostic.get("file", "")).replace("\\", "/")
        matched = False
        for index, allowlist in enumerate(contract["allowlisted_findings"]):
            if (
                diagnostic.get("rule") == allowlist["rule"]
                and diagnostic.get("level") == allowlist["level"]
                and normalized_path == allowlist["path"]
                and diagnostic.get("message") == allowlist["message"]
            ):
                counts[index] += 1
                matched = True
                break
        if not matched:
            unexpected.append(diagnostic)
    mismatches = [
        {
            "rule": finding["rule"],
            "path": finding["path"],
            "expected": finding["expected_count"],
            "actual": counts[index],
        }
        for index, finding in enumerate(contract["allowlisted_findings"])
        if counts[index] != finding["expected_count"]
    ]
    files_checked = payload.get("files_checked")
    files_checked_mismatch = (
        None
        if files_checked == contract["expected_files_checked"]
        else {"expected": contract["expected_files_checked"], "actual": files_checked}
    )
    return {
        "accepted": not unexpected and not mismatches and files_checked_mismatch is None,
        "files_checked": payload.get("files_checked", 0),
        "files_checked_mismatch": files_checked_mismatch,
        "diagnostics": len(payload["diagnostics"]),
        "unexpected_errors": unexpected,
        "allowlist_count_mismatches": mismatches,
        "warnings": warnings,
    }


def score_evaluation(payload: object, contract: dict) -> dict:
    """Apply efficacy floors and require Claude, Codex, and shared rule coverage."""
    if not isinstance(payload, dict) or not isinstance(payload.get("rules"), dict):
        return {"accepted": False, "reason": "invalid evaluation payload"}
    precision = payload.get("overall_precision")
    recall = payload.get("overall_recall")
    rule_ids = {
        value.get("rule_id")
        for value in payload["rules"].values()
        if isinstance(value, dict) and isinstance(value.get("rule_id"), str)
    }
    missing = [
        prefix
        for prefix in contract["evaluation"]["required_rule_prefixes"]
        if not any(rule.startswith(prefix) for rule in rule_ids)
    ]
    numeric = lambda value: isinstance(value, (int, float)) and not isinstance(value, bool)
    accepted = (
        type(payload.get("cases_run")) is int
        and payload["cases_run"] > 0
        and payload.get("cases_failed") == 0
        and numeric(precision)
        and numeric(recall)
        and precision >= contract["evaluation"]["minimum_precision"]
        and recall >= contract["evaluation"]["minimum_recall"]
        and not missing
    )
    return {
        "accepted": accepted,
        "cases_run": payload.get("cases_run"),
        "cases_failed": payload.get("cases_failed"),
        "precision": precision,
        "recall": recall,
        "missing_rule_prefixes": missing,
    }


def _json_object_from_output(output: str) -> object:
    """Decode the first JSON object from agnix output that may include status prose."""
    start = output.find("{")
    if start < 0:
        return None
    try:
        payload, _ = json.JSONDecoder().raw_decode(output[start:])
    except json.JSONDecodeError:
        return None
    return payload


def run_conformance(
    candidate_root: Path,
    fixtures_root: Path,
    evaluation_root: Path,
    output_root: Path,
    contract: dict,
    *,
    runner: Callable[..., subprocess.CompletedProcess] = subprocess.run,
) -> dict:
    """Run telemetry proof and lint under the same fixed quarantine boundary."""
    defects = validate_contract(contract)
    if defects:
        raise ValueError("invalid agnix contract: " + "; ".join(defects))
    telemetry_command = _trial_command(
        candidate_root,
        fixtures_root,
        output_root,
        contract["image"]["id"],
        ["/usr/bin/env", "DO_NOT_TRACK=1", "/candidate/agnix", "telemetry", "status"],
    )
    telemetry = runner(  # nosec B603 - argv is fixed after contract validation.
        telemetry_command, capture_output=True, text=True, check=False
    )
    telemetry_disabled = (
        telemetry.returncode == 0
        and "Configured: disabled" in telemetry.stdout
        and "Effective: disabled" in telemetry.stdout
    )
    lint = runner(  # nosec B603 - argv is fixed after contract validation.
        build_trial_command(candidate_root, fixtures_root, output_root, contract),
        capture_output=True,
        text=True,
        check=False,
    )
    payload = _json_object_from_output(lint.stdout)
    assessment = assess_diagnostics(payload, contract)
    evaluation_process = runner(  # nosec B603 - argv is fixed after contract validation.
        _trial_command(
            candidate_root,
            evaluation_root,
            output_root,
            contract["image"]["id"],
            [
                "/usr/bin/env",
                "DO_NOT_TRACK=1",
                "/candidate/agnix",
                "eval",
                "--format",
                "json",
                "/fixtures/eval.yaml",
            ],
        ),
        capture_output=True,
        text=True,
        check=False,
    )
    evaluation_payload = _json_object_from_output(evaluation_process.stdout)
    evaluation = score_evaluation(evaluation_payload, contract)
    evaluation["agnix_exit_code"] = evaluation_process.returncode
    evaluation["accepted"] = bool(evaluation["accepted"] and evaluation_process.returncode == 0)
    assessment["evaluation"] = evaluation
    assessment["telemetry_disabled"] = telemetry_disabled
    assessment["agnix_exit_code"] = lint.returncode
    assessment["accepted"] = bool(
        assessment["accepted"]
        and evaluation["accepted"]
        and telemetry_disabled
        and lint.returncode in {0, 1}
    )
    return assessment


def workflow_environment(contract: dict) -> dict[str, str]:
    """Derive Linux CI acquisition values from the validated tracked contract."""
    defects = validate_contract(contract)
    if defects:
        raise ValueError("invalid agnix contract: " + "; ".join(defects))
    linux = next(row for row in contract["artifacts"] if row["platform"] == "linux-x86_64")
    return {
        "AGNIX_ARTIFACT_URL": linux["url"],
        "AGNIX_ARTIFACT_SHA256": linux["sha256"],
        "AGNIX_IMAGE_REFERENCE": contract["image"]["reference"],
        "AGNIX_IMAGE_ID": contract["image"]["id"],
        "AGNIX_EVALUATION_URL": contract["evaluation"]["corpus_url"],
        "AGNIX_EVALUATION_SHA256": contract["evaluation"]["corpus_sha256"],
    }


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--check-contract", action="store_true")
    parser.add_argument("--github-env", type=Path)
    parser.add_argument("--stage-source", type=Path)
    parser.add_argument("--candidate-root", type=Path)
    parser.add_argument("--fixtures-root", type=Path)
    parser.add_argument("--evaluation-root", type=Path)
    parser.add_argument("--output-root", type=Path)
    parser.add_argument("--output", type=Path)
    args = parser.parse_args(argv)
    contract = load_contract()
    defects = validate_contract(contract)
    if defects:
        for defect in defects:
            print(f"agnix-contract: {defect}", file=sys.stderr)
        return 1
    if args.check_contract:
        print("agnix conformance contract is valid")
        if not any(
            (
                args.github_env,
                args.stage_source,
                args.candidate_root,
                args.fixtures_root,
                args.evaluation_root,
                args.output_root,
                args.output,
            )
        ):
            return 0
    if args.github_env:
        values = workflow_environment(contract)
        with args.github_env.open("a", encoding="utf-8") as environment_file:
            environment_file.write("".join(f"{key}={value}\n" for key, value in values.items()))
    if args.stage_source:
        if args.fixtures_root is None:
            parser.error("--stage-source requires --fixtures-root")
        stage_harness(args.stage_source, args.fixtures_root, contract)
    if args.candidate_root:
        if None in (args.fixtures_root, args.evaluation_root, args.output_root, args.output):
            parser.error(
                "--candidate-root requires --fixtures-root, --evaluation-root, --output-root, and --output"
            )
        try:
            report = run_conformance(
                args.candidate_root,
                args.fixtures_root,
                args.evaluation_root,
                args.output_root,
                contract,
            )
        except (OSError, ValueError) as error:
            report = {"accepted": False, "external_blocker": str(error)}
        args.output.write_text(json.dumps(report, indent=2, sort_keys=True) + "\n", encoding="utf-8")
        print(json.dumps(report, sort_keys=True))
        return 0 if report.get("accepted") else 1
    if not args.check_contract and not args.github_env and not args.stage_source:
        parser.error("choose --check-contract, --github-env, --stage-source, or --candidate-root")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
