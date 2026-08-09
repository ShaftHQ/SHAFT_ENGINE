#!/usr/bin/env python3
"""Validate and support the quarantined online skill-candidate intake (#4643)."""

from __future__ import annotations

import json
import re
import stat
from datetime import date
from pathlib import Path


ROOT = Path(__file__).resolve().parents[2]
INTAKE_ROOT = Path("agent-plugins/shaft-skills/candidate-intake")
POLICY_PATH = INTAKE_ROOT / "policy.json"
REVIEW_PATH = INTAKE_ROOT / "candidates.json"
README_PATH = INTAKE_ROOT / "README.md"
DECISIONS = {"adopt-code", "adopt-pattern", "retain-test-target", "reject"}
STAGES = ("provenance_license", "static_review", "quarantine_trial", "local_evaluation")
STAGE_STATUSES = {"pass", "halt", "not_run", "not_applicable"}
REQUIRED_CATEGORIES = {"documents", "plugin-evaluation", "cross-client-packaging"}
REQUIRED_CANDIDATE_FIELDS = {
    "id",
    "category",
    "source_url",
    "source_paths",
    "revision",
    "version",
    "official_source",
    "license",
    "permissions",
    "network",
    "scripts",
    "overlap",
    "material_kind",
    "stage_results",
    "evaluation",
    "decision",
    "decision_reason",
    "vendor_policy_duplication",
    "promotion_pr",
    "adopted_files",
}
EXECUTABLE_SUFFIXES = {
    ".bat",
    ".cmd",
    ".com",
    ".dll",
    ".exe",
    ".jar",
    ".js",
    ".mjs",
    ".ps1",
    ".py",
    ".sh",
}
SECRET_PATTERNS = (
    ("private-key", re.compile(r"-----BEGIN [A-Z0-9 ]*PRIVATE KEY-----")),
    ("aws-access-key", re.compile(r"\bAKIA[0-9A-Z]{16}\b")),
    ("github-token", re.compile(r"\bgh[pousr]_[A-Za-z0-9]{20,}\b")),
    ("api-secret", re.compile(r"\bsk-[A-Za-z0-9_-]{20,}\b")),
)
PROTECTED_ROOTS = (
    ROOT / "shaft-skills",
    ROOT / "agent-plugins/shaft-skills",
    ROOT / ".agents/skills",
    ROOT / ".claude/skills",
    ROOT / ".codex/skills",
)


def _defect(code: str, message: str, path: str = "") -> dict:
    return {"code": code, "path": path, "message": message}


def _inside(path: Path, root: Path) -> bool:
    try:
        path.resolve(strict=False).relative_to(root.resolve(strict=False))
        return True
    except ValueError:
        return False


def _overlaps(left: Path, right: Path) -> bool:
    return _inside(left, right) or _inside(right, left)


def scan_candidate(candidate_root: Path) -> dict:
    """Statically inventory one already-quarantined candidate without executing it."""
    root = Path(candidate_root).resolve(strict=True)
    if not root.is_dir():
        raise ValueError("candidate_root must be a directory")
    files: list[str] = []
    symlinks: list[str] = []
    containment: list[str] = []
    executables: list[str] = []
    secrets: list[dict] = []
    install_hooks: list[str] = []
    opaque: list[dict] = []
    for path in sorted(root.rglob("*")):
        relative = path.relative_to(root).as_posix()
        if path.is_symlink():
            symlinks.append(relative)
        try:
            resolved = path.resolve(strict=True)
            resolved.relative_to(root)
        except (FileNotFoundError, ValueError):
            containment.append(relative)
            continue
        if not resolved.is_file():
            continue
        files.append(relative)
        mode = resolved.stat().st_mode
        if resolved.suffix.lower() in EXECUTABLE_SUFFIXES or mode & (
            stat.S_IXUSR | stat.S_IXGRP | stat.S_IXOTH
        ):
            executables.append(relative)
        if resolved.stat().st_size > 1_000_000:
            opaque.append({"path": relative, "reason": "over-1mb"})
            continue
        try:
            content = resolved.read_text(encoding="utf-8")
        except UnicodeDecodeError:
            opaque.append({"path": relative, "reason": "not-utf8"})
            continue
        except OSError:
            opaque.append({"path": relative, "reason": "unreadable"})
            continue
        if resolved.name == "package.json":
            try:
                package = json.loads(content)
            except json.JSONDecodeError:
                package = {}
            scripts = package.get("scripts", {}) if isinstance(package, dict) else {}
            if isinstance(scripts, dict):
                for hook in ("preinstall", "install", "postinstall", "prepare"):
                    if isinstance(scripts.get(hook), str) and scripts[hook].strip():
                        install_hooks.append(f"{relative}:{hook}")
        for name, pattern in SECRET_PATTERNS:
            for match in pattern.finditer(content):
                secrets.append(
                    {
                        "path": relative,
                        "kind": name,
                        "line": content.count("\n", 0, match.start()) + 1,
                    }
                )
    return {
        "file_count": len(files),
        "files": files,
        "symlink_files": symlinks,
        "containment_violations": containment,
        "executable_files": executables,
        "install_hooks": install_hooks,
        "secret_findings": secrets,
        "opaque_files": opaque,
    }


def quarantine_command(
    candidate_root: Path,
    fixtures_root: Path,
    output_root: Path,
    image: str,
    argv: list[str],
) -> list[str]:
    """Build the only permitted trial shape; callers execute it explicitly."""
    try:
        candidate = Path(candidate_root).resolve(strict=True)
        fixtures = Path(fixtures_root).resolve(strict=True)
    except FileNotFoundError as error:
        raise ValueError("candidate and fixtures must be existing directories") from error
    output = Path(output_root).resolve(strict=False)
    roots = [root.resolve(strict=False) for root in PROTECTED_ROOTS]
    if not all(path.is_dir() for path in (candidate, fixtures, output)):
        raise ValueError("candidate, fixtures, and output must be existing directories")
    if not re.fullmatch(r"sha256:[0-9a-f]{64}", image):
        raise ValueError("trial image must use an immutable sha256 digest")
    if not argv or not all(isinstance(part, str) and part for part in argv):
        raise ValueError("trial argv must contain non-empty strings")
    for path, label in ((candidate, "candidate"), (fixtures, "fixtures"), (output, "output")):
        if any(_overlaps(path, root) for root in roots):
            raise ValueError(f"{label} must not overlap a canonical skill root")
    if _overlaps(output, candidate) or _overlaps(output, fixtures):
        raise ValueError("writable output must not overlap read-only inputs")
    return [
        "docker",
        "run",
        "--rm",
        "--pull",
        "never",
        "--network",
        "none",
        "--read-only",
        "--cap-drop",
        "ALL",
        "--security-opt",
        "no-new-privileges",
        "--pids-limit",
        "64",
        "--memory",
        "512m",
        "--cpus",
        "1",
        "--user",
        "65532:65532",
        "--tmpfs",
        "/tmp:rw,noexec,nosuid,nodev,size=64m",
        "--mount",
        f"type=bind,src={candidate},dst=/candidate,readonly",
        "--mount",
        f"type=bind,src={fixtures},dst=/fixtures,readonly",
        "--mount",
        f"type=bind,src={output},dst=/output",
        image,
        *argv,
    ]


def validate_policy(policy: dict) -> list[dict]:
    defects: list[dict] = []
    if policy.get("schema_version") != 1:
        defects.append(_defect("policy-schema", "policy schema_version must be 1"))
    if set(policy.get("decision_kinds", [])) != DECISIONS:
        defects.append(_defect("policy-decisions", "policy must declare all four decision kinds"))
    if tuple(policy.get("required_stages", [])) != STAGES:
        defects.append(_defect("policy-stages", "policy stages or order drifted"))
    trial = policy.get("trial_contract", {})
    required_trial = {
        "container_only": True,
        "network": "none",
        "read_only_root": True,
        "credentials": "none",
        "non_root_user": "65532:65532",
        "canonical_roots_mounted": False,
    }
    if any(trial.get(key) != value for key, value in required_trial.items()):
        defects.append(_defect("trial-contract", "trial contract must stay no-network, read-only, nonroot, and credential-free"))
    roots = set(policy.get("canonical_roots", []))
    if not {
        "shaft-skills",
        "agent-plugins/shaft-skills",
        ".agents/skills",
        ".claude/skills",
        ".codex/skills",
    }.issubset(roots):
        defects.append(_defect("canonical-roots", "all canonical skill roots must be protected"))
    return defects


def validate_review(review: dict, policy: dict) -> list[dict]:
    defects: list[dict] = []
    if type(review.get("schema_version")) is not int or review["schema_version"] != 1:
        defects.append(_defect("review-schema", "review schema_version must be integer 1"))
    reviewed_at = review.get("reviewed_at")
    try:
        if not isinstance(reviewed_at, str) or date.fromisoformat(reviewed_at).isoformat() != reviewed_at:
            raise ValueError
    except ValueError:
        defects.append(_defect("review-date", "reviewed_at must be an ISO calendar date"))
    candidates = review.get("candidates")
    if not isinstance(candidates, list):
        return [_defect("review-shape", "candidates must be a list")]
    ids: list[str] = []
    categories: set[str] = set()
    for index, candidate in enumerate(candidates):
        path = f"candidates[{index}]"
        if not isinstance(candidate, dict):
            defects.append(_defect("candidate-shape", "candidate must be an object", path))
            continue
        missing = sorted(REQUIRED_CANDIDATE_FIELDS - set(candidate))
        for field in missing:
            defects.append(_defect("candidate-field", f"missing required field {field}", path))
        identifier = candidate.get("id")
        if not isinstance(identifier, str) or not re.fullmatch(r"[a-z0-9-]+", identifier):
            defects.append(_defect("candidate-id", "candidate id must use lowercase hyphen-case", path))
        else:
            ids.append(identifier)
        category = candidate.get("category")
        if isinstance(category, str):
            categories.add(category)
        revision = candidate.get("revision")
        if not isinstance(revision, str) or not re.fullmatch(r"[0-9a-f]{40}", revision):
            defects.append(_defect("immutable-revision", "candidate revision must be a full immutable commit SHA", path))
        if candidate.get("official_source") is not True:
            defects.append(_defect("official-source", "official_source must be boolean true", path))
        if not isinstance(candidate.get("license"), str) or not candidate.get("license", "").strip():
            defects.append(_defect("candidate-field", "license evidence is required", path))
        for field in (
            "source_url",
            "permissions",
            "network",
            "scripts",
            "overlap",
            "evaluation",
            "decision_reason",
        ):
            if not isinstance(candidate.get(field), str) or not candidate.get(field, "").strip():
                defects.append(_defect("candidate-evidence", f"{field} evidence is required", path))
        source_paths = candidate.get("source_paths")
        if not isinstance(source_paths, list) or not source_paths or not all(
            isinstance(item, str) and item.strip() for item in source_paths
        ):
            defects.append(_defect("candidate-evidence", "source_paths evidence is required", path))
        if candidate.get("material_kind") not in {"code", "pattern", "test-target"}:
            defects.append(_defect("candidate-evidence", "material_kind is invalid", path))
        decision = candidate.get("decision")
        if decision not in DECISIONS:
            defects.append(_defect("candidate-decision", "unknown candidate decision", path))
        stages = candidate.get("stage_results")
        if not isinstance(stages, dict) or set(stages) != set(STAGES):
            defects.append(_defect("stage-results", "every required stage needs one result", path))
            stages = {}
        terminated = False
        for stage in STAGES:
            result = stages.get(stage, {})
            status = result.get("status") if isinstance(result, dict) else None
            evidence = result.get("evidence") if isinstance(result, dict) else None
            if status not in STAGE_STATUSES or not isinstance(evidence, str) or not evidence.strip():
                defects.append(_defect("stage-result", f"invalid {stage} result", path))
            if status == "not_applicable" and stage != "quarantine_trial":
                defects.append(_defect("stage-status", f"{stage} is mandatory and cannot be not_applicable", path))
            if terminated and status != "not_run":
                defects.append(_defect("halt-order", f"{stage} must not run after HALT", path))
            if status == "not_run" and not terminated:
                defects.append(_defect("stage-order", f"{stage} cannot be not_run without an earlier HALT", path))
            terminated = terminated or status in {"halt", "not_run"}
        if type(candidate.get("vendor_policy_duplication")) is not bool:
            defects.append(_defect("candidate-field", "vendor_policy_duplication must be boolean", path))
        if candidate.get("vendor_policy_duplication") and decision != "reject":
            defects.append(_defect("vendor-policy", "vendor-specific policy duplication must be rejected", path))
        if decision == "reject" and not any(
            isinstance(stages.get(stage), dict) and stages[stage].get("status") == "halt"
            for stage in STAGES
        ):
            defects.append(_defect("reject-halt", "a rejection must identify its HALT stage", path))
        if decision == "adopt-code":
            if any(stages.get(stage, {}).get("status") != "pass" for stage in STAGES):
                defects.append(_defect("adopt-code-gates", "code adoption requires every gate to pass", path))
            promotion = candidate.get("promotion_pr")
            if not isinstance(promotion, str) or not re.fullmatch(
                r"https://github\.com/ShaftHQ/SHAFT_ENGINE/pull/\d+", promotion
            ):
                defects.append(_defect("promotion-pr", "code adoption requires a separate SHAFT promotion PR", path))
        if decision in {"adopt-pattern", "retain-test-target"}:
            required = ("provenance_license", "static_review", "local_evaluation")
            if any(stages.get(stage, {}).get("status") != "pass" for stage in required):
                defects.append(_defect("pattern-gates", "pattern and test-target reuse require cleared provenance, static review, and local evaluation", path))
            quarantine_status = stages.get("quarantine_trial", {}).get("status")
            if quarantine_status not in {"pass", "not_applicable"}:
                defects.append(_defect("pattern-gates", "pattern and test-target quarantine must pass or be explicitly inapplicable", path))
            if quarantine_status == "not_applicable" and candidate.get("material_kind") == "code":
                defects.append(_defect("pattern-gates", "candidate code cannot waive the quarantine trial", path))
        adopted_files = candidate.get("adopted_files")
        if not isinstance(adopted_files, list):
            defects.append(_defect("adopted-files", "adopted_files must be a list", path))
        elif adopted_files:
            defects.append(_defect("candidate-code", "this review PR must not contain adopted candidate files", path))
    if len(ids) != len(set(ids)):
        defects.append(_defect("candidate-id", "candidate ids must be unique"))
    if categories != REQUIRED_CATEGORIES:
        defects.append(_defect("candidate-categories", "review must cover documents, plugin evaluation, and cross-client packaging"))
    scope = review.get("review_scope", {})
    if not isinstance(scope, dict):
        defects.append(_defect("review-scope", "review_scope must be an object"))
        scope = {}
    if set(scope.get("candidate_ids", [])) != set(ids):
        defects.append(_defect("review-scope", "review_scope candidate ids drifted"))
    rejected = {candidate.get("id") for candidate in candidates if isinstance(candidate, dict) and candidate.get("decision") == "reject"}
    if set(scope.get("rejected_candidate_ids", [])) != rejected:
        defects.append(_defect("rejection-ledger", "every intentional rejection must be listed"))
    if review.get("code_adopted") is not False:
        defects.append(_defect("candidate-code", "candidate code must not enter the intake PR"))
    return defects


def validate_repository(root: Path = ROOT) -> list[dict]:
    root = Path(root)
    try:
        policy = json.loads((root / POLICY_PATH).read_text(encoding="utf-8"))
        review = json.loads((root / REVIEW_PATH).read_text(encoding="utf-8"))
        readme = (root / README_PATH).read_text(encoding="utf-8")
    except (OSError, json.JSONDecodeError) as error:
        return [_defect("intake-source", str(error))]
    defects = validate_policy(policy)
    defects.extend(validate_review(review, policy))
    for phrase in (
        "HALT conditions",
        "No host fallback",
        "separate adoption PR",
        "adopt code",
        "adopt a pattern",
        "retain a test target",
        "reject",
    ):
        if phrase not in readme:
            defects.append(_defect("intake-documentation", f"README must include {phrase!r}"))
    return defects


def main() -> int:
    defects = validate_repository()
    if defects:
        for defect in defects:
            print(f"{defect['code']}: {defect['path']}: {defect['message']}")
        return 1
    review = json.loads((ROOT / REVIEW_PATH).read_text(encoding="utf-8"))
    print(f"SHAFT candidate intake is valid: {len(review['candidates'])} reviewed candidates")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
