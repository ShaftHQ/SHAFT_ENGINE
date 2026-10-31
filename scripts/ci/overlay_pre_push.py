#!/usr/bin/env python3
"""Local pre-push contract for overlay markdown, skills, and hook entrypoints."""

from __future__ import annotations

import re

import importlib.util
import os
import shutil
import subprocess  # nosec B404 - fixed git invocations, list args only.
import sys
from pathlib import Path

# #6195: running this file directly must not need PYTHONPATH=. -- the repo
# root is two levels up and owns the `scripts.ci` package imports below.
REPO_ROOT = Path(__file__).resolve().parents[2]
if str(REPO_ROOT) not in sys.path:
    sys.path.insert(0, str(REPO_ROOT))

from scripts.ci.workflow_template_sync import stale_template_actions, touches_workflows  # noqa: E402

PLAYBOOK = "chaos-engine/references/work-github-playbook.md"
TIP_PREFLIGHT = "chaos-engine/skills/local-agency/scripts/tip_preflight.py"
BYTE_BUDGET = 16384
PLAYBOOK_HEADROOM = 1024  # room for the next lesson; CI asserts the same (#6846)
PINNED_CLAUSES = (
    "Before committing any subagent's work",
    "Before reviewing or shipping any nontrivial diff",
    "deferred/out-of-scope/adjacent-finding/follow-up",
)


def touches_overlay_contract(path: str) -> bool:
    """True for overlay markdown or a hook/skill entrypoint."""
    normalized = path.replace("\\", "/").removeprefix("./")
    if normalized.startswith("chaos-engine/") and normalized.endswith(".md"):
        return True
    if normalized.startswith("chaos-engine/hooks/") and normalized.endswith(".py"):
        return True
    return normalized.startswith("chaos-engine/skills/") and normalized.endswith("SKILL.md")


def playbook_contract_failures(text: str, *, budget: int = BYTE_BUDGET) -> list[str]:
    """Fail an over-budget playbook and a playbook that drops a pinned clause."""
    failures: list[str] = []
    size = len(text.encode("utf-8"))
    if size > budget:
        failures.append(f"playbook is {size} bytes; budget is {budget}")
    elif budget == BYTE_BUDGET and size + PLAYBOOK_HEADROOM > budget:
        failures.append(f"playbook is {size} bytes; keep {PLAYBOOK_HEADROOM} bytes headroom under {budget} "
                        "(move detail into references/details/)")
    for clause in PINNED_CLAUSES:
        if clause not in text:
            failures.append(f"dropped pinned clause: {clause}")
    return failures


NEAR_CAP_BYTES = 512


def skill_budget_findings(root: Path) -> tuple[list[str], list[str]]:
    """#6176: SKILL.md over its `skill_budgets` cap fails; within 512 B warns."""
    import json

    budget_path = root / "scripts/ci/agent_guidance_budget.json"
    try:
        budgets = json.loads(budget_path.read_text(encoding="utf-8")).get("skill_budgets", {})
    except (OSError, ValueError):
        return [], []
    failures: list[str] = []
    warnings: list[str] = []
    for key, limits in sorted(budgets.items()):
        cap = limits.get("max_skill_md_bytes") if isinstance(limits, dict) else None
        if not isinstance(cap, int):
            continue
        for skill in sorted((root / key).glob("*/SKILL.md")):
            size = len(skill.read_bytes())
            name = skill.relative_to(root).as_posix()
            if size > cap:
                failures.append(f"{name}: {size} bytes exceeds skill cap {cap}")
            elif size > cap - NEAR_CAP_BYTES:
                warnings.append(f"{name}: {size} bytes is within {NEAR_CAP_BYTES} B of cap {cap}")
    return failures, warnings


def _unique(paths: list[str]) -> list[str]:
    seen: set[str] = set()
    ordered: list[str] = []
    for path in paths:
        if path and path not in seen:
            seen.add(path)
            ordered.append(path)
    return ordered


def changed_overlay_paths(root: Path) -> list[str]:
    """Names changed versus origin/main plus the uncommitted worktree."""
    names: list[str] = []
    git = shutil.which("git")
    if git is None:
        return names
    for args in (
        ["diff", "--name-only", "origin/main...HEAD"],
        ["diff", "--name-only", "HEAD"],
        ["diff", "--name-only", "--cached"],
    ):
        try:
            completed = subprocess.run(  # nosec B603 - absolute git from shutil.which, fixed argv.
                [git, *args],
                cwd=root,
                capture_output=True,
                text=True,
                check=False,
            )
        except OSError:
            continue
        if completed.returncode == 0:
            names.extend(line.strip() for line in completed.stdout.splitlines() if line.strip())
    return _unique(names)


def tip_preflight_failures(root: Path, paths: list[str]) -> list[str]:
    """Same-tip B607 + README inventory + Memory content_hash preflight (#6164/#6165/#6169)."""
    script = root / TIP_PREFLIGHT
    if not script.is_file():
        return []
    spec = importlib.util.spec_from_file_location("chaos_engine_tip_preflight", script)
    if spec is None or spec.loader is None:
        return ["tip preflight: checker unavailable"]
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return [f"tip preflight: {failure}" for failure in module.preflight_failures(root, paths)]


def code_quality_failures(root: Path) -> list[str]:
    """Codacy / code-quality classes on changed Python lines, before CI finds them."""
    script = root / "scripts/ci/code_quality_preflight.py"
    if not script.is_file():
        return []
    spec = importlib.util.spec_from_file_location("chaos_engine_code_quality_preflight", script)
    if spec is None or spec.loader is None:
        return ["code-quality preflight: checker unavailable"]
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    findings = module.preflight_findings(root)
    if not findings:
        return []
    more = f" (+{len(findings) - 1} more; run python3 scripts/ci/code_quality_preflight.py)" if len(findings) > 1 else ""
    return [f"code-quality preflight: {findings[0]}{more}"]



RELEASE_NOTE_LABELS = frozenset(
    {"breaking-change", "enhancement", "bug", "skip-release-notes"}
)


def _gh_open_pr_for_head(root: Path) -> dict | None:
    """Return gh JSON for the open PR on HEAD, or None when unavailable."""
    git = shutil.which("git")
    gh = shutil.which("gh")
    if git is None or gh is None:
        return None
    try:
        branch = subprocess.run(  # nosec B603 - absolute git, fixed argv.
            [git, "rev-parse", "--abbrev-ref", "HEAD"],
            cwd=root,
            capture_output=True,
            text=True,
            check=False,
        )
    except OSError:
        return None
    if branch.returncode != 0:
        return None
    name = (branch.stdout or "").strip()
    if not name or name == "HEAD" or name in {"main", "master"}:
        return None
    try:
        completed = subprocess.run(  # nosec B603 - absolute gh, fixed argv.
            [
                gh,
                "pr",
                "view",
                "--json",
                "number,labels,author,isDraft,body,title",
                "--head",
                name,
            ],
            cwd=root,
            capture_output=True,
            text=True,
            check=False,
        )
    except OSError:
        return None
    if completed.returncode != 0 or not (completed.stdout or "").strip():
        return None
    try:
        import json

        payload = json.loads(completed.stdout)
    except (ValueError, TypeError):
        return None
    return payload if isinstance(payload, dict) else None


def pr_body_failures(root: Path, *, pr_fetcher=_gh_open_pr_for_head) -> list[str]:
    """Fail when the open PR body would fail CI's PR Body guard (#6995 miss).

    The guard reads the event payload, so fixing the body after a red run
    still costs a push and a full CI round trip. Fix the body first; hosts
    without the guard hook (bots) get the same check here.
    """
    pr = pr_fetcher(root)
    if not pr or not isinstance(pr.get("body"), str):
        return []
    # Loaded by path: consumer copies of this hook ship without the CI validator.
    validator = root / "scripts" / "ci" / "validate_pr_closing_keywords.py"
    if not validator.is_file():
        return []
    spec = importlib.util.spec_from_file_location("chaos_engine_pr_body_validator", validator)
    if spec is None or spec.loader is None:
        return []
    module = importlib.util.module_from_spec(spec)
    try:
        spec.loader.exec_module(module)
    except Exception:  # nosec B110 - an unloadable validator must not block a push.
        return []
    mindset_body_errors = module.mindset_body_errors
    previous = os.environ.get("PR_TITLE")
    os.environ["PR_TITLE"] = str(pr.get("title") or "")
    try:
        findings = mindset_body_errors(pr["body"])
    finally:
        if previous is None:
            os.environ.pop("PR_TITLE", None)
        else:
            os.environ["PR_TITLE"] = previous
    number = pr.get("number") or "?"
    return [
        f"PR #{number} body would fail the PR Body guard: {item.get('message', item)}; "
        f"fix it with `gh api -X PATCH repos/OWNER/REPO/pulls/{number} -F body=@FILE`, then push"
        for item in findings
    ]


def release_note_label_failures(root: Path, *, pr_fetcher=_gh_open_pr_for_head) -> list[str]:
    """Fail when the open human PR for HEAD lacks exactly one release-note label.

    Catches the #6702 failure mode (governance red until a label is added)
    before the next push. No open PR, bots, and missing gh are soft skips —
    first create still applies the label via ``gh pr create --label``.
    """
    pr = pr_fetcher(root)
    if not pr:
        return []
    author = pr.get("author") if isinstance(pr.get("author"), dict) else {}
    login = str(author.get("login") or "").casefold()
    if login.endswith("[bot]") or str(author.get("is_bot") or "").lower() in {"true", "1"}:
        return []
    labels = {
        str(item.get("name"))
        for item in (pr.get("labels") or [])
        if isinstance(item, dict) and item.get("name")
    }
    selected = sorted(labels & RELEASE_NOTE_LABELS)
    if len(selected) == 1:
        return []
    number = pr.get("number") or "?"
    return [
        "release-note label: open PR #"
        + str(number)
        + " needs exactly one of "
        + ", ".join(sorted(RELEASE_NOTE_LABELS))
        + f"; found {selected or 'none'}. "
        + "Apply with: gh pr edit "
        + str(number)
        + " --add-label <label> (or REST labels API)."
    ]


def portable_core_path_failures(root: Path) -> list[str]:
    """Reject a chaos-engine tree that contains a machine-specific path (#6284).

    This is the same check as
    ``ChaosEnginePortableCoreTest.test_generic_core_has_no_shaft_or_machine_specific_paths``.
    A checkout without the portable installer is not that tree.
    """
    if not (root / "chaos-engine" / "install.py").is_file():
        return []
    import unittest

    from tests.scripts.test_chaos_engine_portable_core import ChaosEnginePortableCoreTest

    case = ChaosEnginePortableCoreTest("test_generic_core_has_no_shaft_or_machine_specific_paths")
    result = unittest.TestResult()
    case.run(result)
    if result.wasSuccessful():
        return []
    detail = ""
    if result.failures or result.errors:
        detail = (result.failures + result.errors)[0][1].strip().splitlines()[-1]
    return [f"portable core path check failed: {detail}"]


# CI-only cheap contract tests that missed pre-push in #6979/#6981, costing a
# CI round trip each. (path predicate, module, test id) run in-process.
CHEAP_CI_CONTRACTS = (
    (lambda p: p.startswith("chaos-engine/") and p.endswith(".md"),
     "tests.scripts.test_ce_lean_harness", "BudgetTests.test_implement_path_fits_16_kib_on_every_host"),
    (lambda p: p.startswith("chaos-engine/") and p.endswith(".md"),
     "tests.scripts.test_ce_lean_harness", "LintTests.test_lean_lint_is_clean_against_its_shrink_only_allowlist"),
    # A relative link that leaves the shipped package breaks installs (#6987 miss).
    (lambda p: p.startswith("chaos-engine/") and p.endswith(".md"),
     "tests.scripts.test_ce_guard_reachability", "GuardReachabilityTest.test_installed_overlay_has_no_broken_relative_links"),
    (lambda p: p.startswith("chaos-engine/") and p.endswith(".md"),
     "tests.scripts.test_assemble_chaos_engine_plugin",
     "AssembleActAsMohabPluginTest.test_assembly_keeps_every_relative_markdown_link_inside_the_package"),
    (lambda p: p == "scripts/ci/harness_pr_gate.py",
     "tests.scripts.test_harness_pr_gate", "UngatedModulesRunInCiTest.test_allowlist_never_exceeds_the_ceiling"),
    # A new workflow missing from .github/workflows/README.md failed CI (#6993 miss).
    (lambda p: p.startswith(".github/workflows/"),
     "tests.scripts.test_validate_quality_configuration",
     "ValidateQualityConfigurationTest.test_repository_configuration_is_valid"),
    # A new skill missing from the ChaosEngine README inventory (#6991 miss).
    (lambda p: p.startswith("chaos-engine/skills/") or p == "chaos-engine/harness_index.py",
     "tests.scripts.test_validate_chaos_engine_readme",
     "ChaosEngineReadmeInventoryTest.test_repository_readme_matches_every_source_derived_inventory"),
    # N4/#7643: tip-churn / skills map / delete-guard consumer / upgrade-session repair
    # must not escape cheap pre-push the way AG Gate catches them in CI.
    (lambda p: p in {
         "chaos-engine/references/tip-churn-preflight.md",
         "chaos-engine/references/ce-upgrade-on-release.md",
         "chaos-engine/references/cleanup-at-close.md",
         "chaos-engine/references/worker-keep-alive.md",
     } or p.startswith("chaos-engine/references/"),
     "tests.scripts.test_ce_memory_abstraction_phase1",
     "ProcessCardsTest.test_tip_churn_links_new_refs"),
    (lambda p: p in {".agents/skills/README.md", "chaos-engine/references/ce-upgrade-on-release.md"}
     or p.startswith("chaos-engine/references/"),
     "tests.scripts.test_agent_router_contract",
     "SkillsMapTest.test_the_map_covers_every_skill_playbook_and_chapter"),
    (lambda p: p in {"chaos-engine/references/harness-map.md", "chaos-engine/harness_map.py"}
     or p == "chaos-engine/references/ce-upgrade-on-release.md",
     "tests.scripts.test_chaos_engine_harness_map_7242",
     "HarnessMapTests.test_map_is_fresh"),
    (lambda p: p == "chaos-engine/delete_guard.py",
     "tests.scripts.test_chaos_engine_delete_guard_7251",
     "DeleteGuardTest.test_backup_remote_silent_in_consumer_mode"),
    (lambda p: p in {"chaos-engine/worktree_overlay.py", "chaos-engine/install.py"},
     "tests.scripts.test_policy_overlay_repair",
     "PolicyOverlayRepairTest.test_linked_worktree_repair_names_the_materialize_fix"),
    (lambda p: p == "chaos-engine/delete_guard.py"
     or p.endswith("test_ce_session_manifest_delete_and_backup_silence.py"),
     "tests.scripts.test_ce_session_manifest_delete_and_backup_silence",
     "SessionManifestDeleteTest.test_forged_manifest_rejected"),
)



def unselected_unit_test_failures(root: Path, changed: list[str]) -> list[str]:
    """Refuse a *UnitTest that no PR Gate shard selects (#7310 / #7529).

    Mirrors ``tests.scripts.test_shard_test_selector.unselected_unit_test_classes``
    without importing PyYAML: the ``-Dtest`` selector is read with a regex from
    ``pr-gate.yml``. Classes under ``com/shaft/test/unittests/`` ride the shard-1
    glob; every other ``*UnitTest`` must be named in the selector. Discovery keeps
    a case-sensitive ``UnitTest.java`` suffix so Windows ``rglob`` cannot treat
    ``JunitTest.java`` as a unit-test class (#7627).
    """
    from scripts.ci.unit_test_discovery import is_unit_test_source_name, iter_unit_test_sources

    normalized = [path.replace("\\", "/").removeprefix("./") for path in changed]
    if not any(
        is_unit_test_source_name(path) or path.endswith(".github/workflows/pr-gate.yml")
        for path in normalized
    ):
        return []
    pr_gate = root / ".github/workflows/pr-gate.yml"
    test_root = root / "shaft-engine/src/test/java"
    if not pr_gate.is_file() or not test_root.is_dir():
        return []
    match = re.search(r"'-Dtest=([^']*)'", pr_gate.read_text(encoding="utf-8"))
    if match is None:
        return ["PR Gate unit-test step lost its -Dtest selector"]
    named = {entry.strip() for entry in match.group(1).split(",")}
    missing = []
    for path in iter_unit_test_sources(test_root):
        relative = path.relative_to(test_root).as_posix()
        if relative.startswith("com/shaft/test/unittests/") or path.stem in named:
            continue
        missing.append(relative)
    if not missing:
        return []
    return [
        "UnitTest class not selected by PR Gate shards (#7310): "
        + missing[0]
        + (" (+%d more)" % (len(missing) - 1) if len(missing) > 1 else "")
        + "; add the class name to -Dtest in .github/workflows/pr-gate.yml "
        "and a matching !%regex exclusion in e2eTests.yml / e2eLocalTests.yml"
    ]


def cheap_ci_contract_failures(root: Path, changed: list[str]) -> list[str]:
    """Run the Agent Guidance Gate's cheap byte-budget/lint/allowlist tests before push."""
    import importlib
    import unittest

    normalized = [p.replace("\\", "/").removeprefix("./") for p in changed]
    failures = []
    for predicate, module_name, test_id in CHEAP_CI_CONTRACTS:
        if not any(predicate(p) for p in normalized):
            continue
        if not (root / (module_name.replace(".", "/") + ".py")).is_file():
            continue
        try:
            module = importlib.import_module(module_name)
            suite = unittest.defaultTestLoader.loadTestsFromName(test_id, module)
        except (ImportError, AttributeError) as error:
            failures.append(f"{module_name}.{test_id}: could not load ({error})")
            continue
        result = unittest.TestResult()
        suite.run(result)
        if not result.wasSuccessful():
            detail = (result.failures + result.errors)[0][1].strip().splitlines()[-1]
            failures.append(f"{module_name}.{test_id} failed: {detail}")
    return failures


def ungated_script_module_failures(root: Path) -> list[str]:
    """Refuse a new tests/scripts module that no harness PR-gate check selects (#6839).

    Loads the stdlib checker by path. Importing the test module pulled PyYAML
    into the hook interpreter, and that crash was reported as invalid JSON.
    A consumer copy of this file has no harness gate and skips the check.
    """
    gate = root / "scripts/ci/harness_pr_gate.py"
    if not gate.is_file():
        return []
    name = "chaos_engine_harness_pr_gate"
    module = sys.modules.get(name)
    if module is None or not hasattr(module, "ungated_script_modules"):
        spec = importlib.util.spec_from_file_location(name, gate)
        if spec is None or spec.loader is None:
            return ["ungated script modules: checker unavailable"]
        module = importlib.util.module_from_spec(spec)
        # Dataclasses look up the class module in sys.modules during class body.
        sys.modules[name] = module
        spec.loader.exec_module(module)
    missing = module.ungated_script_modules(root)
    if not missing:
        return []
    return [
        "script test module not selected by harness_pr_gate "
        "(add it to scripts/ci/harness_pr_gate.py): " + missing[0]
    ]


def shipped_validator_import_failures(root: Path, changed: list[str]) -> list[str]:
    """Shipped validators must ship every scripts.* module they import (#6843). Lazy import keeps the
    consumer copy of this file free of a new module-scope dependency."""
    if not (root / "scripts/mcp/install_shaft_agentic_tools.py").is_file():
        return []
    from scripts.ci import validation_manifest_imports as manifest

    if not manifest.touches_shipped_validators(root, changed):
        return []
    return [f"shipped validator import missing from AGENT_VALIDATION_SCRIPT_FILES (#6843): {item}"
            for item in manifest.missing_imports(root, manifest.shipped_files(root))]


COVERAGE_RATCHETS = ("scripts/ci/validate_javadoc_coverage.py", "scripts/ci/validate_test_reference_coverage.py")


def coverage_ratchet_failures(root: Path, changed: list[str]) -> list[str]:
    """New public Java API must be documented and test-called before push (#6851 ratchet miss)."""
    if not any(p.replace("\\", "/").endswith(".java") and "/src/" in p for p in changed):
        return []
    failures = []
    for script in COVERAGE_RATCHETS:
        if not (root / script).is_file():
            continue
        # nosec B603 - fixed in-repo ratchet scripts, list args only.
        result = subprocess.run([sys.executable, script], cwd=root, capture_output=True, text=True, check=False)  # nosec B603
        if result.returncode:
            lines = (result.stdout + result.stderr).strip().splitlines()
            failures.append(f"{script} failed (#6851): " + "; ".join(lines[:4]))
    return failures


def codecov_yml_failures(root: Path, changed: list[str]) -> list[str]:
    """#6990 lesson: Codecov ignores an invalid codecov.yml wholesale; validate it before push."""
    if "codecov.yml" not in {p.replace("\\", "/").removeprefix("./") for p in changed}:
        return []
    result = subprocess.run(  # nosec B603 - fixed script path
        [sys.executable, str(root / "scripts/ci/validate_codecov_yml.py"), "--soft"],
        cwd=root, capture_output=True, text=True, check=False)
    return [] if result.returncode == 0 else [result.stdout.strip() or "codecov.yml validation failed"]




def tracked_mobile_binary_failures(root: Path) -> list[str]:
    """Fail closed when Phase 0 fixture apk/ipa paths are tracked again (#7547)."""
    helper = root / "scripts" / "ci" / "fetch_mobile_test_apps.py"
    if not helper.is_file():
        return []
    sys.path.insert(0, str(helper.parent))
    import fetch_mobile_test_apps as fetch  # noqa: WPS433

    return list(fetch.tracked_mobile_binary_failures(root))


def no_push_guard_failures(root: Path) -> list[str]:
    """Fail closed when the tracked NO_PUSH guard is absent.

    Sparse fixtures that only copy this checker (OverlayPrePushTest) omit the
    helper; skip rather than shadow playbook failures. Full checkouts that ship
    ``chaos-engine/mobile_retro.py`` must keep ``scripts/ci/no_push_guard.py``
    tracked.
    """
    helper = root / "chaos-engine" / "mobile_retro.py"
    if not helper.is_file():
        return []
    sys.path.insert(0, str(root / "chaos-engine"))
    import mobile_retro  # noqa: WPS433

    return list(mobile_retro.no_push_guard_tracked(root))


def evidence_pin_failures(root: Path) -> list[str]:
    """
    Fail closed when a persisted mobile-retro evidence pin mismatches (#7498).

    Absent pin file → []. Sparse fixtures without mobile_retro skip.
    """
    helper = root / "chaos-engine" / "mobile_retro.py"
    runtime = root / "scripts" / "ci" / "mobile_retro_runtime.py"
    if not helper.is_file() or not runtime.is_file():
        return []
    sys.path.insert(0, str(root / "chaos-engine"))
    spec = importlib.util.spec_from_file_location("mobile_retro_runtime", runtime)
    if spec is None or spec.loader is None:
        return ["evidence pin checker unavailable"]
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return list(module.evidence_pin_failures(root))


def client_name_scrub_failures(
    root: Path,
    *,
    environ: dict[str, str] | None = None,
) -> list[str]:
    """Fail closed when a configured client-name denylist matches PR-visible text.

    Unconfigured denylist → []. Sparse OverlayPrePush fixtures without the
    helper module skip. See references/client-name-scrub.md.
    """
    helper = root / "chaos-engine" / "client_name_scrub.py"
    if not helper.is_file():
        return []
    sys.path.insert(0, str(root / "chaos-engine"))
    import client_name_scrub  # noqa: WPS433

    return list(client_name_scrub.client_name_scrub_failures(root, environ=environ))



NO_VERSION_BUMP_MARKER = "<!-- chaos-engine:no-version-bump -->"
VERSION_FILE = "chaos-engine/VERSION"


def pr_scope_failures(root: Path, paths: list[str], *, pr_fetcher=_gh_open_pr_for_head) -> list[str]:
    """H-PR-SCOPE: agentic open PR diff must match ## Scope / scope comment.

    Soft-skips when there is no open PR, no gh, or the PR is not agentic.
    """
    pr = pr_fetcher(root)
    if not pr or not isinstance(pr.get("body"), str):
        return []
    try:
        from scripts.ci.pr_scope_check import is_agentic_pr, evaluate
    except Exception:  # nosec B110 - missing helper must not block push
        return []
    if not is_agentic_pr(pr):
        return []
    result = evaluate(pr["body"], list(paths))
    if result.get("ok"):
        return []
    number = pr.get("number") or "?"
    error = result.get("error") or "diff outside stated scope"
    hits = result.get("hits") or []
    return [
        f"pr-scope (H-PR-SCOPE): PR #{number} {error}; "
        "declare ## Scope / <!-- chaos-engine:scope ... --> matching git diff paths"
    ] + [f"pr-scope: {h}" for h in hits]


def ce_version_bump_failures(root: Path, paths: list[str]) -> list[str]:
    """
    Require VERSION bump when consumer-facing chaos-engine/ paths change.

    Waive with PR body marker ``<!-- chaos-engine:no-version-bump -->`` or label
    ``ce-no-version-bump`` (checked when an open PR exists for HEAD).
    """
    normalized = [p.replace("\\", "/").removeprefix("./") for p in paths]
    ce_hits = [p for p in normalized if p.startswith("chaos-engine/")]
    if not ce_hits:
        return []
    if VERSION_FILE in ce_hits or any(p == VERSION_FILE for p in normalized):
        return []
    # Allow waive via marker file staged beside VERSION for local pre-push tests
    waive = root / ".chaos-engine-state" / "no-version-bump"
    if waive.is_file():
        return []
    pr = _gh_open_pr_for_head(root)
    if isinstance(pr, dict):
        body = str(pr.get("body") or "")
        labels = {
            str(item.get("name") or "").lower()
            for item in (pr.get("labels") or [])
            if isinstance(item, dict)
        }
        if NO_VERSION_BUMP_MARKER in body or "ce-no-version-bump" in labels:
            return []
    return [
        "ce-version-bump: chaos-engine/ changed without chaos-engine/VERSION; "
        "bump VERSION or add <!-- chaos-engine:no-version-bump --> / label ce-no-version-bump"
    ]


def skillspector_gate_failures(root: Path, paths: list[str]) -> list[str]:
    """Run the static SkillSpector gate when first-party skill paths change."""
    from scripts.ci.skillspector_gate import gate_failures, skill_dirs_from_paths

    if not skill_dirs_from_paths(paths):
        return []
    return [f"skillspector: {item}" if not item.startswith("skillspector") else item
            for item in gate_failures(root, paths=paths)]


def overlay_pre_push_failures(root: Path, paths: list[str] | None = None) -> list[str]:
    """Return contract failures for one overlay diff. Empty means the push may proceed."""
    changed = list(paths) if paths is not None else changed_overlay_paths(root)
    failures: list[str] = tip_preflight_failures(root, changed)
    failures.extend(skillspector_gate_failures(root, changed))
    failures.extend(ce_version_bump_failures(root, changed))
    if paths is None:
        failures.extend(tracked_mobile_binary_failures(root))
        failures.extend(no_push_guard_failures(root))
        failures.extend(evidence_pin_failures(root))
        failures.extend(client_name_scrub_failures(root))
        failures.extend(code_quality_failures(root))
        failures.extend(release_note_label_failures(root))
        failures.extend(pr_body_failures(root))
        failures.extend(pr_scope_failures(root, changed))
    if any(path.replace("\\", "/").removeprefix("./").startswith("chaos-engine/") for path in changed):
        failures.extend(portable_core_path_failures(root))
    if any(path.replace("\\", "/").startswith("tests/scripts/test_") for path in changed):
        failures.extend(ungated_script_module_failures(root))
    failures.extend(shipped_validator_import_failures(root, changed))
    failures.extend(unselected_unit_test_failures(root, changed))
    failures.extend(cheap_ci_contract_failures(root, changed))
    failures.extend(coverage_ratchet_failures(root, changed))
    failures.extend(codecov_yml_failures(root, changed))
    if "scripts/mcp/install_shaft_agentic_tools.py" in {p.replace("\\", "/") for p in changed}:
        # #6849: Installer verification is CI-only and path-filtered; it runs this ~30 s end-to-end check.
        print("overlay pre-push: warning: installer changed; run "
              "`python3 scripts/ci/verify_shaft_mcp_installer_release.py` before pushing", file=sys.stderr)
    if any(touches_workflows(path) for path in changed):
        failures.extend(
            f"workflow template drift (#3798): {item}; sync shaft-engine/src/main/resources/examples/.github/workflows"
            for item in stale_template_actions(root)
        )
    if not any(touches_overlay_contract(path) for path in changed):
        return failures
    playbook = root / PLAYBOOK
    if playbook.is_file():
        failures.extend(playbook_contract_failures(playbook.read_text(encoding="utf-8")))
    elif any(path.replace("\\", "/").endswith(PLAYBOOK) for path in changed):
        failures.append("dropped pinned clause: playbook missing")
    budget_path = root / "scripts/ci/agent_guidance_budget.json"
    if budget_path.is_file():
        from scripts.ci.validate_agent_guidance import load_budget, validate_file_budgets

        for item in validate_file_budgets(root, load_budget(budget_path)):
            failures.append(f"{item.get('path')}: {item.get('message')}")
    skill_failures, skill_warnings = skill_budget_findings(root)
    failures.extend(skill_failures)
    for warning in skill_warnings:
        print(f"overlay pre-push: warning: {warning}", file=sys.stderr)
    skill = root / "chaos-engine/skills/chaos-engine/SKILL.md"
    if skill.is_file():
        from scripts.ci.validate_agent_setup import validate_harness_reachability

        for item in validate_harness_reachability(root):
            failures.append(f"{item.get('path')}: {item.get('message')}")
    return failures


def main(argv: list[str] | None = None) -> int:
    """Exit 1 when the overlay pre-push contract fails."""
    root = Path(argv[1]).resolve() if argv and len(argv) > 1 else Path.cwd()
    failures = overlay_pre_push_failures(root)
    if not failures:
        print("overlay pre-push: pass")
        return 0
    for failure in failures:
        print(f"overlay pre-push: {failure}", file=sys.stderr)
    return 1


if __name__ == "__main__":
    raise SystemExit(main(sys.argv))
