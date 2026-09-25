#!/usr/bin/env python3
"""Run fast, change-scoped ChaosEngine pull-request checks."""

from __future__ import annotations

import argparse
import fnmatch
import json
import os
import re
import shutil
import signal
import subprocess  # nosec B404 - executes fixed unittest commands without a shell.
import sys
import tempfile
import time
from concurrent.futures import ThreadPoolExecutor
from dataclasses import dataclass
from datetime import datetime, timedelta, timezone
from pathlib import Path, PurePosixPath
from typing import Any


class GateError(ValueError):
    """Invalid gate input."""


class ChangedPath(str):
    """A changed path annotated with whether its head revision can execute."""

    executable: bool

    def __new__(cls, value: str, *, executable: bool) -> ChangedPath:
        instance = str.__new__(cls, value)
        instance.executable = executable
        return instance


@dataclass(frozen=True)
class Check:
    id: str
    surface: str
    modules: tuple[str, ...]
    protected: bool = False
    # #6222: an open issue tracking content drift that is red on main today.
    # A failure is reported as "known-drift" (never blocking) until the issue
    # is fixed; a pass tells the author to promote the check by clearing it.
    known_drift: str = ""

    @property
    def reproduction_command(self) -> str:
        return "python -m unittest " + " ".join(self.modules) + " -v"


@dataclass(frozen=True)
class GatePlan:
    surfaces: tuple[str, ...] = ()
    checks: tuple[Check, ...] = ()
    unknown_paths: tuple[str, ...] = ()

    @property
    def test_modules(self) -> tuple[str, ...]:
        return tuple(dict.fromkeys(module for check in self.checks for module in check.modules))


@dataclass(frozen=True)
class WaiverReceipt:
    head_sha: str
    check_ids: tuple[str, ...]
    expires_at: datetime


PROTECTED_IDS = frozenset(
    {
        "protected-security",
        "protected-ownership",
        "protected-corruption",
        "protected-rollback",
        "protected-secret-safety",
        "protected-installer-acceptance",
        "protected-confirmed-correctness",
    }
)
WAIVER_OWNER = "MohabMohie"
# Always-on protected checks start first so long suites cannot starve them.
PRIORITY_CHECK_IDS = ("protected-ownership", "protected-secret-safety")
# Longest suites (CI receipts: ~370 s, ~350 s, ~75 s) start next so the
# concurrent wall time is bounded by the slowest suite, not by queueing (#6191).
LONG_RUNNING_CHECK_IDS = (
    "protected-rollback",
    "protected-installer-acceptance",
    "setup-aggregator-contract",
)
# Issue #6207: the two longest protected modules run as deterministic shards
# (scripts/ci/unittest_shard.py) so the concurrent gate is not bounded by one
# module. The union of a module's shards is exactly that module.
SHARDED_MODULES = {
    "tests.scripts.test_chaos_engine_installer": 4,
    "tests.scripts.test_chaos_engine_bootstrap": 2,
}
RECORDED_BASELINE_MEDIAN_SECONDS = 600
PR_BUDGET_SECONDS = 360
WAIVER_FENCE = re.compile(
    r"```chaos-engine-waiver[ \t]*\r?\n(.*?)\r?\n```", re.DOTALL
)

CHECKS = {
    "kernel-contract": Check(
        "kernel-contract", "kernel", ("tests.scripts.test_chaos_engine_kernel",), True
    ),
    "lifecycle-contract": Check(
        "lifecycle-contract",
        "lifecycle",
        ("tests.scripts.test_chaos_engine_hook",),
        True,
    ),
    "eval-parity-contract": Check(
        "eval-parity-contract",
        "lifecycle",
        ("tests.scripts.test_chaos_engine_eval_parity_fixtures",),
    ),
    "host-contract": Check(
        "host-contract", "hosts", ("tests.scripts.test_chaos_engine_hosts",), True
    ),
    "guidance-contract": Check(
        "guidance-contract",
        "guidance",
        (
            "tests.scripts.test_validate_agent_guidance",
            "tests.scripts.test_omniroute",
            "tests.scripts.test_omniroute_workflow_contract",
            "tests.scripts.test_omniroute_orchestrator_docs",
            "tests.scripts.test_omniroute_tdd_pdca",
            "tests.scripts.test_omniroute_portability",
        ),
    ),
    "skill-contract": Check(
        "skill-contract", "guidance", ("tests.scripts.test_validate_skills",)
    ),
    "guidance-reachability-contract": Check(
        "guidance-reachability-contract",
        "guidance",
        ("tests.scripts.test_agent_harness_reachability",),
    ),
    "plugin-contract": Check(
        "plugin-contract",
        "plugins",
        ("tests.scripts.test_validate_agent_plugins",),
    ),
    "plugin-quality-contract": Check(
        "plugin-quality-contract", "plugins", ("tests.scripts.test_shaft_skill_quality",)
    ),
    "retrieval-contract": Check(
        "retrieval-contract",
        "retrieval",
        ("tests.scripts.test_knowledge_stores",),
    ),
    "graph-resolver-contract": Check(
        "graph-resolver-contract", "retrieval", ("tests.scripts.test_resolve_graph_out",)
    ),
    "ci-contract": Check(
        "ci-contract",
        "ci",
        ("tests.scripts.test_harness_pr_gate", "tests.scripts.test_unittest_shard"),
    ),
    "setup-aggregator-contract": Check(
        "setup-aggregator-contract",
        "ci",
        ("tests.scripts.test_validate_agent_setup",),
    ),
    "documentation-inventory-contract": Check(
        "documentation-inventory-contract",
        "documentation",
        ("tests.scripts.test_validate_chaos_engine_readme",),
    ),
    "accessibility-contract": Check(
        "accessibility-contract",
        "accessibility",
        ("tests.scripts.test_accessibility_quality_gates",),
    ),
    "dependency-account-contract": Check(
        "dependency-account-contract",
        "installer",
        (
            "tests.scripts.test_chaos_engine_dependencies.ChaosEngineDependenciesTest."
            "test_account_tool_plan_uses_resolved_stable_versions_then_matching_rerun_reuses",
            "tests.scripts.test_chaos_engine_dependencies.ChaosEngineDependenciesTest."
            "test_account_install_passes_resolved_tool_versions_to_the_account_plan",
        ),
    ),
    "identity-contract": Check(
        "identity-contract",
        "identities",
        ("tests.scripts.test_chaos_gauge_contracts",),
    ),
    "identity-recovery-contract": Check(
        "identity-recovery-contract",
        "identities",
        ("tests.scripts.test_chaos_gauge_recovery",),
    ),
    "promotion-contract": Check(
        "promotion-contract",
        "promotion",
        ("tests.scripts.test_chaos_engine_promotion",),
    ),
    "fallback-contract": Check(
        "fallback-contract",
        "fallback",
        ("tests.scripts.test_validate_agent_setup",),
    ),
    "javadoc-param-arity-contract": Check(
        "javadoc-param-arity-contract",
        "javadoc",
        (
            "tests.scripts.test_check_javadoc_param_arity",
            "tests.scripts.test_chaos_engine_zero_llm_catalog",
        ),
    ),
    "protected-ownership": Check(
        "protected-ownership",
        "protected",
        ("tests.scripts.test_validate_agent_ownership",),
        True,
    ),
    "protected-secret-safety": Check(
        "protected-secret-safety",
        "protected",
        (
            "tests.scripts.test_guard_nul_corruption",
            "tests.scripts.test_harness_pr_gate",
            "tests.scripts.test_guard_lifecycle.ReflectionCheckpointContractTest.test_receipt_rejects_stale_fingerprint_secret_and_user_path",
            "tests.scripts.test_guard_lifecycle.ReflectionReceiptPrivacyTest.test_session_token_and_closed_schema_reject_forged_receipts",
            "tests.scripts.test_guard_lifecycle.ReflectionReceiptPrivacyTest.test_failure_classifications_never_persist_secret_or_user_path",
        ),
        True,
    ),
    "protected-security": Check(
        "protected-security",
        "lifecycle",
        (
            "tests.scripts.test_guard_memory_worktree.MemoryWriteFromLinkedWorktreeTest.test_memory_write_from_a_linked_worktree_without_a_target_is_denied",
            "tests.scripts.test_guard_memory_worktree.MemoryWriteFromLinkedWorktreeTest.test_memory_patch_write_from_a_linked_worktree_is_denied_too",
            "tests.scripts.test_guard_memory_worktree.MemoryWriteFromLinkedWorktreeTest.test_a_write_targeted_at_another_tree_is_still_denied",
            "tests.scripts.test_guard_memory_worktree.MemoryWriteFromLinkedWorktreeTest.test_the_refusal_does_not_claim_where_the_write_would_land",
        ),
        True,
    ),
    "protected-installer-acceptance": Check(
        "protected-installer-acceptance",
        "installer",
        (
            "tests.scripts.test_chaos_engine_bootstrap",
            "tests.scripts.test_chaos_engine_dependencies",
            "tests.scripts.test_chaos_engine_generation_runtime",
            "tests.scripts.test_chaos_engine_live_installer_acceptance",
        ),
        True,
    ),
    "protected-rollback": Check(
        "protected-rollback",
        "installer",
        ("tests.scripts.test_chaos_engine_installer",),
        True,
    ),
    "token-waste-contract": Check(
        "token-waste-contract",
        "token-waste",
        (
            "tests.scripts.test_chaos_engine_token_waste_6161",
            "tests.scripts.test_watch_pr_checks",
            "tests.scripts.test_overlay_pre_push",
        ),
    ),
    "token-economy-contract": Check(
        "token-economy-contract",
        "token-economy",
        (
            "tests.scripts.test_chaos_engine_token_economy_6173",
            "tests.scripts.test_retrieve_gate_scope",
            "tests.scripts.test_ce_guard_reachability",
            "tests.scripts.test_skill_index_parity",
            "tests.scripts.test_chaos_engine_ui_delivery",
        ),
    ),
    # #6222: modules that only ran in the weekly acceptance run, each selected
    # by the paths it actually reads, so drift surfaces on the PR that causes it.
    "user-harness-sync-contract": Check(
        "user-harness-sync-contract",
        "user-harness",
        ("tests.scripts.test_sync_user_harness",),
    ),
    "portable-core-contract": Check(
        "portable-core-contract",
        "portable-core",
        ("tests.scripts.test_chaos_engine_portable_core",),
    ),
    "router-contract": Check(
        "router-contract",
        "guidance",
        ("tests.scripts.test_agent_router_contract",),
    ),
    "harness-portability-contract": Check(
        "harness-portability-contract",
        "guidance",
        ("tests.scripts.test_agent_harness_portability",),
    ),
    "plugin-assembly-contract": Check(
        "plugin-assembly-contract",
        "plugin-assembly",
        ("tests.scripts.test_assemble_chaos_engine_plugin",),
    ),
    "research-matrix-contract": Check(
        "research-matrix-contract",
        "research",
        ("tests.scripts.test_chaos_engine_research",),
    ),
    "graphify-maintenance-contract": Check(
        "graphify-maintenance-contract",
        "retrieval",
        ("tests.scripts.test_graphify_maintenance",),
    ),
    "installer-ux-contract": Check(
        "installer-ux-contract",
        "installer",
        (
            "tests.scripts.test_chaos_engine_installer_ux",
            "tests.scripts.test_chaos_engine_managed_runtimes",
            "tests.scripts.test_chaos_engine_install_wrappers",
            # Issue #6186: the ubuntu fresh-installer job no longer runs these
            # itself; this Linux check is their PR-time home.
            "tests.scripts.test_chaos_engine_same_commit_payload_heal_5839",
            "tests.scripts.test_chaos_engine_overlay_only_host_pointers",
        ),
    ),
}

SURFACE_CHECKS = {
    "kernel": ("kernel-contract",),
    "lifecycle": ("lifecycle-contract", "eval-parity-contract", "protected-security"),
    "hosts": ("host-contract",),
    "guidance": (
        "guidance-contract",
        "skill-contract",
        "guidance-reachability-contract",
        "router-contract",
        "harness-portability-contract",
    ),
    "plugins": ("plugin-contract", "plugin-quality-contract"),
    "retrieval": ("retrieval-contract", "graph-resolver-contract", "graphify-maintenance-contract"),
    "user-harness": ("user-harness-sync-contract",),
    "portable-core": ("portable-core-contract",),
    "plugin-assembly": ("plugin-assembly-contract",),
    "research": ("research-matrix-contract",),
    "ci": ("ci-contract", "setup-aggregator-contract"),
    "installer": (
        "protected-installer-acceptance",
        "protected-rollback",
        "installer-ux-contract",
    ),
    "documentation": ("documentation-inventory-contract",),
    "accessibility": ("accessibility-contract",),
    "identities": ("identity-contract", "identity-recovery-contract"),
    "promotion": ("promotion-contract",),
    "fallback": ("fallback-contract",),
    "javadoc": ("javadoc-param-arity-contract",),
    "token-waste": ("token-waste-contract",),
    "token-economy": ("token-economy-contract",),
}

DEPENDENCY_CLOSURE_PATHS = frozenset({"chaos-engine/dependencies.json"})

SURFACE_PATTERNS = {
    "kernel": (
        "chaos-engine/hooks/kernel.py",
        "tests/scripts/test_chaos_engine_kernel.py",
    ),
    "installer": (
        "chaos-engine/bootstrap.py",
        "chaos-engine/dependencies.json",
        "chaos-engine/dependencies.py",
        "chaos-engine/distributions.json",
        "chaos-engine/install.py",
        "chaos-engine/install.sh",
        "chaos-engine/install.ps1",
        "chaos-engine/profiles/*/profile.json",
        "chaos-engine/vendor/*/PIN.json",
        "chaos-engine/vendor/*/hooks/*.json",
        "chaos-engine/vendor/*/src/hooks/package.json",
        "scripts/ci/chaos_engine_live_installer_acceptance.py",
        "tests/scripts/test_chaos_engine_bootstrap.py",
        "tests/scripts/test_chaos_engine_dependencies.py",
        "tests/scripts/test_chaos_engine_generation_runtime.py",
        "tests/scripts/test_chaos_engine_installer.py",
        "tests/scripts/test_chaos_engine_installer_ux.py",
        "tests/scripts/test_chaos_engine_install_wrappers.py",
        "tests/scripts/test_chaos_engine_live_installer_acceptance.py",
        "tests/scripts/test_chaos_engine_managed_runtimes.py",
        # Issue #6186: every pr-gate `chaos_installer` path selects this
        # surface, because it replaced the ubuntu fresh-installer UX step.
        "chaos-engine/hosts.py",
        "tests/scripts/test_chaos_engine_same_commit_payload_heal_5839.py",
        "tests/scripts/test_chaos_engine_overlay_only_host_pointers.py",
    ),
    "hosts": (
        "chaos-engine/hosts.py",
        ".claude/settings.json",
        ".codex/hooks.json",
        ".github/hooks/*.json",
        "tests/scripts/test_chaos_engine_hosts.py",
    ),
    "lifecycle": (
        "chaos-engine/hooks/*",
        "chaos-engine/evals/*",
        "scripts/agents/guard.py",
        "scripts/ci/chaos_engine_eval_parity.py",
        "tests/scripts/test_chaos_engine_hook.py",
        "tests/scripts/test_chaos_engine_eval_parity_fixtures.py",
        "tests/scripts/test_guard*.py",
    ),
    "guidance": (
        "AGENTS.md",
        "CLAUDE.md",
        ".agents/*",
        ".claude/agents/*",
        ".claude/skills/*",
        ".codex/agents/*",
        "chaos-engine/skills/*",
        "chaos-engine/references/*",
        "chaos-engine/profiles/*",
        "chaos-engine/vendor/*",
        "tests/scripts/test_agent_router_contract.py",
        "tests/scripts/test_agent_harness_portability.py",
        "tests/scripts/test_agent_harness_reachability.py",
        "tests/scripts/test_omniroute*.py",
        "tests/scripts/test_validate_agent_guidance.py",
        "tests/scripts/test_validate_skills.py",
        "scripts/ci/agent_guidance_budget.json",
        "tests/scripts/test_chaos_engine_self_improve.py",
        "tests/scripts/test_chaos_engine_host_parity_matrix.py",
    ),
    "plugins": (
        "agent-plugins/*",
        "shaft-skills/*",
        ".claude-plugin/*",
        ".github/skills/*",
        "scripts/ci/*agent_plugin*",
        "scripts/ci/shaft_skill_*",
        "tests/scripts/test_*plugin*.py",
        "tests/scripts/test_shaft_skill*.py",
    ),
    "retrieval": (
        ".memory/*",
        ".mcp.json",
        "mempalace.yaml",
        "tools/repository-map/*",
        "tools/agent-infra/shaft_knowledge_refresh.py",
        "scripts/agents/knowledge_stores.py",
        "chaos-engine/stores.py",
        "tests/scripts/test_chaos_engine_stores.py",
        "tests/scripts/test_knowledge_stores.py",
        "tests/scripts/test_resolve_*.py",
        "tests/scripts/test_shaft_knowledge_refresh.py",
        "tests/scripts/test_graphify_maintenance.py",
    ),
    "ci": (
        ".github/workflows/pr-gate.yml",
        ".github/workflows/agent-plugin-acceptance.yml",
        ".github/workflows/README.md",
        "scripts/ci/harness_pr_gate.py",
        "scripts/ci/unittest_shard.py",
        "scripts/ci/agent_ownership.json",
        "scripts/ci/validate_agent_ownership.py",
        "scripts/ci/validate_agent_setup.py",
        "tests/scripts/test_harness_pr_gate.py",
        "tests/scripts/test_unittest_shard.py",
        "tests/scripts/test_validate_agent_ownership.py",
        "tests/scripts/test_validate_agent_setup.py",
        "tests/scripts/test_validate_workflow_timeouts.py",
    ),
    "documentation": (
        "chaos-engine/README.md",
        "scripts/ci/validate_chaos_engine_readme.py",
        "tests/scripts/test_validate_chaos_engine_readme.py",
    ),
    "accessibility": (
        "scripts/ci/accessibility_quality_gates.py",
        "tests/fixtures/accessibility_quality_gates_sample.json",
        "tests/scripts/test_accessibility_quality_gates.py",
    ),
    "identities": (
        "chaos-engine/**",
    ),
    "promotion": (
        "scripts/ci/chaos_engine_promotion*.py",
        "tests/scripts/test_chaos_engine_promotion.py",
    ),
    "token-waste": (
        "chaos-engine/references/ci-status-economy.md",
        "chaos-engine/references/tip-churn-preflight.md",
        "chaos-engine/references/codacy-action-required-gate.md",
        "chaos-engine/references/codacy-complexity-gate.md",
        "chaos-engine/references/eliminate-waste.md",
        "chaos-engine/references/context-economy.md",
        "chaos-engine/references/work-github-playbook.md",
        "chaos-engine/references/process-owner-scrum-master.md",
        "chaos-engine/references/orchestrator-follow-through.md",
        "chaos-engine/skills/local-agency/references/coach-loop.md",
        "chaos-engine/skills/local-agency/references/when-to-use-local.md",
        "chaos-engine/skills/local-agency/references/parent-rog-shell.md",
        "chaos-engine/skills/local-agency/SKILL.md",
        "chaos-engine/guides/local-agency.md",
        "chaos-engine/skills/local-agency/scripts/tip_preflight.py",
        "chaos-engine/skills/local-agency/scripts/executor_brief.py",
        "chaos-engine/skills/self-improve/references/activation.md",
        "scripts/agents/status_lease.py",
        "scripts/agents/watch_pr_checks.py",
        "scripts/ci/overlay_pre_push.py",
        "tests/scripts/test_chaos_engine_token_waste_6161.py",
        "tests/scripts/test_overlay_pre_push.py",
    ),
    "token-economy": (
        "chaos-engine/harness_index.py",
        "chaos-engine/harness-index.json",
        "chaos-engine/worktree_overlay.py",
        "chaos-engine/mcp_policy.py",
        "chaos-engine/identity.md",
        "chaos-engine/hooks/retrieve_justification.py",
        "chaos-engine/hooks/guard.py",
        "chaos-engine/hooks/lifecycle.py",
        "chaos-engine/references/*.md",
        "chaos-engine/skills/*/SKILL.md",
        "chaos-engine/skills/*/references/*.md",
        "chaos-engine/profiles/*/profile.json",
        "chaos-engine/dependencies.json",
        "chaos-engine/hosts.py",
        "chaos-engine/install.py",
        "chaos-engine/tool.py",
        "AGENTS.md",
        "CLAUDE.md",
        "GEMINI.md",
        "scripts/agents/session_worktree.py",
        "scripts/agents/watch_pr_checks.py",
        "scripts/ci/harness_pr_gate.py",
        "scripts/ci/overlay_pre_push.py",
        "scripts/ci/validate_agent_guidance.py",
        "scripts/ci/validate_skills.py",
        "scripts/ci/agent_guidance_budget.json",
        "scripts/ci/agent_harness_parity.json",
        "tests/scripts/ce_installed_fixture.py",
        "tests/scripts/ce_host_files.py",
        "tests/scripts/test_chaos_engine_portable_core.py",
        "tests/scripts/test_chaos_engine_token_economy_6173.py",
        "tests/scripts/test_retrieve_gate_scope.py",
        "tests/scripts/test_ce_guard_reachability.py",
        "tests/scripts/test_skill_index_parity.py",
        "tests/scripts/test_chaos_engine_ui_delivery.py",
    ),
    # #6222: inputs of the formerly weekly-only modules. Each is a cheap
    # (< 5 s) single-module check, so broad content globs are affordable.
    "user-harness": (
        "scripts/agents/sync_user_harness.py",
        "scripts/agents/user_harness_retired_manifest.json",
        "scripts/agents/user-harness/*",
        ".claude/user-harness/*",
        "tests/scripts/test_sync_user_harness.py",
    ),
    "portable-core": (
        "chaos-engine/*",
        "AGENTS.md",
        ".memory/memory/*",
        "scripts/ci/agent_guidance_budget.json",
        "tests/scripts/test_chaos_engine_portable_core.py",
    ),
    "plugin-assembly": (
        "chaos-engine/*",
        "agent-plugins/*",
        # The zipapp runtime sources (RUNTIME_SOURCES), listed exactly so a
        # new scripts/agents file still reaches the unknown-path fallback.
        "scripts/agents/chaos_engine_cli.py",
        "scripts/agents/delivery_status.py",
        "scripts/agents/github_client.py",
        "scripts/agents/issue_filing.py",
        "scripts/agents/planning_contract.py",
        "scripts/agents/pr_audit.py",
        "scripts/agents/repository_context.py",
        "scripts/agents/status_lease.py",
        "scripts/agents/watch_pr_checks.py",
        "scripts/ci/assemble_chaos_engine_plugin.py",
        "tests/scripts/test_assemble_chaos_engine_plugin.py",
    ),
    "research": (
        "chaos-engine/RESEARCH.md",
        "agent-plugins/release.json",
        ".github/workflows/mavenCentral_cd.yml",
        "tests/scripts/test_chaos_engine_research.py",
    ),
    "javadoc": (
        "scripts/ci/check_javadoc_param_arity.py",
        "chaos-engine/references/zero-llm-catalog.md",
        "chaos-engine/profiles/shaft/references/playbooks/framework-source.md",
        "tests/scripts/test_check_javadoc_param_arity.py",
        "tests/scripts/test_chaos_engine_zero_llm_catalog.py",
        "shaft-engine/src/main/java/com/shaft/gui/element/internal/interaction/*",
    ),
}

HARNESS_PREFIXES = (
    ".agents/",
    ".claude/",
    ".codex/",
    ".github/instructions/",
    ".github/hooks/",
    ".github/skills/",
    ".github/workflows/",
    ".memory/",
    "agent-plugins/",
    "chaos-engine/",
    "scripts/agents/",
    "shaft-skills/",
    "tools/agent-infra/",
    "tools/repository-map/",
)
HARNESS_FILES = frozenset(
    {
        "AGENTS.md",
        "CLAUDE.md",
        ".mcp.json",
        "mempalace.yaml",
        ".github/copilot-instructions.md",
        "scripts/ci/external_guardrail_corpus.py",
        "scripts/ci/external_guardrail_corpus.json",
        "scripts/ci/local_gate.py",
        "scripts/ci/build_retry.sh",
        "scripts/ci/extract_allure_failures.py",
        "scripts/ci/watch_pr_checks.py",
        "scripts/ci/worktree_hygiene.py",
        "tests/scripts/test_build_retry.py",
        "tests/scripts/test_extract_allure_failures.py",
        "tests/scripts/test_intellij_recording_powershell.py",
        "tests/scripts/test_repository_context.py",
        "tests/scripts/test_watch_pr_checks.py",
        "tests/scripts/test_worktree_hygiene.py",
        "tests/scripts/test_sync_user_harness.py",
        "tests/scripts/test_graphify_maintenance.py",
        "tests/scripts/test_shaft_knowledge_refresh.py",
    }
)
HARNESS_PATTERNS = (
    "scripts/ci/*agent*",
    "scripts/ci/*skill*",
    "scripts/ci/agnix*",
    "tests/scripts/test_agent*.py",
    "tests/scripts/test_guard*.py",
    "tests/scripts/test_chaos_engine*.py",
    "tests/scripts/test_omniroute*.py",
    "tools/intellij-plugin-recording/*",
)


def classify_paths(paths: list[str]) -> GatePlan:
    selected: list[str] = []
    unknown: list[str] = []
    dependency_closure = False
    for raw_path in paths:
        path = raw_path.replace("\\", "/")
        while path.startswith("./"):
            path = path[2:]
        parts = PurePosixPath(path).parts
        if (
            not path
            or path.startswith("/")
            or "\0" in path
            or ".." in parts
            or (parts and parts[0].endswith(":"))
        ):
            raise GateError(f"unsafe changed path: {raw_path!r}")
        matched = False
        for surface, patterns in SURFACE_PATTERNS.items():
            if surface == "lifecycle" and path == "chaos-engine/hooks/kernel.py":
                continue
            if any(fnmatch.fnmatchcase(path, pattern) for pattern in patterns):
                if surface not in selected:
                    selected.append(surface)
                matched = True
        if path in DEPENDENCY_CLOSURE_PATHS:
            dependency_closure = True
            if "documentation" not in selected:
                selected.append("documentation")
        harness_path = (
            path in HARNESS_FILES
            or path.startswith(HARNESS_PREFIXES)
            or any(fnmatch.fnmatchcase(path, pattern) for pattern in HARNESS_PATTERNS)
        )
        if harness_path and not matched:
            unknown.append(path)
    if unknown and "fallback" not in selected:
        selected.append("fallback")
    if not selected:
        return GatePlan()

    check_ids = [check_id for surface in selected for check_id in SURFACE_CHECKS[surface]]
    if dependency_closure:
        check_ids.append("dependency-account-contract")
    for protected_id in ("protected-ownership", "protected-secret-safety"):
        if protected_id not in check_ids:
            check_ids.append(protected_id)
    return GatePlan(
        tuple(selected),
        (
            *tuple(CHECKS[check_id] for check_id in dict.fromkeys(check_ids)),
        ),
        tuple(sorted(set(unknown))),
    )


def write_generated_artifacts(root: Path, plan: GatePlan) -> tuple[str, ...]:
    """Refresh owned generated artifacts for the selected changed-closure surfaces."""
    written: list[str] = []
    root = root.resolve()
    if "documentation" in plan.surfaces:
        from scripts.ci.validate_chaos_engine_readme import write_generated as write_readme

        readme = root / "chaos-engine/README.md"
        before = readme.read_bytes() if readme.is_file() else b""
        write_readme(root)
        if readme.read_bytes() != before:
            written.append("chaos-engine/README.md")
    if "identities" in plan.surfaces:
        from scripts.ci.chaos_gauge.validate_experiment import write_generated as write_identities

        targets = (
            root / "scripts/ci/chaos_gauge/experiment.json",
            root / "scripts/ci/chaos_gauge/job-configs/chaos-engine.yaml",
            root / "scripts/ci/chaos_gauge/job-configs/full-pilot-chaos-engine.yaml",
        )
        before = {path: path.read_bytes() for path in targets if path.is_file()}
        write_identities(root)
        for path, payload in before.items():
            if path.read_bytes() != payload:
                written.append(path.relative_to(root).as_posix())
    return tuple(dict.fromkeys(written))


def _waiver_payload(body: str) -> dict[str, object] | None:
    matches = WAIVER_FENCE.findall(body)
    if not matches:
        if "chaos-engine-waiver" in body:
            raise GateError("waiver fence is malformed")
        return None
    if len(matches) != 1:
        raise GateError("exactly one waiver receipt is allowed")
    try:
        payload = json.loads(matches[0])
    except json.JSONDecodeError as error:
        raise GateError(f"waiver JSON is malformed: {error.msg}") from error
    if not isinstance(payload, dict):
        raise GateError("waiver must be a JSON object")
    required = {
        "schema",
        "allowed_check_ids",
        "expires_at",
        "rationale",
        "replacement_proof",
    }
    if set(payload) != required:
        raise GateError("waiver fields must exactly match schema")
    if payload["schema"] != 1:
        raise GateError("unsupported waiver schema")
    return payload


def _waiver_check_ids(payload: dict[str, object]) -> tuple[str, ...]:
    check_ids = payload["allowed_check_ids"]
    if (
        not isinstance(check_ids, list)
        or not check_ids
        or len(check_ids) > 8
        or any(not isinstance(item, str) or not item for item in check_ids)
        or len(check_ids) != len(set(check_ids))
    ):
        raise GateError("waiver allowed_check_ids must be 1-8 unique exact IDs")
    if any("*" in item for item in check_ids):
        raise GateError("blanket waiver IDs are forbidden")
    if any(item in PROTECTED_IDS or item.startswith("protected-") for item in check_ids):
        raise GateError("protected checks cannot be waived")
    waivable = {check_id for check_id, check in CHECKS.items() if not check.protected}
    unknown = sorted(set(check_ids) - waivable)
    if unknown:
        raise GateError("unknown waiver check IDs: " + ", ".join(unknown))
    return tuple(check_ids)


def _waiver_expiry(payload: dict[str, object], current: datetime) -> datetime:
    for field in ("rationale", "replacement_proof"):
        value = payload[field]
        if not isinstance(value, str) or not value.strip():
            raise GateError(f"waiver {field} must not be blank")
    try:
        expiry = datetime.fromisoformat(str(payload["expires_at"]).replace("Z", "+00:00"))
    except ValueError as error:
        raise GateError("waiver expires_at must be ISO-8601") from error
    if expiry.tzinfo is None:
        raise GateError("waiver expires_at must include a timezone")
    expiry = expiry.astimezone(timezone.utc)
    current = current.astimezone(timezone.utc)
    if expiry <= current:
        raise GateError("waiver has expired")
    if expiry > current + timedelta(days=14):
        raise GateError("waiver expiry exceeds 14 days")
    return expiry


def parse_waiver(
    body: str, *, now: datetime | None = None, head_sha: str = ""
) -> WaiverReceipt | None:
    payload = _waiver_payload(body)
    if payload is None:
        return None
    check_ids = _waiver_check_ids(payload)
    expiry = _waiver_expiry(payload, now or datetime.now(timezone.utc))
    return WaiverReceipt(head_sha, check_ids, expiry)


def _required_executable(name: str) -> str:
    resolved = shutil.which(name)
    if resolved is None:
        raise GateError(f"required executable is unavailable: {name}")
    return resolved


def render_json(plan: GatePlan, *, head_sha: str, budget_seconds: int) -> str:
    return json.dumps(
        {
            "schema": 1,
            "valid": True,
            "head_sha": head_sha,
            "surfaces": list(plan.surfaces),
            "unknown_paths": list(plan.unknown_paths),
            "checks": [
                {
                    "id": check.id,
                    "surface": check.surface,
                    "protected": check.protected,
                    "class": "blocking-protected-invariant" if check.protected else "change-scoped",
                    "tests": list(check.modules),
                    "reproduction_command": check.reproduction_command,
                }
                for check in plan.checks
            ],
            "timing": {
                "budget_seconds": budget_seconds,
                "elapsed_seconds": 0.0,
                "recorded_baseline_median_seconds": RECORDED_BASELINE_MEDIAN_SECONDS,
                "maximum_budget_reduction": round(
                    1 - budget_seconds / RECORDED_BASELINE_MEDIAN_SECONDS, 3
                ),
            },
            "deferred_classes": ["scheduled-exhaustive", "release-promotion"],
            "safe_history_update_command": "git push --force-with-lease origin HEAD",
        },
        indent=2,
        sort_keys=True,
    )


def changed_paths(root: Path, base: str, head: str) -> list[ChangedPath]:
    completed = subprocess.run(  # nosec B603 - fixed read-only git invocation.
        [
            _required_executable("git"),
            "diff",
            "--merge-base",
            "--diff-filter=ACDMRT",
            "--name-status",
            "-z",
            base,
            head,
        ],
        cwd=root,
        capture_output=True,
        text=True,
        timeout=30,
        check=False,
    )
    if completed.returncode:
        raise GateError("git could not resolve changed paths")
    tokens = [token for token in completed.stdout.split("\0") if token]
    paths: list[ChangedPath] = []
    index = 0
    while index < len(tokens):
        status = tokens[index]
        index += 1
        kind = status[:1]
        if kind not in {"A", "C", "D", "M", "R", "T"}:
            raise GateError(f"git returned an unsupported change status: {status!r}")
        required_paths = 2 if kind in {"C", "R"} else 1
        if index + required_paths > len(tokens):
            raise GateError("git returned malformed changed-path data")
        status_paths = tokens[index : index + required_paths]
        index += required_paths
        if kind in {"C", "R"}:
            paths.append(ChangedPath(status_paths[0], executable=False))
            paths.append(ChangedPath(status_paths[1], executable=True))
        else:
            path = status_paths[0]
            executable = kind in {"A", "M"} or (
                kind == "T" and (root / path).is_file()
            )
            paths.append(ChangedPath(path, executable=executable))
    return paths


def event_waiver(
    reviews_path: Path | None,
    expected_head: str,
    *,
    now: datetime | None = None,
) -> WaiverReceipt | None:
    """Load one owner-authored, submitted review receipt for the exact PR head."""
    if reviews_path is None:
        return None
    try:
        reviews = json.loads(reviews_path.read_text(encoding="utf-8"))
    except (OSError, json.JSONDecodeError) as error:
        raise GateError("GitHub review JSON is malformed") from error
    if not isinstance(reviews, list):
        raise GateError("GitHub review JSON must be an array")
    receipts: list[WaiverReceipt] = []
    for review in reviews:
        if not isinstance(review, dict):
            raise GateError("GitHub review JSON contains a malformed review")
        user = review.get("user")
        author = user.get("login") if isinstance(user, dict) else None
        if (
            author != WAIVER_OWNER
            or review.get("commit_id") != expected_head
            or review.get("state") not in {"APPROVED", "COMMENTED"}
            or not review.get("submitted_at")
            or review.get("last_edited_at") is not None
        ):
            continue
        body = review.get("body") or ""
        receipt = parse_waiver(str(body), now=now, head_sha=expected_head)
        if receipt is not None:
            receipts.append(receipt)
    if len(receipts) > 1:
        raise GateError("exactly one owner waiver review is allowed for this head")
    return receipts[0] if receipts else None


def _failure_tail(stream, root: Path, limit: int = 4096) -> str:
    """Return a bounded redacted tail for one failed check."""
    stream.flush()
    size = stream.tell()
    stream.seek(max(0, size - limit * 2))
    text = stream.read(limit * 2).decode("utf-8", errors="replace")
    for value in {str(root), str(Path.home()), tempfile.gettempdir()}:
        if value:
            text = text.replace(value, "<path>")
    for name, value in os.environ.items():
        if re.search(r"TOKEN|SECRET|PASSWORD|API_KEY|PRIVATE_KEY", name, re.I) and len(value) >= 4:
            text = text.replace(value, "<redacted>")
    text = re.sub(r"(https?://)[^\s/@]+@", r"\1<redacted>@", text, flags=re.I)
    text = re.sub(
        r"(?i)(token|secret|password|api[_-]?key|private[_-]?key)(\s*[:=]\s*)\S+",
        r"\1\2<redacted>",
        text,
    )
    return text[-limit:].strip()


def _emit_failure_tail(stream, root: Path) -> None:
    detail = _failure_tail(stream, root)
    if detail:
        print(f"harness-pr-gate bounded failure tail:\n{detail}", file=sys.stderr)


def _isolated_environment(temp_root: str) -> dict[str, str]:
    """Give one check its own temp directory so concurrent checks never share it."""
    return {**os.environ, "TMPDIR": temp_root, "TEMP": temp_root, "TMP": temp_root}


def _run_check(command: list[str], root: Path, timeout: float) -> tuple[str, int | None]:
    """Run one quiet check, exposing only a bounded redacted tail on failure."""
    with tempfile.TemporaryDirectory(prefix="harness-gate-") as temp_root:
        return _run_isolated_check(command, root, timeout, _isolated_environment(temp_root))


def _run_isolated_check(
    command: list[str], root: Path, timeout: float, env: dict[str, str]
) -> tuple[str, int | None]:
    windows = os.name == "nt"
    with tempfile.TemporaryFile() as output:
        process = subprocess.Popen(  # nosec B603 - fixed unittest command without a shell.
            command,
            cwd=root,
            env=env,
            stdout=output,
            stderr=subprocess.STDOUT,
            start_new_session=not windows,
            creationflags=subprocess.CREATE_NEW_PROCESS_GROUP if windows else 0,
        )
        try:
            exit_code = process.wait(timeout=timeout)
            if exit_code:
                _emit_failure_tail(output, root)
            return ("passed" if exit_code == 0 else "failed"), exit_code
        except subprocess.TimeoutExpired:
            if windows:
                subprocess.run(  # nosec B603 - fixed Windows process-tree termination.
                    [_required_executable("taskkill"), "/PID", str(process.pid), "/T", "/F"],
                    stdout=subprocess.DEVNULL,
                    stderr=subprocess.DEVNULL,
                    timeout=10,
                    check=False,
                )
            else:
                try:
                    os.killpg(process.pid, signal.SIGKILL)
                except ProcessLookupError:
                    # The process group exited after the timeout observation.
                    pass
            try:
                process.wait(timeout=10)
            except subprocess.TimeoutExpired:
                process.kill()
                process.wait()
            _emit_failure_tail(output, root)
            return "timeout", None


def resolve_jobs(requested: int) -> int:
    """Return the worker count: an explicit positive value, or one per CPU for 0."""
    if requested < 0:
        raise GateError("jobs must be zero (one per CPU) or a positive count")
    return requested or max(1, os.cpu_count() or 1)


Unit = tuple[str, ...]


def execution_units(modules: tuple[str, ...]) -> list[Unit]:
    """Split one check's module tuple into runnable units (#6207).

    Unsharded modules stay together as one unit; every sharded module becomes
    ``(module, "--shard", "i/n")`` units that run in their own process.
    """
    plain = tuple(module for module in modules if module not in SHARDED_MODULES)
    units: list[Unit] = [plain] if plain else []
    for module in modules:
        total = SHARDED_MODULES.get(module)
        if total:
            units.extend((module, "--shard", f"{index}/{total}") for index in range(1, total + 1))
    return units


def unit_command(unit: Unit) -> list[str]:
    """Build the process argv for one unit."""
    if len(unit) == 3 and unit[1] == "--shard":
        return [sys.executable, "-m", "scripts.ci.unittest_shard", *unit, "-v"]
    return [sys.executable, "-m", "unittest", *unit, "-v"]


def _execution_order(plan: GatePlan) -> list[Unit]:
    """Unique units: protected always-on checks, then the longest suites, then the rest."""
    first = (*PRIORITY_CHECK_IDS, *LONG_RUNNING_CHECK_IDS)
    ordered_checks = [
        *(check for pid in first for check in plan.checks if check.id == pid),
        *plan.checks,
    ]
    return list(
        dict.fromkeys(unit for check in ordered_checks for unit in execution_units(check.modules))
    )


def _combine(outcomes: list[tuple[str, int | None, float]]) -> tuple[str, int | None, float]:
    """One check's outcome from its units: timeout beats failure beats pass."""
    duration = round(sum(outcome[2] for outcome in outcomes), 3)
    statuses = [outcome[0] for outcome in outcomes]
    if "timeout" in statuses:
        return "timeout", None, duration
    exit_code = next((outcome[1] for outcome in outcomes if outcome[1]), 0)
    return ("passed" if all(status == "passed" for status in statuses) else "failed"), exit_code, duration


def _result_record(
    check: Check,
    outcome: tuple[str, int | None, float],
    waiver: WaiverReceipt | None,
) -> dict[str, Any]:
    status, exit_code, duration = outcome
    if status == "failed" and waiver and check.id in waiver.check_ids and not check.protected:
        status = "waived"
    if status == "failed" and check.known_drift and not check.protected:
        status = "known-drift"
    record_class = "blocking-protected-invariant" if check.protected else "change-scoped"
    if check.known_drift:
        record_class = "known-drift-advisory"
    return {
        "id": check.id,
        "surface": check.surface,
        "protected": check.protected,
        "class": record_class,
        "known_drift": check.known_drift,
        "promote": bool(check.known_drift) and status == "passed",
        "tests": list(check.modules),
        "status": status,
        "exit_code": exit_code,
        "duration_seconds": duration,
        "reproduction_command": check.reproduction_command,
    }


def run_plan(
    root: Path,
    plan: GatePlan,
    *,
    head_sha: str,
    budget_seconds: int,
    waiver: WaiverReceipt | None,
    jobs: int = 1,
) -> tuple[dict[str, Any], int]:
    """
    Run every selected check, concurrently when ``jobs`` > 1 (#6191).

    Each unique module tuple runs once (identical tuples share one execution),
    protected always-on checks are submitted first, and every check gets the
    budget that remains when it actually starts.
    """
    selected_ids = {check.id for check in plan.checks}
    if waiver and not set(waiver.check_ids) <= selected_ids:
        raise GateError("waiver names checks not selected by this change")
    started = time.monotonic()

    def _execute(unit: Unit) -> tuple[str, int | None, float]:
        check_started = time.monotonic()
        remaining = budget_seconds - (check_started - started)
        if remaining <= 0:
            return "timeout", None, 0.0
        status, exit_code = _run_check(unit_command(unit), root, remaining)
        return status, exit_code, round(time.monotonic() - check_started, 3)

    order = _execution_order(plan)
    workers = max(1, min(jobs, len(order)))
    if workers == 1:
        unit_outcomes = {unit: _execute(unit) for unit in order}
    else:
        with ThreadPoolExecutor(max_workers=workers) as pool:
            futures = {unit: pool.submit(_execute, unit) for unit in order}
            unit_outcomes = {unit: future.result() for unit, future in futures.items()}
    outcomes = {
        check.id: _combine([unit_outcomes[unit] for unit in execution_units(check.modules)])
        for check in plan.checks
    }

    results = [_result_record(check, outcomes[check.id], waiver) for check in plan.checks]
    failed_ids = {check.id for check in plan.checks if outcomes[check.id][0] != "passed"}
    if waiver and set(waiver.check_ids) != failed_ids.intersection(waiver.check_ids):
        raise GateError("waiver is stale because a named check did not fail")
    valid = all(result["status"] in {"passed", "waived", "known-drift"} for result in results)
    payload = {
        "schema": 1,
        "valid": valid,
        "head_sha": head_sha,
        "surfaces": list(plan.surfaces),
        "unknown_paths": list(plan.unknown_paths),
        "checks": results,
        "waiver": {
            "applied_check_ids": sorted(
                result["id"] for result in results if result["status"] == "waived"
            )
        },
        "timing": {
            "budget_seconds": budget_seconds,
            "elapsed_seconds": round(time.monotonic() - started, 3),
            "recorded_baseline_median_seconds": RECORDED_BASELINE_MEDIAN_SECONDS,
            "maximum_budget_reduction": round(
                1 - budget_seconds / RECORDED_BASELINE_MEDIAN_SECONDS, 3
            ),
            "jobs": workers,
            "units": len(order),
            "sum_check_seconds": round(sum(outcome[2] for outcome in unit_outcomes.values()), 3),
        },
        "deferred_classes": ["scheduled-exhaustive", "release-promotion"],
        "safe_history_update_command": "git push --force-with-lease origin HEAD",
    }
    return payload, 0 if valid else 1


def render_text(payload: dict[str, Any]) -> str:
    """Render run and --plan-only payloads; planned checks have no status yet (#6194)."""
    surfaces = ",".join(payload.get("surfaces", [])) or "none"
    timing = payload.get("timing", {})
    lines = [
        f"harness-pr-gate valid={str(payload.get('valid', True)).lower()} surfaces={surfaces} "
        f"elapsed={timing.get('elapsed_seconds', 0)}s/{timing.get('budget_seconds', 0)}s"
    ]
    lines.extend(
        f"{item['id']} known-drift={item['known_drift']}: now passes; promote it by clearing known_drift"
        for item in payload.get("checks", [])
        if item.get("promote")
    )
    lines.extend(
        f"{item['id']} status={item.get('status', 'planned')} "
        f"protected={str(item.get('protected', False)).lower()} "
        f"tests={','.join(item.get('tests', []))} reproduce={item.get('reproduction_command', '')}"
        for item in payload.get("checks", [])
    )
    return "\n".join(lines)


def build_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--root", type=Path, default=Path(__file__).resolve().parents[2])
    parser.add_argument("--base", required=True)
    parser.add_argument("--head", required=True)
    parser.add_argument("--reviews", type=Path)
    parser.add_argument("--budget-seconds", type=int, default=PR_BUDGET_SECONDS)
    parser.add_argument(
        "--jobs",
        type=int,
        default=0,
        help="concurrent checks: 0 = one per CPU (default), 1 = sequential escape hatch",
    )
    parser.add_argument("--output", type=Path)
    parser.add_argument("--format", choices=("text", "json"), default="text")
    parser.add_argument("--plan-only", action="store_true")
    parser.add_argument(
        "--write-generated",
        action="store_true",
        help="refresh owned generated inventory and identity artifacts before validation",
    )
    return parser


def main() -> int:
    args = build_parser().parse_args()
    try:
        if args.budget_seconds < 1 or args.budget_seconds > 1200:
            raise GateError("budget must be between 1 and 1200 seconds")
        if not re.fullmatch(r"[0-9a-f]{40}", args.head):
            raise GateError("head must be a full lowercase SHA")
        if args.plan_only and args.write_generated:
            raise GateError("plan-only cannot write generated artifacts")
        root = args.root.resolve()
        plan = classify_paths(changed_paths(root, args.base, args.head))
        written = write_generated_artifacts(root, plan) if args.write_generated else ()
        if args.plan_only:
            payload = json.loads(
                render_json(plan, head_sha=args.head, budget_seconds=args.budget_seconds)
            )
            payload["written"] = list(written)
            exit_code = 0
        else:
            waiver = event_waiver(args.reviews, args.head)
            payload, exit_code = run_plan(
                root,
                plan,
                head_sha=args.head,
                budget_seconds=args.budget_seconds,
                waiver=waiver,
                jobs=resolve_jobs(args.jobs),
            )
            payload["written"] = list(written)
    except GateError as error:
        payload = {"schema": 1, "valid": False, "error": str(error)}
        exit_code = 2
    serialized = json.dumps(payload, indent=2, sort_keys=True)
    if args.output:
        args.output.write_text(serialized + "\n", encoding="utf-8")
    print(serialized if args.format == "json" else render_text(payload) if "checks" in payload else serialized)
    return exit_code


if __name__ == "__main__":
    raise SystemExit(main())
