"""#6173 wave 2 contracts: lean bootstrap (C), CE always loaded (E), install
hygiene (F), overlay cleanup (G), and the #6194-#6197 harness bugs."""

from __future__ import annotations

import json
import os
import shutil
import subprocess  # nosec B404 - fixed git/python invocations in temp dirs.
import sys
import tempfile
import unittest
from pathlib import Path

from tests.scripts.ce_installed_fixture import (
    ROOT,
    SOURCE,
    build_installed_project,
    load_module,
    parity_hosts,
)

BUDGET = json.loads((ROOT / "scripts/ci/agent_guidance_budget.json").read_text(encoding="utf-8"))


def git(cwd: Path, *arguments: str) -> str:
    return subprocess.run(  # nosec B603 B607 - fixed git arguments in a temp repo.
        ["git", *arguments], cwd=cwd, check=True, capture_output=True, text=True
    ).stdout.strip()


def temp_repo(path: Path) -> Path:
    path.mkdir(parents=True)
    git(path, "init", "-q", "-b", "main")
    git(path, "config", "user.email", "t@example.invalid")
    git(path, "config", "user.name", "t")
    (path / "AGENTS.md").write_text("# a\n", encoding="utf-8")
    git(path, "add", "AGENTS.md")
    git(path, "commit", "-q", "-m", "init")
    return path


class LeanBootstrapTest(unittest.TestCase):
    """#6176."""

    def test_router_core_card_is_small(self):
        self.assertLessEqual(len((SOURCE / "skills/chaos-engine/SKILL.md").read_bytes()), 7168)

    def test_mandated_chain_budget_holds_on_every_host(self):
        guidance = load_module("ce_c_guidance", ROOT / "scripts/ci/validate_agent_guidance.py")
        chain = BUDGET["mandated_chain"]
        self.assertEqual(set(parity_hosts()), set(chain["hosts"]))
        for host in parity_hosts():
            with self.subTest(host=host):
                self.assertLessEqual(guidance.mandated_chain_bytes(ROOT, BUDGET, host), chain["max_bytes"])
        self.assertLessEqual(chain["max_bytes"], 24576)
        self.assertEqual([], guidance.mandated_chain_errors(ROOT, BUDGET))

    def test_old_chain_would_fail_the_budget(self):
        guidance = load_module("ce_c_guidance_old", ROOT / "scripts/ci/validate_agent_guidance.py")
        errors = guidance.mandated_chain_errors(ROOT, {**BUDGET, "mandated_chain": {**BUDGET["mandated_chain"], "max_bytes": 1000}})
        self.assertTrue(errors)

    def test_delegate_card_is_small_and_complete(self):
        card = (SOURCE / "references/delegate-card.md").read_text(encoding="utf-8")
        self.assertLessEqual(len(card.encode("utf-8")), 2048)
        for phrase in ("Iron laws", "Stop on ambiguity", "Report", "one duty"):
            self.assertIn(phrase, card)

    def test_role_adapters_load_the_delegate_card_not_the_router(self):
        hosts = load_module("ce_c_hosts", SOURCE / "hosts.py")
        for relative in hosts.ROLE_ADAPTER_PATHS:
            body = hosts.role_adapter_desired(relative).decode("utf-8")
            with self.subTest(adapter=relative):
                self.assertIn(".chaos-engine/references/delegate-card.md", body)
                self.assertNotIn("skills/chaos-engine/SKILL.md", body)

    def test_pre_push_checks_skill_budgets(self):
        pre_push = load_module("ce_c_pre_push", ROOT / "scripts/ci/overlay_pre_push.py")
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            (root / "scripts/ci").mkdir(parents=True)
            shutil.copyfile(ROOT / "scripts/ci/agent_guidance_budget.json", root / "scripts/ci/agent_guidance_budget.json")
            skill = root / "chaos-engine/skills/huge/SKILL.md"
            skill.parent.mkdir(parents=True)
            skill.write_text("x" * 20001, encoding="utf-8")
            failures, warnings = pre_push.skill_budget_findings(root)
            self.assertTrue(any("huge" in item for item in failures))
            skill.write_text("x" * 19800, encoding="utf-8")
            failures, warnings = pre_push.skill_budget_findings(root)
            self.assertEqual([], failures)
            self.assertTrue(any("huge" in item for item in warnings))


class AlwaysLoadedTest(unittest.TestCase):
    """#6178."""

    def test_single_pointer(self):
        self.assertEqual("@AGENTS.md\n", (ROOT / "CLAUDE.md").read_text(encoding="utf-8"))
        agents = (ROOT / "AGENTS.md").read_text(encoding="utf-8")
        self.assertEqual(1, agents.count("skills/chaos-engine/SKILL.md"))
        hosts = load_module("ce_e_hosts", SOURCE / "hosts.py")
        self.assertEqual("@AGENTS.md\n", hosts.claude_pointer_bytes().decode("utf-8"))

    def test_gemini_loads_agents_md(self):
        gemini = (ROOT / "GEMINI.md").read_text(encoding="utf-8")
        self.assertTrue(gemini.startswith("@AGENTS.md\n"), gemini)
        self.assertNotIn("CHAOSENGINE:START", gemini)

    def test_instruction_only_hosts_are_registered(self):
        hosts = load_module("ce_e_hosts_rows", SOURCE / "hosts.py")
        self.assertEqual({"opencode", "cursor", "grok-bot"}, set(hosts.INSTRUCTION_ONLY_HOSTS))
        matrix = (SOURCE / "references/host-parity-matrix.md").read_text(encoding="utf-8")
        for token in ("OpenCode", "Cursor", "Grok Bot", "GAP-OPENCODE-HOOKS", "GAP-CURSOR-HOOKS", "GAP-GROKBOT-HOOKS"):
            self.assertIn(token, matrix)

    def test_ce_loads_from_subdirectory_session_worktree_and_fresh_clone(self):
        overlay = load_module("ce_e_overlay", SOURCE / "worktree_overlay.py")
        with tempfile.TemporaryDirectory() as temporary:
            base = Path(temporary)
            primary = temp_repo(base / "primary")
            build_installed_project(primary)
            (primary / ".claude").mkdir()
            (primary / ".claude/settings.json").write_text("{}\n", encoding="utf-8")
            report = overlay.verify_ce_loads(primary / "src/sub")
            self.assertEqual({"ok"}, set(report.values()), report)
            session = base / "primary.session-x"
            git(primary, "worktree", "add", "-q", "--detach", str(session), "HEAD")
            self.assertNotEqual({"ok"}, set(overlay.verify_ce_loads(session).values()))
            overlay.materialize(primary, session)
            self.assertEqual({"ok"}, set(overlay.verify_ce_loads(session).values()))
            self.assertTrue((session / ".claude/settings.json").exists())
            clone = base / "clone"
            git(base, "clone", "-q", str(primary), str(clone))
            fresh = overlay.verify_ce_loads(clone)
            self.assertIn("missing-overlay", set(fresh.values()))
            self.assertEqual(set(parity_hosts()), set(fresh))

    def test_installer_refuses_linked_worktree_with_one_line_reason(self):
        overlay = load_module("ce_e_overlay_reason", SOURCE / "worktree_overlay.py")
        with tempfile.TemporaryDirectory() as temporary:
            base = Path(temporary)
            primary = temp_repo(base / "primary")
            session = base / "wt"
            git(primary, "worktree", "add", "-q", "--detach", str(session), "HEAD")
            self.assertIsNone(overlay.linked_worktree_reason(primary))
            reason = overlay.linked_worktree_reason(session)
            self.assertIsNotNone(reason)
            self.assertNotIn("\n", reason)
            self.assertIn(str(primary.resolve()), reason)


class InstallHygieneTest(unittest.TestCase):
    """#6179."""

    def test_hook_documents_never_embed_the_managed_interpreter(self):
        hosts = load_module("ce_f_hosts", SOURCE / "hosts.py")
        managed = Path("/home/example/.local/share/uv/python/cpython-3.14/bin/python3.14")
        for host in ("claude", "codex", "grok", "copilot"):
            with self.subTest(host=host):
                document = hosts.lifecycle_hooks_document(host, managed_python=managed).decode("utf-8")
                self.assertNotIn(str(managed), document)

    def test_tracked_adapters_have_no_absolute_paths(self):
        tracked = git(ROOT, "ls-files", ".mcp.json", ".claude", ".codex", ".gemini", ".grok", ".github/hooks").split()
        for relative in tracked:
            with self.subTest(file=relative):
                text = (ROOT / relative).read_text(encoding="utf-8")
                self.assertNotRegex(text, r"(/home/|/Users/|[A-Za-z]:\\\\Users)")

    def test_default_mcp_catalog_has_no_cli_equivalents(self):
        policy = load_module("ce_f_mcp", SOURCE / "mcp_policy.py")
        self.assertEqual(set(), set(policy.default_mcp_catalog("portable")) & policy.CLI_EQUIVALENT_MCP_IDS)
        self.assertEqual(set(), set(policy.default_mcp_catalog("shaft")) & policy.CLI_EQUIVALENT_MCP_IDS)
        self.assertIn("maven-tools-mcp", policy.default_mcp_catalog("shaft"))
        self.assertNotIn("maven-tools-mcp", policy.default_mcp_catalog("portable"))
        self.assertTrue(policy.CLI_EQUIVALENT_MCP_IDS <= set(policy.default_mcp_catalog("portable", with_mcp=True)))

    def test_update_if_changed_is_a_noop_on_matching_bytes(self):
        install = load_module("ce_f_install", SOURCE / "install.py")
        with tempfile.TemporaryDirectory() as temporary:
            project = build_installed_project(Path(temporary) / "adopter")
            self.assertTrue(install.overlay_unchanged(SOURCE, project / ".chaos-engine"))
            (project / ".chaos-engine/tool.py").write_text("# drift\n", encoding="utf-8")
            self.assertFalse(install.overlay_unchanged(SOURCE, project / ".chaos-engine"))

    def test_ledger_only_records_existing_files(self):
        with tempfile.TemporaryDirectory() as temporary:
            project = build_installed_project(Path(temporary) / "adopter")
            gate = load_module("ce_f_gate", project / ".chaos-engine/hooks/retrieve_justification.py")
            noisy = (
                "Downloading 0.00/79.3M\n12.3M/79.3M\n"
                "home/box/.cache/chroma/onnx_models/all-MiniLM-L6-v2/onnx.tar.gz\n"
                "Source: src/Foo.java\n"
            )
            self.assertEqual(["src/Foo.java"], gate.record_citations(project, "graphify", noisy))

    def test_retrieve_is_quiet(self):
        tool = load_module("ce_f_tool", SOURCE / "tool.py")
        environment = tool.quiet_environment({})
        self.assertEqual("1", environment["TQDM_DISABLE"])
        self.assertEqual("1", environment["HF_HUB_DISABLE_PROGRESS_BARS"])

    def test_installer_seeds_graphify_ignore(self):
        install = load_module("ce_f_install_ignore", SOURCE / "install.py")
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary)
            install.seed_graphify_ignore(project)
            lines = (project / ".graphifyignore").read_text(encoding="utf-8").splitlines()
            for entry in (".chaos-engine/", "plugins/", ".claude/", ".codex/", ".memory/"):
                self.assertIn(entry, lines)
            (project / ".graphifyignore").write_text("custom/\n", encoding="utf-8")
            install.seed_graphify_ignore(project)
            self.assertIn("custom/", (project / ".graphifyignore").read_text(encoding="utf-8"))

    def test_doctor_digest_is_one_line_when_healthy(self):
        hosts = load_module("ce_f_hosts_digest", SOURCE / "hosts.py")
        healthy = hosts.doctor_digest({"status": "healthy", "checks": [{"id": "a", "status": "healthy"}]})
        self.assertEqual(1, len(healthy.splitlines()))
        broken = hosts.doctor_digest(
            {"status": "recovery-required", "checks": [{"id": "a", "status": "healthy"}, {"id": "b", "status": "recovery-required", "fixNext": "run x"}]}
        )
        self.assertIn("b", broken)
        self.assertIn("run x", broken)
        self.assertNotIn("a:", broken)

    def test_desync_notice_skips_detached_session_worktrees(self):
        tool = load_module("ce_f_tool_desync", SOURCE / "tool.py")
        with tempfile.TemporaryDirectory() as temporary:
            repo = temp_repo(Path(temporary) / "r")
            self.assertTrue(tool.desync_notice_applies(repo))
            git(repo, "checkout", "-q", "--detach")
            self.assertFalse(tool.desync_notice_applies(repo))

    def test_graphify_task_impact_matches_the_guard(self):
        install = load_module("ce_f_install_impact", SOURCE / "install.py")
        self.assertIn("graphify", install.RETRIEVE_GATE_COMPONENTS)
        policy, _ = install.load_capability_policy(SOURCE, "repository")
        self.assertEqual("required-when-indexed", policy["graphify"]["taskImpact"])


class OverlayCleanupTest(unittest.TestCase):
    """#6180."""

    ALWAYS_LOADED = ("AGENTS.md", "chaos-engine/identity.md", "chaos-engine/skills/chaos-engine/SKILL.md")

    def test_identity_line_matches_6171(self):
        identity = (SOURCE / "identity.md").read_text(encoding="utf-8")
        self.assertIn("Cloud implementers write code directly", identity)
        self.assertIn("local runtimes are optional for narrow mechanical or offline jobs", identity)

    def test_no_always_loaded_file_mandates_local_writers(self):
        for relative in self.ALWAYS_LOADED:
            text = (ROOT / relative).read_text(encoding="utf-8")
            for phrase in ("prefer a READY loopback coder", "stay orchestrator for product and overlay", "Keep long work-machine writers"):
                with self.subTest(file=relative, phrase=phrase):
                    self.assertNotIn(phrase, text)
        rog = (SOURCE / "skills/local-agency/references/parent-rog-shell.md").read_text(encoding="utf-8")
        self.assertNotIn("Keep long work-machine writers", rog)
        self.assertIn("one-shot job", rog)

    def test_delta_only_status_and_conditional_metering(self):
        text = (SOURCE / "references/process-owner-scrum-master.md").read_text(encoding="utf-8")
        self.assertIn("Delta-only status", text)
        self.assertIn("nothing changed means no message", text)
        self.assertIn("only when a local inference channel was actually used", text)

    def test_permanent_rules_live_in_ce(self):
        text = (SOURCE / "references/permanent-rules.md").read_text(encoding="utf-8")
        self.assertIn("Assistant memory keeps only this pointer", text)
        self.assertIn("no-proxy", text)


class HarnessBugsTest(unittest.TestCase):
    """#6194-#6197."""

    def test_6194_plan_only_text_format_renders(self):
        gate = load_module("ce_bug_gate", ROOT / "scripts/ci/harness_pr_gate.py")
        plan = gate.classify_paths(["chaos-engine/hooks/guard.py"])
        payload = json.loads(gate.render_json(plan, head_sha="0" * 40, budget_seconds=60))
        text = gate.render_text(payload)
        self.assertIn("status=planned", text)

    def test_6195_overlay_pre_push_runs_without_pythonpath(self):
        environment = {key: value for key, value in os.environ.items() if key != "PYTHONPATH"}
        with tempfile.TemporaryDirectory() as temporary:
            result = subprocess.run(  # nosec B603 - fixed interpreter and script.
                [sys.executable, str(ROOT / "scripts/ci/overlay_pre_push.py"), str(ROOT)],
                cwd=temporary, env=environment, capture_output=True, text=True, check=False, timeout=300,
            )
        self.assertNotIn("ModuleNotFoundError", result.stderr)

    def test_6196_unregistered_replacement_is_not_red(self):
        watch = load_module("ce_bug_watch", ROOT / "scripts/agents/watch_pr_checks.py")
        checks = [{"name": "PR Gate Summary", "state": "CANCELLED", "link": ""}, {"name": "x", "state": "SUCCESS", "link": ""}]
        self.assertEqual("PENDING", watch.classify_checks(checks, registration_grace=True)[0])
        self.assertEqual("RED", watch.classify_checks(checks)[0])

    def test_6196_default_cap_outlasts_the_slowest_required_check(self):
        watch = load_module("ce_bug_watch_cap", ROOT / "scripts/agents/watch_pr_checks.py")
        for interval in (30, 60, 180, 300):
            with self.subTest(interval=interval):
                self.assertGreaterEqual(watch.default_max_polls(interval) * interval, watch.SLOWEST_REQUIRED_CHECK_MINUTES * 60)
        self.assertGreaterEqual(watch.SLOWEST_REQUIRED_CHECK_MINUTES, 45)

    def test_6197_contract_tests_do_not_read_untracked_host_files(self):
        helper = load_module("ce_bug_host_files", ROOT / "tests/scripts/ce_host_files.py")
        for relative in (".codex/config.toml", ".claude/settings.json", ".codex/hooks.json", ".agents/skills/chaos-engine/SKILL.md"):
            with self.subTest(file=relative):
                self.assertTrue(helper.host_file_text(relative))
        offenders = [
            name
            for name in ("test_agent_router_contract", "test_guard_lifecycle", "test_guard_memory_worktree", "test_chaos_engine_portable_core")
            if "ce_host_files" not in (ROOT / f"tests/scripts/{name}.py").read_text(encoding="utf-8")
        ]
        self.assertEqual([], offenders)


if __name__ == "__main__":
    unittest.main()
