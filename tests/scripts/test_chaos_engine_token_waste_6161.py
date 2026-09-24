"""#6161 token-waste epic contracts for #6162-#6169 and #6171 (RED-first).

Each class pins one child ticket's portable ChaosEngine overlay behavior so
Codex, Claude, Grok CLI, Gemini, Copilot, and Grok Bot share one outcome.
"""

from __future__ import annotations

import importlib.util
import io
import json
import os
import sys
import tempfile
import time
import unittest
from contextlib import redirect_stderr, redirect_stdout
from pathlib import Path
from types import SimpleNamespace
from unittest import mock

ROOT = Path(__file__).resolve().parents[2]
if str(ROOT) not in sys.path:
    sys.path.insert(0, str(ROOT))

from scripts.agents import status_lease, watch_pr_checks  # noqa: E402
from scripts.ci import overlay_pre_push  # noqa: E402

REFS = ROOT / "chaos-engine/references"
PLAYBOOK = REFS / "work-github-playbook.md"
CI_ECONOMY = REFS / "ci-status-economy.md"
TIP_DOC = REFS / "tip-churn-preflight.md"
CODACY_GATE = REFS / "codacy-action-required-gate.md"
PROCESS_OWNER = REFS / "process-owner-scrum-master.md"
ORCHESTRATOR = REFS / "orchestrator-follow-through.md"
ELIMINATE = REFS / "eliminate-waste.md"
CONTEXT = REFS / "context-economy.md"
LEVEL1 = REFS / "level-1-catalog.md"
COACH = ROOT / "chaos-engine/skills/local-agency/references/coach-loop.md"
ACTIVATION = ROOT / "chaos-engine/skills/self-improve/references/activation.md"
LOCAL_AGENCY = ROOT / "chaos-engine/skills/local-agency"
LOCAL_SKILL = LOCAL_AGENCY / "SKILL.md"
LOCAL_RULE = LOCAL_AGENCY / "references/when-to-use-local.md"
LOCAL_GUIDE = ROOT / "chaos-engine/guides/local-agency.md"
MEMORY_WORKFLOW = (
    ROOT
    / ".memory/memory/workflows/"
    "repairing-memory-object-content-hash-mismatches-objectcontenthashmismatch.md"
)
SCRIPTS = ROOT / "chaos-engine/skills/local-agency/scripts"
HOSTS = ("Codex", "Claude", "Grok CLI", "Gemini", "Copilot", "Grok Bot")


def load(path: Path, name: str):
    spec = importlib.util.spec_from_file_location(name, path)
    if spec is None or spec.loader is None:
        raise RuntimeError(f"cannot load {path}")
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def text(path: Path) -> str:
    return path.read_text(encoding="utf-8")


tip = load(SCRIPTS / "tip_preflight.py", "ce_tip_preflight_6161")
brief = load(SCRIPTS / "executor_brief.py", "ce_executor_brief_6161")


class BanditB607PreflightTest(unittest.TestCase):
    """#6165: Bandit B607 is a local blocking preflight."""

    def test_bare_git_subprocess_is_red(self):
        src = 'import subprocess\nsubprocess.run(["git", "status"], check=False)  # nosec B603\n'
        findings = tip.b607_findings(src, "a.py")
        self.assertEqual(1, len(findings))
        self.assertIn("a.py:2", findings[0])
        self.assertIn("B607", findings[0])
        self.assertIn("'git'", findings[0])

    def test_resolved_or_absolute_executable_is_green(self):
        src = 'import shutil, subprocess\nGIT = shutil.which("git")\nsubprocess.run([GIT, "status"], check=False)  # nosec B603\nsubprocess.run(["/usr/bin/git", "log"], check=False)  # nosec B603\n'
        self.assertEqual([], tip.b607_findings(src, "a.py"))

    def test_nosec_b607_suppresses_but_b603_only_does_not(self):
        src = 'import subprocess\nsubprocess.run(["git"])  # nosec B603 B607\nsubprocess.run(["gh"])  # nosec B603\n'
        findings = tip.b607_findings(src, "a.py")
        self.assertEqual(1, len(findings))
        self.assertIn("'gh'", findings[0])

    def test_windows_filename_keeps_line_order(self):
        src = 'import subprocess\nsubprocess.call(["b"])\nsubprocess.Popen(["a"])\n'
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            (root / "a.py").write_text(src, encoding="utf-8")
            findings = tip.b607_findings(src, "C:\\repo\\a.py")
            self.assertEqual(2, len(findings))
            self.assertIn(":2:", findings[0])
            self.assertIn(":3:", findings[1])

    def test_preflight_blocks_changed_python_only(self):
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            (root / "a.py").write_text('import subprocess\nsubprocess.run(["git"])\n', encoding="utf-8")
            self.assertTrue(any("B607" in item for item in tip.preflight_failures(root, ["a.py"])))
            self.assertEqual([], tip.preflight_failures(root, ["README.md"]))

    def test_overlay_python_tree_is_b607_clean(self):
        paths = sorted(p for p in (ROOT / "chaos-engine").rglob("*.py") if "__pycache__" not in p.parts)
        findings = []
        for p in paths:
            findings.extend(tip.b607_findings(p.read_text(encoding="utf-8"), p.relative_to(ROOT).as_posix()))
        self.assertEqual([], findings)

    def test_coach_loop_and_doc_name_the_preflight(self):
        coach = text(COACH)
        self.assertIn("tip_preflight.py", coach)
        self.assertIn("B607", coach)
        doc = text(TIP_DOC)
        self.assertIn("B603", doc)
        self.assertIn("shutil.which", doc)
        self.assertIn("#6165", doc)


def _failing_checks(count: int) -> list[dict]:
    return [
        {
            "name": f"Check {index} " + "x" * 200,
            "state": "FAILURE",
            "link": "https://github.com/o/r/actions/runs/1/job/" + "9" * 200,
        }
        for index in range(count)
    ]


class DigestCiStatusTest(unittest.TestCase):
    """#6162: agents read a bounded digest, never the raw statusCheckRollup."""

    def test_hundred_checks_digest_is_bounded(self):
        checks = _failing_checks(100)
        bucket, failing = watch_pr_checks.classify_checks(checks)
        digest = watch_pr_checks.build_digest("a" * 40, bucket, checks, failing)
        size = len(json.dumps(digest).encode("utf-8"))
        self.assertLessEqual(size, watch_pr_checks.DIGEST_MAX_BYTES)
        self.assertEqual(4096, watch_pr_checks.DIGEST_MAX_BYTES)
        self.assertEqual(10, len(digest["failing"]))
        self.assertEqual(100, digest["failing_total"])
        self.assertEqual("red", digest["state"])
        raw = json.dumps({"statusCheckRollup": checks})
        self.assertTrue(watch_pr_checks.rollup_is_waste(raw))
        self.assertFalse(watch_pr_checks.rollup_is_waste(json.dumps(digest)))
        self.assertGreater(len(raw), 10 * size)

    def test_digest_schema_is_stable(self):
        checks = [{"name": "unit", "state": "IN_PROGRESS", "link": ""}]
        bucket, failing = watch_pr_checks.classify_checks(checks)
        digest = watch_pr_checks.build_digest(None, bucket, checks, failing)
        self.assertEqual({"sha", "state", "failing", "failing_total", "pending_count", "codacy_action_required", "updated_at"}, set(digest))
        self.assertEqual("pending", digest["state"])
        self.assertEqual(1, digest["pending_count"])

    def test_codacy_action_required_is_blocking_in_digest(self):
        checks = [{"name": "Codacy Static Code Analysis", "state": "ACTION_REQUIRED", "link": "https://app.codacy.com/x"}, {"name": "unit", "state": "SUCCESS", "link": ""}]
        bucket, failing = watch_pr_checks.classify_checks(checks)
        digest = watch_pr_checks.build_digest("b" * 40, bucket, checks, failing)
        self.assertEqual(["Codacy Static Code Analysis"], digest["codacy_action_required"])
        self.assertIn("(blocking)", watch_pr_checks.format_digest_line(digest))

    def test_red_digest_keeps_failing_jobs_and_publishes_lease(self):
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            digest_path = root / "digest.json"
            context = SimpleNamespace(repo="o/r", pr_number=5, root=root)
            checks = [{"name": "unit", "state": "FAILURE", "link": "https://checks/1"}]
            out = io.StringIO()
            with mock.patch.object(watch_pr_checks, "resolve_gh", return_value="/gh"), \
                 mock.patch.object(watch_pr_checks, "resolve_repository_context", return_value=context), \
                 mock.patch.object(watch_pr_checks, "resolve_pr_number", return_value=5), \
                 mock.patch.object(watch_pr_checks, "poll_once", return_value=checks), \
                 mock.patch.object(watch_pr_checks, "fetch_head_sha", return_value="c" * 40), \
                 redirect_stdout(out):
                code = watch_pr_checks.main(["--pr", "5", "--repo", "o/r", "--poll-once", "--digest", "--digest-out", str(digest_path), "--status-lease", "--root", str(root)])
            self.assertEqual(1, code)
            lines = out.getvalue().splitlines()
            self.assertEqual(1, len(lines))
            payload = json.loads(lines[0])
            self.assertEqual("unit", payload["failingJobs"][0]["name"])
            self.assertEqual("red", payload["state"])
            self.assertEqual("c" * 40, payload["sha"])
            self.assertEqual("red", json.loads(digest_path.read_text(encoding="utf-8"))["state"])
            lease = json.loads((root / ".chaos-engine/runtime/status-lease-5.json").read_text(encoding="utf-8"))
            self.assertEqual("red", lease["state"])

    def test_agent_playbooks_never_cite_raw_rollup(self):
        for path in (PLAYBOOK, PROCESS_OWNER, ORCHESTRATOR, COACH):
            with self.subTest(path=path.name):
                self.assertNotIn("statusCheckRollup", text(path))
        economy = text(CI_ECONOMY)
        for line in economy.splitlines():
            if "statusCheckRollup" in line:
                self.assertIn("never", line.casefold())
        self.assertIn("--digest", economy)
        self.assertIn("--digest", text(PLAYBOOK))
        self.assertIn("ci-status-economy.md", text(ELIMINATE))


class OneStatusChannelTest(unittest.TestCase):
    """#6163: a live watch lease silences every other status channel."""

    def test_live_lease_silences_the_status_routine(self):
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            status_lease.write_lease(root, 9, pid=os.getpid(), state="pending")
            self.assertEqual("", status_lease.routine_status_line(root, 9))
            out = io.StringIO()
            with redirect_stdout(out):
                code = status_lease.main(["routine", "--pr", "9", "--root", str(root)])
            self.assertEqual(0, code)
            self.assertEqual("", out.getvalue())

    def test_red_is_reported_once(self):
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            status_lease.write_lease(root, 9, pid=os.getpid(), state="red")
            line = status_lease.routine_status_line(root, 9)
            self.assertIn("red", line)
            self.assertEqual("", status_lease.routine_status_line(root, 9, last_reported="red"))

    def test_owner_ask_dead_or_stale_watch_emits(self):
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            status_lease.write_lease(root, 9, pid=os.getpid(), state="pending")
            self.assertNotEqual("", status_lease.routine_status_line(root, 9, owner_asked=True))
            self.assertNotEqual("", status_lease.routine_status_line(root, 9, alive=lambda pid: False))
            later = time.time() + status_lease.LEASE_MAX_AGE_SECONDS + 60
            self.assertNotEqual("", status_lease.routine_status_line(root, 9, now=later))

    def test_no_lease_means_a_status_is_due(self):
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            self.assertIn("no live watch lease", status_lease.routine_status_line(root, 11))

    def test_docs_state_exactly_one_status_channel(self):
        owner = text(PROCESS_OWNER)
        self.assertIn("exactly one status channel", owner)
        self.assertIn("status_lease.py", owner)
        self.assertIn("overlapping status channels", text(ORCHESTRATOR))
        self.assertIn("status-lease-<pr>.json", text(CI_ECONOMY))


def _template_block(doc: str) -> str:
    for block in doc.split("```")[1::2]:
        if "brief_path:" in block:
            return block.split("\n", 1)[1] if "\n" in block else block
    return ""


class LeanExecutorPromptTest(unittest.TestCase):
    """#6167: delta-sized executor prompts and fingerprint-first failed logs."""

    def test_prompt_schema_requires_brief_path_and_cap(self):
        self.assertTrue(any("brief_path" in f for f in brief.lint_executor_prompt("Goal: fix it\n")))
        big = "brief_path: /tmp/w.md\n" + "x" * (brief.MAX_INLINE_BYTES + 1)
        self.assertTrue(any("bytes" in f for f in brief.lint_executor_prompt(big)))
        self.assertEqual(4096, brief.MAX_INLINE_BYTES)

    def test_completed_out_of_wave_todos_are_rejected(self):
        bad = "brief_path: /tmp/w.md\n- [x] IntelliJ wave slice 3\n"
        self.assertTrue(brief.lint_executor_prompt(bad))
        ok = "brief_path: /tmp/w.md\nGoal: rerun after the job completed\n"
        self.assertEqual([], brief.lint_executor_prompt(ok))

    def test_documented_template_passes_lint(self):
        block = _template_block(text(CI_ECONOMY))
        self.assertIn("brief_path:", block)
        self.assertEqual([], brief.lint_executor_prompt(block))
        self.assertNotIn("[x]", block)

    def test_known_fingerprint_reads_at_most_forty_lines(self):
        lines = [f"line {i}" for i in range(900)]
        lines[500] = "source-derived inventory drift: python-libraries"
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            result = brief.failed_log_budget("\n".join(lines), "inventory drift", root / "spill")
            self.assertEqual(40, brief.FAILED_LOG_LINE_CAP)
            self.assertLessEqual(len(result["excerpt"].splitlines()), 40)
            self.assertIn("inventory drift", result["excerpt"])
            self.assertIsNone(result["spill_path"])

    def test_unknown_failure_spills_full_log(self):
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            log = "\n".join(f"line {i}" for i in range(900))
            result = brief.failed_log_budget(log, None, root / "spill")
            self.assertEqual(log, Path(result["spill_path"]).read_text(encoding="utf-8"))
            self.assertLessEqual(len(result["excerpt"].splitlines()), 40)

    def test_classifier_maps_ci_summaries_to_fingerprints(self):
        self.assertEqual("inventory-drift", tip.classify_failure("source-derived inventory drift: python-libraries"))
        self.assertEqual("graphify-empty-output-after-version", tip.classify_failure("dependency command failed: graphify.exe: no process output"))
        self.assertEqual("bandit-b607", tip.classify_failure("Bandit_B607 at _overlay_push_block"))
        self.assertIsNone(tip.classify_failure("all checks green"))

    def test_docs_pin_the_budget(self):
        self.assertIn("executor_brief.py", text(COACH))
        self.assertIn("brief_path:", text(COACH))
        self.assertIn("fingerprint", text(CONTEXT))
        self.assertIn("40 lines", text(CI_ECONOMY))


class TipChurnPreflightTest(unittest.TestCase):
    """#6164: batch micro-fixes; local preflight catches what used to cost a tip."""

    def test_inventory_drift_fails_like_agent_guidance_gate(self):
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            failures = tip.preflight_failures(root, ["chaos-engine/new_module.py"], inventory_validator=lambda _root: ["source-derived inventory drift: python-libraries"])
            self.assertTrue(any("source-derived inventory drift: python-libraries" in f and "--write" in f for f in failures))

    def test_inventory_skipped_without_overlay_python_change(self):
        calls = []
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            tip.preflight_failures(root, ["README.md", "chaos-engine/references/x.md"], inventory_validator=lambda r: calls.append(r) or [])
            self.assertEqual([], calls)

    def test_live_repository_inventory_is_refreshed_in_the_same_commit(self):
        self.assertEqual([], tip.inventory_drift(ROOT))

    def test_playbook_forbids_tip_per_microfix(self):
        sentence = "Never push one tip per micro-fix when fresh-installer paths are implicated"
        self.assertIn(sentence, text(PLAYBOOK))
        self.assertIn(sentence, text(TIP_DOC))

    def test_coach_loop_checklist_runs_preflight_before_push(self):
        coach = text(COACH)
        for token in ("tip_preflight.py", "B607", "inventory", ".memory/"):
            with self.subTest(token=token):
                self.assertIn(token, coach)

    def test_overlay_pre_push_runs_the_tip_preflight(self):
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            target = root / overlay_pre_push.TIP_PREFLIGHT
            target.parent.mkdir(parents=True)
            target.write_text((SCRIPTS / "tip_preflight.py").read_text(encoding="utf-8"), encoding="utf-8")
            (root / "a.py").write_text('import subprocess\nsubprocess.run(["git"])\n', encoding="utf-8")
            failures = overlay_pre_push.overlay_pre_push_failures(root, ["a.py"])
            self.assertTrue(any(f.startswith("tip preflight:") and "B607" in f for f in failures))


DEPENDENCIES = load(ROOT / "chaos-engine/dependencies.py", "ce_dependencies_6166")
LIVE_ACCEPTANCE = load(
    ROOT / "scripts/ci/chaos_engine_live_installer_acceptance.py", "ce_live_acceptance_6166"
)
FINGERPRINT = "graphify-empty-output-after-version"


class GraphifyEmptyOutputAbsorbTest(unittest.TestCase):
    """#6166: Windows graphify.exe empty output after a healthy --version is a known flake."""

    def _runner(self, calls, *, stderr="", version_code=0):
        def runner(command, **_kwargs):
            calls.append(list(command))
            if list(command)[1:] == ["--version"]:
                return SimpleNamespace(returncode=version_code, stdout="graphify 1\n", stderr="")
            return SimpleNamespace(returncode=1, stdout="", stderr=stderr)
        return runner

    def test_install_empty_output_is_absorbed_with_one_fingerprint(self):
        calls = []
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            summary = root / "summary.md"
            err = io.StringIO()
            with mock.patch.dict(os.environ, {"GITHUB_STEP_SUMMARY": str(summary)}), redirect_stderr(err):
                result = DEPENDENCIES._run_project_setup_command([str(root / "graphify.exe"), "install", "--platform", "agents"], root, runner=self._runner(calls))
                self.assertEqual(0, result.returncode)
                self.assertEqual(["install", "install", "--version"], [c[1] for c in calls])
                self.assertIn(FINGERPRINT, summary.read_text(encoding="utf-8"))
                self.assertIn(FINGERPRINT, err.getvalue())

    def test_real_failure_with_diagnostic_stays_red(self):
        calls = []
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            with self.assertRaises(RuntimeError):
                DEPENDENCIES._run_project_setup_command([str(root / "graphify.exe"), "extract", "."], root, runner=self._runner(calls, stderr="boom: parser crashed"))
            self.assertEqual(1, len(calls))

    def test_unhealthy_version_stays_red(self):
        calls = []
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            with self.assertRaises(RuntimeError):
                DEPENDENCIES._run_project_setup_command([str(root / "graphify.exe"), "extract", "."], root, runner=self._runner(calls, version_code=1))

    def test_classifier_is_narrow(self):
        error = RuntimeError("dependency command failed: graphify.exe: no process output")
        self.assertTrue(DEPENDENCIES.is_graphify_empty_output(["graphify.exe", "extract", "."], error))
        self.assertTrue(DEPENDENCIES.is_graphify_empty_output(["/t/graphify", "install", "--platform", "agents"], error))
        self.assertFalse(DEPENDENCIES.is_graphify_empty_output(["graphify", "query", "x"], error))
        self.assertFalse(DEPENDENCIES.is_graphify_empty_output(["mempalace", "extract"], error))
        self.assertFalse(DEPENDENCIES.is_graphify_empty_output(["graphify", "extract"], RuntimeError("boom")))

    def test_live_acceptance_tags_the_fingerprint(self):
        tagged = LIVE_ACCEPTANCE.fingerprint_detail("dependency command failed: graphify.exe: no process output")
        self.assertIn(FINGERPRINT, tagged)
        self.assertEqual("boom", LIVE_ACCEPTANCE.fingerprint_detail("boom"))

    def test_babysit_fingerprint_table_says_no_tip(self):
        economy = text(CI_ECONOMY)
        self.assertIn(FINGERPRINT, economy)
        self.assertIn("#6166", economy)
        self.assertIn("do not open a tip", economy)


class CodacyActionRequiredGateTest(unittest.TestCase):
    """#6168: Codacy ACTION_REQUIRED (>=medium, any category) equals unit red."""

    def test_gate_covers_every_category_and_keeps_complexity(self):
        gate = text(CODACY_GATE)
        for token in ("ACTION_REQUIRED", "any category", "≥medium", "codacy-complexity-gate.md", "#6165", "auto-merge"):
            with self.subTest(token=token):
                self.assertIn(token, gate)

    def test_playbook_pr_merger_treats_codacy_like_unit_red(self):
        playbook = text(PLAYBOOK)
        self.assertIn("codacy-action-required-gate.md", playbook)
        self.assertIn("any category", playbook)
        self.assertIn("codacy-complexity-gate.md", playbook)

    def test_level1_lists_the_gate_and_complexity_checklist(self):
        level1 = text(LEVEL1)
        self.assertIn("codacy-action-required-gate.md", level1)
        self.assertIn("codacy-complexity-gate.md", level1)

    def test_process_owner_rag_is_red_on_codacy(self):
        owner = text(PROCESS_OWNER)
        self.assertIn("Codacy `ACTION_REQUIRED`", owner)
        self.assertIn("`red`", owner)
        self.assertIn("Blocked", owner)

    def test_watch_classifies_action_required_as_red(self):
        self.assertIn("ACTION_REQUIRED", watch_pr_checks.RED_STATES)


class MemoryContentHashTest(unittest.TestCase):
    """#6169: recompute Memory content_hash before push, in the same tip as the body edit."""

    def test_stale_hash_blocks_and_rehash_repairs(self):
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            folder = root / ".memory/memory/gotchas"
            folder.mkdir(parents=True)
            rel = ".memory/memory/gotchas/x.json"
            (root / rel).write_text(json.dumps({"body_path": "memory/gotchas/x.md", "content_hash": "sha256:stale", "id": "gotcha.x"}), encoding="utf-8")
            (folder / "x.md").write_text("edited body\n", encoding="utf-8")
            failures = tip.memory_hash_failures(root, [".memory/memory/gotchas/x.md"])
            self.assertEqual(1, len(failures))
            self.assertIn("memory-content-hash stale", failures[0])
            tip.rehash_memory_object(root, rel)
            self.assertEqual([], tip.memory_hash_failures(root, [rel]))

    def test_deleted_object_is_not_a_failure(self):
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            self.assertEqual([], tip.memory_hash_failures(root, [".memory/memory/gotchas/gone.json"]))

    def test_recipe_matches_the_validator(self):
        from scripts.ci.validate_agent_setup import memory_content_hash as validator_hash
        sidecar = {"body_path": "memory/a.md", "title": "Ünïcode", "content_hash": "x", "tags": ["b", "a"]}
        body = "line\r\nnext\n"
        self.assertEqual(validator_hash(sidecar, body), tip.memory_content_hash(sidecar, body))

    def test_live_memory_store_is_hash_clean(self):
        paths = sorted(p.relative_to(ROOT).as_posix() for p in (ROOT / ".memory/memory").rglob("*.json"))
        self.assertEqual([], tip.memory_hash_failures(ROOT, paths))

    def test_guidance_promotes_the_rehash_recipe(self):
        self.assertIn("tip_preflight.py --rehash", text(ACTIVATION))
        self.assertIn("memory save --stdin", text(ACTIVATION))
        self.assertIn("content_hash", text(PROCESS_OWNER))
        self.assertIn(".memory/", text(COACH))
        doc = text(TIP_DOC)
        self.assertIn("validate_agent_setup", doc)
        self.assertIn("memory_content_hash", doc)
        self.assertIn("tip-churn-preflight.md", text(MEMORY_WORKFLOW))


class LocalWriterOptionalTest(unittest.TestCase):
    """#6171: implementers write code directly; local writers are an optional, narrow tool."""

    MANDATES = (
        r"no code by (?:the )?host",
        r"host (?:must|may|should) not (?:write|implement)",
        r"(?:never|do not|don't) host-implement",
        r"local writers? only",
        r"(?:must|always) (?:route|dispatch|send) (?:all )?(?:implementation|code)\b[^.\n]{0,40}\blocal",
        r"coach loop \(host process-owner\)\n+local openai-compat writers need",
        r"cadence \(non-negotiable\)",
    )
    JOBS = ("bulk mechanical edits", "spec or ticket drafting", "log summarization", "offline or private work")
    RULE = "handoff cost is well below generation cost"

    def _overlay_docs(self):
        root = ROOT / "chaos-engine"
        for path in sorted(root.rglob("*.md")):
            if "vendor" in path.parts:
                continue
            yield path

    def test_overlay_has_no_local_writer_mandate(self):
        """No overlay doc forces implementation through a local writer or forbids host code."""
        import re

        for path in self._overlay_docs():
            body = text(path).lower()
            for pattern in self.MANDATES:
                with self.subTest(path=str(path.relative_to(ROOT)), pattern=pattern):
                    self.assertIsNone(re.search(pattern, body))

    def test_decision_rule_and_jobs_are_portable(self):
        """The rule lives in the portable local-agency skill, not a host adapter."""
        rule = text(LOCAL_RULE)
        for token in (self.RULE, *self.JOBS, "#6171", "no host-only memory exception") + HOSTS:
            with self.subTest(token=token):
                self.assertIn(token, rule)
        self.assertIn("implementers write code directly", rule)
        self.assertIn("optional", rule)

    def test_entrypoints_link_the_rule(self):
        """Skill, coach loop, guide, process owner, and orchestrator all route to the rule."""
        for path in (LOCAL_SKILL, COACH, LOCAL_GUIDE, PROCESS_OWNER, ORCHESTRATOR):
            with self.subTest(path=path.name):
                self.assertIn("when-to-use-local.md", text(path))
        for path in (LOCAL_SKILL, COACH):
            with self.subTest(path=path.name, token="rule"):
                self.assertIn(self.RULE, text(path))

    def test_token_accounting_method_documented(self):
        """Cost comparisons use real local-server counts (print_timing sums)."""
        rule = text(LOCAL_RULE)
        for token in ("print_timing", "prompt eval", "journalctl"):
            with self.subTest(token=token):
                self.assertIn(token, rule)

    def test_no_invalid_prefer_value_in_guidance(self):
        """dispatch.py has no openai-compat --prefer choice; guidance must not cite it."""
        for path in self._overlay_docs():
            with self.subTest(path=str(path.relative_to(ROOT))):
                self.assertNotIn("--prefer openai-compat", text(path))


class HarnessParityTest(unittest.TestCase):
    """#6161: every new overlay rule is host-neutral (no host-only memory exception)."""

    def test_new_references_name_every_host_and_their_tickets(self):
        """Each new reference names all six hosts and its ticket ids."""
        cases = (
            (CI_ECONOMY, ("#6162", "#6163", "#6166", "#6167")),
            (TIP_DOC, ("#6164", "#6165", "#6169")),
            (CODACY_GATE, ("#6168",)),
        )
        for path, tickets in cases:
            body = text(path)
            for token in HOSTS + tickets:
                with self.subTest(path=path.name, token=token):
                    self.assertIn(token, body)

    def test_no_host_only_memory_exception(self):
        """Parity rule is stated in every new reference."""
        for path in (CI_ECONOMY, TIP_DOC, CODACY_GATE):
            with self.subTest(path=path.name):
                self.assertIn("no host-only memory exception", text(path))
