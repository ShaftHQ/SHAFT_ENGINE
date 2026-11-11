"""Regressions for #7955-#7960. Calls the guards; does not reimplement them."""

from __future__ import annotations

import importlib.util
import subprocess
import sys
import tempfile
import unittest
import unittest.mock
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
CE = ROOT / "chaos-engine"


def _load(name: str, path: Path):
    spec = importlib.util.spec_from_file_location(name, path)
    if spec is None or spec.loader is None:
        raise ImportError(path)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def _git(root: Path, *args: str) -> None:
    subprocess.run(["git", "-C", str(root), *args], check=True, capture_output=True, text=True)


class ForcePushSingleUseTest(unittest.TestCase):
    def test_denial_token_stays_exact_and_approval_is_single_use(self) -> None:
        boundary = _load("ce_boundary_7955", CE / "rules/operation_boundary.py")
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            push = "git push --force origin HEAD"
            self.assertEqual("force-push-without-approval", boundary.force_push_block(root, push))
            hint = boundary.force_push_hint(push)
            self.assertIn("bare --force", hint)
            self.assertIn("force-push-approve -- " + push, hint)
            self.assertNotIn("--force-with-lease=<ref>:<sha>", hint)
            lease = "git push --force-with-lease=refs/heads/main:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa origin"
            lease_hint = boundary.force_push_hint(lease)
            self.assertIn("--force-with-lease=<ref>:<sha>", lease_hint)
            self.assertNotIn("bare --force", lease_hint)
            approval = root / ".chaos-engine-state"
            approval.mkdir()
            (approval / "force-push-approval.txt").write_text(push + "\n", encoding="utf-8")
            self.assertIsNone(boundary.force_push_block(root, push))
            self.assertEqual("force-push-without-approval", boundary.force_push_block(root, push))
            approve = "python3 .chaos-engine/tool.py force-push-approve -- " + push
            self.assertIsNone(boundary.force_push_block(root, approve))
            self.assertEqual(
                "force-push-without-approval",
                boundary.force_push_block(root, approve + " && " + push),
            )

    def test_force_push_approve_writes_the_temp_repo(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            _git(root, "init")
            command = "git push --force origin HEAD"
            proc = subprocess.run(
                [sys.executable, str(CE / "tool.py"), "force-push-approve", "--", command],
                cwd=root,
                capture_output=True,
                text=True,
                check=False,
            )
            self.assertEqual(0, proc.returncode, proc.stderr)
            recorded = (root / ".chaos-engine-state" / "force-push-approval.txt").read_text(encoding="utf-8")
            self.assertEqual(command + "\n", recorded)


class SecretsTextTest(unittest.TestCase):
    def test_placeholders_pass_and_real_values_are_withheld(self) -> None:
        lessons = _load("ce_lessons_7956", CE / "rules/lessons_2026_11_06.py")
        boundary = _load("ce_boundary_7956", CE / "rules/operation_boundary.py")
        for sample in ("password=", "password=//p", "password=$VAR", "token=${TOKEN}", "token=changeme", "password=<password>"):
            self.assertFalse(lessons._password_hit(sample), sample)
        self.assertTrue(lessons._password_hit("password: hunter2"))
        withheld = boundary.output_secret_block(
            {"exit_code": 17, "stdout": "ghp_abcdefghij123456\n"},
        )
        self.assertIn("secret-in-output", withheld)
        self.assertIn("already finished", withheld)
        self.assertIn("17", withheld)
        self.assertNotIn("ghp_", withheld)


class SecretGuardDoctorTest(unittest.TestCase):
    def test_foreign_hooks_path_is_not_live(self) -> None:
        hooks = _load("ce_hooks_7958", CE / "git_hooks.py")
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            _git(root, "init")
            _git(root, "config", "core.hooksPath", "tools/git-hooks")
            status, reason = hooks.guard_health(root)
            self.assertEqual("fail", status)
            self.assertIn("not live", reason)
            self.assertIn("tools/git-hooks", reason)

    def test_install_md_mentions_doctor_before_live(self) -> None:
        text = (CE / "INSTALL.md").read_text(encoding="utf-8")
        start = text.lower().find("### secret guard git hooks")
        self.assertGreaterEqual(start, 0)
        section = text[start : start + 900].lower()
        self.assertIn("not live", section)
        self.assertIn("doctor", section)


class ImpactReceiptTest(unittest.TestCase):
    def test_bare_used_allows_and_stale_or_missing_hits_ask_to_refresh(self) -> None:
        gate = _load("ce_retrieve_7957", CE / "hooks/retrieve_justification.py")
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary)
            sink = project / ".chaos-engine-state" / "research-receipt.md"
            sink.parent.mkdir(parents=True)
            target = "chaos-engine/delete_guard.py"
            call = dict(
                project=project,
                event_name="PreToolUse",
                tool_name="Write",
                tool_input={"file_path": target},
            )
            sink.write_text("retrieve: used\n", encoding="utf-8")
            self.assertIsNone(gate.impact_edit_block_reason(**call))
            sink.write_text("retrieve: used\nthis note says the word stale in prose\n", encoding="utf-8")
            self.assertIsNone(gate.impact_edit_block_reason(**call))
            sink.write_text(
                'retrieve: used\n{"freshness": {"status": "stale"}, "hits": []}\n',
                encoding="utf-8",
            )
            stale = gate.impact_edit_block_reason(**call)
            self.assertIsNotNone(stale)
            self.assertIn("refresh first", stale)
            sink.write_text(
                'retrieve: used\n{"hits": [{"path": "chaos-engine/hosts.py"}]}\n',
                encoding="utf-8",
            )
            missed = gate.impact_edit_block_reason(**call)
            self.assertIsNotNone(missed)
            self.assertIn("refresh first", missed)
            sink.write_text(
                'retrieve: used\n{"hits": [{"path": "chaos-engine/delete_guard.py"}]}\n',
                encoding="utf-8",
            )
            self.assertIsNone(gate.impact_edit_block_reason(**call))
            absolute = dict(call)
            absolute["tool_input"] = {"file_path": str(project / target)}
            self.assertIsNone(gate.impact_edit_block_reason(**absolute))
            gate._mark_session(project, "retrieved-stale", retrieved=True)
            marked = dict(call, session_id="retrieved-stale")
            sink.write_text("retrieve: used\n", encoding="utf-8")
            self.assertIsNone(gate.impact_edit_block_reason(**marked))
            sink.write_text(
                'retrieve: used\n{"freshness": {"status": "stale"}, "hits": [{"path": "chaos-engine/delete_guard.py"}]}\n',
                encoding="utf-8",
            )
            blocked = gate.impact_edit_block_reason(**marked)
            self.assertIsNotNone(blocked)
            self.assertIn("refresh first", blocked)


class IgnoreAndDeleteTest(unittest.TestCase):
    def test_gitignore_classifies_rules_and_secrets_without_star_text_auto(self) -> None:
        hosts = _load("ce_hosts_7959", CE / "hosts.py")
        block = hosts.gitignore_content(None).decode("utf-8")
        self.assertIn("src/main/resources/secrets.properties", block)
        self.assertIn(".grok/rules/*.md", block)
        self.assertNotIn("* text=auto", block)

    def test_identical_untracked_file_needs_no_code(self) -> None:
        guard = _load("ce_delete_7960", CE / "delete_guard.py")
        with tempfile.TemporaryDirectory() as temporary, unittest.mock.patch.object(guard, "DEFAULT_ALLOW", ()):
            root = Path(temporary)
            _git(root, "init")
            _git(root, "config", "user.email", "dev@example.com")
            _git(root, "config", "user.name", "Dev")
            target = root / "hello.txt"
            target.write_text("same\n", encoding="utf-8")
            _git(root, "add", "hello.txt")
            _git(root, "commit", "-m", "add hello")
            tip = subprocess.run(
                ["git", "-C", str(root), "rev-parse", "HEAD"],
                check=True,
                capture_output=True,
                text=True,
            ).stdout.strip()
            name = subprocess.run(
                ["git", "-C", str(root), "rev-parse", "--abbrev-ref", "HEAD"],
                check=True,
                capture_output=True,
                text=True,
            ).stdout.strip()
            remote = f"refs/remotes/origin/{name}"
            _git(root, "update-ref", remote, tip)
            _git(root, "symbolic-ref", "refs/remotes/origin/HEAD", remote)
            _git(root, "checkout", "-b", "feature")
            _git(root, "rm", "hello.txt")
            _git(root, "commit", "-m", "remove hello")
            target.write_text("same\n", encoding="utf-8")
            self.assertEqual("", guard.path_reason(target, root))
            target.write_text("different\n", encoding="utf-8")
            self.assertIn("untracked", guard.path_reason(target, root))
            unique = root / "unique.txt"
            unique.write_text("only-here\n", encoding="utf-8")
            self.assertIn("untracked", guard.path_reason(unique, root))
            refused = guard.check("rm -- " + str(unique), root, root)
            self.assertIn("untracked", refused[0])

    def test_switch_create_uses_the_start_point(self) -> None:
        guard = _load("ce_delete_7960_switch", CE / "delete_guard.py")
        with tempfile.TemporaryDirectory() as temporary, unittest.mock.patch.object(guard, "DEFAULT_ALLOW", ()):
            root = Path(temporary)
            _git(root, "init")
            _git(root, "config", "user.email", "dev@example.com")
            _git(root, "config", "user.name", "Dev")
            target = root / "hello.txt"
            target.write_text("same\n", encoding="utf-8")
            _git(root, "add", "hello.txt")
            _git(root, "commit", "-m", "add hello")
            _git(root, "branch", "-M", "release")
            _git(root, "checkout", "-b", "feature")
            _git(root, "rm", "hello.txt")
            _git(root, "commit", "-m", "remove hello")
            target.write_text("same\n", encoding="utf-8")
            command = "git switch -c other release && rm -- " + str(target)
            self.assertEqual("", guard.check(command, root, root)[0])


if __name__ == "__main__":
    unittest.main()
