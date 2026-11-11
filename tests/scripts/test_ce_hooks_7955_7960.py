"""Regressions for #7955-#7960. Calls the guards; does not reimplement them."""

from __future__ import annotations

import base64
import fnmatch
import importlib.util
import os
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
    subprocess.run(  # nosec B603 B607 - fixed git argv, no shell.
        ["git", "-C", str(root), *args], check=True, capture_output=True, text=True
    )


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

    def test_a_mention_of_the_approve_tool_is_still_a_force_push(self) -> None:
        boundary = _load("ce_boundary_7955_mention", CE / "rules/operation_boundary.py")
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            commented = (
                "git push --force origin HEAD"
                " # python3 .chaos-engine/tool.py force-push-approve"
            )
            substituted = (
                "git push --force origin HEAD"
                " $(python3 .chaos-engine/tool.py force-push-approve -- true)"
            )
            for command in (commented, substituted):
                self.assertEqual(
                    "force-push-without-approval",
                    boundary.force_push_block(root, command),
                    command,
                )
            approve = "python3 .chaos-engine/tool.py force-push-approve -- git push --force origin HEAD"
            approval = root / ".chaos-engine-state"
            approval.mkdir()
            recorded = approval / "force-push-approval.txt"
            recorded.write_text(approve + "\n", encoding="utf-8")
            self.assertIsNone(boundary.force_push_block(root, approve))
            self.assertEqual(approve + "\n", recorded.read_text(encoding="utf-8"))

    def test_a_substitution_or_following_line_still_pushes(self) -> None:
        boundary = _load("ce_boundary_7955_subst", CE / "rules/operation_boundary.py")
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            _git(root, "init")
            push = "git push --force origin HEAD"
            substituted = "python3 .chaos-engine/tool.py force-push-approve -- $(git push --force origin HEAD)"
            backticks = "python3 .chaos-engine/tool.py force-push-approve -- `git push --force origin HEAD`"
            newline = "python3 .chaos-engine/tool.py force-push-approve -- true\n" + push
            into = "python3 .chaos-engine/tool.py force-push-approve -- <(git push --force origin HEAD)"
            out_of = "python3 .chaos-engine/tool.py force-push-approve -- >(git push --force origin HEAD)"
            redirect = "python3 .chaos-engine/tool.py force-push-approve -- true < <(git push --force origin HEAD)"
            quoted = """python3 .chaos-engine/tool.py force-push-approve -- $(python3 -c 'print(")")'; git push --force origin HEAD)"""
            escaped = "python3 .chaos-engine/tool.py force-push-approve -- \\' ; git push --force origin HEAD"
            single = "python3 .chaos-engine/tool.py force-push-approve -- 'x\\' ; git push --force origin HEAD"
            doubled = 'python3 .chaos-engine/tool.py force-push-approve -- "x\\\\" ; git push --force origin HEAD'
            continued = "git push \\\n--force origin HEAD"
            echoed = "echo $(git push \\\n--force origin HEAD)"
            hashed = "python3 .chaos-engine/tool.py force-push-approve -- x#$(git push --force origin HEAD)"
            hashed_ticks = "python3 .chaos-engine/tool.py force-push-approve -- x#`git push --force origin HEAD`"
            hashed_in = "python3 .chaos-engine/tool.py force-push-approve -- x#<(git push --force origin HEAD)"
            hashed_out = "python3 .chaos-engine/tool.py force-push-approve -- x#>(git push --force origin HEAD)"
            hashed_quote = 'python3 .chaos-engine/tool.py force-push-approve -- "x"#$(git push --force origin HEAD)'
            split_sub = "git push $(\n echo --force) origin HEAD"
            split_tick = "git push `\n echo --force` origin HEAD"
            split_short = "git push $(\n echo -f) origin HEAD"
            split_plus = "git push $(\n echo +HEAD)"
            hash_join = "python3 .chaos-engine/tool.py force-push-approve -- x\\\n#$(git push --force origin HEAD)"
            hash_join_tick = "python3 .chaos-engine/tool.py force-push-approve -- x\\\n#`git push --force origin HEAD`"
            comment_cont = "python3 .chaos-engine/tool.py force-push-approve -- true # comment \\\ngit push --force origin HEAD"
            heredoc = "git push $(cat <<EOF\n)\n--force\nEOF\n) origin HEAD"
            expanded = "f=--force\ngit push $f origin HEAD"
            arrayed = 'args=(--force origin HEAD)\ngit push "${args[@]}"'
            inner = "git push origin HEAD $(: # \\\n) --force"
            short_var = "f=$(\n echo -f)\ngit push $f origin HEAD"
            short_array = 'args=(\n-f origin HEAD)\ngit push "${args[@]}"'
            plus_array = 'args=(\n+HEAD)\ngit push "${args[@]}"'
            setdash = 'set -- -f origin HEAD\ngit push "$@"'
            setplus = 'set -- +HEAD\ngit push "$@"'
            word_split = "${x:-git\npush --force origin HEAD}"
            comment_paren = "python3 .chaos-engine/tool.py force-push-approve -- true # (\ngit push --force origin HEAD"
            comment_short = "python3 .chaos-engine/tool.py force-push-approve -- true # (\ngit push -f origin HEAD"
            comment_plus = "python3 .chaos-engine/tool.py force-push-approve -- true # (\ngit push origin +HEAD"
            declared = 'declare -a args=(\n-f origin HEAD)\ngit push "${args[@]}"'
            appended = 'args+=(\n-f origin HEAD)\ngit push "${args[@]}"'
            appended_plus = 'args+=(\n+HEAD)\ngit push "${args[@]}"'
            exported = "export f=$(\n echo -f)\ngit push $f origin HEAD"
            exported_plus = "export f=$(\n echo +HEAD)\ngit push origin $f"
            windows = "py -3 .chaos-engine/tool.py force-push-approve -- " + push
            nested = root / "nested"
            nested.mkdir()
            approval = root / ".chaos-engine-state"
            approval.mkdir()
            recorded = approval / "force-push-approval.txt"
            recorded.write_text(push + "\n", encoding="utf-8")
            for command in (
                substituted, backticks, newline, into, out_of, redirect, quoted,
                escaped, single, doubled, continued, echoed, hashed, hashed_ticks,
                hashed_in, hashed_out, hashed_quote, split_sub, split_tick,
                split_short, split_plus, hash_join, hash_join_tick,
                comment_cont, heredoc, expanded, arrayed, inner, short_var,
                short_array, plus_array, setdash, setplus, word_split,
                comment_paren, comment_short, comment_plus, declared, appended,
                appended_plus, exported, exported_plus,
            ):
                self.assertEqual("force-push-without-approval", boundary.force_push_block(root, command), command)
                self.assertEqual(push + "\n", recorded.read_text(encoding="utf-8"), command)
            self.assertIsNone(boundary.force_push_block(root, windows))
            self.assertEqual(push + "\n", recorded.read_text(encoding="utf-8"))
            noted = "python3 .chaos-engine/tool.py force-push-approve -- " + push + " # note"
            self.assertIsNone(boundary.force_push_block(root, noted))
            self.assertEqual(push + "\n", recorded.read_text(encoding="utf-8"))
            self.assertIsNone(boundary.force_push_block(nested, push))
            self.assertEqual("", recorded.read_text(encoding="utf-8"))
            self.assertEqual("force-push-without-approval", boundary.force_push_block(nested, push))

    def test_force_push_approve_writes_the_temp_repo(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            _git(root, "init")
            command = "git push --force origin HEAD"
            proc = subprocess.run(  # nosec B603 - fixed interpreter and tool argv, no shell.
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
        self.assertTrue(lessons._password_hit('password="hunter2"'))
        self.assertEqual([], boundary.command_blocks(Path("."), 'grep -n "password=" file.sh'))
        withheld = boundary.output_secret_block(
            {"exit_code": 17, "stdout": "ghp_abcdefghij123456\n"},
        )
        self.assertIn("secret-in-output", withheld)
        self.assertIn("already finished", withheld)
        self.assertIn("17", withheld)
        self.assertNotIn("ghp_", withheld)

    def test_a_local_secret_file_value_is_withheld(self) -> None:
        boundary = _load("ce_boundary_7956_file", CE / "rules/operation_boundary.py")
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            folder = root / "src/main/resources/properties"
            folder.mkdir(parents=True)
            value = "local-secret-value-7956"
            (folder / "secrets.properties").write_text(f"password={value}\n", encoding="utf-8")
            withheld = boundary.output_secret_block(
                {"exit_code": 3, "stdout": value + "\n"},
                root,
            )
            self.assertIn("secret-in-output", withheld)
            self.assertIn("already finished", withheld)
            self.assertIn("3", withheld)
            self.assertNotIn(value, withheld)


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
            install = _load("ce_install_7958", CE / "install.py")
            line = install._format_secret_guard_line(root)
            self.assertEqual("secret-guard: not live tools/git-hooks", line)

    def test_repair_blocks_a_commit_that_contains_a_local_secret(self) -> None:
        install = _load("ce_install_7958_repair", CE / "install.py")
        value = "Sup3rSecretAccessKey99"  # nosec B105 - synthetic test value
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            _git(root, "init", "-q", "-b", "feature")
            repaired = install.repair_component(root, "secret-guard")
            self.assertEqual("repaired", repaired["status"])
            secrets = root / "src/main/resources/properties"
            secrets.mkdir(parents=True)
            (secrets / "secrets.properties").write_text(
                f"browserStack.accessKey={value}\n",
                encoding="utf-8",
            )
            blob = base64.b64encode(('{"accessKey":"%s"}' % value).encode()).decode()
            (root / "evidence.txt").write_text(
                '<html><script>d("data/attachments/a.txt","%s")</script></html>' % blob,
                encoding="utf-8",
            )
            env = dict(os.environ)
            env.update(
                GIT_AUTHOR_NAME="t",
                GIT_AUTHOR_EMAIL="t@e",
                GIT_COMMITTER_NAME="t",
                GIT_COMMITTER_EMAIL="t@e",
            )
            subprocess.run(  # nosec B603 B607 - fixed git argv in a temp repo
                ["git", "-C", str(root), "add", "evidence.txt"],
                check=True,
                capture_output=True,
                env=env,
            )
            leak = subprocess.run(  # nosec B603 B607 - fixed git argv in a temp repo
                ["git", "-C", str(root), "commit", "-q", "-m", "leak"],
                capture_output=True,
                text=True,
                env=env,
            )
            self.assertNotEqual(0, leak.returncode, leak.stderr)
            self.assertIn("secret_guard BLOCKED", leak.stderr)
            self.assertIn("known secret value", leak.stderr)
            self.assertNotIn(value, leak.stderr)

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

    def test_nested_hit_labels_and_a_stale_skip_ask_to_refresh(self) -> None:
        gate = _load("ce_retrieve_7957_nested", CE / "hooks/retrieve_justification.py")
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary)
            sink = project / ".chaos-engine-state" / "research-receipt.md"
            sink.parent.mkdir(parents=True)
            call = dict(
                project=project,
                event_name="PreToolUse",
                tool_name="Write",
                tool_input={"file_path": "chaos-engine/delete_guard.py"},
            )
            sink.write_text(
                "retrieve: used\n"
                '{"hits": [{"path": "chaos-engine/hosts.py", "labels": ["Doctor", "Install"]}]}\n',
                encoding="utf-8",
            )
            missed = gate.impact_edit_block_reason(**call)
            self.assertIsNotNone(missed)
            self.assertIn("refresh first", missed)
            cited = dict(call)
            cited["tool_input"] = {"file_path": "chaos-engine/hosts.py"}
            self.assertIsNone(gate.impact_edit_block_reason(**cited))
            sink.write_text('retrieve: used\n"hits": not-json\n', encoding="utf-8")
            unparsable = gate.impact_edit_block_reason(**call)
            self.assertIsNotNone(unparsable)
            self.assertIn("refresh first", unparsable)
            gate._mark_session(project, "skipped-stale", retrieved=True)
            marked = dict(call, session_id="skipped-stale")
            sink.write_text(
                'retrieve: skipped(no-rare-token)\n{"freshness": {"status": "stale"}}\n',
                encoding="utf-8",
            )
            blocked = gate.impact_edit_block_reason(**marked)
            self.assertIsNotNone(blocked)
            self.assertIn("refresh first", blocked)

    def test_a_scratch_write_needs_no_receipt(self) -> None:
        gate = _load("ce_retrieve_7957_scratch", CE / "hooks/retrieve_justification.py")
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary) / "project"
            project.mkdir()
            scratch = Path(temporary) / "scratch" / "note.txt"
            denied = gate.impact_edit_block_reason(
                project=project,
                event_name="PreToolUse",
                tool_name="Write",
                tool_input={"file_path": str(project / "chaos-engine" / "delete_guard.py")},
            )
            self.assertIsNotNone(denied)
            self.assertIn("tool.py retrieve --store graphify", denied)
            self.assertIsNone(
                gate.impact_edit_block_reason(
                    project=project,
                    event_name="PreToolUse",
                    tool_name="Write",
                    tool_input={"file_path": str(scratch)},
                )
            )
            command = [
                sys.executable,
                str(CE / "tool.py"),
                "retrieve",
                "--store",
                "graphify",
                "impact of this edit",
            ]
            proc = subprocess.run(  # nosec B603 - fixed interpreter and tool argv, no shell.
                command,
                cwd=ROOT,
                capture_output=True,
                text=True,
                check=False,
            )
            self.assertEqual(0, proc.returncode, proc.stderr)


class IgnoreAndDeleteTest(unittest.TestCase):
    def test_gitignore_classifies_rules_and_secrets_without_star_text_auto(self) -> None:
        hosts = _load("ce_hosts_7959", CE / "hosts.py")
        block = hosts.gitignore_content(None).decode("utf-8")
        self.assertIn("src/main/resources/secrets.properties", block)
        self.assertIn(".grok/rules/*.md", block)
        self.assertNotIn("* text=auto", block)

    def test_every_published_grok_rule_is_ignored(self) -> None:
        hosts = _load("ce_hosts_7959_publish", CE / "hosts.py")
        publisher = _load("ce_grok_publish_7959", CE / "grok_model_load.py")
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary)
            skill = project / "chaos-engine" / "skills" / "chaos-engine"
            skill.mkdir(parents=True)
            (skill / "SKILL.md").write_text("# skill\n", encoding="utf-8")
            (project / "chaos-engine" / "identity.md").write_text("# id\n", encoding="utf-8")
            written = publisher.publish(project)
            self.assertTrue(written)
            patterns = [
                line.strip()
                for line in hosts.gitignore_content(None).decode("utf-8").splitlines()
                if line.strip() and not line.startswith("#") and not line.startswith("!")
            ]
            for path in written:
                relative = Path(path).resolve().relative_to(project.resolve()).as_posix()
                self.assertTrue(
                    any(fnmatch.fnmatch(relative, pattern) for pattern in patterns),
                    relative,
                )

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
            tip = subprocess.run(  # nosec B603 B607 - fixed git argv, no shell.
                ["git", "-C", str(root), "rev-parse", "HEAD"],
                check=True,
                capture_output=True,
                text=True,
            ).stdout.strip()
            name = subprocess.run(  # nosec B603 B607 - fixed git argv, no shell.
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

    def test_a_session_worktree_copy_does_not_make_a_unique_file_deletable(self) -> None:
        guard = _load("ce_delete_7960_scratch", CE / "delete_guard.py")
        with tempfile.TemporaryDirectory() as temporary, unittest.mock.patch.object(guard, "DEFAULT_ALLOW", ()):
            root = Path(temporary) / "repo"
            scratch = Path(temporary) / "scratch"
            root.mkdir()
            scratch.mkdir()
            _git(root, "init")
            _git(root, "config", "user.email", "dev@example.com")
            _git(root, "config", "user.name", "Dev")
            keep = root / "keep.txt"
            keep.write_text("keep\n", encoding="utf-8")
            _git(root, "add", "keep.txt")
            _git(root, "commit", "-m", "add keep")
            unique = root / "unique.txt"
            unique.write_text("only-here\n", encoding="utf-8")
            (scratch / "unique.txt").write_text("only-here\n", encoding="utf-8")
            with unittest.mock.patch.dict(os.environ, {"CHAOS_SESSION_WORKTREE": str(scratch)}):
                self.assertIn("untracked", guard.path_reason(unique, root))
                refused = guard.check("rm -- " + str(unique), root, root)
                self.assertIn("untracked", refused[0])


class GateSelectionTest(unittest.TestCase):
    def test_changed_hook_paths_select_this_module(self) -> None:
        from scripts.ci.harness_pr_gate import classify_paths

        module = "tests.scripts.test_ce_hooks_7955_7960"
        for path in (
            "chaos-engine/rules/operation_boundary.py",
            "chaos-engine/hooks/retrieve_justification.py",
            "chaos-engine/force_push_approve.py",
            "tests/scripts/test_ce_hooks_7955_7960.py",
        ):
            plan = classify_paths([path])
            self.assertIn(module, plan.test_modules, path)


if __name__ == "__main__":
    unittest.main()
