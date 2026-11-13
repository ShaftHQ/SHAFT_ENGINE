"""Manual tool-output store: full text, no host replacement, no hook."""

from __future__ import annotations

import contextlib
import importlib.util
import io
import json
import os
import sqlite3
import subprocess  # nosec B404 - runs this repo's tool.py with a fixed argv.
import sys
import tempfile
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
CE = ROOT / "chaos-engine"
TOOL = CE / "tool.py"
PLANTED = "PLANTED7976UNIQUELINEONLY"


def secret_assignment() -> str:
    """Build a secret-shaped assignment without putting that shape in this file."""
    name = "api" + "_key"
    value = "Super" + "Secret7976abcd"
    return f"{name} = {value}"


def secret_value() -> bytes:
    return ("Super" + "Secret7976abcd").encode("utf-8")


def load():
    spec = importlib.util.spec_from_file_location("tool_output_store", CE / "tool_output_store.py")
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def run_tool(project: Path, *args: str) -> subprocess.CompletedProcess[str]:
    return subprocess.run(  # nosec B603 - fixed interpreter and tool.py argv.
        [sys.executable, str(TOOL), *args, "--project", str(project)],
        cwd=ROOT,
        capture_output=True,
        text=True,
        check=False,
        timeout=60,
    )


class ToolOutputStoreTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.mod = load()

    def test_triggers_are_not_the_rejected_floors(self):
        self.assertEqual(32768, self.mod.TRIGGER_BYTES)
        self.assertEqual(8192, self.mod.DUMP_TRIGGER_BYTES)
        self.assertEqual({"ultra-lean": 1024, "balanced": 4096, "deep": 8192}, self.mod.BUDGETS)
        self.assertNotIn(self.mod.TRIGGER_BYTES, {4096, 96})
        self.assertNotIn(self.mod.DUMP_TRIGGER_BYTES, {4096, 96})

    def test_over_trigger_is_stored_in_full_and_search_returns_the_planted_line(self):
        head = "Alpha sentence stays whole. " * 400
        planted = f"The planted marker is {PLANTED}. "
        tail = "Tail sentence stays whole. " * 1200
        body = head + planted + tail
        self.assertGreater(len(body.encode("utf-8")), self.mod.TRIGGER_BYTES)
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary)
            row_id = self.mod.index_if_over_trigger(body, name="tool.txt", project=project)
            self.assertIsNotNone(row_id)
            resume = run_tool(project, "output-resume", str(row_id))
            self.assertEqual(0, resume.returncode, resume.stderr)
            self.assertEqual(body, resume.stdout)
            preview = self.mod.replacement_preview(body, name="tool.txt", budget="deep", project=project)
            self.assertNotIn(PLANTED, preview)
            found = run_tool(project, "output-search", PLANTED)
            self.assertEqual(0, found.returncode, found.stderr)
            self.assertIn(PLANTED, found.stdout)
            self.assertIn(planted.strip(), found.stdout)
            stats = run_tool(project, "output-stats")
            self.assertEqual(0, stats.returncode, stats.stderr)
            self.assertIn("engine: fts5", stats.stdout)
            self.assertIn("rows: 2", stats.stdout)

    def test_at_or_under_trigger_is_not_indexed(self):
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary)
            exact = "A" * self.mod.TRIGGER_BYTES
            self.assertIsNone(self.mod.index_if_over_trigger(exact, name="plain.txt", project=project))
            self.assertFalse(self.mod.db_path(project).exists())
            dump = "B" * self.mod.DUMP_TRIGGER_BYTES
            self.assertIsNone(self.mod.index_if_over_trigger(dump, name="surefire-report.xml", project=project))
            self.assertIsNone(self.mod.index_if_over_trigger(dump, name="allure.html", project=project))
            self.assertIsNone(self.mod.index_if_over_trigger(dump, name="run.log", project=project))
            self.assertIsNone(self.mod.index_if_over_trigger(dump, name="trace.har", project=project))
            self.assertFalse(self.mod.db_path(project).exists())

    def test_secret_shaped_body_is_absent_from_the_database(self):
        marker = secret_assignment()
        body = marker + "\n" + ("Noise sentence stays whole. " * 2000)
        self.assertGreater(len(body.encode("utf-8")), self.mod.TRIGGER_BYTES)
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary)
            row_id = self.mod.index_if_over_trigger(body, name="tool.txt", project=project)
            self.assertIsNotNone(row_id)
            blob = b"".join(path.read_bytes() for path in self.mod.db_path(project).parent.iterdir() if path.is_file())
            self.assertNotIn(marker.encode("utf-8"), blob)
            self.assertNotIn(secret_value(), blob)
            self.assertIn(b"redaction-receipt", blob)
            resume = run_tool(project, "output-resume", str(row_id))
            self.assertIn("redaction-receipt", resume.stdout)
            self.assertNotIn(secret_value().decode("utf-8"), resume.stdout)

    def test_preview_does_not_end_mid_sentence_when_the_next_does_not_fit(self):
        first = "The opening sentence fits inside the budget. "
        second = "The following sentence is much longer than the room that remains and must be omitted whole."
        pointer = " [id=7]"
        budget = len(first.encode("utf-8")) + len(pointer.encode("utf-8"))
        preview = self.mod.build_preview(first + second, budget, pointer)
        self.assertEqual(first + pointer, preview)
        self.assertFalse(preview.split(pointer)[0].endswith("The following"))
        frame = '  File "app.py", line 3, in main\n'
        frame_preview = self.mod.build_preview(frame, len(pointer.encode("utf-8")) + 4, pointer)
        self.assertNotIn("line 3", frame_preview)
        self.assertEqual(pointer, frame_preview)
        ready = "Ready sentence. "
        fence = "```\nprint('hello world')\n```\n"
        fence_budget = len(ready.encode("utf-8")) + len(pointer.encode("utf-8"))
        fence_preview = self.mod.build_preview(ready + fence, fence_budget, pointer)
        self.assertEqual(ready + pointer, fence_preview)
        self.assertNotIn("hello", fence_preview)

    def test_preview_does_not_cut_a_sentence_after_a_traceback(self):
        trace = (
            "Traceback (most recent call last):\n"
            "  File \"app.py\", line 3, in main\n"
            "    boom()\n"
        )
        sentence = (
            "The opening of a sentence\n"
            "continues on the next line and must stay whole."
        )
        pointer = " [id=7]"
        body = trace + sentence
        first_line = "The opening of a sentence\n"
        budget = len((trace + first_line).encode("utf-8")) + len(pointer.encode("utf-8"))
        preview = self.mod.build_preview(body, budget, pointer)
        self.assertEqual(trace + pointer, preview)
        self.assertNotIn("The opening of a sentence", preview)
        self.assertEqual(body, self.mod.build_preview(body, len(body.encode("utf-8")) + len(pointer.encode("utf-8")), pointer))

    def test_preview_keeps_a_fence_whole_after_unterminated_prose(self):
        prose = "Command output:\n"
        fence = "```\nError: file not found.\nMore details here.\n```\n"
        sentence = (
            "The opening of a sentence\n"
            "continues on the next line and must stay whole."
        )
        pointer = " [id=7]"
        body = prose + fence + sentence
        partial = "```\nError: file not found.\n"
        budget = len((prose + partial).encode("utf-8")) + len(pointer.encode("utf-8"))
        preview = self.mod.build_preview(body, budget, pointer)
        self.assertEqual(prose + pointer, preview)
        self.assertNotIn("```", preview)
        self.assertNotIn("file not found", preview)
        fit = len((prose + fence).encode("utf-8")) + len(pointer.encode("utf-8"))
        kept = self.mod.build_preview(body, fit, pointer)
        self.assertEqual(prose + fence + pointer, kept)
        self.assertEqual(2, kept.split(pointer)[0].count("```"))

    def test_preview_keeps_a_traceback_whole_after_unterminated_prose(self):
        prose = "Lead in\n"
        trace = (
            "Traceback (most recent call last):\n"
            "  File \"app.py\", line 3, in main\n"
            "    value = \"hello. world\"\n"
        )
        pointer = " [id=7]"
        body = prose + trace
        partial = (
            "Traceback (most recent call last):\n"
            "  File \"app.py\", line 3, in main\n"
            "    value = \"hello. "
        )
        budget = len((prose + partial).encode("utf-8")) + len(pointer.encode("utf-8"))
        preview = self.mod.build_preview(body, budget, pointer)
        self.assertEqual(prose + pointer, preview)
        self.assertNotIn("hello", preview)
        self.assertEqual(
            body,
            self.mod.build_preview(body, len(body.encode("utf-8")) + len(pointer.encode("utf-8")), pointer),
        )

    def test_store_error_returns_the_original_text(self):
        text = "Original sentence stays intact. " * 2000
        self.assertGreater(len(text.encode("utf-8")), self.mod.TRIGGER_BYTES)
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary)
            blocked = self.mod.db_path(project)
            blocked.parent.mkdir(parents=True)
            blocked.mkdir()
            self.assertEqual(text, self.mod.replacement_preview(text, name="tool.txt", project=project))
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary)
            real = self.mod.store_text

            def fail(*_args, **_kwargs):
                raise sqlite3.OperationalError("fts5 simulated failure")

            self.mod.store_text = fail
            try:
                self.assertEqual(text, self.mod.replacement_preview(text, name="tool.txt", project=project))
            finally:
                self.mod.store_text = real

    def test_purge_honors_delete_check(self):
        body = "Purge sentence stays whole. " * 2000
        with tempfile.TemporaryDirectory() as temporary:
            allowed = Path(temporary) / "proj"
            allowed.mkdir()
            self.mod.index_if_over_trigger(body, name="tool.txt", project=allowed)
            path = self.mod.db_path(allowed)
            self.assertTrue(path.is_file())
            removed = run_tool(allowed, "output-purge")
            self.assertEqual(0, removed.returncode, removed.stderr)
            self.assertFalse(path.exists())
        with tempfile.TemporaryDirectory() as temporary:
            refused = Path(temporary) / "yt" / "proj"
            refused.mkdir(parents=True)
            self.mod.index_if_over_trigger(body, name="tool.txt", project=refused)
            path = self.mod.db_path(refused)
            before = path.read_bytes()
            result = run_tool(refused, "output-purge")
            self.assertNotEqual(0, result.returncode)
            self.assertTrue(path.is_file())
            self.assertEqual(before, path.read_bytes())
            self.assertIn("refused", result.stderr.lower())

    def test_help_lists_the_output_verbs(self):
        completed = subprocess.run(  # nosec B603 - fixed tool.py --help argv.
            [sys.executable, str(TOOL), "--help"],
            cwd=ROOT,
            capture_output=True,
            text=True,
            check=False,
            timeout=60,
        )
        self.assertEqual(0, completed.returncode, completed.stderr)
        for verb in ("output-search", "output-resume", "output-stats", "output-purge"):
            self.assertIn(verb, completed.stdout)


LOCATOR = "tool-output: ready; python3 .chaos-engine/tool.py output-search QUERY"


def load_named(name: str, relative: str):
    spec = importlib.util.spec_from_file_location(name, ROOT / relative)
    if spec is None or spec.loader is None:
        raise ImportError(relative)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


class ToolOutputLocatorTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.store = load()
        cls.lifecycle = load_named("ce_lifecycle_7976", "chaos-engine/hooks/lifecycle.py")

    def test_locator_appears_only_when_the_store_has_a_row(self):
        previous = Path.cwd()
        try:
            with tempfile.TemporaryDirectory() as temporary:
                empty = Path(temporary) / "empty"
                empty.mkdir()
                os.chdir(empty)
                missing = self.lifecycle.session_start_context("token", "activation")
                self.assertNotIn("tool-output:", missing)
                self.assertFalse((empty / ".chaos-engine-state" / "tool-output" / "output.db").exists())
                self.store.connect(empty)
                still_empty = self.lifecycle.session_start_context("token", "activation")
                self.assertNotIn("tool-output:", still_empty)
                self.assertFalse(self.store.has_row(empty))
                planted = Path(temporary) / "planted"
                planted.mkdir()
                os.chdir(planted)
                self.store.store_text("One planted sentence.", name="tool.txt", project=planted)
                self.assertTrue(self.store.has_row(planted))
                context = self.lifecycle.session_start_context("token", "activation")
                again = self.lifecycle.session_start_context("token", "activation")
                self.assertEqual(context, again)
                self.assertIn(LOCATOR, context.splitlines())
                self.assertNotIn("\n", LOCATOR)
                self.assertLess(len(LOCATOR.encode("utf-8")), 256)
                self.assertNotRegex(LOCATOR, r"\d{4}-\d{2}-\d{2}")
                self.assertLessEqual(len(context.encode("utf-8")), self.lifecycle.SESSION_START_MAX_BYTES)
                real_cap = self.lifecycle.SESSION_START_MAX_BYTES
                self.lifecycle.SESSION_START_MAX_BYTES = len(context.encode("utf-8")) - 1
                try:
                    capped = self.lifecycle.session_start_context("token", "activation")
                finally:
                    self.lifecycle.SESSION_START_MAX_BYTES = real_cap
                self.assertNotIn(LOCATOR, capped)
        finally:
            os.chdir(previous)


def block_reasons(stdout: str) -> list[str]:
    reasons = []
    for line in stdout.splitlines():
        try:
            payload = json.loads(line)
        except json.JSONDecodeError:
            continue
        if isinstance(payload, dict) and payload.get("decision") == "block":
            reasons.append(str(payload.get("reason") or ""))
    return reasons


class LargeReadDenyTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.guard = load_named("ce_guard_7976_read", "chaos-engine/hooks/guard.py")

    def _run(self, event: dict, process_cwd: Path | None = None) -> tuple[int, str, str]:
        previous = Path.cwd()
        os.chdir(process_cwd or Path(event["cwd"]))
        stdout = io.StringIO()
        stderr = io.StringIO()
        try:
            with contextlib.redirect_stdout(stdout), contextlib.redirect_stderr(stderr):
                code = self.guard._guarded_event(event, "claude")
        finally:
            os.chdir(previous)
        return code, stdout.getvalue(), stderr.getvalue()

    def _read(self, root: Path, path: Path, tool_name: str = "Read", **extra: object) -> dict:
        tool_input = {"file_path": str(path)}
        tool_input.update(extra)
        return {
            "hook_event_name": "PreToolUse",
            "tool_name": tool_name,
            "session_id": "7976-read",
            "cwd": str(root),
            "tool_input": tool_input,
        }

    def _assert_denied(self, event: dict, size: int) -> None:
        code, stdout, stderr = self._run(event)
        self.assertEqual(2, code, stdout + stderr)
        reasons = block_reasons(stdout)
        self.assertEqual(1, len(reasons), stdout + stderr)
        reason = reasons[0]
        self.assertIn(str(size), reason)
        self.assertIn("ranged Read", reason)
        self.assertIn("rg", reason)
        self.assertIn("tail", reason)
        self.assertIn("script that prints only the answer", reason)

    def _assert_allowed(self, event: dict) -> None:
        code, stdout, stderr = self._run(event)
        self.assertEqual(0, code, stdout + stderr)
        self.assertEqual([], block_reasons(stdout), stdout + stderr)

    def test_pretooluse_denies_unranged_dumps_and_huge_files_only(self):
        economy = (CE / "references/context-economy.md").read_text(encoding="utf-8")
        self.assertIn("rules/large_read_deny.py", economy)
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            for name in ("report.html", "report.xml", "run.log", "trace.har", "Report.HTML"):
                path = root / name
                path.write_bytes(b"x" * 8192)
                self._assert_denied(self._read(root, path), 8192)
            small = root / "small.html"
            small.write_bytes(b"x" * 8191)
            self._assert_allowed(self._read(root, small))
            java = root / "App.java"
            java.write_bytes(b"x" * (40 * 1024))
            self.assertEqual(40 * 1024, java.stat().st_size)
            self._assert_allowed(self._read(root, java))
            borderline = root / "Border.java"
            borderline.write_bytes(b"x" * 8192)
            self._assert_allowed(self._read(root, borderline))
            huge_java = root / "Huge.java"
            huge_java.write_bytes(b"x" * 262144)
            self._assert_denied(self._read(root, huge_java), 262144)
            huge_text = root / "notes.txt"
            huge_text.write_bytes(b"x" * 262144)
            self._assert_denied(self._read(root, huge_text), 262144)
            under = root / "under.txt"
            under.write_bytes(b"x" * 262143)
            self._assert_allowed(self._read(root, under))
            ranged = root / "ranged.html"
            ranged.write_bytes(b"x" * 8192)
            self._assert_allowed(self._read(root, ranged, offset=1, limit=20))
            self._assert_allowed(self._read(root, huge_java, startLine=1, endLine=40))
            viewed = root / "viewed.log"
            viewed.write_bytes(b"x" * 9000)
            self._assert_allowed(self._read(root, viewed, view_range=[1, 10]))
            self._assert_denied(self._read(root, viewed, offset=""), 9000)
            elsewhere = root / "elsewhere"
            elsewhere.mkdir()
            relative = {
                "hook_event_name": "PreToolUse",
                "tool_name": "read_file",
                "session_id": "7976-read",
                "cwd": str(root),
                "tool_input": {"filePath": "report.html"},
            }
            code, stdout, stderr = self._run(relative, process_cwd=elsewhere)
            self.assertEqual(2, code, stdout + stderr)
            self.assertIn("8192", block_reasons(stdout)[0])
            missing = root / "gone.html"
            self._assert_allowed(self._read(root, missing))
            self._assert_allowed(
                {
                    "hook_event_name": "PreToolUse",
                    "tool_name": "Grep",
                    "session_id": "7976-grep",
                    "cwd": str(root),
                    "tool_input": {"path": str(viewed), "pattern": "x"},
                }
            )
            safety = {
                "hook_event_name": "PreToolUse",
                "tool_name": "Bash",
                "session_id": "7976-rm",
                "cwd": str(root),
                "tool_input": {"command": "rm -rf /"},
            }
            code, stdout, stderr = self._run(safety)
            self.assertEqual(2, code, stdout + stderr)
            self.assertNotIn("ranged Read", stdout)


if __name__ == "__main__":
    unittest.main()
