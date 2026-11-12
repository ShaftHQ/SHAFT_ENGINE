"""Manual tool-output store: full text, no host replacement, no hook."""

from __future__ import annotations

import importlib.util
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


if __name__ == "__main__":
    unittest.main()
