"""Instruction-file merge and handoff contract for the installer program."""

from __future__ import annotations

import importlib.util
import tempfile
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
HOSTS = ROOT / "chaos-engine" / "hosts.py"


def load_hosts():
    spec = importlib.util.spec_from_file_location("ce_merge_handoff_hosts", HOSTS)
    if spec is None or spec.loader is None:
        raise RuntimeError(f"cannot load {HOSTS}")
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


class InstructionMergeHandoffTest(unittest.TestCase):
    def setUp(self) -> None:
        self.hosts = load_hosts()
        self.hosts.consume_merge_handoffs()

    def test_absent_markers_append_the_owned_block(self) -> None:
        merged = self.hosts.merge_instruction(
            b"foreign notes\n",
            self.hosts.INSTRUCTION,
            relative="AGENTS.md",
        )
        text = merged.decode("utf-8")
        self.assertIn("foreign notes\n", text)
        self.assertIn(self.hosts.START, text)
        self.assertEqual(self.hosts.consume_merge_handoffs(), [])

    def test_current_span_is_replaced_without_handoff(self) -> None:
        before = ("keep\n" + self.hosts.INSTRUCTION + "tail\n").encode()
        merged = self.hosts.merge_instruction(
            before, self.hosts.INSTRUCTION, relative="AGENTS.md"
        )
        text = merged.decode("utf-8")
        self.assertIn("keep\n", text)
        self.assertIn("tail\n", text)
        self.assertEqual(text.count(self.hosts.START), 1)
        self.assertEqual(self.hosts.consume_merge_handoffs(), [])

    def test_edited_span_is_left_unchanged_and_handed_off(self) -> None:
        original = (
            "foreign\n"
            f"{self.hosts.START}\noperator edited this span\n{self.hosts.END}\n"
        ).encode()
        merged = self.hosts.merge_instruction(
            original, self.hosts.INSTRUCTION, relative="AGENTS.md"
        )
        self.assertEqual(merged, original)
        notes = self.hosts.consume_merge_handoffs()
        self.assertEqual(len(notes), 1)
        self.assertEqual(notes[0]["path"], "AGENTS.md")
        self.assertIn("interior", notes[0]["reason"])

    def test_handoff_markdown_names_the_path_and_prompt(self) -> None:
        with tempfile.TemporaryDirectory() as raw:
            project = Path(raw)
            self.hosts.merge_instruction(
                f"{self.hosts.START}\nedited\n{self.hosts.END}\n".encode(),
                self.hosts.INSTRUCTION,
                relative="CLAUDE.md",
            )
            written = self.hosts.write_merge_handoff(
                project, "python3 .chaos-engine/install.py doctor --project ."
            )
            self.assertIsNotNone(written)
            body = written.read_text(encoding="utf-8")
            self.assertIn("## CLAUDE.md", body)
            self.assertIn("Desired owned block:", body)
            prompt = self.hosts.merge_handoff_prompt(
                "python3 .chaos-engine/install.py doctor --project ."
            )
            self.assertTrue(prompt.startswith("Merge ChaosEngine host configuration"))
            self.assertIn(".chaos-engine-state/merge-handoff.md", prompt)
            self.assertNotIn("\n", prompt)


if __name__ == "__main__":
    unittest.main()


class InstallProfileSelectionTest(unittest.TestCase):
    def setUp(self) -> None:
        spec = importlib.util.spec_from_file_location(
            "ce_profile_install", ROOT / "chaos-engine" / "install.py"
        )
        if spec is None or spec.loader is None:
            raise RuntimeError("cannot load install.py")
        self.install = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(self.install)

    def test_empty_and_non_java_stay_portable_java_shaft_selects_repository(self) -> None:
        source = ROOT / "chaos-engine"
        with tempfile.TemporaryDirectory() as raw:
            root = Path(raw)
            empty = root / "empty"
            empty.mkdir()
            non_java = root / "node"
            non_java.mkdir()
            (non_java / "package.json").write_text("{}", encoding="utf-8")
            java = root / "shaft"
            java.mkdir()
            (java / "pom.xml").write_text(
                "<project><artifactId>shaft-engine</artifactId></project>\n",
                encoding="utf-8",
            )
            self.assertEqual(self.install.detect_distribution(empty, source), "portable")
            self.assertEqual(self.install.detect_distribution(non_java, source), "portable")
            self.assertEqual(self.install.detect_distribution(java, source), "repository")
