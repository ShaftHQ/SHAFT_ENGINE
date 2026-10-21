"""#7242 chunk 5b: a bot's work checkpoint is refused until a retrieve is recorded since entry."""

from __future__ import annotations

import importlib.util
import tempfile
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
CE = ROOT / "chaos-engine"
STUB = (
    "from pathlib import Path\n"
    "def retrieved_since(project, since):\n"
    "    marker = Path(project) / 'retrieved'\n"
    "    return marker.is_file() and float(marker.read_text()) >= since\n"
)


def load(path: Path, name: str):
    specification = importlib.util.spec_from_file_location(name, path)
    module = importlib.util.module_from_spec(specification)
    specification.loader.exec_module(module)
    return module


class RetrieveMarkerTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.tool = load(CE / "tool.py", "ce_tool_retrieve_7242")

    def _installed(self, tmp: Path, entry_time: int) -> Path:
        root = tmp / ".chaos-engine"
        (root / "hooks").mkdir(parents=True)
        (root / "runtime").mkdir()
        (root / "hooks/retrieve_justification.py").write_text(STUB, encoding="utf-8")
        (root / "runtime/entry-stamp").write_text(f"abc {entry_time}", encoding="utf-8")
        return root

    def test_work_checkpoint_is_owed_without_a_retrieve_since_entry(self):
        with tempfile.TemporaryDirectory() as tmp:
            root = self._installed(Path(tmp), 1000)
            self.assertTrue(self.tool.retrieve_owed(root, ["TASK", "step"]))
            (Path(tmp) / "retrieved").write_text("999", encoding="utf-8")
            self.assertTrue(self.tool.retrieve_owed(root, ["TASK", "step"]), "retrieve before entry is stale")
            (Path(tmp) / "retrieved").write_text("1001", encoding="utf-8")
            self.assertFalse(self.tool.retrieve_owed(root, ["TASK", "step"]))

    def test_start_budget_and_done_are_never_gated(self):
        with tempfile.TemporaryDirectory() as tmp:
            root = self._installed(Path(tmp), 1000)
            for arguments in (["TASK", "--start"], ["TASK", "--budget"], ["TASK", "done"]):
                with self.subTest(arguments=arguments):
                    self.assertFalse(self.tool.retrieve_owed(root, arguments))

    def test_refusal_names_the_retrieve_command(self):
        self.assertIn("tool.py retrieve --store graphify|mempalace", self.tool.RETRIEVE_REFUSAL)


if __name__ == "__main__":
    unittest.main()
