"""#7178: allure_compose keeps attachments, renames clashing containers and refuses /tmp or missing files."""
from __future__ import annotations

import importlib.util
import json
import tempfile
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
SCRIPT = ROOT / "shaft-skills/shaft-execution-reporting/scripts/allure_compose.py"


def load():
    spec = importlib.util.spec_from_file_location("allure_compose_7178", SCRIPT)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def run_folder(root: Path, name: str, test: str) -> Path:
    folder = root / name / "allure-results"
    folder.mkdir(parents=True)
    result = {"uuid": f"r-{name}", "fullName": test, "parameters": [],
              "steps": [{"attachments": [{"source": f"{name}-shot.png"}]}]}
    (folder / f"r-{name}-result.json").write_text(json.dumps(result), encoding="utf-8")
    (folder / f"{name}-shot.png").write_bytes(b"png")
    container = {"uuid": "same-container", "children": [f"r-{name}"],
                 "befores": [{"attachments": [{"source": f"{name}-setup.txt"}]}]}
    (folder / "same-container-container.json").write_text(json.dumps(container), encoding="utf-8")
    (folder / f"{name}-setup.txt").write_text("log", encoding="utf-8")
    return folder


class AllureComposeTest(unittest.TestCase):
    def setUp(self):
        self.compose = load()
        scratch = ROOT / "target" / "test-7178"  # evidence must not live under /tmp, so neither may the test
        scratch.mkdir(parents=True, exist_ok=True)
        self.temp = tempfile.TemporaryDirectory(dir=scratch)
        self.root = Path(self.temp.name)
        self.first = run_folder(self.root, "en", "T.en")
        self.second = run_folder(self.root, "ar", "T.ar")

    def tearDown(self):
        self.temp.cleanup()

    def test_two_runs_keep_every_attachment_and_get_distinct_containers(self):
        out = self.root / "target/evidence/allure-results"
        self.assertEqual(2, self.compose.compose(out, [self.first, self.second]))
        names = {path.name for path in out.iterdir()}
        self.assertLessEqual({"en-shot.png", "ar-shot.png", "en-setup.txt", "ar-setup.txt"}, names)
        containers = {json.loads(p.read_text())["uuid"] for p in out.glob("*-container.json")}
        self.assertEqual(2, len(containers))
        self.assertNotIn("same-container", containers)

    def test_out_under_tmp_is_refused(self):
        with self.assertRaises(self.compose.ComposeError):
            self.compose.compose(Path("/tmp/ce-7178-evidence"), [self.first])  # nosec B108 - asserted refusal

    def test_a_missing_attachment_fails_without_writing_output(self):
        (self.second / "ar-shot.png").unlink()
        out = self.root / "target/evidence/allure-results"
        self.assertEqual(1, self.compose.main(["--out", str(out), str(self.first), str(self.second)]))
        self.assertFalse(out.exists())


if __name__ == "__main__":
    unittest.main()
