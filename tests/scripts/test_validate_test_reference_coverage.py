"""Issue #6375: the test-reference ratchet only shrinks and matches the repo."""

import importlib.util
import tempfile
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
SCRIPT = ROOT / "scripts" / "ci" / "validate_test_reference_coverage.py"


def _load():
    spec = importlib.util.spec_from_file_location("_test_reference_6375", SCRIPT)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


class TestReferenceCoverageTest(unittest.TestCase):
    def setUp(self):
        self.module = _load()

    def _write(self, root: Path, relative: str, text: str) -> None:
        path = root / relative
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(text, encoding="utf-8")

    def test_untested_member_is_counted_and_called_member_is_not(self):
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            self._write(root, "m/src/main/java/a/Api.java",
                        "public class Api {\n    public void used() {}\n    public void unused() {}\n}\n")
            self._write(root, "m/src/test/java/a/ApiTest.java",
                        "class ApiTest { void t() { new Api().used(); } }\n")
            config = {"exclusions": [], "included_overrides": []}
            self.assertEqual({"m/src/main/java/a/Api.java": 1}, self.module.scan(root, config))

    def test_method_reference_counts_as_a_call(self):
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            self._write(root, "m/src/main/java/a/Api.java", "public class Api {\n    public void viaRef() {}\n}\n")
            self._write(root, "m/src/test/java/a/ApiTest.java", "class ApiTest { Runnable r = api::viaRef; }\n")
            self.assertEqual({}, self.module.scan(root, {"exclusions": [], "included_overrides": []}))

    def test_compare_flags_regressions_and_stale_entries(self):
        regressions, stale = self.module.compare({"a": 2, "b": 1}, {"a": 1, "c": 1})
        self.assertEqual(["a: 2 public members no test calls (baseline 1)",
                          "b: 1 public members no test calls (baseline 0)"], regressions)
        self.assertEqual(["c: baseline 1, now 0"], stale)

    def test_repository_matches_its_baseline(self):
        self.assertEqual(0, self.module.main(["--root", str(ROOT)]))


if __name__ == "__main__":
    unittest.main()
