import json
import tempfile
import unittest
from pathlib import Path

import sys as _sys; from pathlib import Path as _Path; _sys.path.insert(0, str(_Path(__file__).resolve().parents[2]))  # noqa: E402,E702 - self-locating under python3 -I (#7197)
from scripts.ci import jmh_compare


def _write(folder: Path, name: str, values: dict[str, float]) -> Path:
    path = folder / name
    path.write_text(json.dumps([{"benchmark": f"com.x.B.{k}", "primaryMetric": {"score": v}} for k, v in values.items()]))
    return path


class JmhCompareTest(unittest.TestCase):
    def setUp(self):
        self._tmp = tempfile.TemporaryDirectory()
        self.addCleanup(self._tmp.cleanup)
        self.dir = Path(self._tmp.name)

    def test_regression_beyond_threshold_fails(self):
        base = _write(self.dir, "b.json", {"parse": 1.0, "norm": 2.0})
        cur = _write(self.dir, "c.json", {"parse": 1.2, "norm": 2.0})
        self.assertEqual(1, jmh_compare.main([str(base), str(cur)]))

    def test_within_threshold_and_new_benchmarks_pass(self):
        base = _write(self.dir, "b.json", {"parse": 1.0})
        cur = _write(self.dir, "c.json", {"parse": 1.1, "added": 9.0})
        self.assertEqual(0, jmh_compare.main([str(base), str(cur)]))

    def test_missing_baseline_passes(self):
        cur = _write(self.dir, "c.json", {"parse": 1.0})
        self.assertEqual(0, jmh_compare.main([str(self.dir / "none.json"), str(cur)]))


if __name__ == "__main__":
    unittest.main()
