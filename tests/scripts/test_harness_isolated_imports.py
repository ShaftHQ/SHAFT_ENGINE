"""Every harness gate test module imports under ``python3 -I`` (no PYTHONPATH, no cwd on sys.path)."""

from __future__ import annotations

import re
import subprocess  # nosec B404 - fixed list-form argv, never a shell.
import sys
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
GATE = ROOT / "scripts" / "ci" / "harness_pr_gate.py"
# Third-party packages live in user site-packages, which -I hides; that is environment, not imports.
THIRD_PARTY = re.compile(r"No module named '(?:yaml|jsonschema|requests)'")
PROBE = "import runpy, sys; sys.path.insert(0, sys.argv[1].rsplit('/', 1)[0]); runpy.run_path(sys.argv[1], run_name='isolated')"


def gate_modules() -> list[Path]:
    names = sorted(set(re.findall(r'"(tests\.scripts\.test_\w+)"', GATE.read_text(encoding="utf-8"))))
    return [path for path in (ROOT / (name.replace(".", "/") + ".py") for name in names) if path.is_file()]


class IsolatedImportsTest(unittest.TestCase):
    def test_gate_modules_self_locate(self) -> None:
        modules = gate_modules()
        self.assertGreater(len(modules), 50)
        broken = []
        for path in modules:
            run = subprocess.run([sys.executable, "-I", "-c", PROBE, str(path)], cwd="/", capture_output=True,
                                 text=True, timeout=180, check=False)
            last = (run.stderr.strip().splitlines() or [""])[-1]
            if run.returncode and "ModuleNotFoundError" in last and not THIRD_PARTY.search(last):
                broken.append(f"{path.relative_to(ROOT)}: {last}")
        self.assertEqual([], broken, "add the self-locating sys.path line (#7197) before repo imports")


if __name__ == "__main__":
    unittest.main()
