"""Skip SHAFT tests that still read the removed chaos-engine/ tree (#8057 e1).

ChaosEngine moved to ShaftHQ/chaosengine-harness. Tests listed in
moved_quarantine.txt (module, class or test id prefixes) still read
chaos-engine/ and are skipped with a reason until #8057 e2 ports or drops
them. Works under unittest and pytest because both run TestCase.run.
"""

from __future__ import annotations

import unittest
from pathlib import Path

LIST = Path(__file__).with_name("moved_quarantine.txt")
REASON = "moved to ShaftHQ/chaosengine-harness; port or drop in #8057 e2"


def entries() -> tuple[str, ...]:
    if not LIST.is_file():
        return ()
    lines = (line.split("#", 1)[0].strip() for line in LIST.read_text(encoding="utf-8").splitlines())
    return tuple(line for line in lines if line)


def quarantined(test_id: str, prefixes: tuple[str, ...]) -> bool:
    return any(test_id == prefix or test_id.startswith(prefix + ".") for prefix in prefixes)


def install() -> None:
    prefixes = entries()
    if not prefixes or getattr(unittest.TestCase.run, "_moved_quarantine", False):
        return
    original = unittest.TestCase.run

    def run(self, result=None):
        if quarantined(self.id(), prefixes):
            def skipped():
                pass  # replaced by the skip below

            skipped.__unittest_skip__ = True
            skipped.__unittest_skip_why__ = REASON
            setattr(self, self._testMethodName, skipped)
        return original(self, result)

    run._moved_quarantine = True
    unittest.TestCase.run = run
