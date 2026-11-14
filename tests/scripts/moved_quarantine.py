"""Skip SHAFT tests that still read the removed chaos-engine/ tree (#8057 e1).

ChaosEngine moved to ShaftHQ/chaosengine-harness. moved_quarantine.txt lists
what still reads chaos-engine/ and is skipped with a reason until #8057 e2
ports or drops it:

- ``module tests.scripts.x``: the module no longer imports; it is replaced by
  one skipped test (and pytest does not collect it).
- ``class tests.scripts.x.Y``: the class fixture reads the tree; the class is
  skipped.
- ``tests.scripts.x.Y.test_z``: that test is skipped.

Works under unittest and pytest: both import through ``sys.meta_path`` and run
``TestCase.run``.
"""

from __future__ import annotations

import importlib.abc
import sys
import unittest
from pathlib import Path

LIST = Path(__file__).with_name("moved_quarantine.txt")
REASON = "moved to ShaftHQ/chaosengine-harness; port or drop in #8057 e2"


def _lines() -> list[str]:
    if not LIST.is_file():
        return []
    lines = (line.split("#", 1)[0].strip() for line in LIST.read_text(encoding="utf-8").splitlines())
    return [line for line in lines if line]


def entries() -> tuple[set[str], set[str], tuple[str, ...]]:
    modules: set[str] = set()
    classes: set[str] = set()
    tests: list[str] = []
    for line in _lines():
        kind, _, rest = line.partition(" ")
        if kind == "module":
            modules.add(rest.strip())
        elif kind == "class":
            classes.add(rest.strip())
        else:
            tests.append(line)
    return modules, classes, tuple(tests)


def ignored_files() -> list[str]:
    """Module entries as file names for pytest's collect_ignore."""
    return [module.rsplit(".", 1)[-1] + ".py" for module in entries()[0]]


def _skip(cls: type) -> None:
    cls.__unittest_skip__ = True
    cls.__unittest_skip_why__ = REASON


class _Loader(importlib.abc.Loader):
    def __init__(self, real, stub: bool, classes: set[str]):
        self.real, self.stub, self.classes = real, stub, classes

    def create_module(self, spec):
        return self.real.create_module(spec)

    def exec_module(self, module):
        try:
            self.real.exec_module(module)
        except Exception:  # noqa: BLE001 - only listed modules; any failure is the moved tree.
            if not self.stub:
                raise
            for key in [key for key in vars(module) if not key.startswith("__")]:
                delattr(module, key)

            class MovedToHarness(unittest.TestCase):
                def test_moved(self):
                    """Placeholder; skipped."""

            MovedToHarness.__module__ = module.__name__
            _skip(MovedToHarness)
            module.MovedToHarness = MovedToHarness
            return
        for name in self.classes:
            cls = getattr(module, name, None)
            if isinstance(cls, type):
                _skip(cls)


class _Finder(importlib.abc.MetaPathFinder):
    def __init__(self, modules: set[str], classes: set[str]):
        self.modules = modules
        self.classes: dict[str, set[str]] = {}
        for entry in classes:
            module, _, name = entry.rpartition(".")
            self.classes.setdefault(module, set()).add(name)

    def find_spec(self, fullname, path=None, target=None):
        if fullname not in self.modules and fullname not in self.classes:
            return None
        for finder in sys.meta_path:
            if finder is self or not hasattr(finder, "find_spec"):
                continue
            spec = finder.find_spec(fullname, path, target)
            if spec is not None and spec.loader is not None:
                spec.loader = _Loader(spec.loader, fullname in self.modules, self.classes.get(fullname, set()))
                return spec
        return None


def quarantined(test_id: str, prefixes: tuple[str, ...]) -> bool:
    return any(test_id == prefix or test_id.startswith(prefix + ".") for prefix in prefixes)


def install() -> None:
    modules, classes, tests = entries()
    if (modules or classes) and not any(isinstance(finder, _Finder) for finder in sys.meta_path):
        sys.meta_path.insert(0, _Finder(modules, classes))
    if not tests or getattr(unittest.TestCase.run, "_moved_quarantine", False):
        return
    original = unittest.TestCase.run

    def run(self, result=None):
        if quarantined(self.id(), tests):
            def skipped():
                """Placeholder; skipped."""

            skipped.__unittest_skip__ = True
            skipped.__unittest_skip_why__ = REASON
            setattr(self, self._testMethodName, skipped)
        return original(self, result)

    run._moved_quarantine = True
    unittest.TestCase.run = run
