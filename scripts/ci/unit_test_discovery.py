"""Case-sensitive *UnitTest.java discovery shared by PR Gate validators (#7627).

``Path.rglob("*UnitTest.java")`` follows the host filesystem's case rules, so on
Windows it also matches names like ``JunitTest.java``. Callers must keep only
paths whose final component ends with the exact ``UnitTest.java`` suffix.
"""

from __future__ import annotations

from collections.abc import Iterator
from pathlib import Path

UNIT_TEST_SUFFIX = "UnitTest.java"


def iter_unit_test_sources(test_root: Path) -> Iterator[Path]:
    """Yield ``*UnitTest.java`` sources under ``test_root`` with a case-sensitive suffix."""
    for path in sorted(test_root.rglob(f"*{UNIT_TEST_SUFFIX}")):
        if path.name.endswith(UNIT_TEST_SUFFIX):
            yield path


def is_unit_test_source_name(name: str) -> bool:
    """True when ``name`` (a file name or path) has a case-sensitive UnitTest.java suffix."""
    return Path(name).name.endswith(UNIT_TEST_SUFFIX)
