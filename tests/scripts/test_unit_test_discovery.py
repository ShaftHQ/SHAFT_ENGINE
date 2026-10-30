"""Case-sensitive UnitTest.java discovery (#7627)."""

from __future__ import annotations

import tempfile
import unittest
from pathlib import Path

from scripts.ci.unit_test_discovery import is_unit_test_source_name, iter_unit_test_sources


class UnitTestDiscoveryTest(unittest.TestCase):
    def test_suffix_is_case_sensitive(self) -> None:
        self.assertTrue(is_unit_test_source_name("NamedUnitTest.java"))
        self.assertTrue(is_unit_test_source_name("com/x/ForgottenUnitTest.java"))
        self.assertFalse(is_unit_test_source_name("JunitTest.java"))
        self.assertFalse(is_unit_test_source_name("junitTestPackage/JunitTest.java"))
        self.assertFalse(is_unit_test_source_name("BrowserE2eTest.java"))

    def test_iter_skips_junit_test_even_when_rglob_would_match(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            for relative in (
                "com/x/NamedUnitTest.java",
                "junitTestPackage/JunitTest.java",
            ):
                path = root / relative
                path.parent.mkdir(parents=True, exist_ok=True)
                path.write_text("class X {}", encoding="utf-8")
            found = [path.relative_to(root).as_posix() for path in iter_unit_test_sources(root)]
            self.assertEqual(found, ["com/x/NamedUnitTest.java"])


if __name__ == "__main__":
    unittest.main()
