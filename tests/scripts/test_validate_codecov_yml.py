"""Tests for scripts/ci/validate_codecov_yml.py (#6990 lesson)."""
from __future__ import annotations

import importlib.util
import io
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
SPEC = importlib.util.spec_from_file_location("validate_codecov_yml", ROOT / "scripts/ci/validate_codecov_yml.py")
validator = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(validator)


class _Response(io.BytesIO):
    def __enter__(self):
        return self

    def __exit__(self, *exc):
        return False


class ValidateCodecovYmlTest(unittest.TestCase):
    def test_unknown_field_is_rejected(self) -> None:
        self.assertEqual((False, "Error at ['parsers', 'xml']: "),
                         validator.verdict("Error at ['parsers', 'xml']: \nunknown field\n"))

    def test_valid_passes(self) -> None:
        self.assertTrue(validator.verdict("Valid!\n\n{}")[0])

    def test_posts_the_file_body(self) -> None:
        seen = {}

        def opener(request, timeout):
            seen["data"], seen["url"] = request.data, request.full_url
            return _Response(b"Valid!")

        self.assertEqual("Valid!", validator.validate(ROOT / "codecov.yml", opener=opener))
        self.assertEqual("https://codecov.io/validate", seen["url"])
        self.assertEqual((ROOT / "codecov.yml").read_bytes(), seen["data"])

    def test_wired_into_pre_push_and_ci(self) -> None:
        self.assertIn("validate_codecov_yml", (ROOT / "scripts/ci/overlay_pre_push.py").read_text(encoding="utf-8"))
        self.assertIn("scripts/ci/validate_codecov_yml.py", (ROOT / ".github/workflows/pr-gate.yml").read_text(encoding="utf-8"))


if __name__ == "__main__":
    unittest.main()
