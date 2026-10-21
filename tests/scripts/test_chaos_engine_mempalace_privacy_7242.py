"""#7242 chunk 6: the MemPalace privacy check finds private data without echoing it."""

from __future__ import annotations

import contextlib
import importlib.util
import io
import os
import sqlite3
import tempfile
import unittest
from pathlib import Path
from unittest import mock

ROOT = Path(__file__).resolve().parents[2]
CE = ROOT / "chaos-engine"


def load(path: Path, name: str):
    specification = importlib.util.spec_from_file_location(name, path)
    module = importlib.util.module_from_spec(specification)
    specification.loader.exec_module(module)
    return module


class MempalacePrivacyTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.privacy = load(CE / "mempalace_privacy.py", "ce_privacy_7242")

    def _palace(self, tmp: Path, documents: list[str]) -> Path:
        palace = tmp / "palace"
        palace.mkdir()
        connection = sqlite3.connect(palace / "sqlite_exact.sqlite3")
        connection.execute("create table documents (id text, document text)")
        connection.executemany("insert into documents values (?, ?)",
                               [(f"d{i}", text) for i, text in enumerate(documents)])
        connection.commit()
        connection.close()
        return palace

    def _status(self, tmp: Path, documents: list[str]):
        palace = self._palace(tmp, documents)
        with mock.patch.dict(os.environ, {"CHAOS_ENGINE_MEMPALACE": str(palace)}):
            return self.privacy.status(tmp)

    def test_clean_palace_passes(self):
        with tempfile.TemporaryDirectory() as tmp:
            self.assertEqual("pass", self._status(Path(tmp), ["Fluid Engine block spacing."])[0])

    def test_generic_email_and_money_fail(self):
        with tempfile.TemporaryDirectory() as tmp:
            verdict, detail = self._status(Path(tmp), ["mail trainee@example.com", "fee 4,500 EGP"])
        self.assertEqual("fail", verdict)
        self.assertIn("email=1", detail)
        self.assertIn("money=1", detail)

    def test_private_terms_file_and_allow_list(self):
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            (root / ".chaos-engine-state").mkdir()
            (root / ".chaos-engine-state/privacy-terms.txt").write_text("# clients\nAcmeCorp\n", encoding="utf-8")
            (root / ".chaos-engine-state/privacy-allow.txt").write_text("info@example.org\n", encoding="utf-8")
            verdict, detail = self._status(root, ["worked with acmecorp", "contact info@example.org"])
        self.assertEqual(("fail", "term-1=1"), (verdict, detail))

    def test_output_never_echoes_the_matched_text(self):
        with tempfile.TemporaryDirectory() as tmp:
            palace = self._palace(Path(tmp), ["secret.person@example.com"])
            out = io.StringIO()
            with mock.patch.dict(os.environ, {"CHAOS_ENGINE_MEMPALACE": str(palace)}), contextlib.redirect_stdout(out):
                code = self.privacy.main(["--project", tmp])
        self.assertEqual(1, code)
        self.assertNotIn("secret.person", out.getvalue())

    def test_fresh_install_without_a_palace_is_none(self):
        with tempfile.TemporaryDirectory() as tmp, mock.patch.dict(
                os.environ, {"CHAOS_ENGINE_MEMPALACE": str(Path(tmp) / "absent")}):
            self.assertEqual("none", self.privacy.status(Path(tmp))[0])


if __name__ == "__main__":
    unittest.main()
