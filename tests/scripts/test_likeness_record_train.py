"""Owner-recording gate for a likeness checkpoint. Does not record or train."""

from __future__ import annotations

import importlib.util
import io
import sys
import tempfile
import unittest
from pathlib import Path
from unittest import mock

ROOT = Path(__file__).resolve().parents[2]
MODULE_PATH = ROOT / "chaos-engine/references/details/likeness_record_train.py"
REFUSED = "89c5ae999910b74c4325cd06a6ba590f38305947b67367294fb7918ecb6d030b"


def _load():
    spec = importlib.util.spec_from_file_location("likeness_record_train", MODULE_PATH)
    if spec is None or spec.loader is None:
        raise ImportError(MODULE_PATH)
    module = importlib.util.module_from_spec(spec)
    sys.modules[spec.name] = module
    spec.loader.exec_module(module)
    return module


class LikenessRecordTrainTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls) -> None:
        cls.mod = _load()

    def test_read_aloud_script_names_the_owner_and_skips_the_foreign_spelling(self) -> None:
        script = self.mod.read_aloud_script()
        self.assertIn("Mohab Mohie", script)
        self.assertIn("Automatest", script)
        self.assertIn("Um", script)
        self.assertIn("You know", script)
        self.assertNotIn("\u0443\u043c", script)

    def test_missing_recording_is_refused(self) -> None:
        missing = Path(tempfile.gettempdir()) / "likeness-owner-take-missing.wav"
        self.assertFalse(missing.exists())
        self.assertIn("recording is missing", self.mod.reject_reason(missing) or "")

    def test_synthesized_introduction_name_is_refused_without_the_real_sample(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            path = Path(tmp) / "qwen-cv-intro-not-the-sample.txt"
            path.write_bytes(b"not a recording")
            reason = self.mod.reject_reason(path)
        self.assertIsNotNone(reason)
        self.assertIn("synthesized introduction", reason or "")

    def test_refused_digest_membership_without_hashing_the_owner_sample(self) -> None:
        # Chosen bytes only. Do not open the owner wav to build the refused digest.
        self.assertIn(REFUSED, self.mod.REFUSED_SHA256)
        with tempfile.TemporaryDirectory() as tmp:
            path = Path(tmp) / "owner-take.txt"
            path.write_bytes(b"owner-take-bytes-not-the-refused-file")
            digest = self.mod.file_sha256(path)
            self.assertNotEqual(digest, REFUSED)
            self.assertIsNone(self.mod.reject_reason(path))
            with mock.patch.object(self.mod, "file_sha256", return_value=REFUSED):
                reason = self.mod.reject_reason(path)
        self.assertEqual(reason, "this file is a refused synthesized introduction")

    def test_adopt_checkpoint_beats_floor_and_same_run_base(self) -> None:
        # 0.9861 is strictly above floor 0.9860 and strictly above the same-run base.
        self.assertTrue(self.mod.adopt_checkpoint(0.9860, 0.9861))
        self.assertFalse(self.mod.adopt_checkpoint(0.9860, 0.9860))
        self.assertTrue(self.mod.adopt_checkpoint(0.9860, 0.9900))
        self.assertFalse(self.mod.adopt_checkpoint(0.9900, 0.9870))

    def test_record_argv_returns_ffmpeg_and_does_not_run_it(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            destination = Path(tmp) / "take.wav"
            with mock.patch.object(self.mod.shutil, "which", return_value=None), mock.patch.object(
                self.mod.subprocess, "run"
            ) as run:
                argv = self.mod.record_argv(destination)
                run.assert_not_called()
                code = self.mod.main(["record", "--out", str(destination), "--execute-record"])
                run.assert_not_called()
        self.assertEqual(code, 2)
        self.assertEqual(argv[0], "ffmpeg")
        self.assertEqual(
            argv,
            [
                "ffmpeg",
                "-y",
                "-f",
                "pulse",
                "-i",
                "default",
                "-ac",
                "1",
                "-ar",
                "48000",
                str(destination),
            ],
        )
        self.assertNotIn("--execute-record", argv)

    def test_main_script_prints_the_read_aloud_text(self) -> None:
        stdout = io.StringIO()
        with mock.patch("sys.stdout", stdout):
            code = self.mod.main(["script"])
        self.assertEqual(code, 0)
        self.assertEqual(stdout.getvalue().strip(), self.mod.read_aloud_script())


if __name__ == "__main__":
    unittest.main()
