"""Reconcile attaches a real provenance bundle only when the release lacks one."""
from __future__ import annotations

import json
import subprocess  # nosec B404 - tests monkeypatch subprocess, never running external commands.
import tempfile
import unittest
from io import StringIO
from pathlib import Path
from unittest import mock

import sys as _sys; from pathlib import Path as _Path; _sys.path.insert(0, str(_Path(__file__).resolve().parents[2]))  # noqa: E402,E702 - self-locating under python3 -I (#7197)
import scripts.ci.attach_release_provenance as attach


VERSION = "10.4.20261024"


class AttachReleaseProvenanceTest(unittest.TestCase):
    def _run(self, assets: list[dict], argv: list[str]) -> tuple[int, str, list[list[str]]]:
        commands: list[list[str]] = []

        def fake_run(command, **kwargs):
            commands.append(list(command))
            self.assertNotEqual(kwargs.get("shell"), True)
            if command[1:5] == ["release", "view", VERSION, "--json"]:
                stdout = json.dumps({"assets": assets})
                return subprocess.CompletedProcess(command, 0, stdout=stdout, stderr="")
            if command[1:3] == ["release", "upload"]:
                return subprocess.CompletedProcess(command, 0, stdout="", stderr="")
            raise AssertionError(command)

        stdout = StringIO()
        with (
            mock.patch.object(attach.shutil, "which", return_value="gh"),
            mock.patch.object(attach.subprocess, "run", side_effect=fake_run),
            mock.patch("sys.stdout", stdout),
        ):
            code = attach.main(argv)
        return code, stdout.getvalue(), commands

    def test_asset_already_present_does_not_upload(self) -> None:
        with tempfile.TemporaryDirectory() as temp_dir:
            bundle = Path(temp_dir) / "bundle.jsonl"
            bundle.write_text("{}\n", encoding="utf-8")
            code, output, commands = self._run(
                [{"name": "notes.intoto.jsonl"}],
                ["--version", VERSION, "--bundle", str(bundle)],
            )

        self.assertEqual(0, code)
        self.assertIn("present", output)
        self.assertEqual(
            [["gh", "release", "view", VERSION, "--json", "assets"]],
            commands,
        )

    def test_missing_bundle_exits_zero_without_upload(self) -> None:
        missing = Path(tempfile.gettempdir()) / "shaft-missing-provenance-bundle.intoto.jsonl"
        self.assertFalse(missing.exists())
        code, output, commands = self._run(
            [],
            ["--version", VERSION, "--bundle", str(missing)],
        )

        self.assertEqual(0, code)
        self.assertIn("unavailable", output)
        self.assertFalse(any(command[1:3] == ["release", "upload"] for command in commands))

    def test_bundle_present_uploads_named_asset_without_clobber(self) -> None:
        payload = b'{"predicateType":"https://slsa.dev/provenance/v1"}\n'
        seen: dict[str, bytes] = {}

        def fake_run(command, **kwargs):
            self.assertNotEqual(kwargs.get("shell"), True)
            if command[1:3] == ["release", "view"]:
                return subprocess.CompletedProcess(
                    command, 0, stdout=json.dumps({"assets": []}), stderr=""
                )
            if command[1:3] == ["release", "upload"]:
                staged = Path(command[4])
                seen["command"] = command
                seen["bytes"] = staged.read_bytes()
                return subprocess.CompletedProcess(command, 0, stdout="", stderr="")
            raise AssertionError(command)

        with tempfile.TemporaryDirectory() as temp_dir:
            bundle = Path(temp_dir) / "attest-bundle.intoto.jsonl"
            bundle.write_bytes(payload)
            stdout = StringIO()
            with (
                mock.patch.object(attach.shutil, "which", return_value="gh"),
                mock.patch.object(attach.subprocess, "run", side_effect=fake_run),
                mock.patch("sys.stdout", stdout),
            ):
                code = attach.main(["--version", VERSION, "--bundle", str(bundle)])

        command = seen["command"]
        staged = Path(command[4])
        self.assertEqual(0, code)
        self.assertEqual(
            ["gh", "release", "upload", VERSION, str(staged)],
            command,
        )
        self.assertEqual(f"SHAFT_ENGINE-{VERSION}.intoto.jsonl", staged.name)
        self.assertNotIn("--clobber", command)
        self.assertEqual(payload, seen["bytes"])
        self.assertIn("Uploaded", stdout.getvalue())

    def test_dry_run_does_not_upload(self) -> None:
        with tempfile.TemporaryDirectory() as temp_dir:
            bundle = Path(temp_dir) / "bundle.jsonl"
            bundle.write_text("{}\n", encoding="utf-8")
            code, output, commands = self._run(
                [],
                ["--version", VERSION, "--bundle", str(bundle), "--dry-run"],
            )

        self.assertEqual(0, code)
        self.assertIn(f"gh release upload {VERSION} SHAFT_ENGINE-{VERSION}.intoto.jsonl", output)
        self.assertFalse(any(command[1:3] == ["release", "upload"] for command in commands))


if __name__ == "__main__":
    unittest.main()
