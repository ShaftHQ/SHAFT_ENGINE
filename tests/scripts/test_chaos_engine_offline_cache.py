"""Offline install from a checksum-verified bundle (#6415) and the shared download cache (#6416)."""

from __future__ import annotations

import hashlib
import importlib.util
import io
import os
import socket
import tempfile
import unittest
import unittest.mock as mock
import zipfile
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
SOURCE = ROOT / "chaos-engine"
COMMIT = "1" * 40


def load(path: Path, name: str):
    spec = importlib.util.spec_from_file_location(name, path)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


INSTALL = load(SOURCE / "install.py", "ce_install_offline_6415")
DEPENDENCIES = load(SOURCE / "dependencies.py", "ce_dependencies_cache_6416")


def _no_network(*_args, **_kwargs):
    raise OSError("network blocked by test guard")


class OfflineBundleTest(unittest.TestCase):
    def test_bundle_installs_with_network_blocked(self):
        with tempfile.TemporaryDirectory() as temporary:
            bundle = INSTALL.build_bundle(SOURCE, COMMIT, Path(temporary) / "ce.zip")
            project = Path(temporary) / "consumer"
            project.mkdir()
            with mock.patch.object(socket.socket, "connect", _no_network), \
                    mock.patch("urllib.request.urlopen", _no_network):
                source, commit = INSTALL.extract_bundle(bundle, Path(temporary) / "unpacked")
                target = INSTALL.install(project, source, commit)
                manifest = INSTALL.verify_install(target)
            self.assertEqual(COMMIT, commit)
            self.assertEqual(COMMIT, manifest["source"]["commit"])

    def test_tampered_file_aborts(self):
        with tempfile.TemporaryDirectory() as temporary:
            bundle = INSTALL.build_bundle(SOURCE, COMMIT, Path(temporary) / "ce.zip")
            tampered = Path(temporary) / "tampered.zip"
            with zipfile.ZipFile(bundle) as original, zipfile.ZipFile(tampered, "w") as copy:
                for item in original.infolist():
                    data = original.read(item)
                    if item.filename == "install.py":
                        data += b"\n# tampered\n"
                    copy.writestr(item, data)
            with self.assertRaisesRegex(ValueError, "checksum"):
                INSTALL.extract_bundle(tampered, Path(temporary) / "unpacked")
            self.assertFalse((Path(temporary) / "unpacked").exists())

    def test_unlisted_member_aborts(self):
        with tempfile.TemporaryDirectory() as temporary:
            bundle = INSTALL.build_bundle(SOURCE, COMMIT, Path(temporary) / "ce.zip")
            with zipfile.ZipFile(bundle, "a") as archive:
                archive.writestr("../escape.py", "x")
            with self.assertRaises(ValueError):
                INSTALL.extract_bundle(bundle, Path(temporary) / "unpacked")

    def test_cli_accepts_from_bundle_without_source(self):
        args = INSTALL.parser().parse_args(["install", "--project", ".", "--from-bundle", "ce.zip"])
        self.assertEqual(Path("ce.zip"), args.from_bundle)
        self.assertIsNone(args.source)
        bundle_args = INSTALL.parser().parse_args(["bundle", "--source", "s", "--commit", COMMIT, "--output", "o.zip"])
        self.assertEqual("bundle", bundle_args.command)


class _Response(io.BytesIO):
    headers: dict = {}

    def __enter__(self):
        return self

    def __exit__(self, *_):
        self.close()


class DownloadCacheTest(unittest.TestCase):
    def test_second_project_downloads_nothing_and_purge_empties(self):
        payload = b"runtime-archive"
        expected = hashlib.sha256(payload).hexdigest()
        calls = []

        def opener(url, timeout=None):
            calls.append(url)
            return _Response(payload)

        with tempfile.TemporaryDirectory() as temporary, \
                mock.patch.dict(os.environ, {"CHAOS_ENGINE_CACHE_DIR": str(Path(temporary) / "cache")}):
            for project in ("one", "two"):
                destination = Path(temporary) / project / "runtime.zip"
                destination.parent.mkdir()
                DEPENDENCIES._download_artifact("https://example.invalid/runtime.zip", destination, expected, opener)
                self.assertEqual(payload, destination.read_bytes())
            self.assertEqual(1, len(calls))
            self.assertEqual(1, DEPENDENCIES.download_cache_status()["artifacts"])
            self.assertEqual(1, DEPENDENCIES.purge_download_cache()["removed"])
            self.assertEqual(0, DEPENDENCIES.download_cache_status()["artifacts"])

    def test_corrupt_cache_entry_is_redownloaded(self):
        payload = b"runtime-archive"
        expected = hashlib.sha256(payload).hexdigest()
        with tempfile.TemporaryDirectory() as temporary, \
                mock.patch.dict(os.environ, {"CHAOS_ENGINE_CACHE_DIR": str(Path(temporary) / "cache")}):
            entry = DEPENDENCIES.download_cache_root() / expected
            entry.parent.mkdir(parents=True)
            entry.write_bytes(b"corrupt")
            destination = Path(temporary) / "runtime.zip"
            DEPENDENCIES._download_artifact("u", destination, expected, lambda url, timeout=None: _Response(payload))
            self.assertEqual(payload, destination.read_bytes())
            self.assertEqual(payload, entry.read_bytes())

    def test_cache_root_defaults_to_user_cache(self):
        with mock.patch.dict(os.environ, {}, clear=True):
            root = DEPENDENCIES.download_cache_root()
        self.assertEqual("chaos-engine", root.parent.name)


if __name__ == "__main__":
    unittest.main()
