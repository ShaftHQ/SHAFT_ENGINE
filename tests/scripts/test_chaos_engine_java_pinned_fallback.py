"""Latest Temurin with no binary for this platform yet falls back to the pinned runtime."""
import importlib.util
import sys
import urllib.error
from pathlib import Path
from unittest import TestCase, mock

ROOT = Path(__file__).resolve().parents[2]
sys.path.insert(0, str(ROOT / "chaos-engine"))


def load():
    spec = importlib.util.spec_from_file_location("ce_deps_java", ROOT / "chaos-engine/dependencies.py")
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module




def http(code):
    return urllib.error.HTTPError("u", code, "x", {}, None)


class JavaPinnedFallbackTest(TestCase):
    def setUp(self):
        self.assertRegex(load()._pinned_temurin_version(), r"^25\.\d+\.\d+\+\d+$")

    def test_404_on_latest_falls_back_to_pinned(self):
        module = load()
        calls = []

        def fake(version, **_):
            calls.append(version)
            if version == "25.0.5+8":
                raise http(404)
            return "/java"

        with mock.patch.object(module, "_install_temurin_release", side_effect=fake):
            self.assertEqual("/java", module.install_exact_java("25.0.5+8", system="windows"))
        self.assertEqual(["25.0.5+8", "25.0.4+7"], calls)

    def test_other_errors_and_pinned_itself_still_raise(self):
        module = load()
        with mock.patch.object(module, "_install_temurin_release", side_effect=http(403)):
            with self.assertRaises(urllib.error.HTTPError):
                module.install_exact_java("25.0.5+8", system="windows")
        with mock.patch.object(module, "_install_temurin_release", side_effect=http(404)) as fake:
            with self.assertRaises(urllib.error.HTTPError):
                module.install_exact_java("25.0.4+7", system="windows")
            self.assertEqual(1, fake.call_count)
