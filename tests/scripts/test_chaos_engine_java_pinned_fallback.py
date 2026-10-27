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
        pinned = module._pinned_temurin_version()
        requested = "25.0.99+1"
        self.assertNotEqual(requested, pinned)
        calls = []

        def fake(version, **_):
            calls.append(version)
            if version == requested:
                raise http(404)
            return "/java"

        with mock.patch.object(module, "_install_temurin_release", side_effect=fake):
            self.assertEqual("/java", module.install_exact_java(requested, system="windows"))
        self.assertEqual([requested, pinned], calls)

    def test_other_errors_and_pinned_itself_still_raise(self):
        module = load()
        pinned = module._pinned_temurin_version()
        with mock.patch.object(module, "_install_temurin_release", side_effect=http(403)):
            with self.assertRaises(urllib.error.HTTPError):
                module.install_exact_java("25.0.99+1", system="windows")
        with mock.patch.object(module, "_install_temurin_release", side_effect=http(404)) as fake:
            with self.assertRaises(urllib.error.HTTPError):
                module.install_exact_java(pinned, system="windows")
            self.assertEqual(1, fake.call_count)


class JavaResolveAgreesWithInstallTest(TestCase):
    """A rerun must reuse the pinned JDK when the latest GA is unpublished here."""

    def test_unpublished_latest_resolves_to_pinned(self):
        module = load()
        pinned = module._pinned_temurin_version()

        def opener(url, timeout):
            raise http(404)

        self.assertEqual(pinned, module._published_temurin_or_pinned("25.0.99+1", opener=opener))

    def test_published_latest_and_other_errors_keep_latest(self):
        module = load()

        class Ok:
            def __enter__(self):
                return self

            def __exit__(self, *a):
                return False

        self.assertEqual("25.0.99+1", module._published_temurin_or_pinned("25.0.99+1", opener=lambda u, timeout: Ok()))

        def forbidden(url, timeout):
            raise http(403)

        self.assertEqual("25.0.99+1", module._published_temurin_or_pinned("25.0.99+1", opener=forbidden))
