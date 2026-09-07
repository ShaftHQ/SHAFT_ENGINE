"""Wave A (#5619/#5620): status ⊆ doctor, activationProof, repair, default bundle."""

from __future__ import annotations

import importlib.util
import sys
import tempfile
import unittest
import unittest.mock as mock
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
INSTALL_PATH = ROOT / "chaos-engine/install.py"
POLICY_PATH = ROOT / "chaos-engine/headroom_policy.py"
INSTALL_MD = ROOT / "chaos-engine/INSTALL.md"


def load(path: Path, name: str):
    spec = importlib.util.spec_from_file_location(name, path)
    module = importlib.util.module_from_spec(spec)
    sys.modules[spec.name] = module
    spec.loader.exec_module(module)
    return module


class WaveAHealthTruthTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.install = load(INSTALL_PATH, "ce_install_wave_a")
        cls.policy = load(POLICY_PATH, "ce_headroom_wave_a")

    def test_install_docs_cover_status_subset_and_bundle_flags(self):
        text = INSTALL_MD.read_text(encoding="utf-8")
        self.assertIn("status` ⊆ `doctor", text)
        self.assertIn("activationProof", text)
        self.assertIn("--without-headroom", text)
        self.assertIn("repair --project . --component plugins", text)
        self.assertIn("Default-on all-in-one bundle", text)

    def test_bundle_options_default_on_and_disable_flags(self):
        options = self.install.default_bundle_options()
        for name in self.install.DEFAULT_BUNDLE_COMPONENTS:
            self.assertTrue(options[name], name)
        disabled = self.install.normalize_bundle_options(
            {"without_headroom": True, "without_memory": True}
        )
        self.assertFalse(disabled["headroom"])
        self.assertFalse(disabled["memory"])
        self.assertTrue(disabled["ponytail"])

    def test_bundle_options_round_trip(self):
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary)
            written = self.install.write_bundle_options(
                project, {"headroom": False, "memory": True}
            )
            self.assertTrue(written.is_file())
            loaded = self.install.read_bundle_options(project)
            self.assertFalse(loaded["headroom"])
            self.assertTrue(loaded["memory"])

    def test_status_subset_helper_detects_plugins_false_healthy(self):
        status_doc = {
            "components": {
                "plugins": {"status": "healthy", "taskImpact": "required"},
                "hooks": {"status": "healthy", "taskImpact": "required"},
            }
        }
        doctor_doc = {
            "components": {
                "plugins": {"status": "recovery-required", "taskImpact": "required"},
                "hooks": {"status": "healthy", "taskImpact": "required"},
            }
        }
        self.assertEqual(
            ["plugins"],
            self.install.status_subset_of_doctor(status_doc, doctor_doc),
        )
        repaired = {
            "components": {
                "plugins": {"status": "recovery-required", "taskImpact": "required"},
                "hooks": {"status": "healthy", "taskImpact": "required"},
            }
        }
        self.assertEqual([], self.install.status_subset_of_doctor(repaired, doctor_doc))

    def test_apply_plugin_client_health_closes_false_healthy(self):
        result = {
            "status": "healthy",
            "components": {
                "plugins": {"status": "healthy", "taskImpact": "required"},
            },
        }
        host = mock.Mock()
        host.detected_plugin_status.return_value = {
            "codex": {
                "status": "absent",
                "marketplace": "absent",
                "plugin": "absent",
                "plugins": {"chaos-engine": "absent"},
            }
        }
        self.install.apply_plugin_client_health(
            result,
            Path("."),
            host,
            attach_clients=True,
            attach_activation_proof=True,
        )
        self.assertEqual("recovery-required", result["status"])
        self.assertEqual("recovery-required", result["components"]["plugins"]["status"])
        self.assertIn("codex", result["clients"])
        self.assertIn("codex", result["activationProof"])
        self.assertEqual("absent", result["activationProof"]["codex"]["status"])

    def test_activation_proof_is_sorted_and_bounded(self):
        proof = self.install.activation_proof_from_clients(
            {
                "claude": {
                    "status": "healthy",
                    "marketplace": "healthy",
                    "plugin": "healthy",
                    "plugins": {"ponytail": "healthy", "chaos-engine": "healthy"},
                },
                "codex": {"status": "absent", "marketplace": "absent", "plugin": "absent"},
            }
        )
        self.assertEqual(["claude", "codex"], list(proof))
        self.assertEqual(
            ["chaos-engine", "ponytail"],
            list(proof["claude"]["plugins"]),
        )

    def test_repair_parser_exposes_components_and_without_flags(self):
        parser = self.install.parser()
        args = parser.parse_args(
            [
                "repair",
                "--project",
                ".",
                "--component",
                "plugins",
            ]
        )
        self.assertEqual("repair", args.command)
        self.assertEqual("plugins", args.component)
        install_args = parser.parse_args(
            [
                "install",
                "--project",
                ".",
                "--source",
                ".",
                "--commit",
                "a" * 40,
                "--without-headroom",
                "--without-memory",
            ]
        )
        self.assertTrue(install_args.without_headroom)
        self.assertTrue(install_args.without_memory)
        self.assertFalse(install_args.without_ponytail)

    def test_plugins_fix_next_prefers_repair_command(self):
        fix = self.install.component_fix_next(
            "plugins",
            {"status": "recovery-required", "taskImpact": "required"},
        )
        self.assertIsNotNone(fix)
        self.assertIn("repair --project . --component plugins", fix)


    def test_headroom_ensure_installed_skips_network_under_ci(self):
        def which(name, path=None):
            return None

        with mock.patch.dict(self.policy.os.environ, {"CI": "true"}, clear=False):
            with mock.patch.object(self.policy.shutil, "which", side_effect=which):
                result = self.policy.ensure_installed(which=which)
        self.assertEqual("absent", result["status"])
        self.assertEqual("skipped-ci", result["action"])

    def test_headroom_ensure_installed_reuses_when_present(self):
        with mock.patch.object(self.policy.shutil, "which", side_effect=lambda name: "/bin/headroom" if name == "headroom" else None):
            result = self.policy.ensure_installed()
        self.assertEqual("healthy", result["status"])
        self.assertEqual("reused", result["action"])

    def test_headroom_ensure_installed_runs_uv_when_missing(self):
        calls = []

        def which(name, path=None):
            if name == "headroom":
                return "/bin/headroom" if calls else None
            if name == "uv":
                return "/bin/uv"
            return None

        def runner(command, **_kwargs):
            calls.append(command)
            return mock.Mock(returncode=0, stdout="", stderr="")

        with mock.patch.object(self.policy.shutil, "which", side_effect=which):
            result = self.policy.ensure_installed(runner=runner, which=which)
        self.assertEqual("healthy", result["status"])
        self.assertEqual("installed", result["action"])
        self.assertTrue(calls)
        self.assertIn("tool", calls[0])
        self.assertIn("install", calls[0])


if __name__ == "__main__":
    unittest.main()
