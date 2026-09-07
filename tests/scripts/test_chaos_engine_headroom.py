"""Native Headroom companion — pin, policy, doctor, SessionStart (#5613)."""

from __future__ import annotations

import importlib.util
import json
import os
import sys
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
PIN = ROOT / "chaos-engine/vendor/headroom/PIN.json"
SKILL = ROOT / "chaos-engine/vendor/headroom/skills/headroom/SKILL.md"
POLICY = ROOT / "chaos-engine/headroom_policy.py"
DOC = ROOT / "chaos-engine/references/headroom.md"
NOTICES = ROOT / "chaos-engine/THIRD_PARTY_NOTICES.md"
MATRIX = ROOT / "chaos-engine/references/host-parity-matrix.md"


def load(path: Path, name: str):
    spec = importlib.util.spec_from_file_location(name, path)
    module = importlib.util.module_from_spec(spec)
    sys.modules[spec.name] = module
    spec.loader.exec_module(module)
    return module


class HeadroomCompanionTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.policy = load(POLICY, "ce_headroom_policy")
        cls.lifecycle = load(ROOT / "chaos-engine/hooks/lifecycle.py", "ce_lifecycle_hr")
        cls.install = load(ROOT / "chaos-engine/install.py", "ce_install_hr")

    def test_pin_enforces_agent_90_and_apache(self):
        pin = json.loads(PIN.read_text(encoding="utf-8"))
        self.assertEqual("0.37.0", pin["version"])
        self.assertEqual("headroom-ai", pin["package"])
        self.assertEqual("Apache-2.0", pin["license"])
        self.assertEqual("agent-90", pin["ce_defaults"]["HEADROOM_SAVINGS_PROFILE"])
        self.assertEqual("off", pin["ce_defaults"]["HEADROOM_BEACON"])
        self.assertEqual("disabled", pin["ce_defaults"]["HEADROOM_MEMORY_INJECTION_MODE"])
        self.assertTrue(SKILL.is_file())
        self.assertTrue(DOC.is_file())
        notices = NOTICES.read_text(encoding="utf-8")
        self.assertIn("Headroom", notices)
        self.assertIn("Apache-2.0", notices)

    def test_policy_maps_token_budgets_and_xor_output_shaper(self):
        self.assertEqual(0, self.policy.self_check())
        self.assertEqual("agent-90", self.policy.profile_for_token_budget("ultra" + "-lean"))  # nosec B105
        self.assertEqual("balanced", self.policy.profile_for_token_budget("balanced"))
        self.assertEqual("coding", self.policy.profile_for_token_budget("deep"))
        self.assertEqual(
            "0",
            self.policy.resolve_output_shaper(ponytail_active=True, requested="1"),
        )
        self.assertEqual(
            "1",
            self.policy.resolve_output_shaper(ponytail_active=False, requested="1"),
        )
        env = self.policy.ce_env(token_budget="ultra" + "-lean", ponytail_active=True)  # nosec B105
        self.assertEqual("agent-90", env["HEADROOM_SAVINGS_PROFILE"])
        self.assertEqual("0.10", env["HEADROOM_TARGET_RATIO"])
        self.assertEqual("1", env["HEADROOM_FORCE_KOMPRESS"])
        self.assertEqual("token", env["HEADROOM_MODE"])
        self.assertEqual("off", env["HEADROOM_BEACON"])
        self.assertEqual("disabled", env["HEADROOM_MEMORY_INJECTION_MODE"])
        self.assertEqual("0", env["HEADROOM_OUTPUT_SHAPER"])

    def test_doctor_status_is_optional_with_fix_next(self):
        status = self.policy.doctor_status()
        self.assertEqual("optional", status["taskImpact"])
        self.assertIn(status["status"], {"healthy", "absent", "broken"})
        self.assertIn("headroom-ai==0.37.0", status["pin"])
        self.assertIn("headroom-ai==0.37.0", status["detail"])
        if status["status"] == "absent":
            self.assertIn("uv tool install", status["detail"])
            # Optional absence stays non-blocking (fix-next None), matching maven-tools-mcp.
            self.assertIsNone(self.install.component_fix_next("headroom", status))
        broken = {
            **status,
            "status": "broken",
            "detail": 'Install the managed pin: `uv tool install --python 3.13 "headroom-ai==0.37.0"`.',
        }
        fix = self.install.component_fix_next("headroom", broken)
        self.assertIsNotNone(fix)
        self.assertIn("uv tool install", fix)

    def test_session_start_includes_headroom_under_byte_budget(self):
        previous = os.environ.get("CHAOS_ENGINE_TOKEN_BUDGET")
        os.environ["CHAOS_ENGINE_TOKEN_BUDGET"] = "ultra" + "-lean"  # nosec B105
        try:
            context = self.lifecycle.session_start_context("t", "activation")
        finally:
            if previous is None:
                os.environ.pop("CHAOS_ENGINE_TOKEN_BUDGET", None)
            else:
                os.environ["CHAOS_ENGINE_TOKEN_BUDGET"] = previous
        self.assertIn("agent-90", context)
        self.assertIn("headroom", context.casefold())
        self.assertIn("beacon=off", context.casefold())
        self.assertLessEqual(
            len(context.encode("utf-8")),
            self.lifecycle.SESSION_START_MAX_BYTES,
        )

    def test_host_parity_lists_headroom_row(self):
        text = MATRIX.read_text(encoding="utf-8")
        self.assertIn("Headroom", text)
        self.assertIn("GAP-HEADROOM-GEMINI", text)

    def test_capability_policy_includes_optional_headroom(self):
        caps, _ = self.install.load_capability_policy(
            ROOT / "chaos-engine", "portable"
        )
        self.assertEqual("optional", caps["headroom"]["taskImpact"])
        self.assertIn("headroom", self.install.CAPABILITY_COMPONENTS)

    def test_export_env_and_install_command(self):
        script = self.policy.export_env_script(token_budget="ultra" + "-lean")  # nosec B105
        self.assertIn("HEADROOM_SAVINGS_PROFILE", script)
        self.assertIn("agent-90", script)
        self.assertIn('headroom-ai==0.37.0', self.policy.install_command())

    def test_doctor_loads_headroom_policy_from_installed_target_after_bootstrap_cleanup(self):
        """Bootstrap deletes the download tree before Verify; doctor must use target (#5613)."""
        import runpy
        import shutil
        import tempfile
        import types

        source = ROOT / "chaos-engine"
        project = Path(tempfile.mkdtemp(prefix="ce-headroom-verify-"))
        self.addCleanup(shutil.rmtree, project, ignore_errors=True)
        target = self.install.install(project=project, source=source, commit="c" * 40)
        temp = Path(tempfile.mkdtemp(prefix="chaos-engine-bootstrap-"))
        shutil.copytree(
            source,
            temp / "chaos-engine",
            ignore=shutil.ignore_patterns("__pycache__"),
        )
        installer = types.SimpleNamespace(
            **runpy.run_path(str(temp / "chaos-engine" / "install.py"))
        )
        shutil.rmtree(temp)

        class _Hosts:
            def retrieval_configs_healthy(self, _project):
                return True

            def mempalace_runtime_status(self, _project):
                return {"status": "healthy"}

            def maven_tools_cache_status(self):
                return {"status": "absent"}

        result = {"status": "healthy"}
        installer.attach_component_status(
            result,
            project,
            target,
            "healthy",
            _Hosts(),
            inspect_retrieval_state=False,
        )
        headroom = result["components"]["headroom"]
        self.assertEqual("optional", headroom["taskImpact"])
        self.assertIn(headroom["status"], {"healthy", "absent"})
        self.assertNotEqual("broken", headroom["status"])





    def test_upgrade_keeps_pre_headroom_backup_for_rollback(self):
        """Older manifests omitting optional headroom must still verify as backups."""
        import hashlib
        import json
        import shutil
        import tempfile

        source = ROOT / "chaos-engine"
        project = Path(tempfile.mkdtemp(prefix="ce-headroom-rollback-"))
        self.addCleanup(shutil.rmtree, project, ignore_errors=True)
        target = self.install.install(project=project, source=source, commit="a" * 40)
        # Downgrade the installed tree to a pre-headroom capability set.
        manifest = json.loads((target / "manifest.json").read_text(encoding="utf-8"))
        capabilities = dict(manifest["capabilities"])
        capabilities.pop("headroom", None)
        encoded = json.dumps(
            self.install._validated_capabilities(capabilities),
            sort_keys=True,
            separators=(",", ":"),
        ).encode()
        manifest["capabilities"] = capabilities
        manifest["capabilityPolicySha256"] = hashlib.sha256(encoded).hexdigest()
        files = dict(manifest["files"])
        for relative in list(files):
            if "headroom" in relative:
                path = target / relative
                if path.is_file():
                    path.unlink()
                files.pop(relative)
        manifest["files"] = files
        (target / "manifest.json").write_text(
            json.dumps(manifest, indent=2, sort_keys=True) + "\n", encoding="utf-8"
        )
        self.assertIsNotNone(self.install.try_verify_install(target))
        self.install.install(project=project, source=source, commit="b" * 40)
        backup = project / ".chaos-engine.backup"
        self.assertTrue(backup.is_dir())
        self.assertIsNotNone(self.install.try_verify_install(backup))
        self.install.rollback(project)
        self.assertTrue((project / ".chaos-engine" / "manifest.json").is_file())


if __name__ == "__main__":
    unittest.main()
