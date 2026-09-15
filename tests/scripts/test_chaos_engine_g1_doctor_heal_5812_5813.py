"""G1: doctor overall soft statuses + #5812/#5813 official heal follow-ons."""

from __future__ import annotations

import importlib.util
import tempfile
import unittest
from pathlib import Path


ROOT = Path(__file__).resolve().parents[2]
INSTALL = ROOT / "chaos-engine" / "install.py"
HEAL = ROOT / "chaos-engine" / "official_self_heal.py"


def load(path: Path, name: str):
    spec = importlib.util.spec_from_file_location(name, path)
    if spec is None or spec.loader is None:
        raise AssertionError(f"failed to load {path}")
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


class DoctorOverallSoftStatusG1Test(unittest.TestCase):
    def test_compatible_legacy_does_not_escalate_overall(self):
        module = load(INSTALL, "ce_g1_install_soft")
        result = {
            "status": "recovery-required",
            "components": {
                "core": {"status": "healthy", "taskImpact": "required"},
                "memory": {
                    "status": "compatible-legacy",
                    "taskImpact": "advisory",
                    "reason": "legacy v5",
                },
            },
            "hosts": {"status": "healthy"},
        }
        module.reconcile_doctor_overall_status(result)
        self.assertEqual("healthy", result["status"])

    def test_advisory_absent_gh_does_not_escalate_overall(self):
        module = load(INSTALL, "ce_g1_install_gh")
        result = {
            "status": "recovery-required",
            "components": {
                "core": {"status": "healthy", "taskImpact": "required"},
                "gh": {
                    "status": "absent",
                    "taskImpact": "advisory",
                    "detail": "gh-cli-missing",
                },
                "maven-tools-mcp": {
                    "status": "absent",
                    "taskImpact": "optional",
                },
            },
            "hosts": {"status": "healthy"},
        }
        module.reconcile_doctor_overall_status(result)
        self.assertEqual("healthy", result["status"])

    def test_sync_advisory_does_not_escalate_overall(self):
        module = load(INSTALL, "ce_g1_install_sync")
        result = {
            "status": "recovery-required",
            "components": {
                "skills": {"status": "healthy", "taskImpact": "required"},
                "identity": {"status": "sync-advisory", "taskImpact": "advisory"},
            },
            "hosts": {"status": "healthy", "grokLeanSkills": {"status": "sync-advisory"}},
        }
        module.reconcile_doctor_overall_status(result)
        self.assertEqual("healthy", result["status"])

    def test_required_hard_fail_still_escalates(self):
        module = load(INSTALL, "ce_g1_install_hard")
        result = {
            "status": "healthy",
            "components": {
                "mcps": {"status": "recovery-required", "taskImpact": "required"},
            },
            "hosts": {"status": "healthy"},
        }
        module.reconcile_doctor_overall_status(result)
        self.assertEqual("recovery-required", result["status"])

    def test_component_escalates_helper(self):
        module = load(INSTALL, "ce_g1_install_helper")
        self.assertFalse(
            module.component_escalates_overall(
                {"status": "compatible-legacy", "taskImpact": "advisory"}
            )
        )
        self.assertFalse(
            module.component_escalates_overall(
                {"status": "sync-advisory", "taskImpact": "required"}
            )
        )
        self.assertTrue(
            module.component_escalates_overall(
                {"status": "recovery-required", "taskImpact": "required"}
            )
        )
        self.assertTrue(
            module.component_escalates_overall(
                {"status": "recovery-required", "taskImpact": "advisory"}
            )
        )
        self.assertFalse(
            module.component_escalates_overall(
                {"status": "absent", "taskImpact": "optional"}
            )
        )
        self.assertFalse(
            module.component_escalates_overall(
                {"status": "invalid", "taskImpact": "optional"}
            )
        )


class OfficialHealFollowOn5812_5813Test(unittest.TestCase):
    def test_doctor_heals_mcps_skills_via_repair(self):
        module = load(HEAL, "ce_g1_heal_mcps")
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary)
            (project / ".chaos-engine-state").mkdir(parents=True)
            result = {
                "status": "recovery-required",
                "components": {
                    "mcps": {"status": "absent", "taskImpact": "required"},
                    "skills": {"status": "absent", "taskImpact": "required"},
                },
            }
            repaired: list[str] = []

            def fake_repair(proj, name, **_kwargs):
                repaired.append(name)
                return {"status": "repaired", "component": name, "action": "rebind"}

            def fake_rematerialize(proj, *, names=None):
                for n in names or ():
                    path = Path(proj) / f"plugins/{n}/skills/{n}/SKILL.md"
                    path.parent.mkdir(parents=True, exist_ok=True)
                    path.write_text("# skill\n", encoding="utf-8")
                return {"status": "healed", "names": list(names or [])}

            summary = module.apply_doctor_official_self_heal(
                result,
                project,
                rematerialize=fake_rematerialize,
                repair=fake_repair,
                bundle={
                    "memory": False,
                    "mempalace": False,
                    "graphify": False,
                    "caveman": False,
                    "ponytail": False,
                },
            )
            self.assertIn("mcps", repaired)
            self.assertIn("skills", repaired)
            self.assertIn("mcps", summary["healed"])
            self.assertIn("skills", summary["healed"])
            self.assertEqual("healthy", result["components"]["mcps"]["status"])
            self.assertEqual("healthy", result["components"]["skills"]["status"])

    def test_doctor_heals_context7_when_dependency_unhealthy(self):
        module = load(HEAL, "ce_g1_heal_ctx7")
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary)
            (project / ".chaos-engine-state").mkdir(parents=True)
            result = {
                "status": "healthy",
                "components": {},
                "dependencies": {
                    "components": {
                        "context7": {"status": "absent", "healthy": False},
                    }
                },
            }

            def fake_repair(proj, name, **_kwargs):
                self.assertEqual("tools", name)
                return {"status": "repaired", "component": name}

            def fake_rematerialize(proj, *, names=None):
                return {"status": "healed", "names": []}

            summary = module.apply_doctor_official_self_heal(
                result,
                project,
                rematerialize=fake_rematerialize,
                repair=fake_repair,
                bundle={
                    "memory": False,
                    "mempalace": False,
                    "graphify": False,
                    "caveman": False,
                    "ponytail": False,
                },
            )
            self.assertIn("context7", summary["healed"])
            self.assertEqual("healthy", result["components"]["context7"]["status"])

    def test_doctor_heals_managed_runtimes_when_node_unhealthy(self):
        module = load(HEAL, "ce_g1_heal_runtime")
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary)
            (project / ".chaos-engine-state").mkdir(parents=True)
            result = {
                "status": "healthy",
                "components": {},
                "dependencies": {
                    "components": {
                        "node": {"status": "absent", "healthy": False},
                        "java": {"status": "healthy", "healthy": True},
                    }
                },
            }

            def fake_repair(proj, name, **_kwargs):
                self.assertEqual("tools", name)
                return {"status": "repaired", "component": name}

            def fake_rematerialize(proj, *, names=None):
                return {"status": "healed", "names": []}

            summary = module.apply_doctor_official_self_heal(
                result,
                project,
                rematerialize=fake_rematerialize,
                repair=fake_repair,
                bundle={
                    "memory": False,
                    "mempalace": False,
                    "graphify": False,
                    "caveman": False,
                    "ponytail": False,
                },
            )
            self.assertIn("node", summary["healed"])

    def test_doctor_skips_managed_tools_heal_when_dependencies_healthy(self):
        module = load(HEAL, "ce_g1_heal_skip_tools")
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary)
            (project / ".chaos-engine-state").mkdir(parents=True)
            result = {
                "status": "healthy",
                "components": {
                    "tools": {"status": "healthy"},
                    "maven-tools-mcp": {"status": "healthy"},
                },
                "dependencies": {
                    "status": "healthy",
                    "components": {
                        "node": {"status": "healthy", "healthy": True},
                        "java": {"status": "healthy", "healthy": True},
                        "maven": {"status": "healthy", "healthy": True},
                        "context7": {"status": "healthy", "healthy": True},
                    },
                },
            }
            repaired: list[str] = []

            def fake_repair(proj, name, **_kwargs):
                repaired.append(name)
                return {"status": "repaired", "component": name}

            def fake_rematerialize(proj, *, names=None):
                return {"status": "healed", "names": []}

            summary = module.apply_doctor_official_self_heal(
                result,
                project,
                rematerialize=fake_rematerialize,
                repair=fake_repair,
                bundle={
                    "memory": False,
                    "mempalace": False,
                    "graphify": False,
                    "caveman": False,
                    "ponytail": False,
                },
            )
            self.assertEqual([], repaired)
            self.assertEqual([], summary.get("healed", []))
            self.assertNotIn("tools", summary.get("failed", []))

    def test_doctor_skips_context7_tools_repair_when_dependencies_healthy(self):
        module = load(HEAL, "ce_g1_heal_skip_ctx7")
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary)
            (project / ".chaos-engine-state").mkdir(parents=True)
            result = {
                "status": "healthy",
                "components": {},
                "dependencies": {
                    "status": "healthy",
                    "components": {
                        "context7": {"status": "healthy", "healthy": True},
                    },
                },
            }
            repaired: list[str] = []

            def fake_repair(proj, name, **_kwargs):
                repaired.append(name)
                return {"status": "repaired", "component": name}

            def fake_rematerialize(proj, *, names=None):
                return {"status": "healed", "names": []}

            summary = module.apply_doctor_official_self_heal(
                result,
                project,
                rematerialize=fake_rematerialize,
                repair=fake_repair,
                bundle={
                    "memory": False,
                    "mempalace": False,
                    "graphify": False,
                    "caveman": False,
                    "ponytail": False,
                },
            )
            self.assertEqual([], repaired)
            self.assertNotIn("context7", summary.get("healed", []))


if __name__ == "__main__":
    unittest.main()
