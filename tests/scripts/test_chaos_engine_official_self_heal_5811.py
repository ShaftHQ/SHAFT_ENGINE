"""Official self-heal via each dependency's install command (#5811)."""

from __future__ import annotations

import importlib.util
import tempfile
import unittest
from pathlib import Path


ROOT = Path(__file__).resolve().parents[2]
HEAL = ROOT / "chaos-engine" / "official_self_heal.py"
HOSTS = ROOT / "chaos-engine" / "hosts.py"


def load(path: Path, name: str):
    spec = importlib.util.spec_from_file_location(name, path)
    if spec is None or spec.loader is None:
        raise AssertionError(f"failed to load {path}")
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


class MemoryStoreDataIsNotHealedByReinstall6234Test(unittest.TestCase):
    """#6234: a schema-invalid Memory store stays unhealthy; reinstalling the CLI cannot fix data."""

    def test_schema_failure_gets_its_own_code(self):
        hosts = load(HOSTS, "ce_hosts_store_schema_6234")
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary)
            tool = project / ".chaos-engine/tool.py"
            tool.parent.mkdir(parents=True)
            tool.write_text(
                "import json, sys\n"
                "print(json.dumps({'ok': False, 'error': {'code': 'MemorySchemaValidationFailed',"
                " 'details': {'issues': [{'path': '.memory/memory/gotchas/x.json',"
                " 'field': '/evidence/0/kind'}]}}}))\n"
                "sys.exit(1)\n",
                encoding="utf-8",
            )
            status = hosts.retrieval_runtime_status(project)
        self.assertEqual("recovery-required", status["status"])
        self.assertEqual("memory-store-schema-invalid", status["code"])
        self.assertIn(".memory/memory/gotchas/x.json", status["reason"])

    def test_doctor_does_not_mark_store_data_healed(self):
        module = load(HEAL, "ce_official_self_heal_store_6234")
        for code in ("memory-store-schema-invalid", "memory-check-invalid-store"):
            with self.subTest(code=code), tempfile.TemporaryDirectory() as temporary:
                project = Path(temporary)
                (project / ".chaos-engine-state").mkdir(parents=True)
                result = {
                    "status": "recovery-required",
                    "components": {
                        "memory": {
                            "status": "recovery-required",
                            "taskImpact": "advisory",
                            "code": code,
                        },
                        "mempalace": {"status": "healthy", "taskImpact": "advisory"},
                        "graphify": {"status": "healthy", "taskImpact": "advisory"},
                    },
                }
                repairs = []

                def fake_repair(proj, name, **_kwargs):
                    repairs.append(name)
                    return {"status": "repaired", "component": name}

                summary = module.apply_doctor_official_self_heal(
                    result,
                    project,
                    rematerialize=lambda _p, names=None: {"status": "healthy"},
                    repair=fake_repair,
                    bundle={name: True for name in ("memory", "mempalace", "graphify", "caveman", "ponytail")},
                )
                memory = result["components"]["memory"]
                self.assertEqual([], [name for name in repairs if name == "memory"])
                self.assertNotIn("memory", summary["healed"])
                self.assertEqual("recovery-required", memory["status"])
                self.assertIn("memory check --json", memory["fixNext"])
                self.assertEqual("recovery-required", result["status"])


class OfficialSelfHeal5811Test(unittest.TestCase):
    def test_inventory_covers_bundle_and_companions(self):
        module = load(HEAL, "ce_official_self_heal_inv")
        items = {row["item"] for row in module.inventory_table()}
        for name in (
            "caveman",
            "ponytail",
            "memory",
            "mempalace",
            "graphify",
            "context7",
            "gh",
            "node",
            "java",
            "uv",
            "mcps",
            "skills",
        ):
            self.assertIn(name, items)
            self.assertTrue(module.official_command_for(name))

    def test_rematerialize_companions_heals_missing_skills(self):
        hosts = load(HOSTS, "ce_hosts_rematerialize_5811")
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary)
            skill = project / "plugins/caveman/skills/caveman/SKILL.md"
            self.assertFalse(skill.is_file())
            result = hosts.rematerialize_companions(project, names=("caveman",))
            self.assertEqual("healthy", result["status"])
            self.assertTrue(skill.is_file())
            self.assertIn("plugins/caveman/skills/caveman/SKILL.md", result["written"])
            # Idempotent second pass.
            again = hosts.rematerialize_companions(project, names=("caveman",))
            self.assertEqual("healthy", again["status"])

    def test_doctor_heal_success_for_companion_and_bundle_tool(self):
        module = load(HEAL, "ce_official_self_heal_success")
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary)
            (project / ".chaos-engine-state").mkdir(parents=True)
            result = {
                "status": "recovery-required",
                "components": {
                    "memory": {
                        "status": "absent",
                        "taskImpact": "advisory",
                    },
                    "mempalace": {"status": "healthy", "taskImpact": "advisory"},
                    "graphify": {"status": "healthy", "taskImpact": "advisory"},
                },
            }

            def fake_rematerialize(proj, *, names=None):
                for name in names or ():
                    path = Path(proj) / f"plugins/{name}/skills/{name}/SKILL.md"
                    path.parent.mkdir(parents=True, exist_ok=True)
                    path.write_text("# skill\n", encoding="utf-8")
                return {"status": "healed", "written": list(names or []), "names": list(names or [])}

            def fake_repair(proj, name, **_kwargs):
                return {"status": "repaired", "component": name, "action": "account-reinstall"}

            summary = module.apply_doctor_official_self_heal(
                result,
                project,
                rematerialize=fake_rematerialize,
                repair=fake_repair,
                bundle={
                    "memory": True,
                    "mempalace": True,
                    "graphify": True,
                    "caveman": True,
                    "ponytail": True,
                },
            )
            self.assertIn("caveman", summary["healed"])
            self.assertIn("ponytail", summary["healed"])
            self.assertIn("memory", summary["healed"])
            self.assertEqual("healthy", result["components"]["companion-caveman"]["status"])
            self.assertEqual("healthy", result["components"]["memory"]["status"])
            self.assertFalse(
                (project / ".chaos-engine-state/companion-handoff.md").is_file()
            )
            self.assertFalse(
                (project / ".chaos-engine-state/official-self-heal-handoff.md").is_file()
            )

    def test_doctor_handoff_on_heal_failure_for_two_items(self):
        module = load(HEAL, "ce_official_self_heal_fail")
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary)
            (project / ".chaos-engine-state").mkdir(parents=True)
            result = {
                "status": "healthy",
                "components": {
                    "memory": {"status": "absent", "taskImpact": "advisory"},
                    "graphify": {"status": "broken", "taskImpact": "advisory"},
                    "mempalace": {"status": "healthy", "taskImpact": "advisory"},
                },
            }

            def boom_rematerialize(proj, *, names=None):
                raise RuntimeError("vendor-bytes-unavailable")

            def boom_repair(proj, name, **_kwargs):
                raise RuntimeError(f"cannot-install-{name}")

            summary = module.apply_doctor_official_self_heal(
                result,
                project,
                rematerialize=boom_rematerialize,
                repair=boom_repair,
                bundle={
                    "memory": True,
                    "mempalace": True,
                    "graphify": True,
                    "caveman": True,
                    "ponytail": False,
                },
            )
            self.assertIn("caveman", summary["failed"])
            self.assertIn("memory", summary["failed"])
            self.assertIn("graphify", summary["failed"])
            self.assertIn("ponytail", summary["skipped"])
            self.assertEqual("recovery-required", result["status"])

            companion_handoff = project / ".chaos-engine-state/companion-handoff.md"
            self.assertTrue(companion_handoff.is_file())
            companion_text = companion_handoff.read_text(encoding="utf-8")
            self.assertIn("Agent prompt:", companion_text)
            self.assertIn("official", companion_text.casefold())
            self.assertNotIn("rerun doctor", companion_text.casefold().split("agent prompt:")[-1][:80])
            # Pasteable prompt must include official command, not bare rerun.
            prompt = result["components"]["companion-caveman"]["agentPrompt"]
            self.assertIn("rematerialize", str(prompt).casefold())
            self.assertNotEqual("rerun doctor", str(prompt).casefold().strip())

            tool_handoff = project / ".chaos-engine-state/official-self-heal-handoff.md"
            self.assertTrue(tool_handoff.is_file())
            tool_text = tool_handoff.read_text(encoding="utf-8")
            self.assertIn("Official install command:", tool_text)
            self.assertIn("uv tool install", tool_text + result["components"]["graphify"].get("officialCommand", ""))
            memory_prompt = result["components"]["memory"]["agentPrompt"]
            self.assertIn("npm install -g @aictx/memory", str(memory_prompt))

    def test_opt_out_bundle_stays_off(self):
        module = load(HEAL, "ce_official_self_heal_optout")
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary)
            result = {"status": "healthy", "components": {}}
            called = {"rematerialize": 0, "repair": 0}

            def rematerialize(proj, *, names=None):
                called["rematerialize"] += 1
                return {"status": "healed", "names": list(names or [])}

            def repair(proj, name, **_kwargs):
                called["repair"] += 1
                return {"status": "repaired", "component": name}

            summary = module.apply_doctor_official_self_heal(
                result,
                project,
                rematerialize=rematerialize,
                repair=repair,
                bundle={
                    "memory": False,
                    "mempalace": False,
                    "graphify": False,
                    "caveman": False,
                    "ponytail": False,
                },
            )
            self.assertEqual(0, called["rematerialize"])
            self.assertEqual(0, called["repair"])
            self.assertEqual(
                {"caveman", "ponytail", "memory", "mempalace", "graphify"},
                set(summary["skipped"]),
            )
            self.assertEqual("optional", result["components"]["companion-caveman"]["taskImpact"])


if __name__ == "__main__":
    unittest.main()
