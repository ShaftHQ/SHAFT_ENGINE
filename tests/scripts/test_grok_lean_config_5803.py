"""Unit tests for ChaosEngine Grok lean config (#5802/#5804/#5805)."""

from __future__ import annotations

import importlib.util
import tempfile
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
MODULE = ROOT / "chaos-engine/grok_lean_config.py"


def load():
    spec = importlib.util.spec_from_file_location("grok_lean_config_5803", MODULE)
    assert spec is not None and spec.loader is not None
    mod = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(mod)
    return mod


class GrokLeanCompatTests(unittest.TestCase):
    def test_merge_idempotent_preserves_foreign_keys(self):
        mod = load()
        before = (
            "# user comment\n"
            "model = \"grok-4\"\n\n"
            "[mcp_servers.context7]\n"
            "command = \"npx\"\n"
        )
        once = mod.merge_lean_compat(before)
        twice = mod.merge_lean_compat(once)
        self.assertEqual(once, twice)
        self.assertIn("model = \"grok-4\"", twice)
        self.assertIn("[mcp_servers.context7]", twice)
        self.assertIn(mod.LEAN_START, twice)
        self.assertIn(mod.LEAN_END, twice)
        self.assertIn("[compat.claude]", twice)
        self.assertIn("[compat.cursor]", twice)
        for key in mod.COMPAT_FALSE_KEYS:
            self.assertIn(f"{key} = false", twice)
        self.assertTrue(mod.lean_compat_applied(twice))

    def test_strips_outside_duplicate_compat_tables(self):
        mod = load()
        before = (
            "[compat.claude]\n"
            "hooks = true\n"
            "rules = true\n\n"
            "[other]\n"
            "x = 1\n"
        )
        after = mod.merge_lean_compat(before)
        # Exactly one lean span; outside hooks=true table removed.
        self.assertEqual(after.count("[compat.claude]"), 1)
        self.assertNotIn("hooks = true", after)
        self.assertIn("[other]", after)
        self.assertIn("x = 1", after)

    def test_uninstall_removes_only_ce_span(self):
        mod = load()
        text = mod.merge_lean_compat("keep = true\n")
        removed = mod.remove_lean_compat(text)
        self.assertNotIn(mod.LEAN_START, removed)
        self.assertNotIn("[compat.claude]", removed)
        self.assertIn("keep = true", removed)

    def test_apply_and_remove_file(self):
        mod = load()
        with tempfile.TemporaryDirectory() as tmp:
            home = Path(tmp)
            path = home / ".grok" / "config.toml"
            path.parent.mkdir(parents=True)
            path.write_text("alpha = 1\n", encoding="utf-8")
            applied = mod.apply_lean_compat_file(path)
            self.assertTrue(applied["changed"])
            body = path.read_text(encoding="utf-8")
            self.assertTrue(mod.lean_compat_applied(body))
            self.assertIn("alpha = 1", body)
            removed = mod.remove_lean_compat_file(path)
            self.assertTrue(removed["changed"])
            self.assertNotIn(mod.LEAN_START, path.read_text(encoding="utf-8"))

    def test_should_apply_when_hooks_present_without_binary(self):
        mod = load()
        with tempfile.TemporaryDirectory() as tmp:
            project = Path(tmp)
            hooks = project / ".grok" / "hooks"
            hooks.mkdir(parents=True)
            (hooks / "lifecycle.json").write_text("{}", encoding="utf-8")
            self.assertTrue(
                mod.should_apply_grok_lean(project, which=lambda _n: None)
            )

    def test_doctor_heals_missing_compat(self):
        mod = load()
        with tempfile.TemporaryDirectory() as tmp:
            home = Path(tmp)
            project = Path(tmp) / "proj"
            hooks = project / ".grok" / "hooks"
            hooks.mkdir(parents=True)
            (hooks / "lifecycle.json").write_text("{}", encoding="utf-8")
            # Point GROK_HOME via home arg
            result = mod.doctor_lean_compat(
                project, home=home, which=lambda _n: None, heal=True
            )
            self.assertEqual(result["status"], "healthy")
            cfg = home / ".grok" / "config.toml"
            self.assertTrue(cfg.is_file())
            self.assertTrue(mod.lean_compat_applied(cfg.read_text(encoding="utf-8")))


class GrokLeanSkillsTests(unittest.TestCase):
    def test_flag_env_and_merge(self):
        mod = load()
        self.assertFalse(mod.lean_skills_flag_enabled(env={}))
        self.assertTrue(
            mod.lean_skills_flag_enabled(env={"CHAOS_ENGINE_LEAN_GROK_SKILLS": "1"})
        )
        self.assertTrue(mod.lean_skills_flag_enabled(cli_flag=True, env={}))
        text = mod.merge_lean_skills("x = 1\n")
        self.assertTrue(mod.lean_skills_applied(text))
        for name in mod.LEAN_GROK_SKILL_DISABLE:
            self.assertIn(name, text)
        self.assertEqual(mod.merge_lean_skills(text), text)
        undone = mod.remove_lean_skills(text)
        self.assertNotIn(mod.SKILLS_START, undone)
        self.assertIn("x = 1", undone)

    def test_doctor_tip_when_flag_unused(self):
        mod = load()
        with tempfile.TemporaryDirectory() as tmp:
            project = Path(tmp)
            hooks = project / ".grok" / "hooks"
            hooks.mkdir(parents=True)
            (hooks / "lifecycle.json").write_text("{}", encoding="utf-8")
            tip = mod.doctor_lean_skills_tip(
                project, home=Path(tmp), which=lambda _n: None, env={}
            )
            self.assertEqual(tip["status"], "sync-advisory")
            self.assertIn("lean-grok-skills", tip["fixNext"])


class GrokSkillDedupeTests(unittest.TestCase):
    def test_duplicate_detection(self):
        mod = load()
        payload = {
            "skills": [
                {"name": "chaos-engine", "path": ".agents/skills/chaos-engine/SKILL.md"},
                {
                    "name": "chaos-engine",
                    "path": "plugins/chaos-engine/skills/chaos-engine/SKILL.md",
                },
            ]
        }
        dupes = mod.duplicate_chaos_engine_skills_from_inspect(payload)
        self.assertEqual(len(dupes), 2)
        self.assertTrue(mod.expected_chaos_engine_pointer_pair(dupes))
        self.assertEqual(
            mod.duplicate_chaos_engine_skills_from_inspect(
                {"skills": [{"name": "chaos-engine", "path": "a"}]}
            ),
            [],
        )
        extra = list(dupes) + ["~/.grok/skills/chaos-engine/SKILL.md"]
        self.assertFalse(mod.expected_chaos_engine_pointer_pair(extra))


class SkillAdapterPointerTests(unittest.TestCase):
    def test_host_skill_adapters_are_pointers(self):
        hosts_path = ROOT / "chaos-engine/hosts.py"
        spec = importlib.util.spec_from_file_location("hosts_5803_ptr", hosts_path)
        assert spec is not None and spec.loader is not None
        hosts = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(hosts)
        body = hosts.skill_adapter_bytes(".chaos-engine").decode()
        self.assertIn("canonical ChaosEngine", body)
        self.assertNotIn("## Iron laws", body)
        self.assertLess(len(body), 800)


if __name__ == "__main__":
    unittest.main()
