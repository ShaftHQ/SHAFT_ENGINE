"""ICM Architect companion integration (#6001)."""

from __future__ import annotations

import importlib.util
import json
import tempfile
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
VENDOR = ROOT / "chaos-engine/vendor/icm-architect"


def load(path: Path, name: str):
    spec = importlib.util.spec_from_file_location(name, path)
    if spec is None or spec.loader is None:
        raise RuntimeError(f"unable to load module {name} from {path}")
    mod = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(mod)
    return mod


class IcmArchitectVendorTests(unittest.TestCase):
    def test_pin_and_skill_layout(self):
        pin = json.loads((VENDOR / "PIN.json").read_text(encoding="utf-8"))
        self.assertEqual(pin["repository"], "RinDig/icm-architect")
        self.assertEqual(pin["license"], "MIT")
        self.assertTrue(pin["commit"])
        skill = VENDOR / "skills/icm-architect/SKILL.md"
        self.assertTrue(skill.is_file())
        body = skill.read_text(encoding="utf-8")
        self.assertTrue(body.startswith("---"))
        self.assertIn("name: icm-architect", body)
        self.assertIn("ICM", body)
        for rel in pin["files"]:
            path = VENDOR / rel
            self.assertTrue(path.is_file(), rel)
            digest = __import__("hashlib").sha256(path.read_bytes()).hexdigest()
            self.assertEqual(digest, pin["files"][rel], rel)


class IcmArchitectHostsTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.hosts = load(ROOT / "chaos-engine/hosts.py", "hosts_icm_6001")

    def test_companion_constants(self):
        self.assertIn(self.hosts.ICM_ARCHITECT_PLUGIN_NAME, self.hosts.COMPANION_PLUGIN_NAMES)
        self.assertIn(
            self.hosts.ICM_ARCHITECT_PLUGIN_NAME,
            self.hosts.ADVISORY_COMPANION_PLUGIN_NAMES,
        )
        self.assertNotIn(
            self.hosts.ICM_ARCHITECT_PLUGIN_NAME,
            self.hosts.INTENSITY_COMPANION_PLUGIN_NAMES,
        )

    def test_rematerialize_icm_architect(self):
        with tempfile.TemporaryDirectory() as tmp:
            project = Path(tmp)
            result = self.hosts.rematerialize_companions(
                project, names=(self.hosts.ICM_ARCHITECT_PLUGIN_NAME,)
            )
            self.assertEqual(result["status"], "healthy")
            skill = project / "plugins/icm-architect/skills/icm-architect/SKILL.md"
            self.assertTrue(skill.is_file())
            self.assertTrue((project / "plugins/icm-architect/UPSTREAM.md").is_file())
            self.assertTrue(
                (project / "plugins/icm-architect/.claude-plugin/plugin.json").is_file()
            )


class IcmArchitectInstallTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.install = load(ROOT / "chaos-engine/install.py", "install_icm_6001")

    def test_bundle_default_and_without_flag(self):
        self.assertIn("icm-architect", self.install.DEFAULT_BUNDLE_COMPONENTS)
        options = self.install.normalize_bundle_options({"icm-architect": False})
        self.assertFalse(options["icm-architect"])
        parser = self.install.parser()
        install = next(
            action.choices["install"]
            for action in parser._actions
            if getattr(action, "choices", None) and "install" in action.choices
        )
        flags = {opt for action in install._actions for opt in action.option_strings}
        self.assertIn("--without-icm-architect", flags)


class IcmArchitectLifecycleTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.life = load(ROOT / "chaos-engine/hooks/lifecycle.py", "life_icm_6001")

    def test_advisory_not_required_intensity(self):
        self.assertEqual(self.life.COMPANION_NAMES, ("caveman", "ponytail"))
        self.assertEqual(self.life.ADVISORY_COMPANION_NAMES, ("icm-architect",))
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            skill = root / "plugins/icm-architect/skills/icm-architect/SKILL.md"
            skill.parent.mkdir(parents=True)
            skill.write_text("---\nname: icm-architect\n---\n", encoding="utf-8")
            # Also plant intensity companions so required lines stay stable.
            for name in ("caveman", "ponytail"):
                path = root / f"plugins/{name}/skills/{name}/SKILL.md"
                path.parent.mkdir(parents=True)
                path.write_text(f"---\nname: {name}\n---\n", encoding="utf-8")
            # Force search from tmp by chdir
            import os

            previous = os.getcwd()
            try:
                os.chdir(root)
                context = self.life.session_start_context(None, "active")
            finally:
                os.chdir(previous)
        self.assertIn("Advisory companion (design/structure): load", context)
        self.assertIn("icm-architect", context)
        self.assertNotIn("Required companion: read and follow `plugins/icm-architect", context)


class IcmArchitectPhraseGates(unittest.TestCase):
    def test_readme_and_notices_phrases(self):
        readme = (ROOT / "chaos-engine/README.md").read_text(encoding="utf-8")
        self.assertIn("ICM Architect", readme)
        self.assertIn("--without-icm-architect", readme)
        notices = (ROOT / "chaos-engine/THIRD_PARTY_NOTICES.md").read_text(encoding="utf-8")
        self.assertIn("RinDig/icm-architect", notices)
        skill = (ROOT / "chaos-engine/skills/chaos-engine/SKILL.md").read_text(encoding="utf-8")
        self.assertIn("icm-architect", skill)
        design = (ROOT / "chaos-engine/references/design-loop.md").read_text(encoding="utf-8")
        self.assertIn("ICM Architect", design)


if __name__ == "__main__":
    unittest.main()
