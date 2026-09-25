"""Short vendor plugin adapters that still activate the full ruleset (#6198)."""

from __future__ import annotations

import importlib.util
import json
import os
import re
import shutil
import subprocess
import sys
import tempfile
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
SOURCE = ROOT / "chaos-engine"
SHORT = ("caveman", "icm-architect")


def _load(name: str, path: Path):
    spec = importlib.util.spec_from_file_location(name, path)
    if spec is None or spec.loader is None:
        raise RuntimeError(path)
    module = importlib.util.module_from_spec(spec)
    sys.modules[name] = module
    spec.loader.exec_module(module)
    return module


HOSTS = _load("hosts_6198", SOURCE / "hosts.py")


def _description(body: str) -> str:
    match = re.match(r"---\n(.*?)\n---", body, re.S)
    if match is None:
        return ""
    found = re.search(r"^description:\s*(.+)$", match.group(1), re.M)
    return found.group(1).strip().strip("\"'") if found else ""


class ShortAdapterTest(unittest.TestCase):
    def test_short_adapters_point_at_the_vendor_body(self):
        images = HOSTS.companion_plugin_images()
        for name in SHORT:
            body = images[f"plugins/{name}/skills/{name}/SKILL.md"].decode("utf-8")
            with self.subTest(plugin=name):
                self.assertTrue(60 <= len(_description(body)) <= 220, _description(body))
                self.assertIn(f".chaos-engine/vendor/{name}/skills/{name}/SKILL.md", body)
                self.assertLess(len(body), 800)

    def test_adapter_descriptions_match_the_index(self):
        index = json.loads((SOURCE / "harness-index.json").read_text(encoding="utf-8"))
        vendor = {entry["name"]: entry for entry in index["entries"] if entry.get("kind") == "vendor"}
        images = HOSTS.companion_plugin_images()
        for name in SHORT:
            body = images[f"plugins/{name}/skills/{name}/SKILL.md"].decode("utf-8")
            with self.subTest(plugin=name):
                self.assertEqual(vendor[name]["description"], _description(body))

    def test_ponytail_keeps_its_body_because_pinned_hooks_read_it(self):
        images = HOSTS.companion_plugin_images()
        vendor = (SOURCE / "vendor/ponytail/skills/ponytail/SKILL.md").read_bytes()
        self.assertEqual(vendor, images["plugins/ponytail/skills/ponytail/SKILL.md"])


@unittest.skipIf(shutil.which("node") is None, "node is required for the activate hook")
class CavemanActivationTest(unittest.TestCase):
    def test_activation_reads_the_vendor_body_behind_a_short_adapter(self):
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary) / "project"
            for relative, content in HOSTS.companion_plugin_images().items():
                if relative.startswith("plugins/caveman/"):
                    target = project / relative
                    target.parent.mkdir(parents=True, exist_ok=True)
                    target.write_bytes(content)
            vendor = project / ".chaos-engine/vendor/caveman/skills/caveman/SKILL.md"
            vendor.parent.mkdir(parents=True)
            shutil.copyfile(SOURCE / "vendor/caveman/skills/caveman/SKILL.md", vendor)
            home = Path(temporary) / "home"
            home.mkdir()
            plugin = project / "plugins/caveman"
            env = {
                **os.environ,
                "HOME": str(home),
                "USERPROFILE": str(home),
                "CLAUDE_CONFIG_DIR": str(home / ".claude"),
                "CLAUDE_PLUGIN_ROOT": str(plugin),
                "CAVEMAN_DEFAULT_MODE": "ultra",
            }
            completed = subprocess.run(  # nosec B603 - fixed node hook in a temp project.
                [shutil.which("node"), str(plugin / "src/hooks/caveman-activate.js")],
                cwd=project,
                env=env,
                input="{}",
                capture_output=True,
                text=True,
                timeout=30,
                check=False,
            )
        self.assertEqual(0, completed.returncode, completed.stderr)
        self.assertIn("CAVEMAN MODE ACTIVE", completed.stdout)
        self.assertIn("## Intensity", completed.stdout, "fell back to the minimal ruleset")


class ListingBudgetTest(unittest.TestCase):
    def test_plugin_skills_count_toward_the_listing_budget(self):
        budget = json.loads((ROOT / "scripts/ci/agent_guidance_budget.json").read_text(encoding="utf-8"))
        for host in ("codex", "claude"):
            with self.subTest(host=host):
                patterns = budget["host_skill_metadata_globs"][host]
                self.assertIn("chaos-engine/plugin-adapters/*/SKILL.md", patterns)
                self.assertIn("chaos-engine/vendor/ponytail/skills/ponytail/SKILL.md", patterns)


if __name__ == "__main__":
    unittest.main()
