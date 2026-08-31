from __future__ import annotations

import importlib.util
import json
import unittest
from pathlib import Path


ROOT = Path(__file__).resolve().parents[2]
CORE = ROOT / "chaos-engine"


def load(path: Path, name: str):
    spec = importlib.util.spec_from_file_location(name, path)
    if spec is None or spec.loader is None:
        raise RuntimeError(f"could not load module from {path}")
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


class OmniRootPortabilityTest(unittest.TestCase):
    def test_portable_payload_contains_optional_transport_and_workflow_owner(self):
        installer = load(CORE / "install.py", "omniroot_portable_installer")
        packaged = {
            path.relative_to(CORE).as_posix()
            for path in installer.source_files(CORE, "portable")
        }
        self.assertLessEqual(
            {
                "skills/omniroot/SKILL.md",
                "skills/omniroot/scripts/runner.py",
                "references/execution-workflows.md",
            },
            packaged,
        )

    def test_parity_matrix_covers_every_supported_host_without_forking_policy(self):
        matrix = json.loads(
            (ROOT / "scripts/ci/agent_harness_parity.json").read_text(encoding="utf-8")
        )
        hosts = ["claude", "codex", "copilot", "gemini", "grok"]
        self.assertEqual(hosts, matrix["hosts"])
        for capability in matrix["capabilities"]:
            for host in hosts:
                self.assertIn(host, capability)
            if capability["mode"] == "shared":
                evidence = [capability[host] for host in hosts]
                self.assertTrue(all(value == evidence[0] for value in evidence[1:]))

    def test_documentation_routes_to_skill_and_canonical_workflow_owner(self):
        readme = (CORE / "README.md").read_text(encoding="utf-8")
        install = (CORE / "INSTALL.md").read_text(encoding="utf-8")
        map_text = (ROOT / ".agents/skills/README.md").read_text(encoding="utf-8")
        guide = (CORE / "guides/omniroute.md").read_text(encoding="utf-8")
        for text in (readme, install, map_text):
            self.assertIn("omniroot", text.casefold())
        self.assertIn("skills/omniroot/SKILL.md", readme)
        self.assertIn("references/execution-workflows.md", map_text)
        self.assertIn("../skills/omniroot/SKILL.md", guide)
        self.assertIn("optional", guide.casefold())


if __name__ == "__main__":
    unittest.main()
