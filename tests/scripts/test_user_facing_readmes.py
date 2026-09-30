"""Orientation READMEs exist for every reactor module and their relative links resolve."""

import re
import unittest
import xml.etree.ElementTree as ET
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
POM_NS = {"m": "http://maven.apache.org/POM/4.0.0"}
LINK = re.compile(r"!\[[^\]]*\]\(([^)]+)\)|\[[^\]]*\]\(([^)]+)\)")

TOUCHED = (
    "README.md",
    "CONTRIBUTING.md",
    "SECURITY.md",
    "AGENTS.md",
    "CLAUDE.md",
    "GEMINI.md",
    "chaos-engine/README.md",
    ".github/workflows/README.md",
    "scripts/agents/user-harness/README.md",
    "tools/intellij-plugin-recording/README.md",
    "tools/local-ai-poc/README.md",
    "tools/repository-map/README.md",
    "shaft-skills/README.md",
    "shaft-skills/evaluation-prompts.md",
    ".agents/skills/README.md",
)


def reactor_readme_dirs() -> list[str]:
    """Maven modules from the root reactor, plus the Gradle IntelliJ plugin."""
    root_pom = ET.parse(ROOT / "pom.xml").getroot()
    modules = [
        element.text.strip()
        for element in root_pom.findall("m:modules/m:module", POM_NS)
        if element.text and element.text.strip()
    ]
    if "shaft-intellij" not in modules:
        modules.append("shaft-intellij")
    return modules


def scan_relative_links(relative_path: str) -> tuple[int, list[str]]:
    """Count relative markdown links in one file and list those that do not resolve."""
    document = ROOT / relative_path
    checked = 0
    missing: list[str] = []
    for match in LINK.finditer(document.read_text(encoding="utf-8")):
        destination = (match.group(1) or match.group(2) or "").strip().split()[0]
        if destination.startswith(("http://", "https://", "mailto:", "#")):
            continue
        path = destination.split("#", 1)[0].split("?", 1)[0]
        if not path:
            continue
        checked += 1
        target = (document.parent / path).resolve()
        overlay_source = None
        if ".chaos-engine/" in path.replace("\\", "/"):
            overlay_source = (ROOT / path.replace(".chaos-engine/", "chaos-engine/", 1)).resolve()
        if not target.exists() and not (overlay_source is not None and overlay_source.exists()):
            missing.append(destination)
    return checked, missing


class UserFacingReadmeTest(unittest.TestCase):
    def test_each_module_readme_states_purpose_and_skip(self) -> None:
        modules = reactor_readme_dirs()
        self.assertGreaterEqual(len(modules), 19)
        self.assertIn("shaft-engine", modules)
        self.assertIn("shaft-intellij", modules)
        for module in modules:
            readme = ROOT / module / "README.md"
            self.assertTrue(readme.is_file(), module)
            text = readme.read_text(encoding="utf-8")
            self.assertIn("**Purpose:**", text, module)
            self.assertIn("**Use or skip:**", text, module)
            self.assertIn("AGENTS.md", text, module)
            checked, broken = scan_relative_links(f"{module}/README.md")
            print(
                f"MODULE README OK {module}/README.md "
                f"relative_links_checked={checked} broken_relative_links={len(broken)}"
            )
            self.assertGreater(checked, 0, module)
            for destination in broken:
                self.fail(f"{module}/README.md -> {destination}")

    def test_touched_user_facing_links_resolve(self) -> None:
        checked_total = 0
        broken_total = 0
        for relative_path in TOUCHED:
            self.assertTrue((ROOT / relative_path).is_file(), relative_path)
            checked, broken = scan_relative_links(relative_path)
            checked_total += checked
            broken_total += len(broken)
            print(
                f"TOUCHED {relative_path} "
                f"relative_links_checked={checked} broken_relative_links={len(broken)}"
            )
            for destination in broken:
                self.fail(f"{relative_path} -> {destination}")
        self.assertGreater(checked_total, 0)
        self.assertEqual(broken_total, 0)
        print(
            f"zero broken relative links checked_set={checked_total} "
            f"broken_relative_links={broken_total}"
        )

    def test_root_orientation_and_thin_routers(self) -> None:
        readme = (ROOT / "README.md").read_text(encoding="utf-8")
        self.assertIn("https://shafthq.github.io/", readme)
        self.assertIn("mvn test", readme)
        self.assertNotIn("Three-stage UX", readme)
        self.assertNotIn("see `chaos-engine/README.md`", readme)
        print("ASSERTION PASS root README has no Three-stage UX claim and no see `chaos-engine/README.md` pointer")
        self.assertIn("AGENTS.md", readme)
        self.assertLessEqual(len(readme.splitlines()), 160)
        self.assertEqual((ROOT / "CLAUDE.md").read_text(encoding="utf-8").strip(), "@AGENTS.md")
        self.assertEqual((ROOT / "GEMINI.md").read_text(encoding="utf-8").strip(), "@AGENTS.md")
        agents = (ROOT / "AGENTS.md").read_text(encoding="utf-8")
        self.assertIn("CHAOSENGINE:START", agents)
        self.assertIn("CHAOSENGINE:END", agents)


if __name__ == "__main__":
    unittest.main()
