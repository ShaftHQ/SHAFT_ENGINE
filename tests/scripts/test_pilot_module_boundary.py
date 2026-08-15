import re
import unittest
import xml.etree.ElementTree as ET
from pathlib import Path


ROOT = Path(__file__).resolve().parents[2]
NS = {"m": "http://maven.apache.org/POM/4.0.0"}
PROVIDER_SOURCE = ROOT / "shaft-ai/src/main/java/com/shaft/ai"
PROVIDER_SERVICE = (
    ROOT / "shaft-ai/src/main/resources/META-INF/services/com.shaft.pilot.ai.AiProvider"
)


def artifacts(path: Path) -> set[str]:
    root = ET.parse(path).getroot()
    return {
        dependency.findtext("m:artifactId", namespaces=NS)
        for dependency in root.findall("m:dependencies/m:dependency", NS)
    }


def class_declaration(path: Path) -> str:
    """Return the line declaring the class this file is named for."""
    match = re.search(
        rf"^[^\n]*\bclass\s+{re.escape(path.stem)}\b",
        path.read_text(encoding="utf-8"),
        re.MULTILINE,
    )
    if match is None:
        raise AssertionError(f"{path.name} declares no class named {path.stem}")
    return match.group(0)


def concrete_provider_classes() -> list[str]:
    """Every provider a ServiceLoader could actually instantiate, from the sources.

    Abstract is decided from the class's own declaration line rather than by
    searching the file for the word, so a comment or a nested type cannot
    change the answer. `AbstractHttpAiProvider` matches `*Provider.java` and is
    excluded here for the same reason ServiceLoader could not construct it.
    """
    providers = []
    for path in PROVIDER_SOURCE.rglob("*Provider.java"):
        declaration = class_declaration(path)
        content = path.read_text(encoding="utf-8")
        if "abstract" in declaration:
            continue
        if path.parent.name != "provider" and not re.search(
            rf"\bclass\s+{re.escape(path.stem)}\b[^{{]*\bimplements\s+AiProvider\b",
            content,
        ):
            continue
        providers.append(
            ".".join(
                ("com", "shaft", "ai", *path.relative_to(PROVIDER_SOURCE).with_suffix("").parts)
            )
        )
    return sorted(providers)


class PilotModuleBoundaryTest(unittest.TestCase):
    def test_engine_has_no_pilot_or_provider_dependency(self):
        engine_dependencies = artifacts(ROOT / "shaft-engine/pom.xml")

        self.assertNotIn("shaft-pilot-core", engine_dependencies)
        self.assertNotIn("shaft-capture", engine_dependencies)
        self.assertNotIn("shaft-ai", engine_dependencies)

    def test_dependency_direction_is_engine_then_core_then_capture_or_ai(self):
        core_dependencies = artifacts(ROOT / "shaft-pilot-core/pom.xml")
        capture_dependencies = artifacts(ROOT / "shaft-capture/pom.xml")
        ai_dependencies = artifacts(ROOT / "shaft-ai/pom.xml")

        self.assertIn("shaft-engine", core_dependencies)
        self.assertNotIn("shaft-capture", core_dependencies)
        self.assertNotIn("shaft-ai", core_dependencies)
        self.assertIn("shaft-pilot-core", capture_dependencies)
        self.assertNotIn("shaft-ai", capture_dependencies)
        self.assertIn("shaft-pilot-core", ai_dependencies)

    def test_direct_provider_module_uses_service_loader_without_provider_sdks(self):
        """The registration and the classes are one set, derived rather than typed twice.

        The expected list was five hardcoded entries, and it went stale the day
        `LmStudioProvider` was added (`daacce6b77`), staying red on `main` ever
        since because no workflow ran this module (#4506). The hardcoded list
        was the wrong instrument, not merely an out-of-date one: registering a
        provider means editing two files, and only one of them failed when you
        forgot -- so the list could only ever drift toward whichever file
        someone remembered.

        Derived from the sources, both directions fail. A class added without a
        registration is missing from the file; a registration with no class
        behind it is an entry ServiceLoader would throw on. The sorted
        comparison also keeps the file ordered and free of duplicates, which
        `assertEqual` on a list gives for nothing.
        """
        ai_dependencies = artifacts(ROOT / "shaft-ai/pom.xml")
        providers = PROVIDER_SERVICE.read_text(encoding="utf-8").splitlines()
        concrete = concrete_provider_classes()

        # An empty derivation would make the comparison vacuous the moment the
        # glob stopped matching -- a moved package would read as agreement.
        self.assertTrue(concrete, f"no concrete provider classes under {PROVIDER_SOURCE}")
        self.assertEqual(providers, concrete)
        self.assertTrue(all("openai" not in artifact.lower() for artifact in ai_dependencies))
        self.assertTrue(all("anthropic" not in artifact.lower() for artifact in ai_dependencies))
        self.assertTrue(all("gemini" not in artifact.lower() for artifact in ai_dependencies))
        self.assertTrue(all("github" not in artifact.lower() for artifact in ai_dependencies))
        self.assertTrue(all("ollama" not in artifact.lower() for artifact in ai_dependencies))

    def test_bom_and_consumer_fixture_expose_provider_neutral_core(self):
        bom = ET.parse(ROOT / "shaft-bom/pom.xml").getroot()
        managed = {
            dependency.findtext("m:artifactId", namespaces=NS)
            for dependency in bom.findall("m:dependencyManagement/m:dependencies/m:dependency", NS)
        }
        fixture_dependencies = artifacts(
            ROOT / "tools/modularization/consumer-fixtures/pilot-core/pom.xml"
        )

        self.assertIn("shaft-pilot-core", managed)
        self.assertIn("shaft-capture", managed)
        self.assertIn("shaft-ai", managed)
        self.assertEqual(fixture_dependencies, {"shaft-pilot-core"})

    def test_capture_jar_packages_the_versioned_schema(self):
        capture_pom = (ROOT / "shaft-capture/pom.xml").read_text(encoding="utf-8")

        self.assertIn("<artifactId>maven-jar-plugin</artifactId>", capture_pom)
        self.assertIn("<include>schema/**/*</include>", capture_pom)

    def test_mcp_workflow_is_manual_and_daily_only(self):
        workflow = (ROOT / ".github/workflows/shaft-mcp.yml").read_text(encoding="utf-8")

        self.assertIn("workflow_dispatch:", workflow)
        self.assertIn("cron: '00 1 * * *'", workflow)
        self.assertNotIn("pull_request:", workflow)
        self.assertNotIn("\n  push:", workflow)


if __name__ == "__main__":
    unittest.main()
