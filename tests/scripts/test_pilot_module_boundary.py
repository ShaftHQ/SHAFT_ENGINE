import unittest
import xml.etree.ElementTree as ET
from pathlib import Path


ROOT = Path(__file__).resolve().parents[2]
NS = {"m": "http://maven.apache.org/POM/4.0.0"}


def artifacts(path: Path) -> set[str]:
    root = ET.parse(path).getroot()
    return {
        dependency.findtext("m:artifactId", namespaces=NS)
        for dependency in root.findall("m:dependencies/m:dependency", NS)
    }


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
        ai_dependencies = artifacts(ROOT / "shaft-ai/pom.xml")
        service = ROOT / "shaft-ai/src/main/resources/META-INF/services/com.shaft.pilot.ai.AiProvider"
        providers = service.read_text(encoding="utf-8").splitlines()

        self.assertEqual(
            providers,
            [
                "com.shaft.ai.provider.AnthropicProvider",
                "com.shaft.ai.provider.GeminiProvider",
                "com.shaft.ai.provider.OllamaProvider",
                "com.shaft.ai.provider.OpenAiProvider",
            ],
        )
        self.assertTrue(all("openai" not in artifact.lower() for artifact in ai_dependencies))
        self.assertTrue(all("anthropic" not in artifact.lower() for artifact in ai_dependencies))
        self.assertTrue(all("gemini" not in artifact.lower() for artifact in ai_dependencies))
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
