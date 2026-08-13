import unittest
import re
from pathlib import Path


ROOT = Path(__file__).resolve().parents[2]
def dependencies(module: str) -> set[str]:
    pom = (ROOT / module / "pom.xml").read_text(encoding="utf-8")
    sections = re.findall(r"<dependencies>(.*?)</dependencies>", pom, re.DOTALL)
    return set(re.findall(r"<artifactId>([^<]+)</artifactId>", "\n".join(sections)))


class OcrInfrastructureBoundaryTest(unittest.TestCase):
    def test_dependency_direction_keeps_native_ocr_out_of_setup_and_cli(self):
        self.assertNotIn("shaft-engine", dependencies("shaft-infrastructure"))
        self.assertNotIn("shaft-ocr", dependencies("shaft-infrastructure"))
        self.assertIn("shaft-infrastructure", dependencies("shaft-ocr"))
        self.assertNotIn("shaft-ocr", dependencies("shaft-cli"))

    def test_shared_setup_is_the_only_ocr_network_owner(self):
        manager = (ROOT / "shaft-ocr/src/main/java/com/shaft/ocr/internal/TessdataModelManager.java").read_text()
        provider = (ROOT / "shaft-ocr/src/main/java/com/shaft/ocr/internal/TesseractOcrProvider.java").read_text()
        registry = (ROOT / "shaft-infrastructure/src/main/java/com/shaft/infrastructure/InfrastructureSetupService.java").read_text()

        self.assertNotIn("java.net.http", manager)
        self.assertNotIn("HttpClient", manager)
        self.assertNotIn("Files.createDirectories", manager)
        self.assertIn("OcrSetupManifest", provider)
        self.assertIn("new OcrSetupProvider()", registry)

    def test_ocr_provider_is_gated_by_unit_and_mobile_workflows(self):
        pr_gate = (ROOT / ".github/workflows/pr-gate.yml").read_text()
        e2e = (ROOT / ".github/workflows/e2eTests.yml").read_text()

        self.assertIn("shaft-ocr/**", pr_gate)
        self.assertIn("module: [ shaft-infrastructure, shaft-engine", pr_gate)
        self.assertIn("shaft-ocr ]", pr_gate)
        self.assertEqual(2, e2e.count("setup plan --profile OCR"))
        self.assertEqual(2, e2e.count("-Dshaft.ocr.downloadEnabled=false"))


if __name__ == "__main__":
    unittest.main()
