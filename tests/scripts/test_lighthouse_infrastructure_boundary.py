import unittest
from pathlib import Path


ROOT = Path(__file__).resolve().parents[2]


class LighthouseInfrastructureBoundaryTest(unittest.TestCase):
    def test_shared_provider_and_cli_are_registered_without_engine_dependency(self):
        infrastructure_pom = (ROOT / "shaft-infrastructure/pom.xml").read_text(encoding="utf-8")
        registry = (ROOT / "shaft-infrastructure/src/main/java/com/shaft/infrastructure/InfrastructureSetupService.java").read_text(encoding="utf-8")
        cli = (ROOT / "shaft-cli/src/main/java/com/shaft/commandline/command/SetupCommand.java").read_text(encoding="utf-8")

        self.assertNotIn("<artifactId>shaft-engine</artifactId>", infrastructure_pom)
        self.assertIn("new LighthouseSetupProvider()", registry)
        self.assertIn(".supports(profile)", cli)
        self.assertNotIn("profile != SetupProfile.REPORTING && profile != SetupProfile.OCR", cli)

    def test_release_assets_and_runtime_boundary_are_tracked(self):
        self.assertTrue((ROOT / "shaft-infrastructure/src/main/resources/com/shaft/infrastructure/lighthouse/package.json").is_file())
        self.assertTrue((ROOT / "shaft-infrastructure/src/main/resources/com/shaft/infrastructure/lighthouse/package-lock.json").is_file())
        runtime = (ROOT / "shaft-engine/src/main/java/com/shaft/performance/internal/LightHouseGenerateReport.java").read_text(encoding="utf-8")
        active = runtime.split("public void generateLightHouseReport()", 1)[1].split("static int debuggerPort", 1)[0]
        self.assertIn("LighthouseRuntime", active)
        self.assertIn("commandRunner.run", active)
        self.assertNotIn("writeNodeScriptFileInProjectDirectory", active)
        self.assertNotIn("performTerminalCommand", active)

    def test_ci_runs_the_boundary_and_real_managed_lighthouse_flow(self):
        pr_gate = (ROOT / ".github/workflows/pr-gate.yml").read_text(encoding="utf-8")
        local_e2e = (ROOT / ".github/workflows/e2eLocalTests.yml").read_text(encoding="utf-8")

        self.assertIn("tests.scripts.test_lighthouse_infrastructure_boundary", pr_gate)
        self.assertIn("setup plan --profile LIGHTHOUSE", local_e2e)
        self.assertIn("setup install", local_e2e)
        self.assertIn("setup verify --profile LIGHTHOUSE", local_e2e)
        self.assertIn("-DrunManagedLighthouseE2E=true", local_e2e)
        self.assertIn("^shaft-cli-[0-9][0-9.]*\\.jar$", local_e2e)


if __name__ == "__main__":
    unittest.main()
