import unittest
from pathlib import Path


ROOT = Path(__file__).resolve().parents[2]


class AndroidInfrastructureBoundaryTest(unittest.TestCase):
    def test_shared_infrastructure_is_the_only_android_mutation_and_lifecycle_owner(self):
        mcp = (ROOT / "shaft-mcp/src/main/java/com/shaft/mcp/McpMobileToolchainService.java").read_text(encoding="utf-8")
        inspector = (ROOT / "shaft-mcp/src/main/java/com/shaft/mcp/McpMobileInspectorRecordingService.java").read_text(encoding="utf-8")

        self.assertIn("InfrastructureSetupService", mcp)
        self.assertIn("ManagedEnvironment", inspector)
        self.assertNotIn("ensureAndroidCommandLineTools", mcp)
        self.assertNotIn("startAndroidEmulator", mcp)
        self.assertNotIn("--relaxed-security", mcp)

    def test_pr_gate_cannot_omit_android_or_packaged_cli_changes(self):
        workflow = (ROOT / ".github/workflows/pr-gate.yml").read_text(encoding="utf-8")

        self.assertGreaterEqual(workflow.count("- 'shaft-infrastructure/**'"), 2)
        self.assertIn("tests.scripts.test_android_infrastructure_boundary", workflow)
        self.assertIn("setup plan --profile MOBILE_ANDROID", workflow)
        self.assertIn("android-sdk-license", workflow)

    def test_real_acceptance_is_gated_and_proves_uiautomator2_and_aapt2(self):
        workflow = (ROOT / ".github/workflows/e2eLocalTests.yml").read_text(encoding="utf-8")
        acceptance = (ROOT / "shaft-engine/src/test/java/testPackage/ManagedAndroidE2ETest.java").read_text(encoding="utf-8")

        self.assertIn("Ubuntu_Managed_Android", workflow)
        self.assertIn("runManagedAndroidE2E=true", workflow)
        self.assertIn("setup install", workflow)
        self.assertIn("--accept-license android-sdk-license", workflow)
        self.assertIn("mobile-android-install.log", workflow)
        self.assertIn("sudo apt-get install --yes libpulse0", workflow)
        self.assertIn("UiAutomator2Options", acceptance)
        self.assertIn("aapt2", acceptance)
        self.assertIn("getPageSource", acceptance)


if __name__ == "__main__":
    unittest.main()
