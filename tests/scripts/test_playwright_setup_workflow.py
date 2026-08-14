from pathlib import Path
import unittest

import yaml


ROOT = Path(__file__).resolve().parents[2]
WORKFLOW = ROOT / ".github" / "workflows" / "e2eTests.yml"
INSTALLER = ROOT / "scripts" / "ci" / "install_managed_playwright.sh"


class PlaywrightSetupWorkflowTest(unittest.TestCase):
    def test_bundled_playwright_jobs_use_the_packaged_managed_setup_flow(self):
        workflow = WORKFLOW.read_text(encoding="utf-8")
        installer = INSTALLER.read_text(encoding="utf-8")

        self.assertEqual(4, workflow.count("bash scripts/ci/install_managed_playwright.sh"))
        self.assertGreaterEqual(workflow.count("-Dinfrastructure.mode=MANAGED"), 4)
        self.assertGreaterEqual(workflow.count("-Dinfrastructure.profile=PLAYWRIGHT"), 4)
        self.assertGreaterEqual(workflow.count(
            "-Dinfrastructure.cacheDirectory=${{ runner.temp }}/shaft-playwright-cache"), 4)

        self.assertEqual(1, installer.count(" setup plan --profile PLAYWRIGHT --mode MANAGED "))
        self.assertEqual(1, installer.count(" setup install --plan "))
        self.assertEqual(1, installer.count(" setup verify --profile PLAYWRIGHT --mode MANAGED "))
        self.assertIn('--cache-root "$cache_root" --data-root "$data_root"', installer)

    def test_only_explicit_browser_channels_may_use_the_upstream_installer(self):
        workflow = WORKFLOW.read_text(encoding="utf-8")
        raw_installs = [line.strip() for line in workflow.splitlines()
                        if "com.microsoft.playwright.CLI" in line]

        self.assertEqual(4, len(raw_installs))
        self.assertTrue(any("install --with-deps chrome" in line for line in raw_installs))
        self.assertTrue(any("install --with-deps msedge" in line for line in raw_installs))
        self.assertEqual(2, sum("install-deps webkit" in line for line in raw_installs))
        self.assertTrue(all("install webkit" not in line and "install firefox" not in line
                            and "install chromium" not in line for line in raw_installs))

    def test_each_linux_webkit_job_prepares_host_dependencies_after_managed_install(self):
        jobs = yaml.safe_load(WORKFLOW.read_text(encoding="utf-8"))["jobs"]

        for job_name in ("Ubuntu_Playwright_WebKit", "Ubuntu_Playwright_IPhone17ProMax"):
            steps = jobs[job_name]["steps"]
            managed_index = next(index for index, step in enumerate(steps)
                                 if "install_managed_playwright.sh" in step.get("run", ""))
            dependency_indexes = [index for index, step in enumerate(steps)
                                  if "install-deps webkit" in step.get("run", "")]
            test_index = next(index for index, step in enumerate(steps)
                              if "-Dinfrastructure.profile=PLAYWRIGHT" in step.get("run", ""))

            self.assertEqual([managed_index + 1], dependency_indexes, job_name)
            self.assertLess(dependency_indexes[0], test_index, job_name)


if __name__ == "__main__":
    unittest.main()
