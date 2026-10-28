"""#7003: release provenance is additive and can never block a release."""
from __future__ import annotations

import unittest
from pathlib import Path

import yaml

ROOT = Path(__file__).resolve().parents[2]


class ReleaseProvenanceTest(unittest.TestCase):
    def setUp(self) -> None:
        self.wf = yaml.safe_load((ROOT / ".github/workflows/mavenCentral_cd.yml").read_text(encoding="utf-8"))

    def test_attestation_follows_central_verification_and_cannot_fail_release(self) -> None:
        job = self.wf["jobs"]["build_release_and_deliver"]
        names = [s.get("name") for s in job["steps"]]
        attest = names.index("Attest build provenance (jars + SBOM)")
        self.assertGreater(attest, names.index("Verify published Maven Central coordinates"))
        for step in job["steps"][attest:]:
            self.assertTrue(step.get("continue-on-error"), step.get("name"))
        self.assertEqual("write", job["permissions"]["id-token"])
        self.assertEqual("write", job["permissions"]["attestations"])
        self.assertEqual("read", job["permissions"]["contents"])

    def test_sbom_attached_without_blocking_the_release(self) -> None:
        steps = self.wf["jobs"]["announce_release"]["steps"]
        sbom = next(s for s in steps if s.get("name") == "Download release SBOM")
        self.assertTrue(sbom["continue-on-error"])
        self.assertEqual("/tmp/agent-plugin-release-assets", sbom["with"]["path"])
        readme = (ROOT / ".github/workflows/README.md").read_text(encoding="utf-8")
        self.assertIn("gh attestation verify", readme)

    def test_intoto_bundle_staged_for_release_assets(self) -> None:
        job = self.wf["jobs"]["build_release_and_deliver"]
        step = next(s for s in job["steps"] if s.get("name") == "Stage provenance bundle for the GitHub Release")
        self.assertTrue(step["continue-on-error"])
        self.assertIn(".intoto.jsonl", step["run"])
        upload = next(s for s in job["steps"] if s.get("name") == "Keep the SBOM for the GitHub Release")
        self.assertIn("*.intoto.jsonl", upload["with"]["path"])

    def test_reconcile_attaches_provenance_without_failing_the_job(self) -> None:
        text = (ROOT / ".github/workflows/maven-central-reconcile.yml").read_text(encoding="utf-8")
        workflow = yaml.safe_load(text)
        job = workflow["jobs"]["reconcile"]
        self.assertEqual("write", job["permissions"]["contents"])
        self.assertEqual("write", job["permissions"]["id-token"])
        self.assertEqual("write", job["permissions"]["attestations"])
        pin = "actions/attest-build-provenance@4d101475d8b20a2381f78447822ac1eab6504dd8"
        attest = next(step for step in job["steps"] if str(step.get("uses", "")).startswith(pin))
        self.assertEqual(pin, attest["uses"].split()[0])
        self.assertTrue(attest.get("continue-on-error"))
        self.assertIn("inputs.dry_run", attest["if"])
        self.assertIn("*/target/*.jar", attest["with"]["subject-path"])
        self.assertIn("target/bom.json", attest["with"]["subject-path"])
        reconcile = next(
            step for step in job["steps"] if "reconcile_maven_central_release.py" in str(step.get("run", ""))
        )
        attach = next(
            step for step in job["steps"] if "attach_release_provenance.py" in str(step.get("run", ""))
        )
        self.assertGreater(job["steps"].index(attest), job["steps"].index(reconcile))
        self.assertGreater(job["steps"].index(attach), job["steps"].index(attest))
        self.assertTrue(attach.get("continue-on-error"))
        self.assertIn("inputs.dry_run", attach["if"])
        self.assertIn("steps.attest.outcome == 'success'", attach["if"])
        self.assertIn("attach_release_provenance.py", text)
        self.assertIn(pin, text)

    def test_release_blocks_on_unlabelled_binary_break(self) -> None:
        jobs = self.wf["jobs"]
        self.assertIn("cd_api_compat", jobs["build_release_and_deliver"]["needs"])
        compat = jobs["cd_api_compat"]
        self.assertNotIn("continue-on-error", compat)
        judge = next(s for s in compat["steps"] if s.get("name") == "Judge")
        self.assertNotIn("continue-on-error", judge)
        self.assertIn("api_compat_gate.py", judge["run"])

    def test_japicmp_does_not_ignore_missing_classes_globally(self) -> None:
        self.assertNotIn("ignoreMissingClasses", (ROOT / "pom.xml").read_text(encoding="utf-8"))
        self.assertIn("junit-jupiter-api", (ROOT / "shaft-cli/pom.xml").read_text(encoding="utf-8"))


if __name__ == "__main__":
    unittest.main()
