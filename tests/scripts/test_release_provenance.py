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
