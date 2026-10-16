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


if __name__ == "__main__":
    unittest.main()
