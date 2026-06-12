import importlib.util
import tempfile
import unittest
import zipfile
from pathlib import Path


ROOT = Path(__file__).resolve().parents[2]
SCRIPT = ROOT / "scripts/ci/validate_shaft_pilot_release.py"
SPEC = importlib.util.spec_from_file_location("validate_shaft_pilot_release", SCRIPT)
MODULE = importlib.util.module_from_spec(SPEC)
assert SPEC.loader is not None
SPEC.loader.exec_module(MODULE)


class ShaftPilotReleaseValidatorTest(unittest.TestCase):
    def test_repository_static_contract_is_valid(self):
        self.assertEqual([], MODULE.validate_static(ROOT))

    def test_credential_shaped_values_are_rejected(self):
        errors = MODULE.scan_bytes("fixture", b"token=ghp_12345678901234567890")

        self.assertTrue(errors)

    def test_private_key_detection_marker_is_not_treated_as_key_material(self):
        errors = MODULE.scan_bytes("fixture", b"-----BEGIN PRIVATE KEY-----")

        self.assertEqual([], errors)

    def test_private_key_material_is_rejected(self):
        content = (
            b"-----BEGIN PRIVATE KEY-----\n"
            b"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwx"
        )

        self.assertTrue(MODULE.scan_bytes("fixture", content))

    def test_allure_attachments_are_scanned_for_canaries(self):
        with tempfile.TemporaryDirectory() as temp:
            root = Path(temp)
            for module in MODULE.PILOT_MODULES:
                results = root / module / "allure-results"
                results.mkdir(parents=True)
                (results / f"{module}-result.json").write_text(
                    '{"status":"passed"}',
                    encoding="utf-8",
                )
            (
                root / "shaft-capture/allure-results/browser-attachment.txt"
            ).write_text("capture-browser-secret-canary", encoding="utf-8")

            errors = MODULE.validate_test_results(root)

        self.assertTrue(
            any("capture-browser-secret-canary" in error for error in errors)
        )

    def test_packaged_secret_canaries_are_rejected(self):
        with tempfile.TemporaryDirectory() as temp:
            root = Path(temp)
            version = "10.2.20260612"
            (root / "pom.xml").write_text(
                f"""<project xmlns="http://maven.apache.org/POM/4.0.0">
                <modelVersion>4.0.0</modelVersion>
                <groupId>io.github.shafthq</groupId>
                <artifactId>shaft-parent</artifactId>
                <version>{version}</version>
                </project>""",
                encoding="utf-8",
            )
            for module, artifact in MODULE.PUBLIC_ARTIFACTS.items():
                target = root / module / "target"
                target.mkdir(parents=True)
                with zipfile.ZipFile(target / f"{artifact}-{version}.jar", "w") as jar:
                    content = (
                        b"capture-browser-secret-canary"
                        if module == "shaft-capture"
                        else b"safe"
                    )
                    jar.writestr("fixture.txt", content)
            (root / "target").mkdir()
            (root / "target/bom.json").write_text("{}", encoding="utf-8")

            errors = MODULE.validate_build_outputs(root)

        self.assertTrue(
            any("capture-browser-secret-canary" in error for error in errors)
        )


if __name__ == "__main__":
    unittest.main()
