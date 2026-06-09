import importlib.util
import tempfile
import unittest
import zipfile
from pathlib import Path

MODULE_PATH = Path(__file__).resolve().parents[2] / "scripts" / "ci" / "validate_maven_publication.py"
SPEC = importlib.util.spec_from_file_location("validate_maven_publication", MODULE_PATH)
MODULE = importlib.util.module_from_spec(SPEC)
assert SPEC.loader
SPEC.loader.exec_module(MODULE)


class MavenPublicationValidationTest(unittest.TestCase):
    def test_repository_publication_contract_is_valid(self):
        self.assertEqual(MODULE.validate_publication(), [])

    def test_bom_rejects_unpublished_artifact(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            root = Path(temp_dir)
            self._copy_contract(root)
            bom = root / "shaft-bom" / "pom.xml"
            bom.write_text(bom.read_text(encoding="utf-8").replace(
                "</dependencies>",
                "<dependency><groupId>io.github.shafthq</groupId>"
                "<artifactId>missing-module</artifactId><version>1</version></dependency></dependencies>",
                1,
            ), encoding="utf-8")
            self.assertTrue(any("unpublished" in error for error in MODULE.validate_publication(root)))

    def test_bundle_contains_central_layout(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            root = Path(temp_dir)
            self._copy_contract(root)
            version = MODULE._version(root)
            for artifact, (pom_path, packaging) in MODULE.PUBLIC_ARTIFACTS.items():
                target = (root / pom_path).parent / "target"
                target.mkdir(exist_ok=True)
                if packaging == "jar":
                    for suffix in ("", "-sources", "-javadoc"):
                        (target / f"{artifact}-{version}{suffix}.jar").write_bytes(b"jar")
            (root / "target").mkdir(exist_ok=True)
            (root / "target" / "bom.json").write_text("{}", encoding="utf-8")
            bundle = root / "bundle.zip"
            MODULE.create_bundle(bundle, root)
            with zipfile.ZipFile(bundle) as archive:
                names = set(archive.namelist())
            self.assertIn(f"io/github/shafthq/shaft-engine/{version}/shaft-engine-{version}.jar", names)
            self.assertIn(f"io/github/shafthq/SHAFT_ENGINE/{version}/SHAFT_ENGINE-{version}.pom", names)
            self.assertIn("sbom/bom.json", names)

    @staticmethod
    def _copy_contract(root):
        import shutil
        source = MODULE.ROOT
        for relative in [Path("pom.xml"), Path(".github/workflows/mavenCentral_cd.yml"),
                         Path(".github/workflows/publishJavaDocs.yml"),
                         Path("report-aggregate/pom.xml"), Path("shaft-bom/pom.xml")]:
            destination = root / relative
            destination.parent.mkdir(parents=True, exist_ok=True)
            shutil.copy2(source / relative, destination)
        for _, (pom_path, _) in MODULE.PUBLIC_ARTIFACTS.items():
            destination = root / pom_path
            destination.parent.mkdir(parents=True, exist_ok=True)
            shutil.copy2(source / pom_path, destination)


if __name__ == "__main__":
    unittest.main()
