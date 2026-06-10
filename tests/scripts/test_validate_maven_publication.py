import importlib.util
import tempfile
import unittest
import zipfile
import xml.etree.ElementTree as ET
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

    def test_rejects_signing_before_aggregate_sbom_attachment(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            root = Path(temp_dir)
            self._copy_contract(root)
            pom = root / "pom.xml"
            document = ET.parse(pom)
            plugins = document.getroot().find("m:build/m:plugins", MODULE.NS)
            self.assertIsNotNone(plugins)
            plugin_list = list(plugins)
            sbom = next(plugin for plugin in plugin_list
                        if MODULE._text(plugin, "m:artifactId") == "cyclonedx-maven-plugin")
            gpg = next(plugin for plugin in plugin_list
                       if MODULE._text(plugin, "m:artifactId") == "maven-gpg-plugin")
            plugins.remove(sbom)
            plugins.insert(list(plugins).index(gpg) + 1, sbom)
            document.write(pom, encoding="utf-8", xml_declaration=True)
            self.assertTrue(any("attached before Maven GPG" in error
                                for error in MODULE.validate_publication(root)))

    def test_rejects_non_inherited_central_publishing(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            root = Path(temp_dir)
            self._copy_contract(root)
            pom = root / "pom.xml"
            document = ET.parse(pom)
            central = document.getroot().find(
                "m:build/m:plugins/m:plugin[m:artifactId='central-publishing-maven-plugin']",
                MODULE.NS,
            )
            self.assertIsNotNone(central)
            ET.SubElement(central, f"{{{MODULE.NS['m']}}}inherited").text = "false"
            document.write(pom, encoding="utf-8", xml_declaration=True)
            self.assertTrue(any("inherited" in error for error in MODULE.validate_publication(root)))

    def test_signed_outputs_require_aggregate_sbom_signature(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            root = Path(temp_dir)
            self._copy_contract(root)
            self._create_outputs(root, include_signatures=True)
            (root / "target" / "bom.json.asc").unlink()
            self.assertIn("missing signature: target/bom.json.asc",
                          MODULE.validate_publication(root, True, True))

    def test_bundle_contains_central_layout(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            root = Path(temp_dir)
            self._copy_contract(root)
            version = MODULE._version(root)
            self._create_outputs(root, include_signatures=True)
            bundle = root / "bundle.zip"
            MODULE.create_bundle(bundle, root, require_signatures=True)
            with zipfile.ZipFile(bundle) as archive:
                names = set(archive.namelist())
            self.assertIn(f"io/github/shafthq/shaft-engine/{version}/shaft-engine-{version}.jar", names)
            self.assertIn(f"io/github/shafthq/SHAFT_ENGINE/{version}/SHAFT_ENGINE-{version}.pom", names)
            sbom = f"io/github/shafthq/shaft-parent/{version}/shaft-parent-{version}-cyclonedx.json"
            self.assertIn(sbom, names)
            self.assertIn(f"{sbom}.asc", names)

    @staticmethod
    def _create_outputs(root, include_signatures=False):
        version = MODULE._version(root)
        for artifact, (pom_path, packaging) in MODULE.PUBLIC_ARTIFACTS.items():
            target = (root / pom_path).parent / "target"
            target.mkdir(exist_ok=True)
            if include_signatures:
                (target / f"{artifact}-{version}.pom.asc").write_bytes(b"signature")
            if packaging == "jar":
                for suffix in ("", "-sources", "-javadoc"):
                    output = target / f"{artifact}-{version}{suffix}.jar"
                    output.write_bytes(b"jar")
                    if include_signatures:
                        output.with_name(output.name + ".asc").write_bytes(b"signature")
        (root / "target").mkdir(exist_ok=True)
        (root / "target" / "bom.json").write_text("{}", encoding="utf-8")
        if include_signatures:
            (root / "target" / "bom.json.asc").write_bytes(b"signature")

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
