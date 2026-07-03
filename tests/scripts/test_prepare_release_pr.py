import importlib.util
import tempfile
import unittest
from pathlib import Path


ROOT = Path(__file__).resolve().parents[2]
SCRIPT = ROOT / "scripts/ci/prepare_release_pr.py"
SPEC = importlib.util.spec_from_file_location("prepare_release_pr", SCRIPT)
MODULE = importlib.util.module_from_spec(SPEC)
assert SPEC.loader is not None
SPEC.loader.exec_module(MODULE)


def _write(path: Path, text: str) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(text, encoding="utf-8")


class PrepareReleasePrTest(unittest.TestCase):
    def test_updates_release_files_from_release_date_quarter(self):
        with tempfile.TemporaryDirectory() as temp:
            root = Path(temp)
            _write(
                root / "pom.xml",
                """<project xmlns="http://maven.apache.org/POM/4.0.0">
<modelVersion>4.0.0</modelVersion>
<groupId>io.github.shafthq</groupId>
<artifactId>shaft-parent</artifactId>
<version>10.2.20260630</version>
<modules><module>shaft-engine</module></modules>
</project>
""",
            )
            _write(
                root / "shaft-engine/pom.xml",
                """<project xmlns="http://maven.apache.org/POM/4.0.0">
<parent><groupId>io.github.shafthq</groupId><artifactId>shaft-parent</artifactId><version>10.2.20260630</version></parent>
<artifactId>shaft-engine</artifactId>
</project>
""",
            )
            _write(
                root / "tools/modularization/consumer-fixtures/api/pom.xml",
                """<project xmlns="http://maven.apache.org/POM/4.0.0">
<properties><shaft.version>10.2.20260630</shaft.version></properties>
</project>
""",
            )
            _write(
                root / "shaft-engine/src/main/java/com/shaft/properties/internal/Internal.java",
                """interface Internal {
@DefaultValue("10.2.20260630")
String shaftEngineVersion();
}
""",
            )
            _write(root / "shaft-intellij/gradle.properties", "pluginVersion=10.2.20260630\n")
            _write(root / "modular-era-feature-catalog.md", "Plugin 10.2.20260630\n")

            old_version, release_version, changed = MODULE.prepare_release(root, "2026-07-02")

            self.assertEqual("10.2.20260630", old_version)
            self.assertEqual("10.3.20260702", release_version)
            self.assertTrue(changed)
            for path in changed:
                self.assertNotIn("10.2.20260630", path.read_text(encoding="utf-8"))

    def test_rejects_non_newer_release_date(self):
        with tempfile.TemporaryDirectory() as temp:
            root = Path(temp)
            _write(
                root / "pom.xml",
                """<project xmlns="http://maven.apache.org/POM/4.0.0">
<modelVersion>4.0.0</modelVersion>
<version>10.3.20260702</version>
</project>
""",
            )

            with self.assertRaises(ValueError):
                MODULE.prepare_release(root, "2026-07-02")


if __name__ == "__main__":
    unittest.main()
