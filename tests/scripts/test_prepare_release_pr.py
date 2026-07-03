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

@DefaultValue("3.1.0")
String allure3Version();

@DefaultValue("22.1.0")
String nodeLtsVersion();

@DefaultValue("3.1.0")
String appiumServerVersion();

@DefaultValue("2026.1.0")
String appiumInspectorPluginVersion();

@DefaultValue("7.1.0")
String appiumUiAutomator2DriverVersion();

@DefaultValue("10.1.0")
String appiumXcuitestDriverVersion();

@DefaultValue("11076708")
String androidCommandLineToolsVersion();
}
""",
            )
            _write(root / "shaft-intellij/gradle.properties", "pluginVersion=10.2.20260630\n")
            _write(root / "modular-era-feature-catalog.md", "Plugin 10.2.20260630\n")

            old_version, release_version, changed = MODULE.prepare_release(
                root,
                "2026-07-02",
                internal_versions={
                    "allure3Version": "3.14.0",
                    "nodeLtsVersion": "24.18.0",
                    "appiumServerVersion": "3.5.2",
                    "appiumInspectorPluginVersion": "2026.5.1",
                    "appiumUiAutomator2DriverVersion": "8.0.1",
                    "appiumXcuitestDriverVersion": "11.17.1",
                    "androidCommandLineToolsVersion": "15641748",
                },
            )

            self.assertEqual("10.2.20260630", old_version)
            self.assertEqual("10.3.20260702", release_version)
            self.assertTrue(changed)
            for path in changed:
                self.assertNotIn("10.2.20260630", path.read_text(encoding="utf-8"))
            internal = (
                root / "shaft-engine/src/main/java/com/shaft/properties/internal/Internal.java"
            ).read_text(encoding="utf-8")
            self.assertIn('@DefaultValue("3.14.0")', internal)
            self.assertIn('@DefaultValue("24.18.0")', internal)
            self.assertIn('@DefaultValue("3.5.2")', internal)
            self.assertIn('@DefaultValue("2026.5.1")', internal)
            self.assertIn('@DefaultValue("8.0.1")', internal)
            self.assertIn('@DefaultValue("11.17.1")', internal)
            self.assertIn('@DefaultValue("15641748")', internal)

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

    def test_resolves_latest_internal_versions_without_prereleases(self):
        class Response:
            def __enter__(self):
                return self

            def __exit__(self, *_):
                return False

            def read(self):
                return b"""<sdk:sdk-repository xmlns:sdk="http://schemas.android.com/sdk/android/repo/repository2/01">
<remotePackage path="cmdline-tools;latest">
<archives><archive><complete><url>commandlinetools-linux-15641748_latest.zip</url></complete></archive></archives>
</remotePackage>
</sdk:sdk-repository>"""

        def fake_json_url(url):
            if "nodejs.org" in url:
                return [
                    {"version": "v25.1.0", "lts": False},
                    {"version": "v24.18.0", "lts": "Krypton"},
                    {"version": "v24.19.0-rc.1", "lts": "Krypton"},
                    {"version": "v22.20.0", "lts": "Jod"},
                ]
            if "registry.npmjs.org/allure" in url:
                return {
                    "dist-tags": {"latest": "4.0.0"},
                    "versions": {"3.8.0": {}, "4.0.0": {}, "3.9.0-beta.1": {}},
                }
            return {
                "dist-tags": {"latest": "9.9.0-beta.1"},
                "versions": {"9.8.0": {}, "9.9.0-beta.1": {}, "9.7.1": {}},
            }

        original_json_url = MODULE._json_url
        original_urlopen = MODULE.urlopen
        try:
            MODULE._json_url = fake_json_url
            MODULE.urlopen = lambda *_args, **_kwargs: Response()

            versions = MODULE.latest_internal_versions()

            self.assertEqual("3.8.0", versions["allure3Version"])
            self.assertEqual("9.8.0", versions["appiumServerVersion"])
            self.assertEqual("24.18.0", versions["nodeLtsVersion"])
            self.assertEqual("15641748", versions["androidCommandLineToolsVersion"])
        finally:
            MODULE._json_url = original_json_url
            MODULE.urlopen = original_urlopen


if __name__ == "__main__":
    unittest.main()
