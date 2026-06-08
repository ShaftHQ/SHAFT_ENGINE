import json
import tempfile
import unittest
from pathlib import Path
from unittest import mock

import scripts.ci.measure_consumer_dependencies as measure


class MeasureConsumerDependenciesTest(unittest.TestCase):
    def test_fixture_root_coordinate_reads_direct_shaft_dependency(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            pom = Path(temp_dir) / "pom.xml"
            pom.write_text(
                """<project xmlns=\"http://maven.apache.org/POM/4.0.0\">
                <dependencies><dependency><groupId>io.github.shafthq</groupId>
                <artifactId>shaft-engine</artifactId></dependency></dependencies></project>""",
                encoding="utf-8",
            )

            self.assertEqual(
                measure.fixture_root_coordinate(pom, "1.2.3"),
                "io.github.shafthq:shaft-engine:1.2.3",
            )

    def test_parse_dependency_list_records_size_and_sha(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            repo = Path(temp_dir) / "repo"
            jar = repo / "org" / "example" / "demo" / "1.0.0" / "demo-1.0.0.jar"
            jar.parent.mkdir(parents=True)
            jar.write_bytes(b"fixture-jar")
            output = f"""
The following files have been resolved:
   org.example:demo:jar:1.0.0:compile:{jar} -- module org.example.demo
"""

            artifacts = measure.parse_dependency_list(output, repo)

            self.assertEqual(
                artifacts,
                [
                    {
                        "coordinate": "org.example:demo:jar:1.0.0",
                        "scope": "compile",
                        "compressedBytes": len(b"fixture-jar"),
                        "sha256": measure.sha256(jar),
                    }
                ],
            )

    def test_expected_jave_coordinate_is_platform_specific(self):
        cases = [
            ("Linux", "x86_64", "ws.schild:jave-nativebin-linux64:jar:3.5.0"),
            ("Linux", "aarch64", "ws.schild:jave-nativebin-linux-arm64:jar:3.5.0"),
            ("Darwin", "arm64", "ws.schild:jave-nativebin-osxm1:jar:3.5.0"),
            ("Darwin", "x86_64", "ws.schild:jave-nativebin-osx64:jar:3.5.0"),
            ("Windows", "AMD64", "ws.schild:jave-nativebin-win64:jar:3.5.0"),
        ]
        for system, machine, coordinate in cases:
            with self.subTest(system=system, machine=machine):
                self.assertEqual(measure.expected_jave_coordinate(system, machine), coordinate)

    def test_validate_required_artifacts_checks_key_baseline_sizes(self):
        manifest = {
            "fixture": "browserstack-sdk",
            "artifacts": [
                {"coordinate": "org.openpnp:opencv:jar:4.9.0-0", "compressedBytes": 109_619_828},
                {"coordinate": "com.browserstack:browserstack-java-sdk:jar:1.59.8", "compressedBytes": 38_204_017},
                {"coordinate": "ws.schild:jave-nativebin-linux64:jar:3.5.0", "compressedBytes": 28_201_169},
            ]
        }
        with mock.patch.object(
            measure,
            "expected_jave_coordinate",
            return_value="ws.schild:jave-nativebin-linux64:jar:3.5.0",
        ):
            self.assertEqual(measure.validate_required_artifacts(manifest), [])

    def test_validate_required_artifacts_rejects_browserstack_from_lean_fixture(self):
        manifest = {
            "fixture": "testng-web",
            "artifacts": [
                {"coordinate": "org.openpnp:opencv:jar:4.9.0-0", "compressedBytes": 109_619_828},
                {"coordinate": "com.browserstack:browserstack-java-sdk:jar:1.59.8", "compressedBytes": 38_204_017},
                {"coordinate": "ws.schild:jave-nativebin-linux64:jar:3.5.0", "compressedBytes": 28_201_169},
            ],
        }
        with mock.patch.object(
            measure,
            "expected_jave_coordinate",
            return_value="ws.schild:jave-nativebin-linux64:jar:3.5.0",
        ):
            self.assertEqual(
                measure.validate_required_artifacts(manifest),
                ["forbidden artifact com.browserstack:browserstack-java-sdk:jar:1.59.8"],
            )

    def test_verify_manifest_ignores_observational_timing_and_repository_growth(self):
        expected = {
            "schemaVersion": 1,
            "fixture": "browserstack-sdk",
            "artifactCount": 3,
            "artifactBytes": 147_823_045,
            "elapsedSeconds": 1.0,
            "repositoryBytes": 10,
            "repositoryGrowthBytes": 5,
            "seededRepositoryBytes": 5,
            "artifacts": [
                {"coordinate": "org.openpnp:opencv:jar:4.9.0-0", "compressedBytes": 109_619_828},
                {"coordinate": "com.browserstack:browserstack-java-sdk:jar:1.59.8", "compressedBytes": 38_204_017},
                {"coordinate": measure.expected_jave_coordinate(), "compressedBytes": 28_201_200},
            ],
        }
        actual = dict(expected)
        actual.update(
            {
                "elapsedSeconds": 2.5,
                "repositoryBytes": 20,
                "repositoryGrowthBytes": 15,
                "seededRepositoryBytes": 5,
            }
        )
        with tempfile.TemporaryDirectory() as temp_dir:
            expected_path = Path(temp_dir) / "expected.json"
            expected_path.write_text(json.dumps(expected), encoding="utf-8")
            self.assertEqual(measure.verify_manifest(actual, expected_path), [])

    def test_seed_project_artifact_writes_engine_and_parent_poms(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            root = Path(temp_dir)
            repository = root / "repository"
            jar = root / "shaft-engine.jar"
            engine_pom = root / "shaft-engine-pom.xml"
            bom_pom = root / "shaft-bom-pom.xml"
            browserstack_root = root / "shaft-browserstack"
            browserstack_pom = browserstack_root / "pom.xml"
            browserstack_jar = browserstack_root / "target" / "shaft-browserstack-1.2.3.jar"
            legacy_pom = root / "legacy-pom.xml"
            jar.write_bytes(b"jar")
            engine_pom.write_text("<project>engine</project>", encoding="utf-8")
            bom_pom.write_text("<project>bom</project>", encoding="utf-8")
            browserstack_pom.parent.mkdir(parents=True)
            browserstack_pom.write_text("<project>browserstack</project>", encoding="utf-8")
            browserstack_jar.parent.mkdir(parents=True)
            browserstack_jar.write_bytes(b"browserstack-jar")
            legacy_pom.write_text("<project>legacy</project>", encoding="utf-8")
            (root / "pom.xml").write_text("<project>parent</project>", encoding="utf-8")

            with (
                mock.patch.object(measure, "ENGINE_POM", engine_pom),
                mock.patch.object(measure, "BOM_POM", bom_pom),
                mock.patch.object(measure, "BROWSERSTACK_ROOT", browserstack_root),
                mock.patch.object(measure, "BROWSERSTACK_POM", browserstack_pom),
                mock.patch.object(measure, "LEGACY_POM", legacy_pom),
                mock.patch.object(measure, "ROOT", root),
            ):
                seeded_bytes = measure.seed_project_artifact(repository, jar, "1.2.3")

            engine = repository / "io/github/shafthq/shaft-engine/1.2.3"
            parent = repository / "io/github/shafthq/shaft-parent/1.2.3"
            self.assertEqual((engine / "shaft-engine-1.2.3.jar").read_bytes(), b"jar")
            self.assertEqual(
                (engine / "shaft-engine-1.2.3.pom").read_text(encoding="utf-8"),
                "<project>engine</project>",
            )
            self.assertEqual(
                (repository / "io/github/shafthq/shaft-bom/1.2.3/shaft-bom-1.2.3.pom").read_text(encoding="utf-8"),
                "<project>bom</project>",
            )
            self.assertEqual(
                (repository / "io/github/shafthq/shaft-browserstack/1.2.3/shaft-browserstack-1.2.3.pom").read_text(encoding="utf-8"),
                "<project>browserstack</project>",
            )
            self.assertEqual(
                (repository / "io/github/shafthq/shaft-browserstack/1.2.3/shaft-browserstack-1.2.3.jar").read_bytes(),
                b"browserstack-jar",
            )
            self.assertEqual(
                (repository / "io/github/shafthq/SHAFT_ENGINE/1.2.3/SHAFT_ENGINE-1.2.3.pom").read_text(encoding="utf-8"),
                "<project>legacy</project>",
            )
            self.assertEqual(
                (parent / "shaft-parent-1.2.3.pom").read_text(encoding="utf-8"),
                "<project>parent</project>",
            )
            self.assertEqual(seeded_bytes, measure.directory_bytes(repository))

    def test_verify_manifest_ignores_rebuilt_project_jar_but_not_dependency_changes(self):
        expected = {
            "schemaVersion": 1,
            "fixture": "sample",
            "rootCoordinate": "io.github.shafthq:SHAFT_ENGINE:1.0.0",
            "platform": {"system": "Linux", "machine": "x86_64"},
            "artifactCount": 2,
            "artifactBytes": 30,
            "artifacts": [
                {
                    "coordinate": "io.github.shafthq:SHAFT_ENGINE:jar:1.0.0",
                    "scope": "compile",
                    "compressedBytes": 10,
                    "sha256": "old-project-jar",
                },
                {
                    "coordinate": "org.example:dependency:jar:1.0.0",
                    "scope": "compile",
                    "compressedBytes": 20,
                    "sha256": "dependency",
                },
            ],
        }
        actual = json.loads(json.dumps(expected))
        actual["artifactBytes"] = 35
        actual["artifacts"][0]["compressedBytes"] = 15
        actual["artifacts"][0]["sha256"] = "rebuilt-project-jar"

        with tempfile.TemporaryDirectory() as temp_dir:
            expected_path = Path(temp_dir) / "expected.json"
            expected_path.write_text(json.dumps(expected), encoding="utf-8")
            with mock.patch.object(measure, "validate_required_artifacts", return_value=[]):
                self.assertEqual(measure.verify_manifest(actual, expected_path), [])
                actual["artifacts"][1]["sha256"] = "changed-dependency"
                self.assertEqual(
                    measure.verify_manifest(actual, expected_path),
                    [f"stable dependency manifest differs from {expected_path}"],
                )


if __name__ == "__main__":
    unittest.main()
