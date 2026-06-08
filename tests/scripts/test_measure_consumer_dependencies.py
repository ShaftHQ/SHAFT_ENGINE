import json
from pathlib import Path

import pytest

import scripts.ci.measure_consumer_dependencies as measure


def test_parse_dependency_list_records_size_and_sha(tmp_path):
    repo = tmp_path / "repo"
    jar = repo / "org" / "example" / "demo" / "1.0.0" / "demo-1.0.0.jar"
    jar.parent.mkdir(parents=True)
    jar.write_bytes(b"fixture-jar")
    output = f"""
The following files have been resolved:
   org.example:demo:jar:1.0.0:compile:{jar} -- module org.example.demo
"""

    artifacts = measure.parse_dependency_list(output, repo)

    assert artifacts == [
        {
            "coordinate": "org.example:demo:jar:1.0.0",
            "scope": "compile",
            "compressedBytes": len(b"fixture-jar"),
            "sha256": measure.sha256(jar),
        }
    ]


@pytest.mark.parametrize(
    ("system", "machine", "coordinate"),
    [
        ("Linux", "x86_64", "ws.schild:jave-nativebin-linux64:jar:3.5.0"),
        ("Linux", "aarch64", "ws.schild:jave-nativebin-linux-arm64:jar:3.5.0"),
        ("Darwin", "arm64", "ws.schild:jave-nativebin-osxm1:jar:3.5.0"),
        ("Darwin", "x86_64", "ws.schild:jave-nativebin-osx64:jar:3.5.0"),
        ("Windows", "AMD64", "ws.schild:jave-nativebin-win64:jar:3.5.0"),
    ],
)
def test_expected_jave_coordinate_is_platform_specific(system, machine, coordinate):
    assert measure.expected_jave_coordinate(system, machine) == coordinate


def test_validate_required_artifacts_checks_key_baseline_sizes(monkeypatch):
    monkeypatch.setattr(measure, "expected_jave_coordinate", lambda: "ws.schild:jave-nativebin-linux64:jar:3.5.0")
    manifest = {
        "artifacts": [
            {"coordinate": "org.openpnp:opencv:jar:4.9.0-0", "compressedBytes": 109_619_828},
            {"coordinate": "com.browserstack:browserstack-java-sdk:jar:1.59.8", "compressedBytes": 38_204_017},
            {"coordinate": "ws.schild:jave-nativebin-linux64:jar:3.5.0", "compressedBytes": 28_201_169},
        ]
    }

    assert measure.validate_required_artifacts(manifest) == []


def test_verify_manifest_ignores_observational_timing_and_repository_growth(tmp_path):
    expected = {
        "schemaVersion": 1,
        "fixture": "sample",
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
    expected_path = tmp_path / "expected.json"
    expected_path.write_text(json.dumps(expected), encoding="utf-8")

    assert measure.verify_manifest(actual, expected_path) == []


def test_seed_project_artifact_writes_engine_and_parent_poms(tmp_path, monkeypatch):
    repository = tmp_path / "repository"
    jar = tmp_path / "SHAFT_ENGINE.jar"
    engine_pom = tmp_path / "shaft-engine-pom.xml"
    parent_pom = tmp_path / "parent-pom.xml"
    jar.write_bytes(b"jar")
    engine_pom.write_text("<project>engine</project>", encoding="utf-8")
    parent_pom.write_text("<project>parent</project>", encoding="utf-8")
    monkeypatch.setattr(measure, "ENGINE_POM", engine_pom)
    monkeypatch.setattr(measure, "ROOT", tmp_path)
    (tmp_path / "pom.xml").write_text(parent_pom.read_text(encoding="utf-8"), encoding="utf-8")

    seeded_bytes = measure.seed_project_artifact(repository, jar, "1.2.3")

    engine = repository / "io/github/shafthq/SHAFT_ENGINE/1.2.3"
    parent = repository / "io/github/shafthq/shaft-parent/1.2.3"
    assert (engine / "SHAFT_ENGINE-1.2.3.jar").read_bytes() == b"jar"
    assert (engine / "SHAFT_ENGINE-1.2.3.pom").read_text(encoding="utf-8") == "<project>engine</project>"
    assert (parent / "shaft-parent-1.2.3.pom").read_text(encoding="utf-8") == "<project>parent</project>"
    assert seeded_bytes == measure.directory_bytes(repository)


def test_verify_manifest_ignores_rebuilt_project_jar_but_not_dependency_changes(tmp_path, monkeypatch):
    monkeypatch.setattr(measure, "validate_required_artifacts", lambda manifest: [])
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
    expected_path = tmp_path / "expected.json"
    expected_path.write_text(json.dumps(expected), encoding="utf-8")
    actual = json.loads(json.dumps(expected))
    actual["artifactBytes"] = 35
    actual["artifacts"][0]["compressedBytes"] = 15
    actual["artifacts"][0]["sha256"] = "rebuilt-project-jar"

    assert measure.verify_manifest(actual, expected_path) == []

    actual["artifacts"][1]["sha256"] = "changed-dependency"
    assert measure.verify_manifest(actual, expected_path) == [
        f"stable dependency manifest differs from {expected_path}"
    ]
