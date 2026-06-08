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
