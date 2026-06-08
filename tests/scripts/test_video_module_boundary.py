import json
import unittest
import xml.etree.ElementTree as ET
from pathlib import Path


ROOT = Path(__file__).resolve().parents[2]
NAMESPACE = {"m": "http://maven.apache.org/POM/4.0.0"}
FORBIDDEN_ENGINE_DEPENDENCIES = {
    ("ws.schild", "jave-core"),
    ("com.automation-remarks", "video-recorder-testng"),
    ("com.automation-remarks", "video-recorder-junit5"),
}
EXPECTED_PROFILES = {
    "jave-linux-64": ("Linux", None, "amd64", "jave-nativebin-linux64"),
    "jave-linux-arm64": ("Linux", None, "aarch64", "jave-nativebin-linux-arm64"),
    "jave-windows-64": (None, "Windows", "amd64", "jave-nativebin-win64"),
    "jave-macos-amd64": (None, "mac", "x86_64", "jave-nativebin-osx64"),
    "jave-macos-arm64": (None, "mac", "aarch64", "jave-nativebin-osxm1"),
}


def dependencies(pom: Path) -> dict[tuple[str, str], ET.Element]:
    root = ET.parse(pom).getroot()
    return {
        (
            dependency.findtext("m:groupId", namespaces=NAMESPACE),
            dependency.findtext("m:artifactId", namespaces=NAMESPACE),
        ): dependency
        for dependency in root.findall("m:dependencies/m:dependency", NAMESPACE)
    }


class VideoModuleBoundaryTest(unittest.TestCase):
    def test_engine_is_lean_and_retains_appium_recording(self):
        engine_dependencies = dependencies(ROOT / "shaft-engine/pom.xml")
        engine_profiles = ET.parse(ROOT / "shaft-engine/pom.xml").getroot().findall(
            "m:profiles/m:profile", NAMESPACE
        )

        self.assertTrue(FORBIDDEN_ENGINE_DEPENDENCIES.isdisjoint(engine_dependencies))
        self.assertFalse(
            any(
                dependency[0] == "ws.schild" and dependency[1].startswith("jave-nativebin-")
                for dependency in engine_dependencies
            )
        )
        self.assertFalse(
            any((profile.findtext("m:id", namespaces=NAMESPACE) or "").startswith("jave-")
                for profile in engine_profiles)
        )
        self.assertIn(("io.appium", "java-client"), engine_dependencies)
        self.assertTrue(
            (ROOT / "shaft-engine/src/main/java/com/shaft/gui/internal/video/RecordManager.java").is_file()
        )
        self.assertFalse(
            (ROOT / "shaft-engine/src/main/resources/META-INF/services/"
                    "com.shaft.gui.internal.video.DesktopVideoRecordingProvider").exists()
        )

    def test_optional_module_owns_provider_dependencies_and_service_registration(self):
        module_pom = ROOT / "shaft-video/pom.xml"
        module = ET.parse(module_pom).getroot()
        module_dependencies = dependencies(module_pom)
        service = ROOT / (
            "shaft-video/src/main/resources/META-INF/services/"
            "com.shaft.gui.internal.video.DesktopVideoRecordingProvider"
        )

        self.assertEqual(module.findtext("m:packaging", namespaces=NAMESPACE), "jar")
        self.assertIn(("${project.groupId}", "shaft-engine"), module_dependencies)
        self.assertTrue(FORBIDDEN_ENGINE_DEPENDENCIES.issubset(module_dependencies))
        self.assertEqual(
            service.read_text(encoding="utf-8").strip(),
            "com.shaft.gui.internal.video.AutomationRemarksDesktopVideoRecordingProvider",
        )
        self.assertTrue(
            (ROOT / "shaft-video/src/main/java/com/shaft/gui/internal/video/"
                    "AutomationRemarksDesktopVideoRecordingProvider.java").is_file()
        )

    def test_each_supported_platform_profile_resolves_one_native_artifact(self):
        root = ET.parse(ROOT / "shaft-video/pom.xml").getroot()
        profiles = {
            profile.findtext("m:id", namespaces=NAMESPACE): profile
            for profile in root.findall("m:profiles/m:profile", NAMESPACE)
        }

        self.assertEqual(set(profiles), set(EXPECTED_PROFILES))
        for profile_id, (name, family, arch, artifact_id) in EXPECTED_PROFILES.items():
            with self.subTest(profile=profile_id):
                profile = profiles[profile_id]
                self.assertEqual(profile.findtext("m:activation/m:os/m:name", namespaces=NAMESPACE), name)
                self.assertEqual(profile.findtext("m:activation/m:os/m:family", namespaces=NAMESPACE), family)
                self.assertEqual(profile.findtext("m:activation/m:os/m:arch", namespaces=NAMESPACE), arch)
                native_dependencies = profile.findall("m:dependencies/m:dependency", NAMESPACE)
                self.assertEqual(len(native_dependencies), 1)
                self.assertEqual(
                    native_dependencies[0].findtext("m:artifactId", namespaces=NAMESPACE), artifact_id
                )

    def test_bom_and_desktop_fixture_use_optional_module(self):
        bom = ET.parse(ROOT / "shaft-bom/pom.xml").getroot()
        managed_artifacts = {
            dependency.findtext("m:artifactId", namespaces=NAMESPACE)
            for dependency in bom.findall(
                "m:dependencyManagement/m:dependencies/m:dependency", NAMESPACE
            )
        }
        fixture_dependencies = dependencies(
            ROOT / "tools/modularization/consumer-fixtures/desktop-video/pom.xml"
        )

        self.assertIn("shaft-video", managed_artifacts)
        self.assertIn(("io.github.shafthq", "shaft-video"), fixture_dependencies)

    def test_linux_x64_native_payload_meets_issue_threshold(self):
        manifests = ROOT / "docs/modularization/dependency-baseline/manifests"
        summary = json.loads((manifests / "desktop-video.json").read_text(encoding="utf-8"))
        baseline = json.loads(
            (manifests / summary["artifactsRef"]).read_text(encoding="utf-8")
        )
        native = next(
            artifact
            for artifact in baseline["artifacts"]
            if artifact["coordinate"] == "ws.schild:jave-nativebin-linux64:jar:3.5.0"
        )

        self.assertGreaterEqual(native["compressedBytes"], 28_201_169)


if __name__ == "__main__":
    unittest.main()
