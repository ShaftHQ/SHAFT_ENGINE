import tempfile
import unittest
from pathlib import Path

from scripts.ci.validate_orphaned_suite_files import validate_repository


class ValidateOrphanedSuiteFilesTest(unittest.TestCase):
    def setUp(self):
        self.temporary_directory = tempfile.TemporaryDirectory()
        self.root = Path(self.temporary_directory.name)

    def tearDown(self):
        self.temporary_directory.cleanup()

    def write(self, relative_path, content):
        path = self.root / relative_path
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(content, encoding="utf-8")

    SUITE_XML = """<!DOCTYPE suite SYSTEM "http://testng.org/testng-1.0.dtd" >
<suite name="Suite01" verbose="10">
    <test name="Test01">
        <classes>
            <class name="testPackage.SomeTests"/>
        </classes>
    </test>
</suite>
"""

    def test_unreferenced_suite_xml_is_flagged(self):
        self.write("shaft-engine/src/test/resources/TestSuites/orphan.xml", self.SUITE_XML)
        errors = validate_repository(self.root)
        self.assertEqual(len(errors), 1)
        self.assertEqual(errors[0]["code"], "orphaned-suite-xml")
        self.assertEqual(errors[0]["path"], "shaft-engine/src/test/resources/TestSuites/orphan.xml")
        self.assertIn("orphan.xml", errors[0]["message"])

    def test_suite_xml_referenced_in_pom_passes(self):
        self.write("shaft-engine/src/test/resources/TestSuites/wired.xml", self.SUITE_XML)
        self.write(
            "shaft-engine/pom.xml",
            "<project><suiteXmlFile>src/test/resources/TestSuites/wired.xml</suiteXmlFile></project>",
        )
        self.assertEqual(validate_repository(self.root), [])

    def test_suite_xml_referenced_in_workflow_passes(self):
        self.write("shaft-engine/src/test/resources/TestSuites/wired.xml", self.SUITE_XML)
        self.write(
            ".github/workflows/example.yml",
            "jobs:\n  test:\n    steps:\n      - run: mvn test -Dsuite=wired.xml\n",
        )
        self.assertEqual(validate_repository(self.root), [])

    def test_suite_xml_referenced_in_script_passes(self):
        self.write("shaft-engine/src/test/resources/TestSuites/wired.xml", self.SUITE_XML)
        self.write("scripts/ci/run_suite.py", "SUITE = 'wired.xml'\n")
        self.assertEqual(validate_repository(self.root), [])

    def test_non_suite_xml_is_ignored(self):
        self.write(
            "shaft-engine/src/test/resources/TestSuites/config.xml",
            "<config><setting name=\"a\">1</setting></config>",
        )
        self.assertEqual(validate_repository(self.root), [])

    def test_no_suite_xml_files_returns_no_errors(self):
        self.assertEqual(validate_repository(self.root), [])

    def test_multiple_orphans_each_flagged(self):
        self.write("shaft-engine/src/test/resources/TestSuites/orphan1.xml", self.SUITE_XML)
        self.write("shaft-engine/src/test/resources/TestSuites/orphan2.xml", self.SUITE_XML)
        errors = validate_repository(self.root)
        self.assertEqual({error["path"] for error in errors}, {
            "shaft-engine/src/test/resources/TestSuites/orphan1.xml",
            "shaft-engine/src/test/resources/TestSuites/orphan2.xml",
        })

    def test_current_repository_has_no_orphaned_suite_files(self):
        # #4071: cucumber_testSuite.xml was deleted (redundant -- its sole class,
        # cucumberTestRunner.CucumberTests, already runs via e2eLocalTests.yml:350
        # and e2eTests.yml:672's %regex[.*CucumberTests.*]). testSuite02.xml named
        # two classes: ValidationTests already ran via GLOBAL_TESTING_SCOPE's
        # exclusion-only sweep, but JsonActionsTests was explicitly excluded there
        # (!%regex[.*Json.*], !%regex[.*JSON.*], !%regex[.*json.*]) and unreferenced
        # anywhere else -- genuine dark coverage. Verified green with a scoped local
        # run, then rehomed into pr-gate.yml's shaft-engine unit-tests allowlist
        # (it's pure file-read plus JSONCompare, no driver, no network), after which
        # testSuite02.xml was itself redundant and deleted. Zero known orphans left.
        repository_root = Path(__file__).resolve().parents[2]
        self.assertEqual(validate_repository(repository_root), [])


if __name__ == "__main__":
    unittest.main()
