import tempfile
import unittest
import zipfile
from pathlib import Path
from unittest import mock

from scripts.ci.assemble_shard_blob import main


class AssembleShardBlobTest(unittest.TestCase):
    def setUp(self):
        self.temp_dir = tempfile.TemporaryDirectory()
        self.root = Path(self.temp_dir.name)
        self.allure_results = self.root / "allure-results"
        self.allure_results.mkdir()
        (self.allure_results / "abc-result.json").write_text('{"status": "passed"}', encoding="utf-8")
        self.traces_dir = self.root / "target" / "shaft-traces"
        (self.traces_dir / "com.example.Test.a").mkdir(parents=True)
        (self.traces_dir / "com.example.Test.a" / "index.json").write_text("{}", encoding="utf-8")
        self.output = self.root / "shard-1"

    def tearDown(self):
        self.temp_dir.cleanup()

    def test_stages_allure_results_and_traces_without_a_doctor_command(self):
        exit_code = main([
            "--shard", "1",
            "--allure-results", str(self.allure_results),
            "--traces-dir", str(self.traces_dir),
            "--output", str(self.output),
        ])

        self.assertEqual(exit_code, 0)
        self.assertTrue((self.output / "allure-results" / "abc-result.json").is_file())
        self.assertTrue((self.output / "shaft-traces" / "com.example.Test.a" / "index.json").is_file())
        self.assertFalse((self.output / "execution-intelligence.json").exists())

    def test_missing_allure_results_directory_fails_with_a_clear_error(self):
        with self.assertRaises(SystemExit):
            main([
                "--shard", "1",
                "--allure-results", str(self.root / "does-not-exist"),
                "--output", str(self.output),
            ])

    def test_missing_traces_directory_still_assembles_a_valid_blob(self):
        exit_code = main([
            "--shard", "1",
            "--allure-results", str(self.allure_results),
            "--traces-dir", str(self.root / "no-traces-here"),
            "--output", str(self.output),
        ])

        self.assertEqual(exit_code, 0)
        self.assertTrue((self.output / "allure-results" / "abc-result.json").is_file())
        self.assertFalse((self.output / "shaft-traces").exists())

    @mock.patch("scripts.ci.assemble_shard_blob.subprocess.run")
    def test_successful_doctor_command_copies_execution_intelligence(self, run):
        run.return_value = mock.Mock(returncode=0, stdout="", stderr="")
        doctor_output = self.root / "target" / "shaft-doctor"
        doctor_output.mkdir(parents=True)
        (doctor_output / "execution-intelligence.json").write_text('{"schemaVersion": "1.0"}', encoding="utf-8")

        exit_code = main([
            "--shard", "1",
            "--allure-results", str(self.allure_results),
            "--traces-dir", str(self.traces_dir),
            "--doctor-command", "shaft-doctor analyze --input allure-results",
            "--doctor-output", str(doctor_output),
            "--output", str(self.output),
        ])

        self.assertEqual(exit_code, 0)
        self.assertTrue((self.output / "execution-intelligence.json").is_file())
        run.assert_called_once()

    @mock.patch("scripts.ci.assemble_shard_blob.subprocess.run")
    def test_failing_doctor_command_still_assembles_a_blob_without_execution_intelligence(self, run):
        run.return_value = mock.Mock(returncode=1, stdout="", stderr="boom")

        exit_code = main([
            "--shard", "1",
            "--allure-results", str(self.allure_results),
            "--traces-dir", str(self.traces_dir),
            "--doctor-command", "shaft-doctor analyze --input allure-results",
            "--output", str(self.output),
        ])

        self.assertEqual(exit_code, 0)
        self.assertFalse((self.output / "execution-intelligence.json").exists())

    def test_no_doctor_command_assembles_a_blob_without_execution_intelligence(self):
        exit_code = main([
            "--shard", "1",
            "--allure-results", str(self.allure_results),
            "--traces-dir", str(self.traces_dir),
            "--output", str(self.output),
        ])

        self.assertEqual(exit_code, 0)
        self.assertFalse((self.output / "execution-intelligence.json").exists())

    def test_zip_flag_produces_a_zip_archive_matching_the_blob_layout(self):
        exit_code = main([
            "--shard", "1",
            "--allure-results", str(self.allure_results),
            "--traces-dir", str(self.traces_dir),
            "--output", str(self.output),
            "--zip",
        ])

        self.assertEqual(exit_code, 0)
        zip_path = self.output.with_suffix(".zip")
        self.assertTrue(zip_path.is_file())
        with zipfile.ZipFile(zip_path) as archive:
            names = set(archive.namelist())
        self.assertIn("allure-results/abc-result.json", names)
        self.assertIn("shaft-traces/com.example.Test.a/index.json", names)


if __name__ == "__main__":
    unittest.main()
