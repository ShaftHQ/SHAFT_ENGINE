import json
import os
import subprocess
import tempfile
import unittest
from pathlib import Path
from unittest import mock

import scripts.ci.reconcile_maven_central_release as reconcile


class MissingModuleDirsTest(unittest.TestCase):
    def test_maps_missing_artifact_paths_to_unique_module_directories_in_order(self):
        missing_paths = [
            "io/github/shafthq/shaft-mcp/1.2.3/shaft-mcp-1.2.3.jar",
            "io/github/shafthq/shaft-mcp/1.2.3/shaft-mcp-1.2.3.jar.asc",
            "io/github/shafthq/shaft-cli/1.2.3/shaft-cli-1.2.3.jar",
        ]

        self.assertEqual(reconcile.missing_module_dirs(missing_paths), ["shaft-mcp", "shaft-cli"])

    def test_maps_root_and_legacy_pom_artifacts_to_their_real_directories(self):
        missing_paths = [
            "io/github/shafthq/shaft-parent/1.2.3/shaft-parent-1.2.3.pom",
            "io/github/shafthq/SHAFT_ENGINE/1.2.3/SHAFT_ENGINE-1.2.3.pom",
        ]

        self.assertEqual(reconcile.missing_module_dirs(missing_paths), [".", "legacy-shaft-engine"])


class BuildDeployCommandTest(unittest.TestCase):
    def test_builds_targeted_deploy_command_mirroring_the_workflow_flags(self):
        with mock.patch.object(reconcile.verify, "maven_executable", return_value="mvn"):
            command = reconcile.build_deploy_command(["shaft-mcp", "shaft-cli"], "KEY123", "s3cr3t")

        self.assertEqual(command, [
            "mvn", "--batch-mode", "deploy", "-pl", "shaft-mcp,shaft-cli",
            "-DskipTests", "-Dgpg.keyname=KEY123", "-Dgpg.passphrase=s3cr3t",
        ])


class ReleaseExistsTest(unittest.TestCase):
    def test_existing_release_reports_true(self):
        with mock.patch.object(
            reconcile.subprocess, "run", return_value=subprocess.CompletedProcess([], 0)
        ) as run:
            self.assertTrue(reconcile.release_exists("1.2.3"))
        run.assert_called_once()
        self.assertEqual(run.call_args.args[0][:3], ["gh", "release", "view"])

    def test_missing_release_reports_false(self):
        with mock.patch.object(
            reconcile.subprocess, "run", return_value=subprocess.CompletedProcess([], 1)
        ):
            self.assertFalse(reconcile.release_exists("1.2.3"))


class SlackPayloadTest(unittest.TestCase):
    def test_payload_matches_the_workflows_inline_python_block(self):
        payload = reconcile.build_slack_payload("1.2.3", "https://example.test/release")

        self.assertEqual(payload["text"], "SHAFT_ENGINE 1.2.3 released: https://example.test/release")
        self.assertIn(
            ":tada: *SHAFT_ENGINE 1.2.3* is now available!",
            payload["blocks"][0]["text"]["text"],
        )
        self.assertEqual(payload["blocks"][2]["elements"][0]["url"], "https://example.test/release")


class RenderReleaseBodyTest(unittest.TestCase):
    def test_substitutes_the_release_version_placeholder(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            template = Path(temp_dir) / "RELEASE_BODY_TEMPLATE.md"
            template.write_text(
                "# SHAFT $RELEASE_VERSION\n\nSee $RELEASE_VERSION docs.\n", encoding="utf-8"
            )

            body = reconcile.render_release_body("1.2.3", template)

        self.assertEqual(body, "# SHAFT 1.2.3\n\nSee 1.2.3 docs.\n")


class ReconcileReleaseTest(unittest.TestCase):
    def setUp(self):
        patcher = mock.patch.dict(os.environ, {}, clear=False)
        patcher.start()
        self.addCleanup(patcher.stop)
        os.environ.pop("SLACK_WEBHOOK_URL", None)

    def test_all_artifacts_missing_deploys_every_configured_module(self):
        all_missing = reconcile.verify.publication_paths("1.2.3")
        with mock.patch.object(
            reconcile.verify, "missing_publication_paths", return_value=all_missing
        ), mock.patch.object(reconcile, "release_exists", return_value=True), mock.patch.object(
            reconcile.subprocess, "run", return_value=subprocess.CompletedProcess([], 0)
        ) as run, mock.patch.object(
            reconcile.verify, "maven_executable", return_value="mvn"
        ):
            exit_code = reconcile.reconcile_release(
                version="1.2.3",
                repository_url=reconcile.verify.DEFAULT_REPOSITORY,
                gpg_keyname="KEY",
                gpg_passphrase="PASS",
                dry_run=False,
            )

        self.assertEqual(exit_code, 0)
        run.assert_called_once()
        command = run.call_args.args[0]
        self.assertEqual(command[:4], ["mvn", "--batch-mode", "deploy", "-pl"])
        modules = command[4].split(",")
        self.assertEqual(set(modules), set(reconcile.MODULE_DIR_BY_ARTIFACT.values()))

    def test_some_artifacts_missing_deploys_only_those_modules(self):
        missing = [p for p in reconcile.verify.publication_paths("1.2.3") if "/shaft-cli/" in p]
        with mock.patch.object(
            reconcile.verify, "missing_publication_paths", return_value=missing
        ), mock.patch.object(reconcile, "release_exists", return_value=True), mock.patch.object(
            reconcile.subprocess, "run", return_value=subprocess.CompletedProcess([], 0)
        ) as run:
            reconcile.reconcile_release(
                version="1.2.3",
                repository_url=reconcile.verify.DEFAULT_REPOSITORY,
                gpg_keyname="KEY",
                gpg_passphrase="PASS",
                dry_run=False,
            )

        command = run.call_args.args[0]
        self.assertEqual(command[command.index("-pl") + 1], "shaft-cli")

    def test_no_missing_artifacts_and_missing_release_creates_release_and_posts_slack(self):
        os.environ["SLACK_WEBHOOK_URL"] = "https://hooks.slack.test/services/x"
        release_result = subprocess.CompletedProcess(
            [], 0, stdout="https://github.test/releases/1.2.3\n"
        )
        with mock.patch.object(
            reconcile.verify, "missing_publication_paths", return_value=[]
        ), mock.patch.object(reconcile, "release_exists", return_value=False), mock.patch.object(
            reconcile.subprocess, "run", return_value=release_result
        ) as run, mock.patch.object(
            reconcile.urllib.request, "urlopen"
        ) as urlopen:
            urlopen.return_value.__enter__.return_value = object()
            exit_code = reconcile.reconcile_release(
                version="1.2.3",
                repository_url=reconcile.verify.DEFAULT_REPOSITORY,
                gpg_keyname="KEY",
                gpg_passphrase="PASS",
                dry_run=False,
            )

        self.assertEqual(exit_code, 0)
        run.assert_called_once()
        self.assertEqual(run.call_args.args[0][:3], ["gh", "release", "create"])
        urlopen.assert_called_once()
        request = urlopen.call_args.args[0]
        self.assertEqual(request.full_url, "https://hooks.slack.test/services/x")
        payload = json.loads(request.data)
        self.assertIn("https://github.test/releases/1.2.3", payload["text"])

    def test_no_missing_artifacts_and_existing_release_is_a_clean_no_op(self):
        with mock.patch.object(
            reconcile.verify, "missing_publication_paths", return_value=[]
        ), mock.patch.object(reconcile, "release_exists", return_value=True), mock.patch.object(
            reconcile.subprocess, "run"
        ) as run, mock.patch.object(
            reconcile.urllib.request, "urlopen"
        ) as urlopen:
            exit_code = reconcile.reconcile_release(
                version="1.2.3",
                repository_url=reconcile.verify.DEFAULT_REPOSITORY,
                gpg_keyname="KEY",
                gpg_passphrase="PASS",
                dry_run=False,
            )

        self.assertEqual(exit_code, 0)
        run.assert_not_called()
        urlopen.assert_not_called()

    def test_repeated_no_op_run_stays_idempotent(self):
        with mock.patch.object(
            reconcile.verify, "missing_publication_paths", return_value=[]
        ), mock.patch.object(reconcile, "release_exists", return_value=True), mock.patch.object(
            reconcile.subprocess, "run"
        ) as run, mock.patch.object(
            reconcile.urllib.request, "urlopen"
        ) as urlopen:
            kwargs = dict(
                version="1.2.3",
                repository_url=reconcile.verify.DEFAULT_REPOSITORY,
                gpg_keyname="KEY",
                gpg_passphrase="PASS",
                dry_run=False,
            )
            first = reconcile.reconcile_release(**kwargs)
            second = reconcile.reconcile_release(**kwargs)

        self.assertEqual((first, second), (0, 0))
        run.assert_not_called()
        urlopen.assert_not_called()

    def test_missing_slack_webhook_url_skips_cleanly_after_release_creation(self):
        os.environ.pop("SLACK_WEBHOOK_URL", None)
        release_result = subprocess.CompletedProcess(
            [], 0, stdout="https://github.test/releases/1.2.3\n"
        )
        with mock.patch.object(
            reconcile.verify, "missing_publication_paths", return_value=[]
        ), mock.patch.object(reconcile, "release_exists", return_value=False), mock.patch.object(
            reconcile.subprocess, "run", return_value=release_result
        ), mock.patch.object(
            reconcile.urllib.request, "urlopen"
        ) as urlopen:
            exit_code = reconcile.reconcile_release(
                version="1.2.3",
                repository_url=reconcile.verify.DEFAULT_REPOSITORY,
                gpg_keyname="KEY",
                gpg_passphrase="PASS",
                dry_run=False,
            )

        self.assertEqual(exit_code, 0)
        urlopen.assert_not_called()

    def test_dry_run_prints_plan_without_deploying_or_creating_anything(self):
        missing = [p for p in reconcile.verify.publication_paths("1.2.3") if "/shaft-cli/" in p]
        with mock.patch.object(
            reconcile.verify, "missing_publication_paths", return_value=missing
        ), mock.patch.object(reconcile.subprocess, "run") as run, mock.patch.object(
            reconcile.urllib.request, "urlopen"
        ) as urlopen:
            exit_code = reconcile.reconcile_release(
                version="1.2.3",
                repository_url=reconcile.verify.DEFAULT_REPOSITORY,
                gpg_keyname="KEY",
                gpg_passphrase="PASS",
                dry_run=True,
            )

        self.assertEqual(exit_code, 0)
        run.assert_not_called()
        urlopen.assert_not_called()

    def test_dry_run_with_nothing_missing_reports_the_planned_release_without_creating_it(self):
        with mock.patch.object(
            reconcile.verify, "missing_publication_paths", return_value=[]
        ), mock.patch.object(reconcile, "release_exists", return_value=False), mock.patch.object(
            reconcile.subprocess, "run"
        ) as run, mock.patch.object(
            reconcile.urllib.request, "urlopen"
        ) as urlopen:
            exit_code = reconcile.reconcile_release(
                version="1.2.3",
                repository_url=reconcile.verify.DEFAULT_REPOSITORY,
                gpg_keyname="KEY",
                gpg_passphrase="PASS",
                dry_run=True,
            )

        self.assertEqual(exit_code, 0)
        run.assert_not_called()
        urlopen.assert_not_called()


class MainTest(unittest.TestCase):
    def test_main_reads_version_from_the_reactor_when_not_given(self):
        version_result = subprocess.CompletedProcess([], 0, stdout="9.9.20260101\n")
        with mock.patch.object(
            reconcile, "reconcile_release", return_value=0
        ) as reconcile_fn, mock.patch.object(
            reconcile.subprocess, "run", return_value=version_result
        ), mock.patch.object(
            reconcile.verify, "maven_executable", return_value="mvn"
        ):
            exit_code = reconcile.main([])

        self.assertEqual(exit_code, 0)
        reconcile_fn.assert_called_once()
        self.assertEqual(reconcile_fn.call_args.kwargs["version"], "9.9.20260101")

    def test_main_uses_the_explicit_version_flag_without_reading_the_reactor(self):
        with mock.patch.object(
            reconcile, "reconcile_release", return_value=0
        ) as reconcile_fn, mock.patch.object(reconcile.subprocess, "run") as run:
            exit_code = reconcile.main(["--version", "1.2.3", "--dry-run"])

        self.assertEqual(exit_code, 0)
        run.assert_not_called()
        self.assertEqual(reconcile_fn.call_args.kwargs["version"], "1.2.3")
        self.assertTrue(reconcile_fn.call_args.kwargs["dry_run"])


if __name__ == "__main__":
    unittest.main()
