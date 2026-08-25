"""The IntelliJ plugin gate survives one transient connect failure.

Issue #4494: `:verifyPlugin` resolves the JetBrains Plugin Verifier and the IDE
distributions it verifies against at dependency-resolution time, so one
`java.net.ConnectException: Connection timed out` sank PR #4491 -- a diff of 14
Python comments and two memory files that cannot influence a Gradle plugin
build. Re-running it turned it green.

What is asserted here is the wiring, because the flake itself cannot be
summoned on a runner: the verify build runs through the fresh-process retry
helper, the publish build deliberately does not, and every job that calls the
composite action leaves room for a second attempt instead of trading a red
check for a timed-out one.
"""

import importlib.util
import unittest
from pathlib import Path

import yaml

REPO_ROOT = Path(__file__).resolve().parents[2]
ACTION = REPO_ROOT / ".github/actions/intellij-verify/action.yml"
RETRY_SCRIPT = REPO_ROOT / "scripts/ci/build_retry.sh"
WORKFLOWS = REPO_ROOT / ".github/workflows"
PUBLISH_WORKFLOW = WORKFLOWS / "publish-intellij-plugin.yml"
CENTRAL_WORKFLOW = WORKFLOWS / "mavenCentral_cd.yml"
BUILD_FILE = REPO_ROOT / "shaft-intellij/build.gradle.kts"
MARKETPLACE_RECEIPT_MARKERS = (
    "assert_intellij_marketplace_receipt",
    "BUILD SUCCESSFUL",
)
MARKETPLACE_MISS_MARKERS = (
    "intellij-marketplace-publish-miss",
    "issues: write",
)

# One failed attempt (2m38s in the run this issue was filed from) plus a pause
# plus a full successful build (5m11s in run 30940070004) does not fit in ten
# minutes. A retry under a ten-minute ceiling converts a red check into a
# timed-out check and fixes nothing.
MINIMUM_TIMEOUT_MINUTES = 20


def _run_steps() -> list[dict]:
    document = yaml.safe_load(ACTION.read_text(encoding="utf-8"))
    return [step for step in document["runs"]["steps"] if "run" in step]


def _publish_workflow() -> dict:
    return yaml.safe_load(PUBLISH_WORKFLOW.read_text(encoding="utf-8")) or {}


def _central_workflow() -> dict:
    return yaml.safe_load(CENTRAL_WORKFLOW.read_text(encoding="utf-8")) or {}


def _intellij_verify_with(step: dict) -> dict:
    if "intellij-verify" not in str(step.get("uses", "")):
        return {}
    with_block = step.get("with") or {}
    return with_block if isinstance(with_block, dict) else {}


def _truthy(value: object) -> bool:
    return str(value).strip().strip("'\"").lower() == "true"


class IntellijVerifyRetryTest(unittest.TestCase):
    def test_the_retry_helper_the_action_calls_exists(self):
        self.assertTrue(RETRY_SCRIPT.is_file(), f"missing: {RETRY_SCRIPT}")

    def test_the_verify_build_runs_through_the_retry_helper(self):
        verifying = [step for step in _run_steps() if "verifyPlugin" in step["run"]]
        self.assertTrue(verifying, "the action no longer runs verifyPlugin")
        for step in verifying:
            self.assertIn(
                "scripts/ci/build_retry.sh",
                step["run"],
                f"step {step.get('name')!r} runs verifyPlugin without the retry helper",
            )

    def test_publishing_is_never_retried(self):
        # A JetBrains Marketplace version number is single-use. Re-running a
        # publish that failed after the upload landed cannot succeed, and a
        # retry would bury the real reason under a second rejection. So the
        # publish cannot share a step with the verify build: whatever retries
        # the one would retry the other.
        publishing = [step for step in _run_steps() if "publishPlugin" in step["run"]]
        self.assertTrue(publishing, "the action no longer publishes the plugin")
        for step in publishing:
            self.assertNotIn(
                "build_retry.sh",
                step["run"],
                f"step {step.get('name')!r} retries a Marketplace publish",
            )
            self.assertNotIn(
                "verifyPlugin",
                step["run"],
                f"step {step.get('name')!r} publishes from the retried build",
            )

    def test_every_caller_leaves_room_for_a_second_attempt(self):
        callers = []
        for path in sorted(WORKFLOWS.glob("*.yml")):
            document = yaml.safe_load(path.read_text(encoding="utf-8")) or {}
            for name, job in (document.get("jobs") or {}).items():
                if not isinstance(job, dict):
                    continue
                runs_verify = False
                for step in job.get("steps") or []:
                    if not isinstance(step, dict):
                        continue
                    extras = _intellij_verify_with(step)
                    if "intellij-verify" not in str(step.get("uses", "")):
                        continue
                    if "verify" in extras and not _truthy(extras.get("verify")):
                        continue
                    runs_verify = True
                if runs_verify:
                    callers.append((path.name, name, job.get("timeout-minutes")))
        self.assertTrue(callers, "no workflow calls the intellij-verify action")
        for workflow, job, timeout in callers:
            self.assertIsNotNone(timeout, f"{workflow}:{job} declares no timeout")
            self.assertGreaterEqual(
                timeout,
                MINIMUM_TIMEOUT_MINUTES,
                f"{workflow}:{job} cannot fit a retried IntelliJ build",
            )

    def test_the_plugin_verifier_is_pinned(self):
        build = BUILD_FILE.read_text(encoding="utf-8")
        self.assertRegex(
            build,
            r'pluginVerifier\("\d+\.\d+"\)',
            "verifyPlugin must declare a reviewable, explicit Plugin Verifier version",
        )

    def test_gradle_cache_excludes_large_artifact_transforms(self):
        document = yaml.safe_load(ACTION.read_text(encoding="utf-8"))
        setup_gradle = next(
            step for step in document["runs"]["steps"]
            if step.get("uses", "").startswith("gradle/actions/setup-gradle@")
        )
        self.assertIn(
            "caches/transforms-3",
            setup_gradle.get("with", {}).get("gradle-home-cache-excludes", ""),
            "the shared 10 GiB Actions cache must not retain Gradle's multi-gigabyte artifact transforms",
        )


class IntellijMarketplacePublishSplitTest(unittest.TestCase):
    """Issue #5221: a 20-minute verify+publish job can cancel before Marketplace."""

    def test_publish_on_release_does_not_share_the_verify_timeout(self):
        jobs = _publish_workflow().get("jobs") or {}
        verify_jobs = []
        publish_jobs = []
        shared = []
        for name, job in jobs.items():
            if not isinstance(job, dict):
                continue
            publish_true = False
            verify_enabled = False
            for step in job.get("steps") or []:
                if not isinstance(step, dict):
                    continue
                extras = _intellij_verify_with(step)
                if not extras and "intellij-verify" not in str(step.get("uses", "")):
                    continue
                publish_true = publish_true or _truthy(extras.get("publish"))
                # Default verify stays on. An explicit false is the publish-only path.
                verify_enabled = verify_enabled or not (
                    "verify" in extras and not _truthy(extras.get("verify"))
                )
            if publish_true and verify_enabled:
                shared.append((name, job.get("timeout-minutes")))
            elif publish_true:
                publish_jobs.append((name, job.get("timeout-minutes")))
            elif verify_enabled and any(
                "intellij-verify" in str(step.get("uses", ""))
                for step in job.get("steps") or []
                if isinstance(step, dict)
            ):
                verify_jobs.append((name, job.get("timeout-minutes")))

        self.assertEqual(
            [],
            shared,
            "publish-on-release still runs verifyPlugin and publishPlugin in one job; "
            "a 20-minute timeout can cancel before Marketplace",
        )
        self.assertTrue(verify_jobs, "publish-intellij-plugin.yml has no verify-only job")
        self.assertTrue(publish_jobs, "publish-intellij-plugin.yml has no publish-only job")
        for name, timeout in verify_jobs:
            self.assertGreaterEqual(
                timeout,
                MINIMUM_TIMEOUT_MINUTES,
                f"{name} cannot fit a retried IntelliJ verify",
            )
        for name, timeout in publish_jobs:
            self.assertIsNotNone(timeout, f"{name} declares no timeout")
            self.assertGreater(timeout, 0, f"{name} timeout must be a real bound")

    def test_standalone_publish_workflow_is_manual_only(self):
        document = yaml.load(
            PUBLISH_WORKFLOW.read_text(encoding="utf-8"), Loader=yaml.BaseLoader
        )
        self.assertEqual({"workflow_dispatch": ""}, document["on"])

    def test_full_release_owns_publish_only_job_after_public_installer_gate(self):
        jobs = _central_workflow()["jobs"]
        publish = jobs.get("publish_intellij_plugin")
        self.assertIsNotNone(publish, "mavenCentral_cd.yml has no automatic IntelliJ publish job")
        self.assertEqual(
            {"build_release_and_deliver", "verify_public_shaft_mcp_installer"},
            set(publish.get("needs") or []),
        )
        publish_steps = [
            step
            for step in publish.get("steps") or []
            if "intellij-verify" in str(step.get("uses", ""))
        ]
        self.assertEqual(1, len(publish_steps))
        self.assertTrue(_truthy(_intellij_verify_with(publish_steps[0]).get("publish")))
        self.assertFalse(_truthy(_intellij_verify_with(publish_steps[0]).get("verify")))

    def test_announcement_and_intellij_publish_share_prerequisites(self):
        jobs = _central_workflow()["jobs"]
        publish = jobs.get("publish_intellij_plugin") or {}
        self.assertEqual(
            set(jobs["announce_release"].get("needs") or []),
            set(publish.get("needs") or []),
        )
        self.assertNotIn("publish_intellij_plugin", jobs["announce_release"].get("needs") or [])

    def test_release_cancelled_or_timed_out_publish_is_a_failure_not_skip(self):
        text = PUBLISH_WORKFLOW.read_text(encoding="utf-8")
        self.assertNotIn(
            "notify-nightly-failure",
            text,
            "nightly cancelled=skip must not own a missed Marketplace publish",
        )
        self.assertNotRegex(
            text,
            r"contains\(needs\.\*\.result,\s*'cancelled'\)\s*&&\s*'skip'",
            "a cancelled release publish must not be mapped to skip",
        )
        for marker in MARKETPLACE_MISS_MARKERS:
            self.assertIn(
                marker,
                text,
                f"cancelled/timeout before publishPlugin must file a dedicated tracker ({marker})",
            )
        jobs = _publish_workflow()["jobs"]
        notify = jobs.get("notify-missed-publish") or jobs.get(
            "notify-intellij-marketplace-publish"
        )
        self.assertIsNotNone(
            notify,
            "cancelled/timeout before publishPlugin must run notify-missed-publish",
        )
        self.assertEqual(
            "always()",
            notify.get("if"),
            "notify-missed-publish must use if: always(); coverage upload always() is not the pin",
        )

    def test_coverage_upload_always_does_not_satisfy_notify_always_pin(self):
        text = PUBLISH_WORKFLOW.read_text(encoding="utf-8")
        mutated = text.replace(
            "  notify-missed-publish:\n"
            "    name: Fail closed on missed Marketplace publish\n"
            "    needs: [verify, publish]\n"
            "    if: always()\n",
            "  notify-missed-publish:\n"
            "    name: Fail closed on missed Marketplace publish\n"
            "    needs: [verify, publish]\n",
            1,
        )
        self.assertIn("if: always()", mutated)
        jobs = (yaml.safe_load(mutated) or {}).get("jobs") or {}
        notify = jobs.get("notify-missed-publish") or jobs.get(
            "notify-intellij-marketplace-publish"
        )
        self.assertIsNotNone(notify)
        self.assertNotEqual(
            "always()",
            notify.get("if"),
            "deleting notify-missed-publish if: always() must fail even when coverage keeps if: always()",
        )
        coverage_always = any(
            isinstance(step, dict) and step.get("if") == "always()"
            for step in (jobs.get("verify") or {}).get("steps") or []
        )
        self.assertTrue(coverage_always, mutated)
        spec = importlib.util.spec_from_file_location(
            "validate_shaft_pilot_release",
            REPO_ROOT / "scripts/ci/validate_shaft_pilot_release.py",
        )
        validator = importlib.util.module_from_spec(spec)
        assert spec.loader is not None
        spec.loader.exec_module(validator)
        errors = validator.validate_intellij_marketplace_publish(
            mutated, ACTION.read_text(encoding="utf-8")
        )
        self.assertTrue(
            any("always()" in error and "notify" in error for error in errors),
            errors,
        )

    def test_publish_plugin_requires_a_marketplace_or_gradle_receipt(self):
        action = ACTION.read_text(encoding="utf-8")
        workflow = PUBLISH_WORKFLOW.read_text(encoding="utf-8")
        blob = action + "\n" + workflow
        self.assertIn("publishPlugin", blob)
        self.assertTrue(
            any(marker in blob for marker in MARKETPLACE_RECEIPT_MARKERS),
            "successful publishPlugin must assert a Gradle or Marketplace receipt",
        )

    def test_pr_gate_runs_the_marketplace_publish_pins(self):
        gate = (REPO_ROOT / ".github/workflows/pr-gate.yml").read_text(encoding="utf-8")
        for token in (
            "tests.scripts.test_intellij_verify_retry",
            "tests.scripts.test_assert_intellij_marketplace_receipt",
            "tests.scripts.test_validate_shaft_pilot_release",
            "scripts/ci/assert_intellij_marketplace_receipt.py",
            "tests/scripts/test_assert_intellij_marketplace_receipt.py",
        ):
            self.assertIn(token, gate, f"PR Gate must re-run {token} on workflow edits")


if __name__ == "__main__":
    unittest.main()
