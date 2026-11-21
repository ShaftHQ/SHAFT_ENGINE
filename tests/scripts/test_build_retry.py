"""Behaviour of scripts/ci/build_retry.sh."""
# The loop lives in a standalone script rather than inline in each workflow step
# so it can be exercised here against a fake command, on both runners the agent
# guidance gate uses. A cold-cache Maven Central 429 cannot be reproduced on
# demand (issue #4445), so the contract proven here is the control flow: what
# retries, what does not, and how many attempts each costs.
#
# Issue #4494 added the second caller: the Gradle IntelliJ plugin gate, whose
# remote fails with a connect error rather than a rate limit.

import os
import shutil
import subprocess  # nosec B404 - runs the checked-in retry script under bash.
import tempfile
import unittest
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[2]
RETRY_SCRIPT = REPO_ROOT / "scripts" / "ci" / "build_retry.sh"

# The real thing, trimmed from run 30876948089's failing job.
CENTRAL_429 = (
    "[ERROR] Non-resolvable import POM: The following artifacts could not be "
    "resolved: org.junit:junit-bom:pom:6.1.2 (absent): Could not transfer "
    "artifact org.junit:junit-bom:pom:6.1.2 from/to central "
    "(https://repo.maven.apache.org/maven2): status code: 429, reason phrase: "
    "Too Many Requests (429)"
)
# Central 404 burst for artifacts that exist (run 38076665231, PR #6809).
CENTRAL_404 = (
    "[ERROR] Failed to execute goal org.apache.maven.plugins:maven-surefire-plugin:3.5.6:test "
    "(default-test) on project shaft-infrastructure: The following artifacts could not be resolved: "
    "org.apache.maven.surefire:surefire-junit-platform:jar:3.5.6 (absent): Could not find artifact "
    "org.apache.maven.surefire:surefire-junit-platform:jar:3.5.6 in central (https://repo.maven.apache.org/maven2)"
)
NON_CENTRAL_MISS = (
    "[ERROR] Could not find artifact io.github.shafthq:shaft-visual:jar:1.0 in shaft-local-repo (file:///tmp/repo)"
)
# Run 44699679485 attempt 2: Maven kept the Central miss in the local repo and
# did not ask again. build_retry treated that wording as a hard failure.
CACHED_CENTRAL_MISS = (
    "[ERROR] org.apache.maven.plugins:maven-jar-plugin:jar:3.5.1 was not found in "
    "https://repo.maven.apache.org/maven2 during a previous attempt. This failure was "
    "cached in the local repository and resolution is not reattempted until the update "
    "interval of central has elapsed or updates are forced"
)
GENUINE_FAILURE = (
    "[ERROR] Failed to execute goal on project shaft-visual: Compilation failure"
)
# Ubuntu_Firefox_Grid, run 38362240871: Central did not return the plugin POM.
# The same install succeeded on Chrome grid. A fresh process is the lever.
CENTRAL_DESCRIPTOR = (
    "[ERROR] Failed to execute goal org.apache.maven.plugins:maven-dependency-plugin:3.11.0:build-classpath "
    "(default-cli) on project allure-cucumber8-javadoc-classpath: Execution default-cli of goal "
    "org.apache.maven.plugins:maven-dependency-plugin:3.11.0:build-classpath failed: "
    "Plugin org.apache.maven.plugins:maven-dependency-plugin:3.11.0 or one of its dependencies could not be resolved: "
    "Failed to read artifact descriptor for org.apache.maven.resolver:maven-resolver-util:jar:1.9.25"
)
# The real thing, from the PR #4491 run this script's second caller was added
# for (issue #4494). Linux reports a connect that never opened as ConnectException
# 'Connection timed out'; a JDK socket connect timeout reports the same event as
# SocketTimeoutException 'Connect timed out'. Both are the same refusal.
GRADLE_CONNECT_REFUSED = (
    "Could not determine the dependencies of task ':verifyPlugin'. > "
    "java.net.ConnectException: Connection timed out"
)
GRADLE_CONNECT_TIMEOUT = (
    "Could not resolve org.jetbrains.intellij.plugins:verifier-cli. > "
    "java.net.SocketTimeoutException: Connect timed out"
)
GRADLE_GENUINE_FAILURE = (
    "Execution failed for task ':verifyPlugin'. > "
    "DEPRECATED_API_USAGES: 1 problem found"
)
# Runs 42467112280 and 42472115062: Gradle could not download the Kotlin
# compiler embeddable jar. The verify job called this a non-transfer error.
GRADLE_GATEWAY_TIMEOUT = (
    "Could not GET https://github.com/JetBrains/kotlin/releases/download/v2.1.20/kotlin-compiler-embeddable-2.1.20.jar. "
    "Received status code 504 from server: Gateway Time-out"
)
# Run 42580722341: flutter build apk hit this wrapper sentence, which does not
# contain "status code 504".
GRADLE_WRAPPER_504 = (
    'Exception in thread "main" java.io.IOException: Server returned HTTP response code: 504 '
    "for URL: https://github.com/gradle/gradle-distributions/releases/download/v9.3.1/gradle-9.3.1-all.zip"
)


def _bash() -> str | None:
    return shutil.which("bash")


class BuildRetryScriptTest(unittest.TestCase):
    def setUp(self):
        if _bash() is None:
            self.skipTest("bash is unavailable on this host")
        if not RETRY_SCRIPT.is_file():
            self.fail(f"retry script is missing: {RETRY_SCRIPT}")
        # ignore_cleanup_errors: on Windows the bash process can still hold the
        # directory briefly after it exits, and a cleanup race must not be
        # reported as a failure of the behaviour under test.
        self._directory = tempfile.TemporaryDirectory(ignore_cleanup_errors=True)
        self.workspace = Path(self._directory.name)
        self.addCleanup(self._directory.cleanup)
        # Everything the run touches is copied into one directory and invoked
        # relative to it. `bash` on a Windows host may be Git Bash (which reads
        # `C:/...`) or WSL (which reads `/mnt/c/...`), and only a relative path
        # from a translated working directory means the same thing to both.
        self.script = self.workspace / "retry.sh"
        self.script.write_text(
            RETRY_SCRIPT.read_text(encoding="utf-8"), encoding="utf-8", newline="\n"
        )

    def _fake_command(self, *, output: str, fail_times: int) -> str:
        # Records one line per invocation, then fails `fail_times` times before
        # succeeding, so the test can count attempts rather than infer them.
        # The failure text is a file: the Gradle wrapper line contains quotes.
        (self.workspace / "failure.txt").write_text(
            output + "\n", encoding="utf-8", newline="\n"
        )
        script = self.workspace / "fake-mvn.sh"
        script.write_text(
            "#!/usr/bin/env bash\n"
            'echo "attempt" >> attempts.log\n'
            "count=$(wc -l < attempts.log)\n"
            f'if [ "$count" -le {fail_times} ]; then\n'
            "  cat failure.txt\n"
            "  exit 1\n"
            "fi\n"
            'echo "BUILD SUCCESS"\n',
            encoding="utf-8",
            newline="\n",
        )
        os.chmod(script, 0o755)  # nosec B103 - test fixture the test itself runs.
        return "./fake-mvn.sh"

    def _run(self, command: str, *, attempts: int = 3, pause: int = 0, path_prefix: str | None = None):
        env = os.environ | {"HOME": "."}
        if path_prefix is not None:
            env["PATH"] = path_prefix + os.pathsep + env.get("PATH", "")
        completed = subprocess.run(  # nosec B603 - fixed bash invocation in a temp dir.
            [_bash(), "./retry.sh", str(attempts), str(pause), "bash", command],
            cwd=self.workspace,
            capture_output=True,
            text=True,
            # Relative HOME keeps $HOME/.m2 inside this workspace for Git Bash and
            # WSL. An absolute temp path is not the same string in both.
            env=env,
        )
        log = self.workspace / "attempts.log"
        made = len(log.read_text(encoding="utf-8").splitlines()) if log.is_file() else 0
        return completed, made

    def test_a_command_that_succeeds_runs_once(self):
        command = self._fake_command(output="", fail_times=0)
        completed, attempts = self._run(command)
        self.assertEqual(0, completed.returncode, completed.stdout + completed.stderr)
        self.assertEqual(1, attempts)

    def test_a_central_429_is_retried_in_a_fresh_process_until_it_succeeds(self):
        command = self._fake_command(output=CENTRAL_429, fail_times=2)
        completed, attempts = self._run(command, attempts=3)
        self.assertEqual(0, completed.returncode, completed.stdout + completed.stderr)
        self.assertEqual(3, attempts)

    def test_a_central_404_for_a_published_artifact_is_retried_in_a_fresh_process(self):  # #6809
        command = self._fake_command(output=CENTRAL_404, fail_times=1)
        completed, attempts = self._run(command, attempts=2)
        self.assertEqual(0, completed.returncode, completed.stdout + completed.stderr)
        self.assertEqual(2, attempts)

    def test_a_cached_central_miss_is_retried_in_a_fresh_process(self):
        command = self._fake_command(output=CACHED_CENTRAL_MISS, fail_times=1)
        completed, attempts = self._run(command, attempts=2)
        self.assertEqual(0, completed.returncode, completed.stdout + completed.stderr)
        self.assertEqual(2, attempts)

    def test_a_retry_drops_maven_negative_cache_before_the_next_attempt(self):
        # Attempt 1 matches the Central miss. Maven then records the miss, and
        # attempt 2 must not still see those markers or it will not contact Central.
        (self.workspace / "failure.txt").write_text(
            "Could not find artifact org.apache.maven.plugins:maven-jar-plugin:jar:3.5.1 "
            "in central (https://repo.maven.apache.org/maven2)\n",
            encoding="utf-8",
            newline="\n",
        )
        script = self.workspace / "fake-mvn.sh"
        script.write_text(
            "#!/usr/bin/env bash\n"
            'echo "attempt" >> attempts.log\n'
            "count=$(wc -l < attempts.log)\n"
            'dir="$HOME/.m2/repository/org/apache/maven/plugins/maven-jar-plugin/3.5.1"\n'
            'if [ "$count" -eq 1 ]; then\n'
            '  mkdir -p "$dir"\n'
            '  echo poisoned > "$dir/maven-jar-plugin-3.5.1.jar.lastUpdated"\n'
            '  echo poisoned > "$dir/resolver-status.properties"\n'
            "  cat failure.txt\n"
            "  exit 1\n"
            "fi\n"
            'if [ -e "$dir/maven-jar-plugin-3.5.1.jar.lastUpdated" ] || '
            '[ -e "$dir/resolver-status.properties" ]; then\n'
            "  cat <<'EOF'\n"
            "was not found in https://repo.maven.apache.org/maven2 during a previous attempt. "
            "This failure was cached in the local repository\n"
            "EOF\n"
            "  exit 1\n"
            "fi\n"
            'echo "BUILD SUCCESS"\n',
            encoding="utf-8",
            newline="\n",
        )
        os.chmod(script, 0o755)  # nosec B103 - test fixture the test itself runs.
        completed, attempts = self._run("./fake-mvn.sh", attempts=2)
        repo = self.workspace / ".m2"
        self.assertEqual(0, completed.returncode, completed.stdout + completed.stderr)
        self.assertEqual(2, attempts)
        self.assertEqual([], list(repo.rglob("*.lastUpdated")))
        self.assertEqual([], list(repo.rglob("resolver-status.properties")))

    def _metadata_curl(self, http_code: str) -> str:
        # Stands in for Central. The script builds the POM URL and asks curl;
        # this fixture returns the injected status and does not contact Central.
        script = self.workspace / "curl"
        script.write_text(
            "#!/usr/bin/env bash\n" f"printf '%s' '{http_code}'\n",
            encoding="utf-8",
            newline="\n",
        )
        os.chmod(script, 0o755)  # nosec B103 - test fixture the test itself runs.
        return str(self.workspace)

    def test_package_setup_recovers_a_published_artifact_after_a_poisoned_pom(self):
        # Run 44833666425: build_retry.sh 2 60 mvn ... package. Maven said
        # maven-enforcer-plugin 3.7.0 was missing while its POM is HTTP 200,
        # because a non-POM file already occupied the release path.
        (self.workspace / "failure.txt").write_text(
            "Could not find artifact org.apache.maven.plugins:maven-enforcer-plugin:jar:3.7.0 "
            "in central (https://repo.maven.apache.org/maven2)\n"
            "The POM for org.apache.maven.plugins:maven-enforcer-plugin:jar:3.7.0 is missing, "
            "no dependency information available\n",
            encoding="utf-8",
            newline="\n",
        )
        script = self.workspace / "fake-mvn.sh"
        script.write_text(
            "#!/usr/bin/env bash\n"
            'echo "attempt" >> attempts.log\n'
            "count=$(wc -l < attempts.log)\n"
            'dir="$HOME/.m2/repository/org/apache/maven/plugins/maven-enforcer-plugin/3.7.0"\n'
            'if [ "$count" -eq 1 ]; then\n'
            '  mkdir -p "$dir"\n'
            "  printf '<html>not a pom</html>' > \"$dir/maven-enforcer-plugin-3.7.0.pom\"\n"
            "  cat failure.txt\n"
            "  exit 1\n"
            "fi\n"
            'if [ -e "$dir/maven-enforcer-plugin-3.7.0.pom" ]; then\n'
            "  echo 'poisoned pom still present'\n"
            "  exit 1\n"
            "fi\n"
            'echo "BUILD SUCCESS"\n',
            encoding="utf-8",
            newline="\n",
        )
        os.chmod(script, 0o755)  # nosec B103 - test fixture the test itself runs.
        completed, attempts = self._run(
            "./fake-mvn.sh", attempts=2, pause=0, path_prefix=self._metadata_curl("200")
        )
        self.assertEqual(0, completed.returncode, completed.stdout + completed.stderr)
        self.assertEqual(2, attempts)
        self.assertIn("BUILD SUCCESS", completed.stdout)

    def test_package_setup_still_fails_when_central_metadata_is_absent(self):
        (self.workspace / "failure.txt").write_text(
            "Could not find artifact org.apache.maven.surefire:surefire-testng:jar:9.9.9 "
            "in central (https://repo.maven.apache.org/maven2)\n",
            encoding="utf-8",
            newline="\n",
        )
        script = self.workspace / "fake-mvn.sh"
        script.write_text(
            "#!/usr/bin/env bash\n"
            'echo "attempt" >> attempts.log\n'
            "cat failure.txt\n"
            "exit 1\n",
            encoding="utf-8",
            newline="\n",
        )
        os.chmod(script, 0o755)  # nosec B103 - test fixture the test itself runs.
        completed, attempts = self._run(
            "./fake-mvn.sh", attempts=2, pause=0, path_prefix=self._metadata_curl("404")
        )
        self.assertNotEqual(0, completed.returncode)
        self.assertEqual(1, attempts)
        self.assertNotIn("BUILD SUCCESS", completed.stdout)
        self.assertIn("release is absent", completed.stderr)

    def test_a_genuine_failure_leaves_maven_resolution_markers(self):
        marker = self.workspace / ".m2" / "repository" / "keep.lastUpdated"
        marker.parent.mkdir(parents=True)
        marker.write_text("keep\n", encoding="utf-8")
        command = self._fake_command(output=GENUINE_FAILURE, fail_times=99)
        completed, attempts = self._run(command, attempts=3)
        self.assertNotEqual(0, completed.returncode)
        self.assertEqual(1, attempts)
        self.assertEqual("keep\n", marker.read_text(encoding="utf-8"))

    def test_a_miss_against_a_repository_other_than_central_is_not_retried(self):  # #6809
        command = self._fake_command(output=NON_CENTRAL_MISS, fail_times=99)
        completed, attempts = self._run(command, attempts=3)
        self.assertNotEqual(0, completed.returncode)
        self.assertEqual(1, attempts)

    def test_a_gradle_connect_refusal_is_retried_until_it_succeeds(self):
        command = self._fake_command(output=GRADLE_CONNECT_REFUSED, fail_times=1)
        completed, attempts = self._run(command, attempts=2)
        self.assertEqual(0, completed.returncode, completed.stdout + completed.stderr)
        self.assertEqual(2, attempts)

    def test_a_gradle_connect_timeout_is_retried_until_it_succeeds(self):
        command = self._fake_command(output=GRADLE_CONNECT_TIMEOUT, fail_times=1)
        completed, attempts = self._run(command, attempts=2)
        self.assertEqual(0, completed.returncode, completed.stdout + completed.stderr)
        self.assertEqual(2, attempts)

    def test_a_gradle_gateway_timeout_is_retried_until_it_succeeds(self):
        command = self._fake_command(output=GRADLE_GATEWAY_TIMEOUT, fail_times=1)
        completed, attempts = self._run(command, attempts=2)
        self.assertEqual(0, completed.returncode, completed.stdout + completed.stderr)
        self.assertEqual(2, attempts)

    def test_a_gradle_wrapper_http_504_is_retried_until_it_succeeds(self):
        command = self._fake_command(output=GRADLE_WRAPPER_504, fail_times=1)
        completed, attempts = self._run(command, attempts=2)
        self.assertEqual(0, completed.returncode, completed.stdout + completed.stderr)
        self.assertEqual(2, attempts)

    def test_e2e_flutter_demo_app_build_wraps_flutter_with_build_retry(self):
        workflow = (REPO_ROOT / ".github/workflows/e2eTests.yml").read_text(encoding="utf-8")
        start = workflow.index("  Flutter_Demo_App_Build:\n")
        end = workflow.index("\n  Android_Native_BrowserStack:\n", start)
        job = workflow[start:end]
        wrapped = (
            'bash "$GITHUB_WORKSPACE/scripts/ci/build_retry.sh" '
            "2 60 flutter build apk --debug"
        )
        self.assertIn(wrapped, job)
        bare = [
            line.strip()
            for line in job.splitlines()
            if line.strip() == "flutter build apk --debug"
        ]
        self.assertEqual([], bare)

    def test_ubuntu_junit_e2e_prefetches_kind_before_kind_action(self):
        workflow = (REPO_ROOT / ".github/workflows/e2eTests.yml").read_text(encoding="utf-8")
        start = workflow.index("  Ubuntu_JUnit_E2E:\n")
        kind_action = workflow.index("uses: helm/kind-action@", start)
        window = workflow[start:kind_action]
        command = (
            "bash scripts/ci/build_retry.sh 4 20 "
            "python3 scripts/ci/prefetch_kind_toolcache.py"
        )
        self.assertIn("Prefetch kind into the runner tool cache", window)
        self.assertIn(command, window)

    def test_live_tools_gradle_steps_call_build_retry(self):
        # Run 42472115062 invoked gradlew directly, so a 504 was not retried.
        workflow = (REPO_ROOT / ".github/workflows/live-tools-nightly.yml").read_text(encoding="utf-8")
        gradlew_lines = [line.strip() for line in workflow.splitlines() if "shaft-intellij/gradlew" in line]
        self.assertGreaterEqual(len(gradlew_lines), 1)
        for line in gradlew_lines:
            self.assertIn("scripts/ci/build_retry.sh", line)

    def test_a_genuine_verifier_finding_is_not_retried(self):
        # A deprecated-API usage is the gate doing its job. Retrying it costs
        # another full IDE verification to reach the same verdict.
        command = self._fake_command(output=GRADLE_GENUINE_FAILURE, fail_times=99)
        completed, attempts = self._run(command, attempts=3)
        self.assertNotEqual(0, completed.returncode)
        self.assertEqual(1, attempts)

    def test_a_central_descriptor_miss_is_retried_in_a_fresh_process(self):
        command = self._fake_command(output=CENTRAL_DESCRIPTOR, fail_times=1)
        completed, attempts = self._run(command, attempts=2)
        self.assertEqual(0, completed.returncode, completed.stdout + completed.stderr)
        self.assertEqual(2, attempts)

    def test_a_genuine_build_failure_is_not_retried(self):
        # Retrying a compilation error would burn the job's timeout to reach the
        # same failure, and would hide it behind two more minutes of noise.
        command = self._fake_command(output=GENUINE_FAILURE, fail_times=99)
        completed, attempts = self._run(command, attempts=3)
        self.assertNotEqual(0, completed.returncode)
        self.assertEqual(1, attempts)

    def test_a_persistent_429_exhausts_the_attempts_and_fails(self):
        command = self._fake_command(output=CENTRAL_429, fail_times=99)
        completed, attempts = self._run(command, attempts=2)
        self.assertNotEqual(0, completed.returncode)
        self.assertEqual(2, attempts)

    def test_the_failing_output_is_still_visible_to_the_job_log(self):
        command = self._fake_command(output=CENTRAL_429, fail_times=99)
        completed, _ = self._run(command, attempts=2)
        self.assertIn("429", completed.stdout + completed.stderr)


if __name__ == "__main__":
    unittest.main()
