from __future__ import annotations

import importlib.util
import inspect
import io
import json
import os
import tempfile
import threading
import time
import urllib.parse
import unittest
import unittest.mock
from pathlib import Path


ROOT = Path(__file__).resolve().parents[2]


def load(name: str, path: Path):
    spec = importlib.util.spec_from_file_location(name, path)
    module = importlib.util.module_from_spec(spec)
    if spec.loader is None:
        raise ImportError(f"cannot load {path}")
    spec.loader.exec_module(module)
    return module


BOOTSTRAP = load("chaos_engine_bootstrap_ux", ROOT / "chaos-engine/bootstrap.py")


class InstallerUxTests(unittest.TestCase):
    def test_reporter_brand_is_first_and_pipe_output_is_durable(self):
        stream = io.StringIO()
        clock = iter((10.0, 12.0, 12.0, 15.0)).__next__
        reporter = BOOTSTRAP.InstallReporter(stream=stream, clock=clock)
        reporter.start("Resolve source", remaining=("Download source",))
        reporter.complete("Resolve source", remaining=("Download source",))
        output = stream.getvalue()
        self.assertNotIn("transparent automation", output)
        self.assertNotIn("AUTONOMOUS INSTALL", output)
        self.assertIn("Chaos Engine", output)
        self.assertNotIn("QUANTUM MANDATE", output)
        self.assertGreaterEqual(output.split("START", 1)[0].count("\n"), 3)
        self.assertIn("START Resolve source", output)
        self.assertIn("DONE  Resolve source", output)
        self.assertNotIn("\r", output)
        self.assertNotIn("\x1b", output)

    def test_reporter_uses_fixed_height_checklist_for_tty(self):
        class Tty(io.StringIO):
            def isatty(self):
                return True

        stream = Tty()
        environment = {key: value for key, value in os.environ.items() if key != "NO_COLOR"}
        environment["TERM"] = "xterm"
        with unittest.mock.patch.dict(os.environ, environment, clear=True), unittest.mock.patch.object(
            BOOTSTRAP.InstallReporter, "_enable_windows_vt", return_value=True
        ):
            reporter = BOOTSTRAP.InstallReporter(stream=stream, clock=lambda: 1.0)
            try:
                reporter.start(
                    "Download source", remaining=("Install core",),
                    detail="https://example.invalid/source",
                )
                output = stream.getvalue()
                self.assertNotIn("transparent automation", output)
                self.assertNotIn("AUTONOMOUS INSTALL", output)
                self.assertIn("Chaos Engine", output)
                self.assertIn("\x1b[38;2;255;59;77m", output)
                self.assertIn("[", output)
                self.assertIn("Download source", output)
                self.assertIn("running", output)
                self.assertIn("Install core", output)
                self.assertIn("Elapsed 00:00", output)
                self.assertIn("ETA calculating", output)
                self.assertIn("\x1b[", output)
            finally:
                reporter.close()
        self.assertFalse(any(thread.name == "chaos-engine-installer" for thread in threading.enumerate()))

    def test_ticker_updates_elapsed_each_second_during_blocking_work(self):
        class Tty(io.StringIO):
            def isatty(self):
                return True

        stream = Tty()
        with unittest.mock.patch.dict(os.environ, {"TERM": "xterm", "NO_COLOR": "1"}):
            reporter = BOOTSTRAP.InstallReporter(stream=stream)
            reporter.start("Verify installation")
            time.sleep(2.2)
            reporter.close()
        output = stream.getvalue()
        self.assertIn("Elapsed 00:01", output)
        self.assertIn("Elapsed 00:02", output)

    def test_tty_reporter_honors_plain_and_ascii_fallbacks_and_width(self):
        class NarrowAsciiTty(io.StringIO):
            encoding = "ascii"

            def isatty(self):
                return True

        stream = NarrowAsciiTty()
        with unittest.mock.patch.dict(os.environ, {"NO_COLOR": "1", "TERM": "dumb", "COLUMNS": "38"}):
            reporter = BOOTSTRAP.InstallReporter(stream=stream, clock=lambda: 1.0)
            try:
                reporter.start("Download source", detail="https://example.invalid/a/very/long/source")
            finally:
                reporter.close()
        output = stream.getvalue()
        self.assertNotIn("\x1b", output)
        self.assertNotIn("✓", output)
        self.assertNotIn("transparent automation", output)
        self.assertNotIn("AUTONOMOUS INSTALL", output)
        self.assertIn("Chaos Engine", output)
        self.assertTrue(all(len(line) <= 38 for line in output.splitlines()))

    def test_narrow_width_uses_brand_narrow(self):
        lines = BOOTSTRAP.brand_lines(width=27, color=False, unicode=False)
        self.assertEqual(list(BOOTSTRAP.BRAND_NARROW), lines)
        self.assertIn("/C|*|E/", "\n".join(lines))
        self.assertNotEqual(
            BOOTSTRAP.brand_lines(width=27, color=False, unicode=False),
            BOOTSTRAP.brand_lines(width=28, color=False, unicode=False),
        )
        class NarrowTty(io.StringIO):
            def isatty(self):
                return True

        stream = NarrowTty()
        environment = {
            key: value
            for key, value in os.environ.items()
            if key not in {"CHAOS_ENGINE_BRAND_SHOWN", "NO_COLOR"}
        }
        environment.update({"TERM": "xterm", "COLUMNS": "20", "NO_COLOR": "1"})
        with unittest.mock.patch.dict(os.environ, environment, clear=True):
            reporter = BOOTSTRAP.InstallReporter(stream=stream, clock=lambda: 1.0)
            reporter.close()
        output = stream.getvalue()
        self.assertIn("/C|*|E/", output)
        self.assertIn("Chaos Engine", output)

    def test_download_progress_uses_measured_bytes_and_rolling_rate(self):
        class Tty(io.StringIO):
            def isatty(self):
                return True

        class Clock:
            def __init__(self):
                self.now = 0.0

            def __call__(self):
                return self.now

        clock = Clock()
        stream = Tty()
        with unittest.mock.patch.dict(os.environ, {"TERM": "xterm"}), unittest.mock.patch.object(
            BOOTSTRAP.threading.Thread, "start", lambda self: None
        ):
            reporter = BOOTSTRAP.InstallReporter(stream=stream, clock=clock)
            try:
                reporter.start("Download source", remaining=("Install core",))
                reporter.begin_download(1000, detail="source files")
                self.assertIn("Speed calculating", stream.getvalue())
                clock.now = 1.0
                reporter.downloaded(250)
                clock.now = 2.0
                reporter.downloaded(250)
                reporter._render_locked()
                output = stream.getvalue()
                self.assertIn("250 B/s", output)
                self.assertIn("ETA 00:02", output)
                self.assertIn("Current action: Download source", output)
            finally:
                reporter._stop.set()
                reporter._thread = None

    def test_nested_start_keeps_inflight_stage_visible_without_fabricated_eta(self):
        class Clock:
            def __init__(self):
                self.now = 0.0

            def __call__(self):
                return self.now

        stream = io.StringIO()
        clock = Clock()
        reporter = BOOTSTRAP.InstallReporter(stream=stream, clock=clock)
        reporter.start(
            "Resolve source",
            remaining=("Install core", "Provision dependencies", "Verify installation"),
        )
        clock.now = 4.0
        reporter.complete(
            "Resolve source",
            remaining=("Install core", "Provision dependencies", "Verify installation"),
        )
        reporter.start(
            "Install core",
            remaining=("Provision dependencies", "Verify installation"),
        )
        reporter.start("Provision dependencies", remaining=("Verify installation",))
        self.assertIn("Install core", getattr(reporter, "_in_flight", ()))
        self.assertIn("Install core", getattr(reporter, "_in_flight", ()))
        self.assertEqual("calculating", reporter._eta(clock.now))

    def test_non_tty_history_has_timestamp_result_duration_and_current_action(self):
        class Clock:
            now = 0.0

            def __call__(self):
                return self.now

        clock = Clock()
        stream = io.StringIO()
        reporter = BOOTSTRAP.InstallReporter(stream=stream, clock=clock)
        reporter.start("Resolve source", remaining=("Download source",), detail="main")
        clock.now = 2.25
        reporter.complete("Resolve source", remaining=("Download source",))
        output = stream.getvalue()
        self.assertIn("Current action: Resolve source", output)
        self.assertRegex(output, r"\[\+00:02\] PASS Resolve source \(00:02\)")

    def test_interactive_confirmation_accepts_only_y_or_yes(self):
        for answer in ("y\n", "YES\n"):
            BOOTSTRAP.confirm_operation("Download source", input_stream=io.StringIO(answer), output=io.StringIO())
        for answer in ("\n", "ok\n", "n\n"):
            with self.assertRaisesRegex(BOOTSTRAP.InstallCancelled, "cancelled before Download source"):
                BOOTSTRAP.confirm_operation("Download source", input_stream=io.StringIO(answer), output=io.StringIO())

    def test_interactive_terminal_preflight_happens_before_network(self):
        called = False

        def opener(*_args, **_kwargs):
            nonlocal called
            called = True
            raise AssertionError("network must not run")

        with tempfile.TemporaryDirectory() as temporary:
            with self.assertRaisesRegex(RuntimeError, "interactive mode requires"):
                BOOTSTRAP.install_latest(
                    Path(temporary), repository="owner/repo", interactive=True,
                    terminal_factory=lambda: (_ for _ in ()).throw(OSError("no tty")), opener=opener,
                )
        self.assertFalse(called)

    def test_default_api_never_requests_confirmation(self):
        signature = inspect.signature(BOOTSTRAP.install_latest)
        self.assertFalse(signature.parameters["interactive"].default)

    def test_cli_keeps_json_on_stdout_and_ux_on_stderr(self):
        result = {"status": "installed", "root": "/project", "commit": "a" * 40}
        stdout = io.StringIO()
        stderr = io.StringIO()
        with unittest.mock.patch.object(BOOTSTRAP, "install_latest", return_value=result), unittest.mock.patch.object(
            BOOTSTRAP.sys, "stdout", stdout
        ), unittest.mock.patch.object(BOOTSTRAP.sys, "stderr", stderr), unittest.mock.patch.object(
            BOOTSTRAP.sys, "argv", ["bootstrap.py", "--project", ".", "--repository", "owner/repo"]
        ):
            self.assertEqual(0, BOOTSTRAP.main())
        self.assertEqual(result, json.loads(stdout.getvalue()))
        self.assertNotIn("transparent automation", stderr.getvalue())
        self.assertNotIn("AUTONOMOUS INSTALL", stderr.getvalue())
        self.assertIn("Chaos Engine", stderr.getvalue())

    def test_wrappers_expose_and_forward_interactive_mode(self):
        shell = (ROOT / "chaos-engine/install.sh").read_text(encoding="utf-8")
        powershell = (ROOT / "chaos-engine/install.ps1").read_text(encoding="utf-8")
        self.assertIn('"--interactive"', shell)
        self.assertIn("[switch]$Interactive", powershell)
        self.assertNotIn("CHAOS_ENGINE_INTERACTIVE", powershell)
        self.assertIn('arguments += "--interactive"', powershell)

    def test_wrappers_print_the_same_chaos_engine_mark(self):
        shell = (ROOT / "chaos-engine/install.sh").read_text(encoding="utf-8")
        powershell = (ROOT / "chaos-engine/install.ps1").read_text(encoding="utf-8")
        for document in (shell, powershell):
            self.assertNotIn("transparent automation", document)
            self.assertNotIn("AUTONOMOUS INSTALL", document)
            self.assertIn("Chaos Engine", document)
            self.assertNotIn("QUANTUM MANDATE", document)
        for line in BOOTSTRAP.brand_lines(width=80, color=False, unicode=False):
            if not line.strip():
                continue
            self.assertIn(line, shell)
            self.assertIn(line, powershell)
        for line in BOOTSTRAP.BRAND_NARROW:
            self.assertIn(line, shell)
            self.assertIn(line, powershell)
        self.assertIn("[ \"$cols\" -lt 28 ]", shell)
        self.assertIn("$cols -lt 28", powershell)
        self.assertIn("/C|*|E/", shell)
        self.assertIn("/C|*|E/", powershell)

    def test_wrappers_put_checklist_then_current_action_below_heading(self):
        shell = (ROOT / "chaos-engine/install.sh").read_text(encoding="utf-8")
        powershell = (ROOT / "chaos-engine/install.ps1").read_text(encoding="utf-8")
        for document in (shell, powershell):
            heading = document.index("Installing ChaosEngine into")
            checklist = document.index("[ ] Resolve source", heading)
            current = document.index("Current action: Download bootstrap", checklist)
            self.assertLess(heading, checklist)
            self.assertLess(checklist, current)

    def test_main_emits_stable_actionable_error_codes(self):
        cases = (
            (ValueError("ChaosEngine Claude marketplace collision"), "CE-CLAUDE-MARKETPLACE-CONFLICT"),
            (RuntimeError("interactive mode requires a usable controlling terminal"), "CE-INTERACTIVE-TERMINAL"),
            (RuntimeError("network broke"), "CE-INSTALL-FAILED"),
            (ValueError("runtime artifact checksum verification failed"), "CE-INSTALL-CHECKSUM"),
            (ValueError("unsupported platform: solaris/sparc"), "CE-INSTALL-UNSUPPORTED-PLATFORM"),
            (RuntimeError("memory-mcp entrypoint probe failed"), "CE-INSTALL-PROBE-FAILED"),
        )
        for error, code in cases:
            with self.subTest(code=code):
                stdout = io.StringIO()
                stderr = io.StringIO()
                with unittest.mock.patch.object(BOOTSTRAP, "install_latest", side_effect=error), unittest.mock.patch.object(
                    BOOTSTRAP.sys, "stdout", stdout
                ), unittest.mock.patch.object(BOOTSTRAP.sys, "stderr", stderr), unittest.mock.patch.object(
                    BOOTSTRAP.sys, "argv", ["bootstrap.py", "--project", ".", "--repository", "owner/repo"]
                ):
                    self.assertEqual(1, BOOTSTRAP.main())
                self.assertEqual("", stdout.getvalue())
                self.assertIn(str(error).split("\n", 1)[0], stderr.getvalue())
                self.assertIn(code, stderr.getvalue())
                self.assertIn("#installer-errors", stderr.getvalue())
                self.assertIn(".chaos-engine/install.py doctor", stderr.getvalue())
                self.assertIn("doctor --project . --json", stderr.getvalue())
                self.assertIn("status --project . --json", stderr.getvalue())
                if code == "CE-INSTALL-FAILED":
                    report = [
                        line for line in stderr.getvalue().splitlines()
                        if line.startswith("Report: ")
                    ][0]
                    self.assertIn("https://github.com/owner/repo/issues/new?", report)
                    self.assertLessEqual(len(report.removeprefix("Report: ")), 2000)

    def test_keyboard_interrupt_emits_cancelled_without_traceback(self):
        stdout = io.StringIO()
        stderr = io.StringIO()
        tree = f"https://github.com/owner/repo/tree/{'c' * 40}/chaos-engine"

        def boom(*_args, **kwargs):
            reporter = kwargs.get("reporter")
            if reporter is not None:
                reporter.start("Download source", remaining=(), detail=tree)
            raise KeyboardInterrupt()

        with unittest.mock.patch.object(BOOTSTRAP, "install_latest", boom), unittest.mock.patch.object(
            BOOTSTRAP.sys, "stdout", stdout
        ), unittest.mock.patch.object(BOOTSTRAP.sys, "stderr", stderr), unittest.mock.patch.object(
            BOOTSTRAP.sys, "argv", ["bootstrap.py", "--project", ".", "--repository", "owner/repo"]
        ):
            try:
                code = BOOTSTRAP.main()
            except KeyboardInterrupt:
                self.fail("KeyboardInterrupt leaked to the caller")
        self.assertEqual(1, code)
        self.assertEqual("", stdout.getvalue())
        err = stderr.getvalue()
        self.assertIn("CE-INSTALL-CANCELLED", err)
        self.assertIn("#installer-errors", err)
        self.assertIn(".chaos-engine/install.py doctor", err)
        self.assertIn("Last verified generation", err)
        self.assertIn("Rerun the same install command", err)
        self.assertNotIn("Traceback", err)
        self.assertNotRegex(err, r"Traceback \(most recent call last\):[^\n]*tree/")
        self.assertNotIn(f"tree/{'c' * 40}/chaos-engine", err.split("CE-INSTALL-CANCELLED", 1)[-1])

    def test_debug_env_prints_traceback_and_unset_does_not(self):
        def boom(*_args, **_kwargs):
            raise RuntimeError("sealed walk exploded")

        def run_main(environment):
            stdout = io.StringIO()
            stderr = io.StringIO()
            with unittest.mock.patch.dict(os.environ, environment, clear=True):
                with unittest.mock.patch.object(BOOTSTRAP, "install_latest", boom):
                    with unittest.mock.patch.object(BOOTSTRAP.sys, "stdout", stdout):
                        with unittest.mock.patch.object(BOOTSTRAP.sys, "stderr", stderr):
                            with unittest.mock.patch.object(
                                BOOTSTRAP.sys,
                                "argv",
                                ["bootstrap.py", "--project", ".", "--repository", "owner/repo"],
                            ):
                                self.assertEqual(1, BOOTSTRAP.main())
            return stderr.getvalue()

        baseline = {key: value for key, value in os.environ.items() if key != "CHAOS_ENGINE_DEBUG"}
        unset_err = run_main(baseline)
        self.assertIn("CE-INSTALL-FAILED", unset_err)
        self.assertNotIn("Traceback", unset_err)
        debug_err = run_main({**baseline, "CHAOS_ENGINE_DEBUG": "1"})
        self.assertIn("CE-INSTALL-FAILED", debug_err)
        self.assertIn("Traceback", debug_err)
        self.assertIn("sealed walk exploded", debug_err)

    def test_unexpected_exception_includes_issue_url_without_traceback(self):
        stdout = io.StringIO()
        stderr = io.StringIO()
        with unittest.mock.patch.object(
            BOOTSTRAP, "install_latest", side_effect=Exception("sealed walk exploded")
        ), unittest.mock.patch.object(BOOTSTRAP.sys, "stdout", stdout), unittest.mock.patch.object(
            BOOTSTRAP.sys, "stderr", stderr
        ), unittest.mock.patch.object(
            BOOTSTRAP.sys, "argv", ["bootstrap.py", "--project", ".", "--repository", "owner/repo"]
        ):
            try:
                code = BOOTSTRAP.main()
            except Exception:
                self.fail("unexpected exception leaked to the caller")
        self.assertEqual(1, code)
        err = stderr.getvalue()
        self.assertIn("CE-INSTALL-FAILED", err)
        self.assertIn("sealed walk exploded", err)
        self.assertIn("https://github.com/owner/repo/issues/new", err)
        self.assertIn(".chaos-engine/install.py status", err)
        self.assertNotIn("Traceback", err)

    def test_prefilled_report_is_bounded_sanitized_and_names_failed_health(self):
        error = BOOTSTRAP.InstallHealthError(
            "Verify installation",
            {"components": {"memory": {"status": "recovery-required"}}},
        )
        stderr = io.StringIO()
        with unittest.mock.patch.object(BOOTSTRAP.sys, "stderr", stderr):
            BOOTSTRAP.emit_install_failure("CE-INSTALL-FAILED", error, "owner/repo")
        report = [
            line.removeprefix("Report: ")
            for line in stderr.getvalue().splitlines()
            if line.startswith("Report: ")
        ][0]
        query = urllib.parse.parse_qs(urllib.parse.urlsplit(report).query)
        body = query["body"][0]
        self.assertIn("Failed phase: Verify installation", body)
        self.assertIn("Unhealthy components: memory", body)
        self.assertIn("doctor --project . --json", body)
        self.assertLessEqual(len(report), 2000)

    def test_failure_cause_redacts_local_paths_and_secret_assignments(self):
        private_path = Path(
            "C:/private/consumer/state.json"
            if os.name == "nt"
            else "/private/consumer/state.json"
        )
        error = RuntimeError(f"failed at {private_path} token=super-secret")
        stderr = io.StringIO()
        with unittest.mock.patch.object(BOOTSTRAP.sys, "stderr", stderr):
            BOOTSTRAP.emit_install_failure("CE-INSTALL-FAILED", error, "owner/repo")
        output = stderr.getvalue()
        report = next(
            line.removeprefix("Report: ")
            for line in output.splitlines()
            if line.startswith("Report: ")
        )
        body = urllib.parse.parse_qs(urllib.parse.urlsplit(report).query)["body"][0]
        self.assertIn("Cause: failed at <path> token=<redacted>", body)
        self.assertNotIn("super-secret", output)
        self.assertNotIn(str(private_path), output)

    def test_pr_gate_runs_fresh_installer_on_exact_three_os_matrix(self):
        workflow = (ROOT / ".github/workflows/pr-gate.yml").read_text(encoding="utf-8")
        self.assertIn("chaos_installer: ${{ steps.filter.outputs.chaos_installer }}", workflow)
        self.assertIn("chaos-engine/bootstrap.py", workflow)
        self.assertIn("chaos-engine/hosts.py", workflow)
        self.assertIn("tests/scripts/test_chaos_engine_installer_ux.py", workflow)
        block = workflow[workflow.index("  chaos-installer-acceptance:"):workflow.index("  summary:")]
        self.assertIn("needs.changes.outputs.chaos_installer == 'true'", block)
        self.assertIn("os: [ubuntu-22.04, macos-15, windows-2025]", block)
        self.assertIn("GITHUB_TOKEN: ${{ github.token }}", block)
        self.assertIn("scripts/ci/chaos_engine_live_installer_acceptance.py", block)
        self.assertIn("--candidate-sha ${{ github.event.pull_request.head.sha }}", block)
        self.assertIn("--base-sha ${{ github.event.pull_request.base.sha }}", block)
        self.assertIn("tests.scripts.test_chaos_engine_bootstrap", block)
        self.assertIn("tests.scripts.test_chaos_engine_install_wrappers", block)
        self.assertNotIn("tests.scripts.test_chaos_engine_live_installer_acceptance", block)
        summary = workflow[workflow.index("  summary:"):]
        self.assertIn("- chaos-installer-acceptance", summary)

    def test_confirmation_callbacks_reach_dependencies_maven_and_activation(self):
        bootstrap = (ROOT / "chaos-engine/bootstrap.py").read_text(encoding="utf-8")
        installer = (ROOT / "chaos-engine/install.py").read_text(encoding="utf-8")
        dependencies = (ROOT / "chaos-engine/dependencies.py").read_text(encoding="utf-8")
        hosts = (ROOT / "chaos-engine/hosts.py").read_text(encoding="utf-8")
        self.assertIn("reporter=reporter", bootstrap)
        self.assertIn("confirmer=confirm", bootstrap)
        self.assertIn("confirmer=confirmer", installer)
        self.assertIn("Download {name} runtime", dependencies)
        self.assertIn("Install {tool} package", dependencies)
        self.assertIn("Activate {name} plugin for {client}", hosts)


if __name__ == "__main__":
    unittest.main()
