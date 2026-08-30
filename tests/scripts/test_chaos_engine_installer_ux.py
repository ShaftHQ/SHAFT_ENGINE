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
        self.assertIn("ChaosEngine", output)
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
                self.assertIn("ChaosEngine", output)
                self.assertIn("\x1b[38;2;255;59;77m", output)
                self.assertIn("[", output)
                self.assertIn("Download source", output)
                self.assertIn("running", output)
                self.assertIn("Install core", output)
                self.assertIn("Elapsed 00:00", output)
                self.assertIn("Trace (last 0 of 0; full log:", output)
                self.assertIn("Summary", output)
                self.assertNotIn("ETA calculating", output)
                self.assertNotIn("Current action:", output)
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
            try:
                reporter.start("Verify installation")
                deadline = time.monotonic() + 5
                while (
                    "Elapsed 00:02" not in stream.getvalue()
                    and time.monotonic() < deadline
                ):
                    time.sleep(0.05)
            finally:
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
        self.assertIn("ChaosEngine", output)
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
        self.assertIn("ChaosEngine", output)

    def test_wide_brands_use_reversed_e_and_keep_red_core(self):
        for unicode, bars, ends in (
            (False, ("---+", "|", "---+", "|", "---+"), ("+", "|", "+", "|", "+")),
            (True, ("───┐", "│", "───┤", "│", "───┘"), ("┐", "│", "┤", "│", "┘")),
        ):
            with self.subTest(unicode=unicode):
                lines = BOOTSTRAP.brand_lines(width=80, color=False, unicode=unicode)
                plain = "\n".join(lines)
                colored = "\n".join(BOOTSTRAP.brand_lines(width=80, color=True, unicode=unicode))
                self.assertIn("ChaosEngine", plain)
                self.assertEqual(
                    [line.rindex(end) for line, end in zip(lines[:5], ends)],
                    [lines[0].rindex(ends[0])] * 5,
                )
                for line, bar in zip(lines[:5], bars):
                    self.assertIn(bar, line)
                self.assertIn(BOOTSTRAP.CYBERNETIC_RED, colored)
                self.assertIn("/C|*|E/", "\n".join(BOOTSTRAP.brand_lines(width=27, color=False)))

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
                self.assertNotIn("calculating", stream.getvalue())
                clock.now = 1.0
                reporter.downloaded(250)
                clock.now = 2.0
                reporter.downloaded(250)
                reporter._render_locked()
                output = stream.getvalue()
                self.assertIn("250 B/s", output)
                self.assertIn("remaining 00:02", output)
                self.assertIn("Elapsed", output)
                self.assertNotIn("Current action:", output)
            finally:
                reporter._stop.set()
                reporter._thread = None

    def test_whole_install_eta_includes_pending_stages_and_never_rounds_to_zero(self):
        class Tty(io.StringIO):
            def isatty(self):
                return True

        class Clock:
            now = 0.0

            def __call__(self):
                return self.now

        clock = Clock()
        stream = Tty()
        with unittest.mock.patch.dict(os.environ, {"TERM": "xterm"}), unittest.mock.patch.object(
            BOOTSTRAP.threading.Thread, "start", lambda self: None
        ):
            reporter = BOOTSTRAP.InstallReporter(stream=stream, clock=clock)
            try:
                reporter.start("Resolve source", remaining=("Download source", "Install core", "Verify installation"))
                clock.now = 4.0
                reporter.complete("Resolve source", remaining=("Download source", "Install core", "Verify installation"))
                reporter.start("Download source", remaining=("Install core", "Verify installation"))
                reporter.begin_download(1000)
                clock.now = 5.0
                reporter.downloaded(250)
                clock.now = 6.0
                reporter.downloaded(250)
                self.assertEqual("00:10", reporter._remaining(clock.now))
                rounding = BOOTSTRAP.InstallReporter(stream=io.StringIO(), clock=clock)
                rounding.start("Download source")
                rounding.begin_download(3)
                clock.now = 7.0
                rounding.downloaded(2)
                self.assertEqual("00:01", rounding._remaining(clock.now))
            finally:
                reporter._stop.set()
                reporter._thread = None

    def test_stalled_transfer_waits_without_stale_speed_and_keeps_eta(self):
        class Tty(io.StringIO):
            def isatty(self):
                return True

        class Clock:
            now = 0.0

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
                reporter.begin_download(1000)
                clock.now = 1.0
                reporter.downloaded(250)
                clock.now = 2.0
                reporter.downloaded(250)
                self.assertEqual("00:02", reporter._remaining(clock.now))
                stream.seek(0)
                stream.truncate(0)
                clock.now = 11.0
                reporter._render_locked()
                output = stream.getvalue()
                self.assertIn("waiting for data", output)
                self.assertIn("remaining 00:02", output)
                self.assertNotIn("250 B/s", output)
            finally:
                reporter._stop.set()
                reporter._thread = None

    def test_eta_ceiling_never_increases_while_progress_advances(self):
        class Clock:
            now = 0.0

            def __call__(self):
                return self.now

        clock = Clock()
        reporter = BOOTSTRAP.InstallReporter(stream=io.StringIO(), clock=clock)
        reporter.start("Resolve source", remaining=("Download source", "Install core"))
        clock.now = 4.0
        reporter.complete("Resolve source", remaining=("Download source", "Install core"))
        reporter.start("Download source", remaining=("Install core",))
        reporter.begin_download(1000)
        clock.now = 5.0
        reporter.downloaded(500)
        first = reporter._remaining(clock.now)
        clock.now = 7.0
        reporter.downloaded(1)
        self.assertEqual(first, reporter._remaining(clock.now))
        reporter.close()

    def test_redirected_stalled_transfer_emits_waiting_heartbeat(self):
        class Clock:
            now = 0.0

            def __call__(self):
                return self.now

        clock = Clock()
        stream = io.StringIO()
        with unittest.mock.patch.object(BOOTSTRAP.threading.Thread, "start", lambda self: None):
            reporter = BOOTSTRAP.InstallReporter(stream=stream, clock=clock)
            reporter.start("Download source")
            reporter.begin_download(1000)
        self.assertIsNotNone(reporter._thread)
        reporter._stop.set()
        reporter._thread = None
        clock.now = 1.0
        reporter.downloaded(250)
        clock.now = 2.0
        reporter.downloaded(250)
        reporter._remaining(clock.now)
        stream.seek(0)
        stream.truncate(0)
        clock.now = 11.0
        reporter._stop = unittest.mock.Mock()
        reporter._stop.wait.side_effect = (False, True)
        reporter._ticker()
        output = stream.getvalue()
        self.assertIn("waiting for data", output)
        self.assertIn("remaining 00:02", output)
        self.assertNotIn("B/s", output)

    def test_success_cta_reports_agent_session_and_user_guide_on_stderr(self):
        stream = io.StringIO()
        reporter = BOOTSTRAP.InstallReporter(stream=stream)
        reporter.start("Activate clients")
        reporter.complete("Activate clients")
        reporter.success(
            Path("/project"),
            {"components": {"memory": {"status": "healthy"}, "core": {"status": "healthy"}}},
            {},
            repository="ShaftHQ/SHAFT_ENGINE",
        )
        output = stream.getvalue()
        self.assertLess(output.index("DONE  Activate clients"), output.index("Installation Successful!"))
        self.assertIn(
            "Installation Successful! You can now start a new agent session using Codex, Claude, Grok, Gemini, or Copilot. Just ask it to use chaos-engine and you should be good to go!",
            output,
        )
        self.assertIn("https://shafthq.github.io/docs/agentic/chaos-engine", output)
        self.assertIn(
            f"Full install trace: {Path('/project/.chaos-engine-state/install-trace.json').as_posix()}",
            output,
        )
        self.assertNotIn("Owned managed dependencies", output)
        self.assertNotIn("Continue working in", output)

    def test_trace_persists_every_event_beyond_live_tty_limit(self):
        reporter = BOOTSTRAP.InstallReporter(stream=io.StringIO())
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary)
            for index in range(BOOTSTRAP.TRACE_LIMIT + 1):
                reporter.trace(f"event {index}")
            BOOTSTRAP.write_install_trace(project, {"status": "installed"}, reporter.traces)
            trace = json.loads(
                (project / ".chaos-engine-state/install-trace.json").read_text(encoding="utf-8")
            )
        self.assertEqual(BOOTSTRAP.TRACE_LIMIT + 1, len(trace["trace"]))
        self.assertEqual("event 0", trace["trace"][0][1])
        self.assertEqual(f"event {BOOTSTRAP.TRACE_LIMIT}", trace["trace"][-1][1])

    def test_tty_success_stops_ticker_before_writing_cta(self):
        class Tty(io.StringIO):
            def isatty(self):
                return True

        stream = Tty()
        with unittest.mock.patch.dict(os.environ, {"TERM": "xterm", "NO_COLOR": "1"}), unittest.mock.patch.object(
            BOOTSTRAP.threading.Thread, "start", lambda self: None
        ):
            reporter = BOOTSTRAP.InstallReporter(stream=stream)
            reporter.start("Activate clients")
            reporter.success(Path("/project"), {}, {}, repository="owner/repo")
        self.assertTrue(reporter._stop.is_set())
        self.assertIn("Installation Successful!", stream.getvalue())

    def test_nested_start_keeps_inflight_stage_visible_with_learned_eta(self):
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
        self.assertEqual("00:08", reporter._remaining(clock.now))

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
        self.assertNotIn("Current action:", output)
        self.assertIn("START Resolve source", output)
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
        self.assertIn("ChaosEngine", stderr.getvalue())

    def test_cli_tty_writes_full_result_and_trace_without_stdout_json(self):
        class Tty(io.StringIO):
            def isatty(self):
                return True

        result = {"status": "installed", "root": "/project", "commit": "a" * 40, "doctor": {"healthy": True}}
        stdout = Tty()
        stderr = io.StringIO()
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary)
            with unittest.mock.patch.object(BOOTSTRAP, "install_latest", return_value=result), unittest.mock.patch.object(
                BOOTSTRAP.sys, "stdout", stdout
            ), unittest.mock.patch.object(BOOTSTRAP.sys, "stderr", stderr), unittest.mock.patch.object(
                BOOTSTRAP.sys, "argv", ["bootstrap.py", "--project", str(project), "--repository", "owner/repo"]
            ):
                self.assertEqual(0, BOOTSTRAP.main())
            trace = json.loads((project / ".chaos-engine-state/install-trace.json").read_text(encoding="utf-8"))
        self.assertEqual(result, trace["result"])
        self.assertEqual([], trace["trace"])
        self.assertNotIn('"doctor"', stdout.getvalue())

    def test_cli_failure_writes_trace(self):
        stderr = io.StringIO()
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary)
            with unittest.mock.patch.object(
                BOOTSTRAP, "install_latest", side_effect=RuntimeError("install failed")
            ), unittest.mock.patch.object(BOOTSTRAP.sys, "stderr", stderr), unittest.mock.patch.object(
                BOOTSTRAP.sys, "argv", ["bootstrap.py", "--project", str(project), "--repository", "owner/repo"]
            ):
                self.assertEqual(1, BOOTSTRAP.main())
            trace = json.loads(
                (project / ".chaos-engine-state/install-trace.json").read_text(encoding="utf-8")
            )
        self.assertEqual("failed", trace["result"]["status"])

    def test_wrappers_expose_and_forward_interactive_mode(self):
        shell = (ROOT / "chaos-engine/install.sh").read_text(encoding="utf-8")
        powershell = (ROOT / "chaos-engine/install.ps1").read_text(encoding="utf-8")
        self.assertIn('"--interactive"', shell)
        self.assertIn("[switch]$Interactive", powershell)
        self.assertNotIn("CHAOS_ENGINE_INTERACTIVE", powershell)
        self.assertIn('arguments += "--interactive"', powershell)

    def test_wrappers_leave_brand_and_checklist_to_python(self):
        shell = (ROOT / "chaos-engine/install.sh").read_text(encoding="utf-8")
        powershell = (ROOT / "chaos-engine/install.ps1").read_text(encoding="utf-8")
        for document in (shell, powershell):
            self.assertNotIn("transparent automation", document)
            self.assertNotIn("AUTONOMOUS INSTALL", document)
            self.assertNotIn("QUANTUM MANDATE", document)
            self.assertNotIn("[ ] Resolve source", document)
            self.assertNotIn("Current action: Download bootstrap", document)
            self.assertIn("Installing ChaosEngine into", document)
            self.assertNotIn("/C|*|E/", document)

    def test_main_emits_stable_actionable_error_codes(self):
        cases = (
            (ValueError("ChaosEngine Claude marketplace collision"), "CE-CLAUDE-MARKETPLACE-CONFLICT"),
            (RuntimeError("interactive mode requires a usable controlling terminal"), "CE-INTERACTIVE-TERMINAL"),
            (RuntimeError("network broke"), "CE-INSTALL-FAILED"),
            (ValueError("runtime artifact checksum verification failed"), "CE-INSTALL-CHECKSUM"),
            (ValueError("unsupported platform: solaris/sparc"), "CE-INSTALL-UNSUPPORTED-PLATFORM"),
            (RuntimeError("memory-mcp entrypoint probe failed"), "CE-INSTALL-PROBE-FAILED"),
        )
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary) / "project"
            project.mkdir()
            for error, code in cases:
                with self.subTest(code=code):
                    stdout = io.StringIO()
                    stderr = io.StringIO()
                    with unittest.mock.patch.object(BOOTSTRAP, "install_latest", side_effect=error), unittest.mock.patch.object(
                        BOOTSTRAP.sys, "stdout", stdout
                    ), unittest.mock.patch.object(BOOTSTRAP.sys, "stderr", stderr), unittest.mock.patch.object(
                        BOOTSTRAP.sys,
                        "argv",
                        [
                            "bootstrap.py",
                            "--project",
                            str(project),
                            "--repository",
                            "owner/repo",
                        ],
                    ):
                        self.assertEqual(1, BOOTSTRAP.main())
                    self.assertEqual("", stdout.getvalue())
                    self.assertIn(str(error).split("\n", 1)[0], stderr.getvalue())
                    self.assertIn(code, stderr.getvalue())
                    self.assertIn("#installer-errors", stderr.getvalue())
                    self.assertIn("Installer CLI is not on disk", stderr.getvalue())
                    if code == "CE-INSTALL-FAILED":
                        self.assertIn("Next step: click this link to open a GitHub issue", stderr.getvalue())
                        report = [
                            line for line in stderr.getvalue().splitlines()
                            if line.startswith("https://github.com/owner/repo/issues/new?")
                        ][0]
                        self.assertIn("template=chaos-engine-installer.yml", report)
                        self.assertLessEqual(len(report), 2000)

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
        self.assertIn("Next step: click this link to open a GitHub issue", err)
        self.assertNotIn("Traceback", err)

    def test_install_health_error_ignores_optional_absent_components(self):
        error = BOOTSTRAP.InstallHealthError(
            "Verify installation",
            {
                "components": {
                    "retrieval-config": {"status": "recovery-required", "taskImpact": "required"},
                    "maven-tools-mcp": {"status": "absent", "taskImpact": "optional"},
                }
            },
        )
        self.assertEqual(("retrieval-config",), error.unhealthy)

    def test_pom_xml_implies_maven_tools_unless_skip_tools(self):
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary) / "maven"
            project.mkdir()
            (project / "pom.xml").write_text("<project/>\n", encoding="utf-8")
            other = Path(temporary) / "plain"
            other.mkdir()
            self.assertTrue(BOOTSTRAP.wants_maven_tools(project, skip_tools=False, requested=False))
            self.assertFalse(BOOTSTRAP.wants_maven_tools(project, skip_tools=True, requested=False))
            self.assertFalse(BOOTSTRAP.wants_maven_tools(other, skip_tools=False, requested=False))
            self.assertTrue(BOOTSTRAP.wants_maven_tools(other, skip_tools=False, requested=True))

    def test_failure_cta_omits_missing_installer_cli(self):
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary) / "project"
            project.mkdir()
            stderr = io.StringIO()
            with unittest.mock.patch.object(BOOTSTRAP.sys, "stderr", stderr):
                BOOTSTRAP.emit_install_failure(
                    "CE-INSTALL-FAILED",
                    RuntimeError("probe failed"),
                    "owner/repo",
                    project=project,
                )
            err = stderr.getvalue()
            self.assertNotIn(".chaos-engine/install.py", err)
            self.assertIn("Installer CLI is not on disk", err)
            self.assertIn("Rerun the same install command", err)
            self.assertIn("https://github.com/owner/repo/issues/new", err)

    def test_failure_cta_names_installer_cli_only_when_present(self):
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary) / "project"
            installed = project / ".chaos-engine"
            installed.mkdir(parents=True)
            (installed / "install.py").write_text("# installer\n", encoding="utf-8")
            stderr = io.StringIO()
            with unittest.mock.patch.object(BOOTSTRAP.sys, "stderr", stderr):
                BOOTSTRAP.emit_install_failure(
                    "CE-INSTALL-FAILED",
                    RuntimeError("probe failed"),
                    "owner/repo",
                    project=project,
                )
            err = stderr.getvalue()
            self.assertIn(".chaos-engine/install.py doctor", err)
            self.assertIn(".chaos-engine/install.py status", err)

    def test_prefilled_report_is_bounded_sanitized_and_names_failed_health(self):
        error = BOOTSTRAP.InstallHealthError(
            "Verify installation",
            {"components": {"memory": {"status": "recovery-required", "taskImpact": "required"}}},
        )
        stderr = io.StringIO()
        with unittest.mock.patch.object(BOOTSTRAP.sys, "stderr", stderr):
            BOOTSTRAP.emit_install_failure("CE-INSTALL-FAILED", error, "owner/repo")
        report = [
            line
            for line in stderr.getvalue().splitlines()
            if line.startswith("https://github.com/owner/repo/issues/new?")
        ][0]
        query = urllib.parse.parse_qs(urllib.parse.urlsplit(report).query)
        self.assertEqual(["chaos-engine-installer.yml"], query["template"])
        self.assertEqual(["Verify installation"], query["failed_phase"])
        self.assertEqual(["memory"], query["unhealthy"])
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
            line
            for line in output.splitlines()
            if line.startswith("https://github.com/owner/repo/issues/new?")
        )
        query = urllib.parse.parse_qs(urllib.parse.urlsplit(report).query)
        self.assertEqual(["failed at <path> token=<redacted>"], query["cause"])
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
        self.assertIn(
            "--base-sha 1dec809c7c43709a8fcceef5e53690d124012eb3", block
        )
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
