from __future__ import annotations

import importlib.util
import inspect
import io
import json
import os
import tempfile
import threading
import time
import urllib.error
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
INSTALL = load("chaos_engine_install_ux", ROOT / "chaos-engine/install.py")


class Response(io.BytesIO):
    def __enter__(self):
        return self

    def __exit__(self, *_args):
        self.close()


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
                self.assertIn("Status", output)
                self.assertRegex(output, r"0/2")
                self.assertNotIn("ETA calculating", output)
                self.assertNotIn("Current action:", output)
                self.assertIn("\x1b[", output)
            finally:
                reporter.close()
        self.assertFalse(any(thread.name == "chaos-engine-installer" for thread in threading.enumerate()))

    def test_live_and_pipe_announce_use_aligned_project_source(self):
        class Tty(io.StringIO):
            def isatty(self):
                return True

        pipe = io.StringIO()
        reporter = BOOTSTRAP.InstallReporter(stream=pipe, clock=lambda: 1.0)
        reporter.announce(Path("/project"), "ShaftHQ/SHAFT_ENGINE", "main")
        pipe_out = pipe.getvalue()
        self.assertIn("Project", pipe_out)
        self.assertIn("Source", pipe_out)
        self.assertIn("ShaftHQ/SHAFT_ENGINE@main", pipe_out)
        self.assertNotIn("Install root:", pipe_out)
        self.assertNotIn("\x1b", pipe_out)

        stream = Tty()
        environment = {key: value for key, value in os.environ.items() if key != "NO_COLOR"}
        environment["TERM"] = "xterm"
        with unittest.mock.patch.dict(os.environ, environment, clear=True), unittest.mock.patch.object(
            BOOTSTRAP.InstallReporter, "_enable_windows_vt", return_value=True
        ), unittest.mock.patch.object(BOOTSTRAP.threading.Thread, "start", lambda self: None):
            tty = BOOTSTRAP.InstallReporter(stream=stream, clock=lambda: 1.0)
            tty.announce(Path("/project"), "owner/repo", "main")
            tty.start("Download source")
            tty.close()
        live = stream.getvalue()
        self.assertIn("Project", live)
        self.assertIn("Status", live)
        self.assertNotIn("Install root:", live)

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
                self.assertIn("Elapsed", output)
                self.assertNotRegex(output, r"remaining \d{2}:\d{2}")
                self.assertNotIn("ETA calculating", output)
                self.assertNotIn("Current action:", output)
            finally:
                reporter._stop.set()
                reporter._thread = None

    def test_download_keeps_pending_stages_and_completed_durations_without_remaining_time(self):
        class Tty(io.StringIO):
            def isatty(self):
                return True

        class Clock:
            now = 0.0

            def __call__(self):
                return self.now

        clock = Clock()
        stream = Tty()
        with unittest.mock.patch.dict(os.environ, {"TERM": "xterm", "NO_COLOR": "1"}), unittest.mock.patch.object(
            BOOTSTRAP.threading.Thread, "start", lambda self: None
        ):
            reporter = BOOTSTRAP.InstallReporter(stream=stream, clock=clock)
            try:
                reporter.start(
                    "Resolve source",
                    remaining=("Download source", "Install core", "Verify installation"),
                )
                clock.now = 4.0
                reporter.complete(
                    "Resolve source",
                    remaining=("Download source", "Install core", "Verify installation"),
                )
                reporter.start("Download source", remaining=("Install core", "Verify installation"))
                reporter.begin_download(1000)
                clock.now = 5.0
                reporter.downloaded(250)
                clock.now = 6.0
                reporter.downloaded(250)
                reporter._render_locked()
                output = stream.getvalue()
                self.assertIn("Resolve source  00:04", output)
                self.assertIn("Install core", output)
                self.assertIn("Verify installation", output)
                self.assertIn("250 B/s", output)
                self.assertIn("Elapsed", output)
                self.assertNotRegex(output, r"remaining \d{2}:\d{2}")
                self.assertNotIn("ETA calculating", output)
            finally:
                reporter._stop.set()
                reporter._thread = None

    def test_stalled_transfer_waits_without_stale_speed_or_remaining_time(self):
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
                stream.seek(0)
                stream.truncate(0)
                clock.now = 11.0
                reporter._render_locked()
                output = stream.getvalue()
                self.assertIn("waiting for data", output)
                self.assertIn("Elapsed", output)
                self.assertNotRegex(output, r"remaining \d{2}:\d{2}")
                self.assertNotIn("250 B/s", output)
            finally:
                reporter._stop.set()
                reporter._thread = None

    def test_progress_never_renders_remaining_time_as_bytes_advance(self):
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
                reporter.start("Resolve source", remaining=("Download source", "Install core"))
                clock.now = 4.0
                reporter.complete("Resolve source", remaining=("Download source", "Install core"))
                reporter.start("Download source", remaining=("Install core",))
                reporter.begin_download(1000)
                clock.now = 5.0
                reporter.downloaded(500)
                first = stream.getvalue()
                clock.now = 7.0
                reporter.downloaded(1)
                second = stream.getvalue()
                for output in (first, second):
                    self.assertNotRegex(output, r"remaining \d{2}:\d{2}")
                    self.assertNotIn("ETA calculating", output)
            finally:
                reporter._stop.set()
                reporter._thread = None

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
        reporter._render_locked()
        self.assertRegex(stream.getvalue(), r"250 B/s|Elapsed")
        stream.seek(0)
        stream.truncate(0)
        clock.now = 11.0
        reporter._stop = unittest.mock.Mock()
        reporter._stop.wait.side_effect = (False, True)
        reporter._ticker()
        output = stream.getvalue()
        self.assertIn("waiting for data", output)
        self.assertNotRegex(output, r"remaining \d{2}:\d{2}")
        self.assertNotIn("B/s", output)

    def test_success_cta_reports_agent_session_and_user_guide_on_stderr(self):
        stream = io.StringIO()
        reporter = BOOTSTRAP.InstallReporter(stream=stream)
        reporter.start("Activate clients")
        reporter.complete("Activate clients")
        commit = "a" * 40
        reporter.success(
            Path("/project"),
            {
                "commit": commit,
                "status": "healthy",
                "components": {
                    "memory": {"status": "healthy"},
                    "core": {"status": "healthy"},
                },
            },
            {"codex": {"status": "healthy"}, "claude": {"status": "healthy"}},
            repository="ShaftHQ/SHAFT_ENGINE",
        )
        output = stream.getvalue()
        self.assertLess(output.index("DONE  Activate clients"), output.index("Installation Successful!"))
        self.assertIn("Installation Successful!", output)
        self.assertNotIn("You can now start a new agent session using Codex, Claude, Grok, Gemini, or Copilot", output)
        self.assertIn(f"Resolved commit: {commit}", output)
        self.assertIn("Doctor: healthy (2/2 components healthy)", output)
        self.assertIn("Clients: claude, codex", output)
        self.assertIn("https://shafthq.github.io/docs/agentic/chaos-engine", output)
        self.assertIn(
            f"Full install trace: {Path('/project/.chaos-engine-state/install-trace.json').as_posix()}",
            output,
        )
        self.assertNotIn("Owned managed dependencies", output)
        self.assertNotIn("Continue working in", output)
        self.assertIn("To get started:", output)
        self.assertIn("First-session brief:", output)
        self.assertIn("Landed: portable core", output)
        self.assertIn("Untracked: generated indexes", output)
        self.assertIn("Open one activated host (claude, codex)", output)
        self.assertIn("Ask the agent to load / use the `chaos-engine` skill.", output)
        self.assertIn("Run a small sample task", output)
        self.assertNotIn("\x1b[", output)
        self.assertNotIn("\r", output)
        report = output[output.index("Installation Successful!"):]
        self.assertLessEqual(len(report.splitlines()), 40)

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

    def test_tty_success_uses_brand_colors_without_painting_every_line(self):
        class Tty(io.StringIO):
            def isatty(self):
                return True

        stream = Tty()
        with unittest.mock.patch.dict(os.environ, {"TERM": "xterm"}, clear=False), unittest.mock.patch.object(
            BOOTSTRAP.threading.Thread, "start", lambda self: None
        ), unittest.mock.patch.dict(os.environ, {"NO_COLOR": ""}, clear=False):
            os.environ.pop("NO_COLOR", None)
            reporter = BOOTSTRAP.InstallReporter(stream=stream)
            reporter.success(
                Path("/project"),
                {"commit": "b" * 40, "status": "healthy", "components": {}},
                {"claude": {"status": "healthy"}},
                repository="ShaftHQ/SHAFT_ENGINE",
            )
        output = stream.getvalue()
        self.assertIn("\x1b[32mInstallation Successful!\x1b[0m", output)
        self.assertIn(BOOTSTRAP.ION_BLUE, output)
        self.assertIn("To get started:", output)
        self.assertNotIn("\r", output)

    def test_core_and_provision_are_sequential_not_both_running(self):
        """#5635: Install core and Provision dependencies must not both show running."""
        class Tty(io.StringIO):
            def isatty(self):
                return True

        class Clock:
            def __init__(self):
                self.now = 0.0

            def __call__(self):
                return self.now

        stream = Tty()
        clock = Clock()
        with unittest.mock.patch.dict(os.environ, {"TERM": "xterm", "NO_COLOR": "1"}), unittest.mock.patch.object(
            BOOTSTRAP.threading.Thread, "start", lambda self: None
        ):
            reporter = BOOTSTRAP.InstallReporter(stream=stream, clock=clock)
            try:
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
                reporter._render_locked()
                mid = stream.getvalue()
                self.assertIn("Install core", mid)
                running_lines = [line for line in mid.splitlines() if " running" in line]
                self.assertTrue(any("Install core" in line for line in running_lines))
                self.assertFalse(
                    any("Provision dependencies" in line for line in running_lines),
                    running_lines,
                )
                reporter.complete(
                    "Install core",
                    remaining=("Provision dependencies", "Verify installation"),
                )
                reporter.start("Provision dependencies", remaining=("Verify installation",))
                reporter.trace("download https://example.test/tool.tgz")
                reporter.trace("run uv python install 3.12 --no-progress")
                stream.truncate(0)
                stream.seek(0)
                reporter._render_locked()
                output = stream.getvalue()
                running_lines = [line for line in output.splitlines() if " running" in line]
                self.assertTrue(
                    any("Provision dependencies" in line for line in running_lines)
                )
                self.assertFalse(
                    any("Install core" in line for line in running_lines),
                    running_lines,
                )
                self.assertIn("Verify installation", reporter.remaining_operations)
                self.assertIn("download https://example.test/tool.tgz", output)
                self.assertIn("run uv python install 3.12 --no-progress", output)
                self.assertIn("Resolve source  00:04", output)
                self.assertNotRegex(output, r"remaining \d{2}:\d{2}")
            finally:
                reporter._stop.set()
                reporter._thread = None

    def test_safe_command_trace_redacts_secret_looking_values(self):
        line = BOOTSTRAP.safe_command_trace(
            ["tool", "--token=abcd", "deadbeefdeadbeefdeadbeefdeadbeef", "ok"]
        )
        self.assertIn("ok", line)
        self.assertNotIn("abcd", line)
        self.assertNotIn("deadbeefdeadbeefdeadbeefdeadbeef", line)
        self.assertIn("***", line)

    def test_reporter_transition_helper_makes_core_and_provision_exclusive(self):
        stream = io.StringIO()
        reporter = BOOTSTRAP.InstallReporter(stream=stream, clock=lambda: 1.0)
        reporter.start(
            "Install core",
            remaining=("Provision dependencies", "Verify installation"),
        )
        INSTALL._reporter_transition_to_provision(reporter, detail="uv/python/node")
        self.assertNotIn("Install core", getattr(reporter, "_in_flight", ()))
        self.assertIn("Install core", reporter.completed_operations)
        self.assertEqual("Provision dependencies", reporter.current_operation)
        self.assertIn("Provision dependencies", reporter._in_flight)
        self.assertEqual(
            1, len([x for x in reporter._in_flight if x == "Provision dependencies"])
        )

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

    def test_detect_install_hosts_marks_path_clis(self):
        mapping = {"claude": "/bin/claude", "codex": "/bin/codex"}

        def which(name):
            return mapping.get(name)

        hosts = BOOTSTRAP.detect_install_hosts(which=which)
        by_id = {host_id: found for host_id, _label, found in hosts}
        self.assertTrue(by_id["claude"])
        self.assertTrue(by_id["codex"])
        self.assertFalse(by_id["grok"])
        self.assertFalse(by_id["gemini"])
        self.assertFalse(by_id["copilot"])

    def test_first_run_wizard_explains_hosts_companions_and_next_actions(self):
        stream = io.StringIO()
        answers = io.StringIO("y\ny\n")
        BOOTSTRAP.run_first_run_wizard(
            project=Path("/project"),
            repository="owner/repo",
            with_maven_tools=True,
            input_stream=answers,
            output=stream,
            which=lambda name: "/bin/claude" if name == "claude" else None,
        )
        output = stream.getvalue()
        self.assertIn("ChaosEngine first-run wizard", output)
        self.assertIn("Detected host CLIs: Claude Code", output)
        self.assertIn("Caveman + Ponytail", output)
        self.assertIn("Maven Tools MCP", output)
        self.assertIn("Hosts", output)
        self.assertIn("on PATH", output)
        self.assertIn("not on PATH", output)
        self.assertIn("Claude Code:", output)
        self.assertIn("Codex:", output)
        self.assertIn("Grok:", output)
        self.assertIn("Gemini:", output)
        self.assertIn("GitHub Copilot:", output)

    def test_first_run_wizard_cancel_before_network_leaves_path_unchanged(self):
        called = False

        def opener(*_args, **_kwargs):
            nonlocal called
            called = True
            raise AssertionError("network must not run")

        class Terminal:
            def __enter__(self):
                return io.StringIO("n\n")

            def __exit__(self, *_exc):
                return False

        with tempfile.TemporaryDirectory() as temporary:
            with self.assertRaises(BOOTSTRAP.InstallCancelled):
                BOOTSTRAP.install_latest(
                    Path(temporary),
                    repository="owner/repo",
                    interactive=True,
                    terminal_factory=Terminal,
                    opener=opener,
                )
        self.assertFalse(called)

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
                        err = stderr.getvalue()
                        self.assertIn("Open issue:", err)
                        self.assertIn("Agent prompt (copy the backtick block):", err)
                        self.assertIn("Continue ChaosEngine install in this folder.", err)
                        report = [
                            line for line in err.splitlines()
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
        self.assertIn("Open issue:", err)
        self.assertIn("Agent prompt (copy the backtick block):", err)
        self.assertIn("Continue ChaosEngine install in this folder.", err)
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
            self.assertIn("Installation failed", err)
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
        output = stderr.getvalue()
        self.assertIn("unhealthy: memory", output)
        self.assertIn("Next fix: paste the heal prompt", output)
        self.assertIn("Open issue:", output)
        self.assertIn("Agent prompt (copy the backtick block):", output)
        self.assertIn("Continue ChaosEngine install in this folder.", output)
        self.assertIn("comment findings, solutions, and troubleshooting steps", output)
        self.assertIn(
            "Ask the user whether they want to attempt a fix by opening an upstream PR",
            output,
        )
        self.assertIn("the GitHub issue URL printed above", output)
        self.assertNotIn("Repair the named", output)
        self.assertNotIn("Give this prompt", output)
        prompt_line = next(
            line for line in output.splitlines() if line.startswith("`Continue ChaosEngine")
        )
        self.assertNotIn("issues/new?", prompt_line)
        report = [
            line
            for line in output.splitlines()
            if line.startswith("https://github.com/owner/repo/issues/new?")
        ][0]
        query = urllib.parse.parse_qs(urllib.parse.urlsplit(report).query)
        self.assertEqual(["chaos-engine-installer.yml"], query["template"])
        self.assertEqual(["Verify installation"], query["failed_phase"])
        self.assertEqual(["memory"], query["unhealthy"])
        self.assertLessEqual(len(report), BOOTSTRAP.MAX_ISSUE_URL_CHARS)
        for field_id in BOOTSTRAP.REQUIRED_ISSUE_FORM_FIELDS:
            self.assertIn(field_id, query, field_id)

    def test_heal_handoff_prompt_continues_and_avoids_compose_url_blob(self):
        compose = (
            "https://github.com/owner/repo/issues/new?template=chaos-engine-installer.yml"
            "&title=long&body=" + ("x" * 200)
        )
        prompt = BOOTSTRAP.heal_handoff_prompt(
            "python3 .chaos-engine/install.py doctor --project . --json",
            compose,
        )
        self.assertTrue(prompt.startswith("Continue ChaosEngine install in this folder."))
        self.assertIn("the GitHub issue URL printed above", prompt)
        self.assertNotIn("issues/new?", prompt)
        self.assertNotIn("Repair the named", prompt)
        filed = BOOTSTRAP.heal_handoff_prompt(
            "python3 .chaos-engine/install.py doctor --project . --json",
            "https://github.com/owner/repo/issues/99",
        )
        self.assertIn("https://github.com/owner/repo/issues/99", filed)
        self.assertIn("upstream PR", filed)

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
        self.assertEqual(["failed at [path] token=<redacted>"], query["cause"])
        self.assertNotIn("super-secret", output)
        self.assertNotIn(str(private_path), output)
        self.assertNotIn("<path>", output)

    def test_one_line_cause_redacts_media_user_mounts_without_html_tags(self):
        cause = BOOTSTRAP.one_line_cause(
            RuntimeError(
                "ChaosEngine host adapter drift detected: "
                "/media/x/OS/Users/y/proj/plugins/chaos-engine/hooks/guard.py"
            )
        )
        self.assertIn("[path]", cause)
        self.assertNotIn("<path>", cause)
        self.assertNotIn("/media/x/OS", cause)
        self.assertNotIn("/Users/y", cause)
        windows = BOOTSTRAP.one_line_cause(
            RuntimeError(r"failed at C:\Users\runner\work\project\state.json")
        )
        self.assertIn("[path]", windows)
        self.assertNotIn(r"C:\Users", windows)

    def test_installer_issue_template_asks_to_attach_trace_not_private_path(self):
        template = (
            ROOT / ".github/ISSUE_TEMPLATE/chaos-engine-installer.yml"
        ).read_text(encoding="utf-8")
        lowered = template.casefold()
        self.assertIn("attach", lowered)
        self.assertIn("install-trace.json", template)
        self.assertIn(".chaos-engine-state/install-trace.json", template)
        self.assertIn("install-console.log", template)
        self.assertIn("doctor-failure.json", template)
        self.assertIn("id: os_name", template)
        self.assertIn("id: os_version", template)
        self.assertIn("id: architecture", template)
        self.assertIn("id: python_version", template)
        self.assertIn("id: machine", template)
        self.assertIn("id: console_log", template)
        self.assertIn("id: doctor_json", template)
        console_block = template.split("id: console_log", 1)[1].split("id: doctor_json", 1)[0]
        self.assertNotIn("required: true", console_block)
        self.assertNotIn("Install trace path", template)
        self.assertNotIn("/Users/", template)
        self.assertNotIn("/home/", template)

    def test_failure_prefill_uses_relative_trace_and_asks_for_attachment(self):
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary) / "project"
            (project / ".chaos-engine-state").mkdir(parents=True)
            (project / ".chaos-engine-state" / "install-trace.json").write_text(
                '{"status":"failed"}\n', encoding="utf-8"
            )
            stderr = io.StringIO()
            with unittest.mock.patch.object(BOOTSTRAP.sys, "stderr", stderr):
                BOOTSTRAP.emit_install_failure(
                    "CE-INSTALL-FAILED",
                    RuntimeError(
                        "ChaosEngine host adapter drift detected: "
                        f"{project / 'plugins/chaos-engine/hooks/guard.py'}"
                    ),
                    "owner/repo",
                    project=project,
                )
            output = stderr.getvalue()
            report = next(
                line
                for line in output.splitlines()
                if line.startswith("https://github.com/owner/repo/issues/new?")
            )
            query = urllib.parse.parse_qs(urllib.parse.urlsplit(report).query)
            self.assertEqual(
                [".chaos-engine-state/install-trace.json"], query["install_trace"]
            )
            self.assertIn("attach", output.casefold())
            self.assertNotIn(str(project), query["install_trace"][0])
            self.assertIn("[path]", query["cause"][0])

    def test_failure_prefill_includes_runtime_and_writes_full_logs(self):
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary) / "project"
            (project / ".chaos-engine-state").mkdir(parents=True)
            (project / ".chaos-engine-state" / "install-trace.json").write_text(
                '{"status":"failed"}\n', encoding="utf-8"
            )
            error = BOOTSTRAP.InstallHealthError(
                "Verify installation",
                {
                    "status": "recovery-required",
                    "commit": "a" * 40,
                    "components": {
                        "hooks": {
                            "status": "recovery-required",
                            "taskImpact": "required",
                            "detail": "managed-python-missing",
                            "code": "CE_MANAGED_PYTHON_MISSING",
                            "fixNext": "repair tools",
                        },
                        "mcps": {
                            "status": "recovery-required",
                            "taskImpact": "required",
                            "detail": "managed-python-missing",
                            "code": "CE_MANAGED_PYTHON_MISSING",
                            "fixNext": "repair tools",
                        },
                    },
                },
            )
            reporter = BOOTSTRAP.InstallReporter(stream=io.StringIO())
            reporter.traces.append((1.0, "PASS Download source (00:01)"))
            stderr = io.StringIO()
            with unittest.mock.patch.object(BOOTSTRAP.sys, "stderr", stderr):
                BOOTSTRAP.emit_install_failure(
                    "CE-INSTALL-FAILED",
                    error,
                    "owner/repo",
                    reporter=reporter,
                    project=project,
                )
            output = stderr.getvalue()
            report = next(
                line
                for line in output.splitlines()
                if line.startswith("https://github.com/owner/repo/issues/new?")
            )
            query = urllib.parse.parse_qs(urllib.parse.urlsplit(report).query)
            self.assertTrue(query.get("os_name"))
            self.assertTrue(query.get("python_version"))
            self.assertTrue(query.get("architecture"))
            self.assertTrue(query.get("machine"))
            self.assertIn("CE_MANAGED_PYTHON_MISSING", query["doctor_details"][0])
            self.assertLessEqual(len(report), BOOTSTRAP.MAX_ISSUE_URL_CHARS)
            console = project / ".chaos-engine-state/install-console.log"
            doctor = project / ".chaos-engine-state/doctor-failure.json"
            self.assertTrue(console.is_file())
            self.assertIn("PASS Download source", console.read_text(encoding="utf-8"))
            payload = json.loads(doctor.read_text(encoding="utf-8"))
            self.assertEqual(
                "CE_MANAGED_PYTHON_MISSING",
                payload["components"]["hooks"]["code"],
            )
            self.assertIn("install-console.log", output)
            self.assertIn("doctor-failure.json", output)
            for field_id in BOOTSTRAP.REQUIRED_ISSUE_FORM_FIELDS:
                self.assertIn(field_id, query, field_id)
            self.assertLessEqual(len(report), BOOTSTRAP.MAX_ISSUE_URL_CHARS)

    def test_failure_form_url_includes_every_template_field(self):
        template_ids = [
            line.split("id:", 1)[1].strip()
            for line in (
                ROOT / ".github/ISSUE_TEMPLATE/chaos-engine-installer.yml"
            ).read_text(encoding="utf-8").splitlines()
            if line.strip().startswith("id:")
        ]
        self.assertIn("console_log", template_ids)
        self.assertIn("doctor_json", template_ids)
        self.assertEqual(template_ids, list(BOOTSTRAP.ISSUE_FORM_FIELD_IDS))

    def test_github_token_files_issue_via_api_with_inlined_logs(self):
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary) / "project"
            (project / ".chaos-engine-state").mkdir(parents=True)
            (project / ".chaos-engine-state" / "install-trace.json").write_text(
                '{"status":"failed"}\n', encoding="utf-8"
            )
            error = BOOTSTRAP.InstallHealthError(
                "Verify installation",
                {
                    "status": "recovery-required",
                    "components": {
                        "hooks": {
                            "status": "recovery-required",
                            "taskImpact": "required",
                            "detail": "managed-python-missing",
                            "code": "CE_MANAGED_PYTHON_MISSING",
                        }
                    },
                },
            )
            reporter = BOOTSTRAP.InstallReporter(stream=io.StringIO())
            reporter.traces.append((1.0, "PASS Download source (00:01)"))
            posted: list[object] = []

            def opener(request, timeout=0):
                del timeout
                posted.append(request)
                payload = json.dumps(
                    {"html_url": "https://github.com/owner/repo/issues/99"}
                ).encode()
                return Response(payload)

            stderr = io.StringIO()
            with unittest.mock.patch.object(BOOTSTRAP.sys, "stderr", stderr):
                BOOTSTRAP.emit_install_failure(
                    "CE-INSTALL-FAILED",
                    error,
                    "owner/repo",
                    reporter=reporter,
                    project=project,
                    opener=opener,
                    token="ghs_test_token",
                )
            output = stderr.getvalue()
            self.assertIn("https://github.com/owner/repo/issues/99", output)
            self.assertIn("GitHub issue:", output)
            self.assertIn("Agent prompt (copy the backtick block):", output)
            self.assertIn("https://github.com/owner/repo/issues/99", output)
            self.assertIn("Continue ChaosEngine install in this folder.", output)
            self.assertFalse(
                any("issues/new?" in line for line in output.splitlines())
            )
            self.assertEqual(1, len(posted))
            body = json.loads(posted[0].data.decode())
            self.assertEqual("[ChaosEngine installer] CE-INSTALL-FAILED", body["title"])
            self.assertIn("### Error code", body["body"])
            self.assertIn("CE-INSTALL-FAILED", body["body"])
            self.assertIn("PASS Download source", body["body"])
            self.assertIn("CE_MANAGED_PYTHON_MISSING", body["body"])
            self.assertLessEqual(len(body["body"]), 65536)

    def test_github_api_failure_falls_back_to_prefilled_form_url(self):
        def opener(request, timeout=0):
            del request, timeout
            raise urllib.error.HTTPError(
                "https://api.github.com/repos/owner/repo/issues",
                401,
                "Unauthorized",
                {},
                None,
            )

        stderr = io.StringIO()
        with unittest.mock.patch.object(BOOTSTRAP.sys, "stderr", stderr):
            BOOTSTRAP.emit_install_failure(
                "CE-INSTALL-FAILED",
                RuntimeError("probe failed"),
                "owner/repo",
                opener=opener,
                token="ghs_test_token",
            )
        output = stderr.getvalue()
        report = next(
            line
            for line in output.splitlines()
            if line.startswith("https://github.com/owner/repo/issues/new?")
        )
        query = urllib.parse.parse_qs(urllib.parse.urlsplit(report).query)
        for field_id in BOOTSTRAP.REQUIRED_ISSUE_FORM_FIELDS:
            self.assertIn(field_id, query, field_id)

    def test_default_emit_does_not_post_issue_when_actions_token_present(self):
        posted: list[object] = []

        def opener(request, timeout=0):
            del timeout
            posted.append(request)
            raise AssertionError("GitHub issue POST must not run by default")

        stderr = io.StringIO()
        env = {
            **os.environ,
            "GITHUB_TOKEN": "ghs_ci_token",
            "GITHUB_ACTIONS": "true",
        }
        with unittest.mock.patch.object(BOOTSTRAP.sys, "stderr", stderr), unittest.mock.patch.dict(
            os.environ, env, clear=False
        ):
            BOOTSTRAP.emit_install_failure(
                "CE-INSTALL-FAILED",
                RuntimeError("probe failed"),
                "owner/repo",
                opener=opener,
            )
        self.assertEqual([], posted)
        self.assertIn("issues/new?", stderr.getvalue())

    def test_first_install_unhealthy_doctor_writes_heal_handoff_and_returns(self):
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary) / "project"
            project.mkdir()

            def install_core(target_project, *_args, **_kwargs):
                root = Path(target_project) / ".chaos-engine"
                root.mkdir(parents=True, exist_ok=True)
                (root / "install.py").write_text("# installer\n", encoding="utf-8")
                return root

            installer = unittest.mock.Mock()
            installer.install_with_dependencies.side_effect = install_core
            installer.doctor_with_dependencies.return_value = {
                "commit": "a" * 40,
                "status": "recovery-required",
                "components": {
                    "hooks": {"status": "recovery-required", "taskImpact": "required"},
                },
                "kernel": {"status": "healthy"},
                "hosts": {"status": "healthy"},
                "dependencies": {"status": "healthy"},
            }
            installer.load_installed_controller.return_value.activate_detected_plugins.return_value = {
                "clients": {}
            }
            stream = io.StringIO()
            reporter = BOOTSTRAP.InstallReporter(stream=stream)
            with unittest.mock.patch.object(
                BOOTSTRAP, "resolve_latest", return_value=("a" * 40, "main")
            ), unittest.mock.patch.object(
                BOOTSTRAP, "download_source", return_value=ROOT / "chaos-engine"
            ), unittest.mock.patch.object(
                BOOTSTRAP, "load_installer", return_value=installer
            ):
                result = BOOTSTRAP.install_latest(
                    project,
                    repository="owner/repo",
                    branch="main",
                    opener=unittest.mock.Mock(),
                    reporter=reporter,
                )
            self.assertEqual("heal-handoff", result["status"])
            self.assertTrue((project / ".chaos-engine-state/heal-handoff.md").is_file())
            output = stream.getvalue()
            self.assertIn("Heal handoff", output)
            self.assertIn("issues/new?", output)
            self.assertIn("Agent prompt (copy the backtick block):", output)
            self.assertIn("Continue ChaosEngine install in this folder.", output)
            self.assertNotIn("Repair the named", output)
            self.assertNotIn("Give this prompt", output)
            handoff = (project / ".chaos-engine-state/heal-handoff.md").read_text(
                encoding="utf-8"
            )
            self.assertIn("Do not rerun the install one-liner", handoff)
            self.assertIn("Continue with one agent step", handoff)
            installer.rollback.assert_not_called()

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

    def test_doctor_human_healthy_report_stays_short(self):
        document = {
            "schemaVersion": 2,
            "identity": "chaos-engine",
            "kind": "doctor",
            "status": "healthy",
            "commit": "a" * 40,
            "components": {
                "core": {"status": "healthy", "taskImpact": "required"},
                "hooks": {"status": "healthy", "taskImpact": "required"},
                "maven-tools-mcp": {"status": "absent", "taskImpact": "optional"},
            },
        }
        rendered = INSTALL.format_health_report(document)
        self.assertIn("ChaosEngine doctor: healthy", rendered)
        self.assertIn("components: 3/3 healthy", rendered)
        self.assertNotIn("fix-next", rendered)
        self.assertNotIn("[error]", rendered)
        self.assertLessEqual(len(rendered.splitlines()), 4)

    def test_doctor_human_broken_fixture_prints_fix_next(self):
        document = {
            "schemaVersion": 2,
            "identity": "chaos-engine",
            "kind": "doctor",
            "status": "recovery-required",
            "commit": "b" * 40,
            "components": {
                "core": {
                    "status": "recovery-required",
                    "taskImpact": "required",
                    "code": "CE_CORE_MISSING",
                },
                "hooks": {"status": "recovery-required", "taskImpact": "required"},
                "memory": {"status": "recovery-required", "taskImpact": "advisory"},
                "maven-tools-mcp": {"status": "absent", "taskImpact": "optional"},
            },
        }
        rendered = INSTALL.format_health_report(document)
        self.assertIn("ChaosEngine doctor: recovery-required", rendered)
        self.assertIn("components: 1/4 healthy", rendered)
        self.assertIn("[error] core", rendered)
        self.assertIn("code=CE_CORE_MISSING", rendered)
        self.assertIn("[error] hooks", rendered)
        self.assertIn("[warning] memory", rendered)
        self.assertIn("(advisory)", rendered)
        self.assertNotIn("[error] maven-tools-mcp", rendered)
        self.assertGreaterEqual(rendered.count("fix-next:"), 3)
        self.assertIn("Rerun the ChaosEngine install one-liner to restore", rendered)
        self.assertIn("Reinstall ChaosEngine hooks", rendered)

    def test_doctor_human_includes_host_onboarding_cards(self):
        healthy = {
            "schemaVersion": 2,
            "identity": INSTALL.CANONICAL_IDENTITY,
            "kind": "doctor",
            "status": "healthy",
            "commit": "e" * 40,
            "distribution": "portable",
            "policySha256": "0" * 64,
            "kernel": {"status": "healthy"},
            "hosts": {"status": "healthy"},
            "dependencies": {"status": "healthy"},
            "components": {"core": {"status": "healthy", "taskImpact": "required"}},
            "clients": {"codex": {"status": "healthy"}},
        }
        stdout = io.StringIO()
        with unittest.mock.patch.object(
            INSTALL, "status_json", return_value=healthy
        ), unittest.mock.patch.object(
            INSTALL.sys, "stdout", stdout
        ), unittest.mock.patch.object(
            INSTALL.sys,
            "argv",
            ["install.py", "doctor", "--project", "."],
        ):
            self.assertEqual(0, INSTALL.main())
        out = stdout.getvalue()
        self.assertIn("Host onboarding cards:", out)
        self.assertIn("Codex", out)
        self.assertIn("gap:", out)

    def test_doctor_cli_human_default_and_json_flag(self):
        healthy = {
            "schemaVersion": 2,
            "identity": INSTALL.CANONICAL_IDENTITY,
            "kind": "doctor",
            "status": "healthy",
            "commit": "c" * 40,
            "distribution": "portable",
            "policySha256": "0" * 64,
            "kernel": {"status": "healthy"},
            "hosts": {"status": "healthy"},
            "dependencies": {"status": "healthy"},
            "components": {"core": {"status": "healthy", "taskImpact": "required"}},
            "clients": {},
        }
        broken = {
            **healthy,
            "status": "recovery-required",
            "components": {
                "hooks": {"status": "recovery-required", "taskImpact": "required"},
            },
        }
        stdout = io.StringIO()
        stderr = io.StringIO()
        with unittest.mock.patch.object(
            INSTALL, "status_json", return_value=healthy
        ), unittest.mock.patch.object(
            INSTALL.sys, "stdout", stdout
        ), unittest.mock.patch.object(
            INSTALL.sys, "stderr", stderr
        ), unittest.mock.patch.object(
            INSTALL.sys,
            "argv",
            ["install.py", "doctor", "--project", "."],
        ):
            self.assertEqual(0, INSTALL.main())
        human = stdout.getvalue()
        self.assertIn("ChaosEngine doctor: healthy", human)
        self.assertNotIn('"schemaVersion"', human)
        self.assertEqual("", stderr.getvalue())

        stdout = io.StringIO()
        with unittest.mock.patch.object(
            INSTALL, "status_json", return_value=broken
        ), unittest.mock.patch.object(
            INSTALL.sys, "stdout", stdout
        ), unittest.mock.patch.object(
            INSTALL.sys,
            "argv",
            ["install.py", "doctor", "--project", "."],
        ):
            self.assertEqual(0, INSTALL.main())
        failing = stdout.getvalue()
        self.assertIn("fix-next:", failing)
        self.assertIn("[error] hooks", failing)

        stdout = io.StringIO()
        with unittest.mock.patch.object(
            INSTALL, "status_json", return_value=healthy
        ), unittest.mock.patch.object(
            INSTALL.sys, "stdout", stdout
        ), unittest.mock.patch.object(
            INSTALL.sys,
            "argv",
            ["install.py", "doctor", "--project", ".", "--json"],
        ):
            self.assertEqual(0, INSTALL.main())
        payload = json.loads(stdout.getvalue())
        self.assertEqual("doctor", payload["kind"])
        self.assertEqual("healthy", payload["status"])

    def test_host_onboarding_cards_cover_five_hosts_with_paths_and_gaps(self):
        rendered = BOOTSTRAP.format_host_onboarding_cards(
            detected=[("claude", "Claude Code", True), ("codex", "Codex", False),
                      ("grok", "Grok", False), ("gemini", "Gemini", False),
                      ("copilot", "GitHub Copilot", True)],
            activated={"claude": {"status": "healthy"}},
        )
        self.assertIn("Host onboarding cards:", rendered)
        self.assertIn("Hosts", rendered)
        for label in ("Claude Code", "Codex", "Grok", "Gemini", "GitHub Copilot"):
            self.assertIn(label, rendered)
        self.assertIn("marketplace/plugin", rendered)
        self.assertIn("file/hook", rendered)
        self.assertIn("activated", rendered)
        self.assertIn("not on PATH", rendered)
        self.assertIn("IDE signal", rendered)
        self.assertIn("gap:", rendered)
        self.assertEqual(1, rendered.count("how:"))
        self.assertEqual(1, rendered.count("gap:"))
        # Actionable host is first detected-not-activated (copilot), not activated Claude.
        self.assertGreater(rendered.index("how:"), rendered.index("GitHub Copilot"))

    def test_success_cta_includes_host_onboarding_cards(self):
        stream = io.StringIO()
        reporter = BOOTSTRAP.InstallReporter(stream=stream)
        with unittest.mock.patch.object(
            BOOTSTRAP,
            "detect_install_hosts",
            return_value=[
                ("claude", "Claude Code", True),
                ("codex", "Codex", False),
                ("grok", "Grok", False),
                ("gemini", "Gemini", False),
                ("copilot", "GitHub Copilot", False),
            ],
        ):
            reporter.success(
                Path("/project"),
                {"commit": "d" * 40, "status": "healthy", "components": {}},
                {"claude": {"status": "healthy"}},
                repository="owner/repo",
            )
        output = stream.getvalue()
        self.assertIn("Host onboarding cards:", output)
        self.assertIn("marketplace/plugin", output)
        self.assertIn("file/hook", output)
        self.assertEqual(1, output.count("how:"))
        self.assertEqual(1, output.count("gap:"))

    def test_first_session_brief_lists_landed_untracked_and_three_next_actions(self):
        with_clients = BOOTSTRAP.format_first_session_brief(
            clients={"codex": {"status": "healthy"}}
        )
        self.assertIn("First-session brief:", with_clients)
        self.assertIn("To get started:", with_clients)
        self.assertIn("Open one activated host (codex)", with_clients)
        self.assertIn("chaos-engine", with_clients)
        self.assertIn("sample task", with_clients)
        generic = BOOTSTRAP.format_first_session_brief(clients={})
        self.assertIn("Open any supported host", generic)
        self.assertLessEqual(len(with_clients.splitlines()), 8)
        landed = "\n".join(BOOTSTRAP.format_landed_untracked_lines())
        self.assertIn("Landed:", landed)
        self.assertIn("Untracked:", landed)

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
