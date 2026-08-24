from __future__ import annotations

import importlib.util
import inspect
import io
import json
import os
import tempfile
import threading
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
        self.assertIn("QUANTUM MANDATE", output)
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
        with unittest.mock.patch.dict(os.environ, environment, clear=True):
            reporter = BOOTSTRAP.InstallReporter(stream=stream, clock=lambda: 1.0)
            try:
                reporter.start(
                    "Download source", remaining=("Install core",),
                    detail="https://example.invalid/source",
                )
                output = stream.getvalue()
                self.assertNotIn("transparent automation", output)
                self.assertNotIn("AUTONOMOUS INSTALL", output)
                self.assertIn("QUANTUM MANDATE", output)
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
        self.assertIn("QUANTUM MANDATE", output)
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
        self.assertIn("QUANTUM MANDATE", output)

    def test_eta_does_not_grow_while_current_stage_overruns(self):
        class Tty(io.StringIO):
            def isatty(self):
                return True

        class Clock:
            def __init__(self):
                self.now = 0.0

            def __call__(self):
                return self.now

        def eta_seconds(text: str) -> int:
            line = [item for item in text.splitlines() if "ETA " in item][-1]
            stamp = line.split("ETA ", 1)[1].split("\x1b", 1)[0].strip()
            minutes, seconds = stamp.split(":")
            return int(minutes) * 60 + int(seconds)

        clock = Clock()
        stream = Tty()
        with unittest.mock.patch.dict(os.environ, {"TERM": "xterm"}), unittest.mock.patch.object(
            BOOTSTRAP.threading.Thread, "start", lambda self: None
        ):
            reporter = BOOTSTRAP.InstallReporter(stream=stream, clock=clock)
            try:
                reporter.start("Resolve source", remaining=("Download source", "Install core"))
                clock.now = 2.0
                reporter.complete("Resolve source", remaining=("Download source", "Install core"))
                reporter.start("Download source", remaining=("Install core",))
                previous = None
                completed_weight = BOOTSTRAP.STAGE_WEIGHTS["Resolve source"]
                remaining_weight = BOOTSTRAP.STAGE_WEIGHTS["Install core"]
                for tick in (5.0, 10.0, 20.0):
                    clock.now = tick
                    reporter._render_locked()
                    current = eta_seconds(stream.getvalue())
                    if previous is None:
                        self.assertGreater(current, 0)
                    else:
                        self.assertLessEqual(
                            current, previous, f"ETA grew {previous} -> {current} at t={tick}"
                        )
                    previous = current
                    old = tick * remaining_weight / completed_weight
                    self.assertGreater(old, current)
            finally:
                reporter._stop.set()
                reporter._thread = None

    def test_nested_start_keeps_inflight_stage_in_remaining_weight(self):
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
        future = sum(
            BOOTSTRAP.STAGE_WEIGHTS.get(item, 1)
            for item in reporter.remaining_operations
        )
        inflight = sum(
            BOOTSTRAP.STAGE_WEIGHTS.get(item, 1)
            for item in reporter._in_flight
            if item != reporter.current_operation
        )
        self.assertEqual(BOOTSTRAP.STAGE_WEIGHTS["Install core"], inflight)
        self.assertGreaterEqual(
            future + inflight,
            BOOTSTRAP.STAGE_WEIGHTS["Install core"] + BOOTSTRAP.STAGE_WEIGHTS["Verify installation"],
        )
        rate = 4.0 / BOOTSTRAP.STAGE_WEIGHTS["Resolve source"]
        with_core = rate * (
            BOOTSTRAP.STAGE_WEIGHTS["Provision dependencies"]
            + BOOTSTRAP.STAGE_WEIGHTS["Install core"]
            + BOOTSTRAP.STAGE_WEIGHTS["Verify installation"]
        )
        without_core = rate * (
            BOOTSTRAP.STAGE_WEIGHTS["Provision dependencies"]
            + BOOTSTRAP.STAGE_WEIGHTS["Verify installation"]
        )
        eta = reporter._eta(clock.now)
        self.assertEqual(BOOTSTRAP.InstallReporter._duration(reporter, with_core), eta)
        self.assertNotEqual(BOOTSTRAP.InstallReporter._duration(reporter, without_core), eta)

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
        self.assertIn("QUANTUM MANDATE", stderr.getvalue())

    def test_wrappers_expose_and_forward_interactive_mode(self):
        shell = (ROOT / "chaos-engine/install.sh").read_text(encoding="utf-8")
        powershell = (ROOT / "chaos-engine/install.ps1").read_text(encoding="utf-8")
        self.assertIn('"--interactive"', shell)
        self.assertIn("[switch]$Interactive", powershell)
        self.assertNotIn("CHAOS_ENGINE_INTERACTIVE", powershell)
        self.assertIn('arguments += "--interactive"', powershell)

    def test_wrappers_print_the_same_quantum_mandate_mark(self):
        shell = (ROOT / "chaos-engine/install.sh").read_text(encoding="utf-8")
        powershell = (ROOT / "chaos-engine/install.ps1").read_text(encoding="utf-8")
        for document in (shell, powershell):
            self.assertNotIn("transparent automation", document)
            self.assertNotIn("AUTONOMOUS INSTALL", document)
            self.assertIn("QUANTUM MANDATE", document)
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
                if code == "CE-INSTALL-FAILED":
                    self.assertIn("https://github.com/owner/repo/issues/new", stderr.getvalue())

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

    def test_pr_gate_runs_fresh_installer_on_exact_three_os_matrix(self):
        workflow = (ROOT / ".github/workflows/pr-gate.yml").read_text(encoding="utf-8")
        self.assertIn("chaos_installer: ${{ steps.filter.outputs.chaos_installer }}", workflow)
        self.assertIn("chaos-engine/bootstrap.py", workflow)
        self.assertIn("chaos-engine/hosts.py", workflow)
        self.assertIn("tests/scripts/test_chaos_engine_installer_ux.py", workflow)
        block = workflow[workflow.index("  chaos-installer-acceptance:"):workflow.index("  summary:")]
        self.assertIn("needs.changes.outputs.chaos_installer == 'true'", block)
        self.assertIn("os: [ubuntu-22.04, macos-15, windows-2025]", block)
        self.assertIn("scripts/ci/chaos_engine_live_installer_acceptance.py", block)
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
