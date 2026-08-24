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
        self.assertTrue(output.startswith("  /\\  CHAOSENGINE // AUTONOMOUS INSTALL\n"))
        self.assertIn("START Resolve source", output)
        self.assertIn("DONE  Resolve source", output)
        self.assertNotIn("\r", output)
        self.assertNotIn("\x1b", output)

    def test_reporter_uses_fixed_height_checklist_for_tty(self):
        class Tty(io.StringIO):
            def isatty(self):
                return True

        stream = Tty()
        with unittest.mock.patch.dict(os.environ, {"TERM": "xterm"}):
            reporter = BOOTSTRAP.InstallReporter(stream=stream, clock=lambda: 1.0)
            try:
                reporter.start(
                    "Download source", remaining=("Install core",),
                    detail="https://example.invalid/source",
                )
                output = stream.getvalue()
                self.assertIn("CHAOSENGINE // AUTONOMOUS INSTALL", output)
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
        self.assertTrue(all(len(line) <= 38 for line in output.splitlines()))

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
        self.assertTrue(stderr.getvalue().startswith("  /\\  CHAOSENGINE // AUTONOMOUS INSTALL\n"))

    def test_wrappers_expose_and_forward_interactive_mode(self):
        shell = (ROOT / "chaos-engine/install.sh").read_text(encoding="utf-8")
        powershell = (ROOT / "chaos-engine/install.ps1").read_text(encoding="utf-8")
        self.assertIn('"--interactive"', shell)
        self.assertIn("[switch]$Interactive", powershell)
        self.assertNotIn("CHAOS_ENGINE_INTERACTIVE", powershell)
        self.assertIn('arguments += "--interactive"', powershell)

    def test_main_emits_stable_actionable_error_codes(self):
        cases = (
            (ValueError("ChaosEngine Claude marketplace collision"), "CE-CLAUDE-MARKETPLACE-CONFLICT"),
            (RuntimeError("interactive mode requires a usable controlling terminal"), "CE-INTERACTIVE-TERMINAL"),
            (RuntimeError("network broke"), "CE-INSTALL-FAILED"),
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
                self.assertIn(str(error), stderr.getvalue())
                self.assertIn(code, stderr.getvalue())
                self.assertIn("#installer-errors", stderr.getvalue())

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
