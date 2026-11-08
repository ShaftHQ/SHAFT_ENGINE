"""Selected-host completion for the ChaosEngine installer (#7814–#7818)."""

from __future__ import annotations

import ast
import importlib.util
import io
import json
import os
import sys
import tempfile
import unittest
import unittest.mock as mock
from pathlib import Path


ROOT = Path(__file__).resolve().parents[2]
CE = ROOT / "chaos-engine"
HOST_IDS = (
    "claude",
    "codex",
    "grok",
    "gemini",
    "copilot",
    "opencode",
    "cursor",
    "grok-bot",
)
IMPLICIT_ENV = (
    "CHAOS_ENGINE_HOST",
    "CLAUDE_CODE_ENTRYPOINT",
    "CLAUDE_PLUGIN_ROOT",
    "CODEX_HOME",
    "GROK_HOOK_EVENT",
    "CURSOR_AGENT",
    "CURSOR_CONVERSATION_ID",
)
NOT_IMPLICIT_ENV = (
    "GROK_SESSION_ID",
    "GEMINI_HOME",
    "GEMINI_PROJECT_DIR",
    "COPILOT_HOME",
)


def load(name: str, path: Path):
    spec = importlib.util.spec_from_file_location(name, path)
    if spec is None or spec.loader is None:
        raise RuntimeError(f"unable to load {path}")
    module = importlib.util.module_from_spec(spec)
    sys.modules[name] = module
    spec.loader.exec_module(module)
    return module


def quiet_host_env() -> dict[str, str]:
    """Process env with every install-host signal removed."""
    env = os.environ.copy()
    for key in (*IMPLICIT_ENV, *NOT_IMPLICIT_ENV, "CHAOS_ENGINE_WITH_DEJA", "CHAOS_ENGINE_ADDONS"):
        env.pop(key, None)
    return env


class MatrixContractTests(unittest.TestCase):
    def test_gap_exit2_and_instruction_only_rows_stay_bound(self):
        matrix = (CE / "references/host-parity-matrix.md").read_text(encoding="utf-8")
        self.assertIn("GAP-EXIT2", matrix)
        self.assertIn("hosts.INSTRUCTION_ONLY_HOSTS", matrix)
        for gap in ("GAP-OPENCODE-HOOKS", "GAP-CURSOR-HOOKS", "GAP-GROKBOT-HOOKS"):
            self.assertIn(gap, matrix)
        kernel = load("ce_kernel_7814", CE / "hooks/kernel.py")
        self.assertFalse(kernel.HOST_CAPABILITIES["grok"].process_exit2_honored)
        self.assertFalse(kernel.HOST_CAPABILITIES["copilot"].process_exit2_honored)
        for host in ("opencode", "cursor", "grok-bot"):
            self.assertNotIn(host, kernel.HOST_CAPABILITIES)
        hosts = load("ce_hosts_7814", CE / "hosts.py")
        self.assertEqual(
            {
                "opencode": "GAP-OPENCODE-HOOKS",
                "cursor": "GAP-CURSOR-HOOKS",
                "grok-bot": "GAP-GROKBOT-HOOKS",
                "independent-bot": "GAP-BOT-ENTRY",
            },
            {host: row["gap"] for host, row in hosts.INSTRUCTION_ONLY_HOSTS.items()},
        )
        self.assertEqual(
            "chaos-engine/references/bot-entry.md",
            hosts.INSTRUCTION_ONLY_HOSTS["grok-bot"]["instructions"],
        )


class ResolverTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.bootstrap = load("ce_bootstrap_7815", CE / "bootstrap.py")

    def resolve(self, explicit=None, **env):
        return self.bootstrap.resolve_install_hosts(explicit=explicit, environ=env)

    def test_each_implicit_signal_selects_one_client(self):
        cases = {
            "CLAUDE_CODE_ENTRYPOINT": "claude",
            "CLAUDE_PLUGIN_ROOT": "claude",
            "CODEX_HOME": "codex",
            "GROK_HOOK_EVENT": "grok",
            "CURSOR_AGENT": "cursor",
            "CURSOR_CONVERSATION_ID": "cursor",
        }
        for key, host in cases.items():
            with self.subTest(signal=key):
                found = self.resolve(**{key: "/signal"})
                self.assertEqual((host,), found.hosts)
                self.assertFalse(found.prompt)

    def test_two_signals_for_one_client_are_one_client(self):
        claude = self.resolve(CLAUDE_CODE_ENTRYPOINT="1", CLAUDE_PLUGIN_ROOT="2")
        cursor = self.resolve(CURSOR_AGENT="1", CURSOR_CONVERSATION_ID="2")
        self.assertEqual((("claude",), False), (claude.hosts, claude.prompt))
        self.assertEqual((("cursor",), False), (cursor.hosts, cursor.prompt))

    def test_two_clients_and_unknown_ids_fail_closed(self):
        with self.assertRaisesRegex(self.bootstrap.InstallHostError, "claude and codex"):
            self.resolve(CLAUDE_PLUGIN_ROOT="/claude", CODEX_HOME="/codex")
        with self.assertRaisesRegex(self.bootstrap.InstallHostError, "choose from: " + ", ".join(HOST_IDS)):
            self.bootstrap.parse_install_hosts("not-a-host")
        with self.assertRaisesRegex(self.bootstrap.InstallHostError, "conflicts with --hosts"):
            self.resolve("grok", CHAOS_ENGINE_HOST="codex")

    def test_explicit_host_wins_and_non_signals_do_not_select(self):
        chosen = self.resolve("grok", CHAOS_ENGINE_HOST="grok", CODEX_HOME="/codex")
        self.assertEqual(("grok",), chosen.hosts)
        self.assertFalse(chosen.prompt)
        ordered = self.bootstrap.parse_install_hosts("grok-bot, claude, CURSOR")
        self.assertEqual(("claude", "cursor", "grok-bot"), ordered)
        absent = self.resolve(
            GROK_SESSION_ID="session",
            GEMINI_HOME="/gemini",
            GEMINI_PROJECT_DIR="/project",
            COPILOT_HOME="/copilot",
            CODEX_HOME="  ",
            CLAUDE_CODE_ENTRYPOINT="",
        )
        self.assertEqual((), absent.hosts)
        self.assertTrue(absent.prompt)
        source = Path(self.bootstrap.resolve_install_hosts.__code__.co_filename)
        body = source.read_text(encoding="utf-8")
        start = body.index("def resolve_install_hosts")
        end = body.index("\ndef prechecked_completion_hosts", start)
        calls = [
            node.func.attr if isinstance(node.func, ast.Attribute) else getattr(node.func, "id", "")
            for node in ast.walk(ast.parse(body[start:end]))
            if isinstance(node, ast.Call)
        ]
        self.assertNotIn("detect_host", calls)

    def test_unknown_host_fails_before_terminal_or_network(self):
        opened = []

        def factory():
            opened.append("tty")
            raise AssertionError("tty")

        def opener(*_args, **_kwargs):
            raise AssertionError("network")

        with tempfile.TemporaryDirectory() as temporary, mock.patch.dict(os.environ, quiet_host_env(), clear=True):
            project = Path(temporary)
            with self.assertRaises(self.bootstrap.InstallHostError):
                self.bootstrap.install_latest(
                    project,
                    repository="Example/Project",
                    hosts="not-a-host",
                    terminal_factory=factory,
                    opener=opener,
                )
            self.assertEqual([], opened)
            self.assertFalse((project / ".chaos-engine-state/host-completion.json").exists())
            self.assertFalse((project / ".cursor/hooks.json").exists())
            self.assertFalse((project / ".opencode/plugins/chaos-engine-receipt.js").exists())


class InteractiveSelectionTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.bootstrap = load("ce_bootstrap_7816", CE / "bootstrap.py")

    def test_precheck_is_only_the_four_clis(self):
        def which(name):
            return f"/bin/{name}" if name in {"claude", "codex", "grok", "gemini", "gh", "code", "cursor"} else None

        self.assertEqual(
            ("claude", "codex", "grok", "gemini"),
            self.bootstrap.prechecked_completion_hosts(which=which),
        )
        output = io.StringIO()
        selected = self.bootstrap.ask_completion_hosts(
            input_stream=io.StringIO("\n"), output=output, which=which
        )
        self.assertEqual(("claude", "codex", "grok", "gemini"), selected)
        self.assertIn("Pre-checked (empty answer selects these only):", output.getvalue())
        rejected = io.StringIO()
        with self.assertRaises(self.bootstrap.InstallHostError):
            self.bootstrap.ask_completion_hosts(
                input_stream=io.StringIO("not-a-host\n"), output=rejected, which=lambda _name: None
            )
        self.assertIn("Unknown client. Choose from: " + ", ".join(HOST_IDS), rejected.getvalue())

    def test_interactive_asks_once_and_explicit_hosts_skip_the_question(self):
        self._wizard("grok\ny\ny\n", hosts=None, expected=("grok",), question=True)
        self._wizard("y\ny\n", hosts="codex", expected=("codex",), question=False)

    def test_default_install_does_not_open_the_terminal(self):
        opened = []
        captured = {}

        def factory():
            opened.append("tty")
            raise AssertionError("tty")

        def opener(*_args, **_kwargs):
            raise AssertionError("network")

        def core(*_args, **kwargs):
            captured["completion_targets"] = kwargs.get("completion_targets", "missing")
            raise RuntimeError("halt")

        with tempfile.TemporaryDirectory() as temporary, mock.patch.dict(os.environ, quiet_host_env(), clear=True):
            with mock.patch.object(self.bootstrap, "_run_core_install", side_effect=core):
                with self.assertRaisesRegex(RuntimeError, "halt"):
                    self.bootstrap.install_latest(
                        Path(temporary),
                        repository="Example/Project",
                        interactive=False,
                        terminal_factory=factory,
                        opener=opener,
                    )
        self.assertEqual([], opened)
        self.assertIsNone(captured["completion_targets"])

    def test_empty_interactive_answer_without_clis_passes_none(self):
        self.assertIsNone(self._interactive_targets(set()))

    def test_empty_interactive_answer_with_four_clis_passes_only_those(self):
        self.assertEqual(
            ("claude", "codex", "grok", "gemini"),
            self._interactive_targets({"claude", "codex", "grok", "gemini", "gh", "code", "cursor"}),
        )

    def test_install_docs_and_wrappers_keep_the_question_off_the_one_liner(self):
        shell = (CE / "install.sh").read_text(encoding="utf-8")
        powershell = (CE / "install.ps1").read_text(encoding="utf-8")
        docs = (CE / "INSTALL.md").read_text(encoding="utf-8")
        interactive_if = shell.index('if [ -n "$interactive" ]; then')
        self.assertLess(interactive_if, shell.index("controlling_tty"))
        self.assertIn("pending_hosts", shell)
        self.assertIn('fail "--hosts requires a client id"', shell)
        self.assertNotIn("Usage:", shell)
        self.assertIn("[string]$Hosts", powershell)
        self.assertIn('arguments += @("--hosts", $Hosts)', powershell)
        self.assertIn("[Console]::IsInputRedirected", powershell)
        self.assertIn("Host completion is separate", docs)
        self.assertIn("does not open `/dev/tty` or `CONIN$`", docs)
        self.assertIn("design-skills", docs)
        self.assertIn("--with-mcp", docs)

    def _wizard(self, answers: str, *, hosts: str | None, expected: tuple[str, ...], question: bool):
        captured = {}

        class Terminal:
            def __enter__(self):
                return io.StringIO(answers)

            def __exit__(self, *_exc):
                return False

        def core(_project, scratch, **kwargs):
            captured["targets"] = kwargs["completion_targets"]
            scratch["temporary"] = mock.Mock()
            return Path("/unused"), "1" * 40, mock.Mock()

        stream = io.StringIO()
        with tempfile.TemporaryDirectory() as temporary, mock.patch.dict(os.environ, quiet_host_env(), clear=True):
            with mock.patch.object(self.bootstrap, "_run_core_install", side_effect=core):
                self.bootstrap.install_latest(
                    Path(temporary),
                    repository="Example/Project",
                    interactive=True,
                    skip_tools=True,
                    hosts=hosts,
                    reporter=self.bootstrap.InstallReporter(stream=stream),
                    terminal_factory=Terminal,
                    opener=lambda *_args, **_kwargs: (_ for _ in ()).throw(AssertionError("network")),
                )
        output = stream.getvalue()
        self.assertEqual(expected, captured["targets"])
        self.assertEqual(1 if question else 0, output.count(self.bootstrap.CLIENT_QUESTION))
        self.assertEqual(1, output.count("Confirm Install companions (Caveman + Ponytail) with the core?"))
        self.assertEqual(1, output.count("Confirm Continue ChaosEngine install?"))

    def _interactive_targets(self, found: set[str]):
        """Empty answer through install_latest. which() is the patched bootstrap.shutil.which."""
        captured = {}

        def which(name, *_args, **_kwargs):
            return f"/usr/bin/{name}" if name in found else None

        def core(_project, scratch, **kwargs):
            captured["completion_targets"] = kwargs["completion_targets"]
            scratch["temporary"] = mock.Mock()
            return Path("/unused"), "1" * 40, mock.Mock()

        class Terminal:
            def __enter__(self):
                return io.StringIO("\ny\ny\n")

            def __exit__(self, *_exc):
                return False

        wizard = self.bootstrap.run_first_run_wizard
        defaults = wizard.__kwdefaults__
        previous = defaults["which"]
        # which= is bound at import. Point it at bootstrap.shutil.which so the patch is visible.
        defaults["which"] = lambda name, *args, **kwargs: self.bootstrap.shutil.which(name, *args, **kwargs)
        try:
            with tempfile.TemporaryDirectory() as temporary, mock.patch.dict(os.environ, quiet_host_env(), clear=True):
                with mock.patch.object(self.bootstrap.shutil, "which", side_effect=which):
                    with mock.patch.object(self.bootstrap, "_run_core_install", side_effect=core):
                        self.bootstrap.install_latest(
                            Path(temporary),
                            repository="Example/Project",
                            interactive=True,
                            skip_tools=True,
                            reporter=self.bootstrap.InstallReporter(stream=io.StringIO()),
                            terminal_factory=Terminal,
                            opener=lambda *_args, **_kwargs: (_ for _ in ()).throw(AssertionError("network")),
                        )
        finally:
            defaults["which"] = previous
        return captured["completion_targets"]


class ActivationTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.install = load("ce_install_7817", CE / "install.py")
        cls.shim = load("ce_receipt_7817", CE / "hooks/receipt_shim.py")
        cls.bootstrap = load("ce_bootstrap_7817", CE / "bootstrap.py")

    def test_cursor_and_opencode_use_the_existing_shim_commands(self):
        with tempfile.TemporaryDirectory() as temporary:
            cursor = Path(temporary) / "cursor"
            cursor.mkdir()
            self.install.apply_install_host_completion(cursor, ("cursor",))
            files = sorted(path.relative_to(cursor).as_posix() for path in cursor.rglob("*") if path.is_file())
            self.assertEqual([".chaos-engine-state/host-completion.json", ".cursor/hooks.json"], files)
            document = json.loads((cursor / ".cursor/hooks.json").read_text(encoding="utf-8"))
            for event in ("beforeReadFile", "beforeShellExecution"):
                commands = [entry["command"] for entry in document["hooks"][event]]
                self.assertTrue(any("receipt_shim.py ensure --host cursor" in command for command in commands))
            opencode = Path(temporary) / "opencode"
            opencode.mkdir()
            self.install.apply_install_host_completion(opencode, ("opencode",))
            plugin = (opencode / ".opencode/plugins/chaos-engine-receipt.js").read_text(encoding="utf-8")
            self.assertIn("receipt_shim.py", plugin)
            self.assertIn("ensure", plugin)
            self.assertIn("opencode", plugin)
            self.assertFalse((opencode / ".cursor/hooks.json").exists())

    def test_grok_bot_install_returns_2_and_writes_no_shim(self):
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary)
            previous = os.getcwd()
            os.chdir(project)
            try:
                code = self.shim.main(["install", "--host", "grok-bot"])
            finally:
                os.chdir(previous)
            self.assertEqual(2, code)
            self.assertEqual([], list(project.iterdir()))
            selected = Path(temporary) / "selected"
            selected.mkdir()

            def which(name):
                raise AssertionError(f"which({name}) must not choose a shim")

            with mock.patch.object(self.install.shutil, "which", side_effect=which):
                self.install.apply_install_host_completion(selected, ("grok-bot",))
            self.assertFalse((selected / ".cursor/hooks.json").exists())
            self.assertFalse((selected / ".opencode/plugins/chaos-engine-receipt.js").exists())
            self.assertEqual(("grok-bot",), self.install.read_completion_targets(selected))
            card = self.bootstrap.format_host_onboarding_cards(targets=("grok-bot",))
            self.assertIn("chaos-engine/references/bot-entry.md", card)
            self.assertIn("GAP-GROKBOT-HOOKS", card)
            self.assertNotIn("Grok Bot", self.bootstrap.format_host_onboarding_cards())

    def test_unattended_keeps_path_shims_and_a_selection_does_not(self):
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary) / "unattended"
            project.mkdir()
            self._drive_core_install(project, None)
            self.assertTrue((project / ".cursor/hooks.json").is_file())
            self.assertTrue((project / ".opencode/plugins/chaos-engine-receipt.js").is_file())
            self.assertFalse((project / ".chaos-engine-state/host-completion.json").exists())
            self.assertEqual((), self.install.read_completion_targets(project))
            chosen = Path(temporary) / "chosen"
            chosen.mkdir()
            self._drive_core_install(chosen, ("grok",))
            self.assertFalse((chosen / ".cursor/hooks.json").exists())
            self.assertFalse((chosen / ".opencode/plugins/chaos-engine-receipt.js").exists())
            self.assertEqual(("grok",), self.install.read_completion_targets(chosen))

    def _drive_core_install(self, project: Path, targets) -> None:
        """Real _run_core_install. Download and installer load are patched; shim methods are not."""
        called = {"install": False}

        def which(name):
            return f"/usr/bin/{name}" if name in {"cursor", "cursor-agent", "opencode"} else None

        def install_with_dependencies(installed, _source, _commit, **_kwargs):
            called["install"] = True
            self.install.wire_receipt_shims(installed, which=which)
            self.install.record_completion_targets(installed, ("stale",))
            return installed / ".chaos-engine"

        installer = type("Installer", (), {})()
        installer.install_with_dependencies = install_with_dependencies
        installer.clear_completion_targets = self.install.clear_completion_targets
        installer.unwire_receipt_shims = self.install.unwire_receipt_shims
        installer.apply_install_host_completion = self.install.apply_install_host_completion
        scratch: dict = {"temporary": None}
        try:
            with mock.patch.object(self.bootstrap, "resolve_latest", return_value=("d" * 40, "main")):
                with mock.patch.object(self.bootstrap, "download_source", return_value=CE):
                    with mock.patch.object(self.bootstrap, "load_installer", return_value=installer):
                        self.bootstrap._run_core_install(
                            project,
                            scratch,
                            repository="Example/Project",
                            branch=None,
                            skip_tools=False,
                            with_maven_tools=False,
                            maven_tools_mode="native",
                            distribution="portable",
                            opener=lambda *_args, **_kwargs: (_ for _ in ()).throw(AssertionError("network")),
                            provisioner=None,
                            reporter=self.bootstrap.InstallReporter(stream=io.StringIO()),
                            bundle_options=None,
                            addons_requested=None,
                            addons_removed=None,
                            confirm=lambda _name: None,
                            remaining=lambda _name: (),
                            completion_targets=targets,
                        )
        finally:
            temporary = scratch["temporary"]
            if temporary is not None:
                temporary.cleanup()
        self.assertTrue(called["install"])

    def test_install_command_reconciles_locked_shims(self):
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary) / "unattended"
            project.mkdir()
            self._run_locked_install(project, hosts=None)
            self.assertTrue((project / ".cursor/hooks.json").is_file())
            self.assertTrue((project / ".opencode/plugins/chaos-engine-receipt.js").is_file())
            self.assertFalse((project / ".chaos-engine-state/host-completion.json").exists())
            chosen = Path(temporary) / "grok-bot"
            chosen.mkdir()
            self._run_locked_install(chosen, hosts=("grok-bot",))
            self.assertFalse((chosen / ".cursor/hooks.json").exists())
            self.assertFalse((chosen / ".opencode/plugins/chaos-engine-receipt.js").exists())
            self.assertEqual(("grok-bot",), self.install.read_completion_targets(chosen))

    def test_skip_tools_apply_wires_only_the_selected_shims(self):
        with tempfile.TemporaryDirectory() as temporary:
            both = Path(temporary) / "both"
            both.mkdir()
            self._run_locked_install(both, hosts=("opencode", "cursor"), skip_tools=True)
            self.assertTrue((both / ".cursor/hooks.json").is_file())
            self.assertTrue((both / ".opencode/plugins/chaos-engine-receipt.js").is_file())
            self.assertEqual(("opencode", "cursor"), self.install.read_completion_targets(both))
            bot = Path(temporary) / "bot"
            bot.mkdir()
            self._run_locked_install(bot, hosts=("grok-bot",), skip_tools=True)
            self.assertFalse((bot / ".cursor/hooks.json").exists())
            self.assertFalse((bot / ".opencode/plugins/chaos-engine-receipt.js").exists())
            self.assertEqual(("grok-bot",), self.install.read_completion_targets(bot))

    def _run_locked_install(self, project: Path, *, hosts: tuple[str, ...] | None, skip_tools: bool = False) -> None:
        """Real _run_install_command. install_with_dependencies is stubbed and writes the shims."""

        def which(name):
            return f"/usr/bin/{name}" if name in {"cursor", "cursor-agent", "opencode"} else None

        def install_with_dependencies(installed, _source, _commit, **_kwargs):
            self.install.wire_receipt_shims(installed, which=which)
            self.install.record_completion_targets(installed, ("stale",))
            return installed / ".chaos-engine"

        def install(installed, _source, _commit, **_kwargs):
            return installed / ".chaos-engine"

        command = [
            "install",
            "--project",
            str(project),
            "--source",
            str(CE),
            "--commit",
            "c" * 40,
            "--skip-prerequisites",
        ]
        if skip_tools:
            command.append("--skip-tools")
        if hosts:
            command.extend(("--hosts", ",".join(hosts)))
        args = self.install.parser().parse_args(command)
        with mock.patch.dict(os.environ, quiet_host_env(), clear=True):
            with mock.patch.object(self.install, "install_with_dependencies", side_effect=install_with_dependencies):
                with mock.patch.object(self.install, "install", side_effect=install):
                    with mock.patch.object(self.install, "load_installed_controller", return_value=object()):
                        with mock.patch.object(self.install, "post_install_housekeeping", return_value=None):
                            self.install._run_install_command(args, set(), set())

    def test_doctor_summary_names_completion_targets(self):
        plain = self.install.format_health_report({"status": "healthy", "commit": "a" * 40})
        report = self.install.with_completion_target_line(plain, ["grok", "cursor"])
        self.assertIn("completion targets: grok, cursor", report)
        self.assertNotIn("completion targets:", plain)
        self.assertNotIn("completionTargets", self.install._DIAGNOSTIC_FIELDS["doctor"])

    def test_claude_activation_requires_the_cli_and_grok_does_not_force_lean(self):
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary)
            controller = mock.Mock()
            controller.activate_detected_plugins.return_value = {"clients": {"claude": "activated"}}

            def which(name):
                return "/bin/claude" if name == "claude" else None

            activated = self.install.activate_completion_targets(
                project, ("claude", "grok", "grok-bot"), host_controller=controller, which=which
            )
            self.assertEqual({"claude": "activated"}, activated["clients"])
            _args, kwargs = controller.activate_detected_plugins.call_args
            self.assertEqual("/bin/claude", kwargs["which"]("claude"))
            self.assertIsNone(kwargs["which"]("grok"))
            self.assertIsNone(kwargs["which"]("cursor"))
            missing = mock.Mock()
            self.assertEqual(
                {"clients": {}},
                self.install.activate_completion_targets(
                    project, ("claude",), host_controller=missing, which=lambda _name: None
                ),
            )
            missing.activate_detected_plugins.assert_not_called()

        user_config = Path.home() / ".grok" / "config.toml"
        before = user_config.read_bytes() if user_config.is_file() else None
        try:
            self._grok_selection_leaves_lean_to_the_predicate()
        finally:
            if not user_config.is_file():
                pass
            elif before is None:
                if "CHAOSENGINE-GROK" in user_config.read_text(encoding="utf-8", errors="replace"):
                    user_config.unlink()
            else:
                user_config.write_bytes(before)

    def _grok_selection_leaves_lean_to_the_predicate(self):
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary) / "project"
            project.mkdir()
            home = Path(temporary) / "grok-home"
            self.install.install(project, CE, "1" * 40)
            hosts = load("ce_hosts_7817_lean", CE / "hosts.py")
            real_load = hosts._load_grok_lean_config
            recorded: list[object] = []

            def patched_load():
                module = real_load()
                real_sync = module.sync_project_grok_lean
                module.should_apply_grok_lean = lambda _project, which=None: True

                def wrapped_sync(*args, **kwargs):
                    recorded.append(kwargs.get("lean_skills", False))
                    return real_sync(*args, **kwargs)

                module.sync_project_grok_lean = wrapped_sync
                return module

            hosts.install.__globals__["_load_grok_lean_config"] = patched_load
            with mock.patch.dict(os.environ, {"GROK_HOME": str(home)}, clear=False):
                os.environ.pop("CHAOS_ENGINE_LEAN_GROK_SKILLS", None)
                hosts.install(project, core_commit="1" * 40)
                self.install.apply_install_host_completion(project, ("grok",))
            config = home / "config.toml"
            text = config.read_text(encoding="utf-8") if config.is_file() else ""
            self.assertTrue(recorded)
            self.assertTrue(all(value is False for value in recorded))
            self.assertIn("# CHAOSENGINE-GROK-LEAN:START", text)
            self.assertNotIn("# CHAOSENGINE-GROK-SKILLS:START", text)
            for relative in ("AGENTS.md", "CLAUDE.md", "GEMINI.md", ".github/copilot-instructions.md"):
                self.assertTrue((project / relative).is_file(), relative)
            self.assertFalse((project / ".cursor/hooks.json").exists())
            self.assertFalse((project / ".opencode/plugins/chaos-engine-receipt.js").exists())
            self.assertEqual(("grok",), self.install.read_completion_targets(project))


class AddonSeparationTests(unittest.TestCase):
    def test_default_install_does_not_select_optional_packs(self):
        install = load("ce_install_7818", CE / "install.py")
        catalog = load("ce_catalog_7818", CE / "addon_catalog.py")
        prerequisites = load("ce_prerequisites_7818", CE / "prerequisites.py")
        found = catalog.discover(CE)
        self.assertEqual([], catalog.resolve(found, set(), set(), set()))
        for host in HOST_IDS:
            self.assertNotIn(host, found)
        with mock.patch.dict(os.environ, quiet_host_env(), clear=True):
            options = install.bundle_from_install_args(install.parser().parse_args([
                "install", "--project", ".", "--skip-tools", "--source", "chaos-engine", "--commit", "1" * 40,
            ]))
        for name in install.DEFAULT_BUNDLE_COMPONENTS:
            self.assertTrue(options[name])
        self.assertFalse(options["deja"])
        args = install.parser().parse_args([
            "install", "--project", ".", "--skip-tools", "--source", "chaos-engine", "--commit", "1" * 40,
        ])
        self.assertFalse(args.with_mcp)
        self.assertIsNone(args.hosts)
        self.assertIn("node", prerequisites.required(ROOT, windows=False))
        self.assertIn('tools.append("node")', (CE / "prerequisites.py").read_text(encoding="utf-8"))


if __name__ == "__main__":
    unittest.main()
