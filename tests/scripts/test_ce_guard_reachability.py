"""#6175: everything ChaosEngine emits is reachable, allowed and valid.

For every host in `agent_harness_parity.json`, on the installed
empty-project fixture: SessionStart locators, router links, catalog paths,
role-adapter paths and every documented `python3 <path>` command must exist
in the installed layout, must not be blocked by the retrieve gate, and each
documented flag value must pass the script's argparse `choices`.
"""

from __future__ import annotations

import ast
import os
import posixpath
import re
import tempfile
import unittest
from functools import lru_cache
from pathlib import Path

from tests.scripts.ce_installed_fixture import (
    SOURCE,
    build_installed_project,
    installed_layout,
    load_module,
    parity_hosts,
)

LINK = re.compile(r"\]\(([^)\s#]+)(?:#[^)]*)?\)")
COMMAND = re.compile(
    r"(?:python3|py -3|python)\s+(?P<path>\.?[\w./-]+\.py)(?P<args>(?:[ \t]+[^\s`|;&]+)*)"
)
REPO_ONLY = re.compile(r"repo-only", re.IGNORECASE)


def markdown_files() -> list[str]:
    return sorted(path for path in installed_layout() if path.endswith(".md"))


def broken_installed_links() -> list[tuple[str, int, str]]:
    layout = installed_layout()
    directories = {posixpath.dirname(path) for path in layout}
    broken = []
    for relative in markdown_files():
        text = (SOURCE / relative).read_text(encoding="utf-8")
        for number, line in enumerate(text.splitlines(), 1):
            for target in LINK.findall(line):
                if "://" in target or target.startswith("mailto:"):
                    continue
                resolved = posixpath.normpath(posixpath.join(posixpath.dirname(relative), target))
                if resolved not in layout and resolved.rstrip("/") not in directories:
                    broken.append((relative, number, target))
    return broken


@lru_cache(maxsize=None)
def argparse_choices(script: str) -> dict[str, frozenset[str]]:
    """Static `add_argument("--flag", choices=...)` map for one script."""
    try:
        tree = ast.parse((SOURCE / script).read_text(encoding="utf-8"))
    except (OSError, SyntaxError):
        return {}
    found: dict[str, frozenset[str]] = {}
    for node in ast.walk(tree):
        if not (isinstance(node, ast.Call) and getattr(node.func, "attr", "") == "add_argument"):
            continue
        flags = [arg.value for arg in node.args if isinstance(arg, ast.Constant) and isinstance(arg.value, str)]
        for keyword in node.keywords:
            if keyword.arg != "choices":
                continue
            try:
                values = ast.literal_eval(keyword.value)
            except ValueError:
                continue
            for flag in flags:
                if flag.startswith("--"):
                    found[flag] = found.get(flag, frozenset()) | {str(value) for value in values}
    return found


def documented_commands() -> list[tuple[str, int, str, list[str], bool]]:
    commands = []
    for relative in markdown_files():
        text = (SOURCE / relative).read_text(encoding="utf-8")
        for number, line in enumerate(text.splitlines(), 1):
            for match in COMMAND.finditer(line):
                commands.append(
                    (relative, number, match.group("path"), match.group("args").split(), bool(REPO_ONLY.search(line)))
                )
    return commands


def installed_script(path: str) -> str | None:
    normalized = path[2:] if path.startswith("./") else path
    if normalized.startswith(".chaos-engine/"):
        return normalized[len(".chaos-engine/"):]
    return None


class GuardReachabilityTest(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls._tmp = tempfile.TemporaryDirectory()
        cls.project = build_installed_project(Path(cls._tmp.name) / "adopter")
        cls.gate = load_module(
            "ce_reachability_gate", cls.project / ".chaos-engine/hooks/retrieve_justification.py"
        )
        lifecycle = load_module("ce_reachability_lifecycle", cls.project / ".chaos-engine/hooks/lifecycle.py")
        previous = Path.cwd()
        os.chdir(cls.project)
        try:
            cls.session_context = lifecycle.session_start_context(None, "startup")
        finally:
            os.chdir(previous)

    @classmethod
    def tearDownClass(cls):
        cls._tmp.cleanup()

    def allowed(self, path: str) -> bool:
        return self.gate.file_read_block_reason(
            project=self.project, event_name="PreToolUse", tool_name="Read",
            tool_input={"file_path": path}, commands=(),
        ) is None

    def emitted_paths(self) -> list[str]:
        paths = list(self.gate.session_start_locators(self.session_context))
        router = ".chaos-engine/skills/chaos-engine/SKILL.md"
        paths.append(router)
        for relative in ("skills/chaos-engine/SKILL.md", "references/catalog.md"):
            text = (SOURCE / relative).read_text(encoding="utf-8")
            for target in LINK.findall(text):
                if "://" in target:
                    continue
                resolved = posixpath.normpath(posixpath.join(posixpath.dirname(relative), target))
                paths.append(f".chaos-engine/{resolved}")
        return list(dict.fromkeys(paths))

    def test_emitted_paths_exist_and_are_allowed_on_every_host(self):
        emitted = self.emitted_paths()
        self.assertTrue(any("vendor/caveman" in path for path in emitted))
        layout = installed_layout()
        for host in parity_hosts():
            for path in emitted:
                with self.subTest(host=host, path=path):
                    self.assertIn(path.removeprefix(".chaos-engine/"), layout)
                    self.assertTrue(self.allowed(path))

    def test_role_adapters_point_at_installed_allowed_paths(self):
        hosts_module = load_module("ce_reachability_hosts", SOURCE / "hosts.py")
        for relative in hosts_module.ROLE_ADAPTER_PATHS:
            body = hosts_module.role_adapter_desired(relative).decode("utf-8")
            with self.subTest(adapter=relative):
                self.assertTrue(self.allowed(relative))
                named = re.findall(r"\.chaos-engine/[\w./-]+\.md", body)
                self.assertTrue(named, body)
                for path in named:
                    self.assertIn(path.removeprefix(".chaos-engine/"), installed_layout())
                    self.assertTrue(self.allowed(path))

    def test_documented_commands_exist_or_are_marked_repo_only(self):
        failures = []
        for relative, number, path, _args, repo_only in documented_commands():
            script = installed_script(path)
            if script is not None and script in installed_layout():
                continue
            if repo_only:
                continue
            failures.append(f"{relative}:{number}: {path}")
        self.assertEqual([], failures)

    def test_documented_flag_values_pass_argparse_choices(self):
        failures = []
        for relative, number, path, args, _repo_only in documented_commands():
            script = installed_script(path)
            if script is None or script not in installed_layout():
                continue
            choices = argparse_choices(script)
            for flag, value in zip(args, args[1:]):
                allowed = choices.get(flag)
                if allowed and value.strip("`'\"") not in allowed and not value.startswith("<"):
                    failures.append(f"{relative}:{number}: {flag} {value}")
        self.assertEqual([], failures)

    def test_installed_overlay_has_no_broken_relative_links(self):
        self.assertEqual([], broken_installed_links())

    def test_stale_prefer_openai_compat_is_gone(self):
        for relative in markdown_files():
            with self.subTest(file=relative):
                self.assertNotIn("--prefer openai-compat", (SOURCE / relative).read_text(encoding="utf-8"))


if __name__ == "__main__":
    unittest.main()
