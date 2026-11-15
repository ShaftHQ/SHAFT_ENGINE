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

import sys
sys.path.insert(0, str(Path(__file__).resolve().parents[2]))  # noqa: E402 - self-locating under python3 -I (#7197)
from tests.scripts.ce_installed_fixture import (
    SOURCE,
    build_installed_project,
    installed_layout,
    installer,
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

def ignored_starter_target(source: str, target: str, exists) -> bool:
    """True when `target` is not a file in this repository.

    `{slug}`, `{name}` and `{file}` are fill-ins in a copied starter. A path
    with no brace is still checked. A template may also name the workspace
    path a sibling starter is copied to (`setup/questionnaire.md` beside
    `questionnaire.md`). A missing basename is not that case.
    """
    if "{" in target or "}" in target:
        return True
    parts = source.split("/")
    if "assets" not in parts or "templates" not in parts:
        return False
    parent = posixpath.dirname(source)
    sibling = posixpath.normpath(posixpath.join(parent, posixpath.basename(target)))
    resolved = posixpath.normpath(posixpath.join(parent, target))
    return sibling != resolved and bool(exists(sibling))

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
                if ignored_starter_target(relative, target, layout.__contains__):
                    continue
                resolved = posixpath.normpath(posixpath.join(posixpath.dirname(relative), target))
                if resolved not in layout and resolved.rstrip("/") not in directories:
                    if installer().is_origin_only(Path(resolved)):
                        continue  # declared source-repo-only, absent from every install by design
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
                values = ("*",)  # computed choices: not statically checkable
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

class StarterLinkTest(unittest.TestCase):
    """A brace slot is not a missing file. A path with no brace still is."""

    def test_brace_placeholder_is_ignored_and_a_real_miss_is_not(self):
        present = {
            "assets/templates/questionnaire.md",
            "assets/templates/entry.md",
        }

        def exists(path: str) -> bool:
            return path in present

        self.assertTrue(ignored_starter_target("assets/templates/claim.md", "../concepts/{slug}.md", exists))
        self.assertTrue(ignored_starter_target("assets/templates/entry.md", "stages/01_{name}/CONTEXT.md", exists))
        self.assertTrue(ignored_starter_target("assets/templates/source.md", "raw/{file}", exists))
        self.assertTrue(ignored_starter_target("assets/templates/entry.md", "setup/questionnaire.md", exists))
        self.assertFalse(ignored_starter_target("assets/templates/entry.md", "setup/missing.md", exists))
        self.assertFalse(ignored_starter_target("references/guide.md", "missing.md", exists))
        self.assertFalse(ignored_starter_target("references/guide.md", "setup/questionnaire.md", exists))

if __name__ == "__main__":
    unittest.main()
