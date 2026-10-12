"""Shipped agent validators must carry every ``scripts.*`` module they import (#6843).

``AGENT_VALIDATION_SCRIPT_FILES`` copies validators such as ``overlay_pre_push.py`` into consumer
projects; a new module-scope import that is not in the manifest crashes the installed copy. The CI
test for this runs only when installer paths change, so the pre-push check runs it locally.
"""

from __future__ import annotations

import ast
import importlib.util
from pathlib import Path

INSTALLER = "scripts/mcp/install_shaft_agentic_tools.py"


def shipped_files(root: Path) -> tuple[str, ...]:
    spec = importlib.util.spec_from_file_location("_shaft_agentic_installer", root / INSTALLER)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)  # type: ignore[union-attr]
    return tuple(module.AGENT_VALIDATION_SCRIPT_FILES)


def _imported_modules(tree: ast.AST) -> list[str]:
    modules: list[str] = []
    for node in ast.walk(tree):
        if isinstance(node, ast.Import):
            modules.extend(alias.name for alias in node.names)
        elif isinstance(node, ast.ImportFrom) and node.level == 0 and node.module:
            modules.append(node.module)
            modules.extend(f"{node.module}.{alias.name}" for alias in node.names)
    return [name for name in modules if name.startswith("scripts.")]


def missing_imports(root: Path, shipped: tuple[str, ...]) -> list[str]:
    shipped_set = set(shipped)
    missing: list[str] = []
    for relative in sorted(shipped_set):
        path = root / relative
        if not relative.endswith(".py") or not path.is_file():
            continue
        for module in _imported_modules(ast.parse(path.read_text(encoding="utf-8"), filename=relative)):
            dependency = module.replace(".", "/") + ".py"
            if (root / dependency).is_file() and dependency not in shipped_set:
                missing.append(f"{relative} imports {module}")
    return sorted(set(missing))


def touches_shipped_validators(root: Path, changed: list[str]) -> bool:
    normalized = {path.replace("\\", "/").removeprefix("./") for path in changed}
    if INSTALLER in normalized:
        return True
    return any(path.startswith("scripts/") and path.endswith(".py") for path in normalized) and bool(
        normalized & set(shipped_files(root)))
