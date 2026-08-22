#!/usr/bin/env python3
"""Validate source-derived ChaosEngine README inventories and topology diagrams."""

from __future__ import annotations

import argparse
import ast
import json
import re
import sys
from pathlib import Path


ROOT = Path(__file__).resolve().parents[2]
README = Path("chaos-engine/README.md")
CORE_PYTHON = (
    "bootstrap.py",
    "dependencies.py",
    "hosts.py",
    "install.py",
    "learning.py",
    "tool.py",
    "hooks/guard.py",
    "hooks/kernel.py",
    "hooks/lifecycle.py",
    "hooks/reflection.py",
)
FIELDS = ("Item", "Purpose", "Source of truth", "Status", "Platforms", "Provisioner", "Owner", "Failure behavior")
REQUIRED_DIAGRAMS = {
    "Prerequisite and dependency topology",
    "Skill topology",
    "Five-host lifecycle topology",
    "Fresh installation flow",
    "Managed upgrade and repair flow",
    "Dependency provisioning flow",
    "POSIX symlink and Windows junction flow",
    "Rollback flow",
    "Ownership and foreign-file preservation flow",
    "Lifecycle terminal-state flow",
}


def _escape(value: object) -> str:
    return " ".join(str(value).replace("|", "\\|").split())


def _table(rows: list[tuple[object, ...]]) -> str:
    lines = [
        "| " + " | ".join(FIELDS) + " |",
        "| " + " | ".join("---" for _ in FIELDS) + " |",
    ]
    lines.extend("| " + " | ".join(_escape(value) for value in row) + " |" for row in rows)
    return "\n".join(lines)


def _description(path: Path) -> str:
    text = path.read_text(encoding="utf-8")
    match = re.search(r"^description:\s*(.+)$", text, re.MULTILINE)
    return match.group(1).strip().strip('"') if match else "Routed ChaosEngine skill."


def _python_libraries(root: Path) -> list[tuple[object, ...]]:
    uses: dict[str, list[str]] = {}
    for relative in CORE_PYTHON:
        path = root / "chaos-engine" / relative
        tree = ast.parse(path.read_text(encoding="utf-8"), filename=str(path))
        for node in ast.walk(tree):
            names: list[str] = []
            if isinstance(node, ast.Import):
                names = [alias.name.split(".")[0] for alias in node.names]
            elif isinstance(node, ast.ImportFrom) and node.module:
                names = [node.module.split(".")[0]]
            for name in names:
                if name != "__future__" and name in sys.stdlib_module_names:
                    uses.setdefault(name, []).append(f"chaos-engine/{relative}")
    return [
        (
            name,
            "Portable runtime standard-library dependency.",
            ", ".join(sorted(set(paths))),
            "required",
            "Windows, Linux, macOS",
            "system Python 3.10+",
            "Python runtime",
            "affected command fails closed",
        )
        for name, paths in sorted(uses.items())
    ]


def _managed_dependencies(root: Path) -> list[tuple[object, ...]]:
    specification = json.loads((root / "chaos-engine/dependencies.json").read_text(encoding="utf-8"))
    rows = []
    for name, value in sorted(specification["tools"].items()):
        packages = [value["package"], *value.get("with", [])]
        commands = ", ".join(value.get("commands", [name]))
        rows.append(
            (
                name,
                f"Managed commands: {commands}; packages: {', '.join(packages)}.",
                "chaos-engine/dependencies.json",
                "required",
                "Windows, Linux, macOS",
                "immutable generation installer",
                "installer receipt",
                "repair full candidate or retain prior active generation",
            )
        )
    return rows


def _skills(root: Path) -> list[tuple[object, ...]]:
    paths = sorted((root / "chaos-engine").glob("skills/*/SKILL.md")) + sorted(
        (root / "chaos-engine/vendor").glob("*/skills/*/SKILL.md")
    )
    rows = []
    for path in paths:
        relative = path.relative_to(root).as_posix()
        name = path.parent.name
        required = name in {"chaos-engine", "caveman", "ponytail"}
        rows.append(
            (
                name,
                _description(path),
                relative,
                "required" if required else "optional routed",
                "all hosts",
                "core or pinned vendor installer",
                "canonical skill" if name == "chaos-engine" else "skill package",
                "required skill blocks routing; optional skill reports capability gap",
            )
        )
    return rows


def _literal_assignments(path: Path) -> dict[str, object]:
    tree = ast.parse(path.read_text(encoding="utf-8"), filename=str(path))
    result = {}
    for node in tree.body:
        if isinstance(node, (ast.Assign, ast.AnnAssign)):
            targets = node.targets if isinstance(node, ast.Assign) else [node.target]
            if len(targets) == 1 and isinstance(targets[0], ast.Name):
                try:
                    result[targets[0].id] = ast.literal_eval(node.value)
                except (ValueError, TypeError):
                    pass
    return result


def _hosts(root: Path) -> list[tuple[object, ...]]:
    path = root / "chaos-engine/hooks/kernel.py"
    tree = ast.parse(path.read_text(encoding="utf-8"), filename=str(path))
    host_names: list[str] = []
    for node in tree.body:
        if isinstance(node, ast.AnnAssign) and isinstance(node.target, ast.Name) and node.target.id == "HOST_CAPABILITIES":
            if isinstance(node.value, ast.Dict):
                host_names = [str(ast.literal_eval(key)) for key in node.value.keys]
    if not host_names:
        raise ValueError("kernel host capability map could not be derived")
    return [
        (
            host,
            "Thin native event and JSON protocol adapter.",
            "chaos-engine/hooks/kernel.py; chaos-engine/hosts.py",
            "supported adapter",
            "Windows, Linux, macOS",
            "host installer",
            "provider-neutral kernel",
            "unsupported native events remain explicit capability gaps",
        )
        for host in sorted(host_names)
    ]


def _events(root: Path) -> list[tuple[object, ...]]:
    values = _literal_assignments(root / "chaos-engine/hooks/lifecycle.py")
    events = values.get("LIFECYCLE_EVENTS")
    if not isinstance(events, tuple):
        raise ValueError("lifecycle event registry could not be derived")
    return [
        (
            event,
            "Normalize native input and evaluate one provider-neutral lifecycle event.",
            "chaos-engine/hooks/lifecycle.py; chaos-engine/hooks/kernel.py",
            "declared",
            "capability-mapped hosts",
            "generated host adapter",
            "lifecycle kernel",
            "missing mapping fails host parity validation",
        )
        for event in events
    ]


def inventory_sections(root: Path = ROOT) -> dict[str, str]:
    static: dict[str, list[tuple[object, ...]]] = {
        "prerequisites": [
            ("Python 3.10+", "Run bootstrap, installer, kernel, and tools.", "install.ps1; install.sh", "required", "Windows, Linux, macOS", "operator", "consumer environment", "install stops before mutation"),
            ("PowerShell or POSIX shell", "Launch the reviewed bootstrap wrapper.", "install.ps1; install.sh", "required", "platform native", "operating system", "consumer environment", "bootstrap does not start"),
            ("curl or wget", "Download immutable source on POSIX.", "install.sh", "required on POSIX", "Linux, macOS", "operator", "consumer environment", "download fails closed"),
            ("Node.js and npm", "Provision Memory and launch Gemini hooks.", "dependencies.json; hooks/launch.js", "required", "Windows, Linux, macOS", "operator", "consumer environment", "dependency generation is not published"),
            ("network", "Resolve source and provision a fresh or upgraded generation.", "bootstrap.py; dependencies.py", "required for fresh install or upgrade", "Windows, Linux, macOS", "operator", "prior verified generation remains active"),
            ("Git and Java 25", "Build optional Maven Tools MCP cache.", "hosts.py", "optional", "Windows, Linux, macOS", "operator", "optional component reports absent"),
        ],
        "external-services": [
            ("GitHub API and raw content", "Resolve immutable source and deliver pull requests.", "bootstrap.py; work-github-playbook.md", "required for remote install and delivery", "network", "operator credentials", "GitHub and repository owner", "install or delivery blocks"),
            ("PyPI and uv package service", "Provision uv, MemPalace, and Graphify packages.", "dependencies.json", "required when generation repair needs Python packages", "network", "uv", "installer generation", "candidate is discarded"),
            ("npm registry", "Provision project-local Memory commands.", "dependencies.json", "required when Memory is missing or stale", "network", "npm", "installer generation", "candidate is discarded"),
            ("five host APIs", "Run scheduled paired promotion trials.", "scripts/ci/chaos_engine_promotion.py", "required for promotion only", "scheduled/manual CI", "host credentials", "promotion evaluator", "promotion remains Blocked"),
            ("ShaftHQ/shafthq.github.io", "Publish companion functional documentation.", "repository documentation policy", "required in same delivery campaign", "GitHub", "documentation PR", "documentation repository", "campaign remains incomplete"),
        ],
        "generated-assets": [
            ("dependency generations", "Immutable project-local tools and interpreters.", "dependencies.py", "generated", "Windows, Linux, macOS", "installer", "receipt-owned", "unselected candidate is removed; foreign content is preserved"),
            ("active and previous pointers", "Atomically select current and rollback generations.", "dependencies.py", "generated", "Windows, Linux, macOS", "installer", "installer control plane", "invalid pointer fails closed"),
            ("host adapters and receipts", "Install native hook, skill, plugin, and MCP projections.", "hosts.py", "generated and trackable", "five hosts", "host installer", "receipt-owned", "rollback restores exact prior bytes"),
            ("Memory, MemPalace, and Graphify state", "Persist canonical data or derived indexes.", ".gitignore; hosts.py", "generated and never tracked", "project local", "owned tools", "project or derived single writer", "doctor reports recovery-required"),
            ("reports, caches, and evaluation receipts", "Carry bounded diagnostics without transcripts or secrets.", ".gitignore; chaos_engine_promotion.py", "generated and never tracked", "local and CI", "requesting command", "ephemeral evidence owner", "missing evidence blocks promotion"),
        ],
    }
    rows = {
        "prerequisites": static["prerequisites"],
        "python-libraries": _python_libraries(root),
        "managed-dependencies": _managed_dependencies(root),
        "skills": _skills(root),
        "hosts": _hosts(root),
        "lifecycle-events": _events(root),
        "external-services": static["external-services"],
        "generated-assets": static["generated-assets"],
    }
    return {name: _table(values) for name, values in rows.items()}


def rendered_inventory(root: Path = ROOT) -> str:
    parts = []
    for name, table in inventory_sections(root).items():
        title = name.replace("-", " ").title()
        parts.append(
            f"### {title}\n\n<!-- inventory:{name}:start -->\n{table}\n<!-- inventory:{name}:end -->"
        )
    return "\n\n".join(parts)


def validate(root: Path = ROOT) -> list[str]:
    readme = (root / README).read_text(encoding="utf-8")
    errors: list[str] = []
    for name, table in inventory_sections(root).items():
        start = f"<!-- inventory:{name}:start -->"
        end = f"<!-- inventory:{name}:end -->"
        if readme.count(start) != 1 or readme.count(end) != 1:
            errors.append(f"inventory marker count is invalid: {name}")
            continue
        actual = readme.split(start, 1)[1].split(end, 1)[0].strip()
        if actual != table:
            errors.append(f"source-derived inventory drift: {name}")
    titles = set(re.findall(r"accTitle:\s*(.+)", readme))
    missing_titles = REQUIRED_DIAGRAMS - titles
    if missing_titles:
        errors.append("missing Mermaid topology diagrams: " + ", ".join(sorted(missing_titles)))
    if "ShaftHQ/shafthq.github.io" not in readme or "same delivery campaign" not in readme:
        errors.append("companion documentation delivery policy is missing")
    lowered = readme.casefold()
    for retired in ("memory load", "mempalace wake-up"):
        if retired in lowered:
            errors.append(f"retired startup retrieval language remains: {retired}")
    return errors


def parser() -> argparse.ArgumentParser:
    result = argparse.ArgumentParser(description=__doc__)
    result.add_argument("--root", type=Path, default=ROOT)
    result.add_argument("--render", action="store_true")
    return result


def main() -> int:
    arguments = parser().parse_args()
    if arguments.render:
        print(rendered_inventory(arguments.root.resolve()))
        return 0
    errors = validate(arguments.root.resolve())
    for error in errors:
        print(error, file=sys.stderr)
    return 1 if errors else 0


if __name__ == "__main__":
    raise SystemExit(main())
