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
AGGREGATE = Path("chaos-engine/decision-quality-calibration.aggregate.json")
EVIDENCE_MARKERS = ("omniroute-calibration",)
EVIDENCE_METRICS = (
    "correctness",
    "tokens",
    "latency_seconds",
    "external_run_minutes",
    "actions",
    "retries",
    "cost_usd",
    "variance",
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
MERMAID_FENCE = re.compile(r"```mermaid[ \t]*\r?\n(.*?)\r?\n```", re.DOTALL)
MERMAID_DIRECTIVE = re.compile(r"(?:flowchart\s+(?:TB|TD|BT|RL|LR)|stateDiagram-v2)")
MERMAID_IDENTIFIER = re.compile(r"[A-Za-z][A-Za-z0-9_]*")


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
    python_root = root / "chaos-engine"
    paths = sorted(
        path
        for path in python_root.rglob("*.py")
        if "__pycache__" not in path.parts and path.is_file() and not path.is_symlink()
    )
    if not paths:
        raise ValueError("packaged Python source catalog is empty")
    for path in paths:
        relative = path.relative_to(root).as_posix()
        tree = ast.parse(path.read_text(encoding="utf-8"), filename=str(path))
        for node in ast.walk(tree):
            names: list[str] = []
            if isinstance(node, ast.Import):
                names = [alias.name.split(".")[0] for alias in node.names]
            elif isinstance(node, ast.ImportFrom) and node.module:
                names = [node.module.split(".")[0]]
            for name in names:
                if name != "__future__" and name in sys.stdlib_module_names:
                    uses.setdefault(name, []).append(relative)
    return [
        (
            name,
            "Portable runtime standard-library dependency.",
            ", ".join(sorted(set(paths))),
            "required",
            "Windows, Linux, macOS",
            "resolved latest stable Python",
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
                    # Dynamic assignments are outside the literal inventory.
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
            ("Latest stable Python", "Run bootstrap, installer, kernel, and tools without old-runtime defects.", "install.ps1; install.sh; dependencies.json", "managed", "Windows, Linux, macOS", "uv managed Python", "user account", "install stops before activation"),
            ("PowerShell or POSIX shell", "Launch the reviewed bootstrap wrapper.", "install.ps1; install.sh", "required", "platform native", "operating system", "consumer environment", "bootstrap does not start"),
            ("curl or wget", "Download immutable source on POSIX.", "install.sh", "required on POSIX", "Linux, macOS", "operator", "consumer environment", "download fails closed"),
            ("Node.js, npm, and npx", "Provision Memory, Context7 CLI, and plugin MCP runtimes.", "dependencies.json; hooks/launch.js", "managed", "Windows, Linux, macOS", "platform standard provider", "user account", "install stops before activation"),
            ("network", "Resolve source and provision a fresh or upgraded generation.", "bootstrap.py; dependencies.py", "required for fresh install or upgrade", "Windows, Linux, macOS", "operator", "prior verified generation remains active"),
            ("Git and Temurin Java 25", "Build optional Maven Tools MCP cache.", "dependencies.json; install.py; hosts.py", "optional and managed with `--with-maven-tools`", "Windows, Linux, macOS", "platform provider plus upstream Maven wrapper", "receipt-owned shared cache", "optional component reports absent"),
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


def _load_aggregate(root: Path) -> dict[str, object]:
    path = root / AGGREGATE
    if not path.is_file():
        raise ValueError(f"missing committed aggregate: {AGGREGATE.as_posix()}")
    value = json.loads(path.read_text(encoding="utf-8"))
    if not isinstance(value, dict):
        raise ValueError("committed aggregate must be a JSON object")
    return value


def render_omniroute_evidence(root: Path = ROOT) -> str:
    """Render the README evidence block from the committed #5522 aggregate only."""
    evidence = _load_aggregate(root)
    identity = evidence.get("identity")
    metrics = evidence.get("metrics")
    comparison = evidence.get("comparison")
    accounting = evidence.get("trialAccounting")
    if not isinstance(identity, dict) or not isinstance(metrics, dict):
        raise ValueError("committed aggregate identity/metrics are invalid")
    if not isinstance(comparison, dict) or not isinstance(accounting, dict):
        raise ValueError("committed aggregate comparison/accounting are invalid")
    gate = comparison.get("gateVerdict")
    if not isinstance(gate, dict):
        raise ValueError("committed aggregate gateVerdict is invalid")
    control = metrics.get("control")
    treatment = metrics.get("chaos-engine")
    if not isinstance(control, dict) or not isinstance(treatment, dict):
        raise ValueError("committed aggregate arm metrics are invalid")

    task_names = []
    for task in identity.get("tasks") or []:
        if isinstance(task, dict) and isinstance(task.get("name"), str):
            task_names.append(task["name"])
    models = evidence.get("modelsUsed") or []
    if not isinstance(models, list):
        models = []
    model_text = ", ".join(str(model) for model in models) or "UNAVAILABLE"

    lines = [
        "This section is generated from",
        f"[`{AGGREGATE.as_posix()}`]({AGGREGATE.name}).",
        "Do not hand-edit the numbers; refresh with",
        "`python3 scripts/ci/validate_chaos_engine_readme.py --write`.",
        "",
        "Label: **directional walking skeleton** (n="
        f"{accounting.get('observed')}/{accounting.get('planned')} observed/planned trials).",
        "This is not a Harbor 95% CI powered pilot.",
        "",
        "| Field | Value |",
        "| --- | --- |",
        f"| Gate verdict | {gate.get('verdict')} |",
        f"| Gate reason | {gate.get('reason')} |",
        f"| Correctness delta (treatment - control) | {comparison.get('correctnessDelta')} |",
        f"| Models used | {model_text} |",
        f"| Preferred model | {identity.get('preferredModel')} |",
        f"| Tasks | {', '.join(task_names)} |",
        f"| Status | {evidence.get('status')} |",
        "",
        "| Metric | control | chaos-engine |",
        "| --- | --- | --- |",
    ]
    for name in EVIDENCE_METRICS:
        lines.append(f"| `{name}` | {control.get(name)} | {treatment.get(name)} |")
    lines.extend(
        [
            "",
            "Methodology and final report:",
            "[decision-quality-calibration.md](decision-quality-calibration.md),",
            "[decision-quality-report.md](decision-quality-report.md).",
            "Missing telemetry remains the literal `UNAVAILABLE` (never `0`).",
        ]
    )
    return "\n".join(lines)


def evidence_sections(root: Path = ROOT) -> dict[str, str]:
    return {"omniroute-calibration": render_omniroute_evidence(root)}


def _skip_spaces(value: str, position: int) -> int:
    while position < len(value) and value[position] in " \t":
        position += 1
    return position


def _parse_mermaid_node(value: str, position: int) -> int:
    position = _skip_spaces(value, position)
    if value.startswith("[*]", position):
        return position + 3
    identifier = MERMAID_IDENTIFIER.match(value, position)
    if identifier is None:
        raise ValueError("node identifier")
    position = identifier.end()
    if position == len(value) or value[position] not in "[{":
        return position
    opener = value[position]
    doubled = opener == "[" and value.startswith("[[", position)
    closing = "]]" if doubled else "]" if opener == "[" else "}"
    content_start = position + (2 if doubled else 1)
    content_end = value.find(closing, content_start)
    if content_end < 0:
        raise ValueError("unclosed node")
    content = value[content_start:content_end]
    if not content.strip() or any(character in content for character in "[]{}"):
        raise ValueError("invalid node label")
    if content[0] in "\"'" and (len(content) < 2 or content[-1] != content[0]):
        raise ValueError("unclosed quoted node label")
    return content_end + len(closing)


def _parse_mermaid_edge(value: str) -> None:
    position = _parse_mermaid_node(value, 0)
    position = _skip_spaces(value, position)
    arrow = next((token for token in ("-.->", "-->") if value.startswith(token, position)), None)
    if arrow is None:
        raise ValueError("edge operator")
    position = _skip_spaces(value, position + len(arrow))
    if position < len(value) and value[position] == "|":
        end = value.find("|", position + 1)
        if end < 0 or not value[position + 1 : end].strip():
            raise ValueError("edge label")
        position = _skip_spaces(value, end + 1)
    position = _parse_mermaid_node(value, position)
    if _skip_spaces(value, position) != len(value):
        raise ValueError("trailing Mermaid syntax")


def _parse_mermaid(readme: str) -> tuple[set[str], list[str]]:
    blocks = MERMAID_FENCE.findall(readme)
    errors: list[str] = []
    if len(blocks) != readme.count("```mermaid"):
        errors.append("Mermaid syntax has an unterminated fenced block")
    titles: list[str] = []
    for index, block in enumerate(blocks, start=1):
        lines = [line.strip() for line in block.splitlines() if line.strip()]
        if not lines or MERMAID_DIRECTIVE.fullmatch(lines[0]) is None:
            errors.append(f"Mermaid syntax block {index} has an invalid diagram directive")
            continue
        title_count = 0
        description_count = 0
        for line in lines[1:]:
            if line.startswith("accTitle:"):
                title = line.partition(":")[2].strip()
                title_count += 1
                if title:
                    titles.append(title)
                else:
                    errors.append(f"Mermaid syntax block {index} has a blank accTitle")
                continue
            if line.startswith("accDescr:"):
                description_count += 1
                if not line.partition(":")[2].strip():
                    errors.append(f"Mermaid syntax block {index} has a blank accDescr")
                continue
            try:
                _parse_mermaid_edge(line)
            except ValueError:
                errors.append(f"Mermaid syntax block {index} has an invalid statement")
        if title_count != 1 or description_count != 1:
            errors.append(
                f"Mermaid syntax block {index} requires one accTitle and one accDescr"
            )
    if len(titles) != len(set(titles)):
        errors.append("Mermaid syntax contains duplicate accTitle values")
    return set(titles), errors


def _replace_marked_section(readme: str, kind: str, name: str, body: str) -> str:
    start = f"<!-- {kind}:{name}:start -->"
    end = f"<!-- {kind}:{name}:end -->"
    if readme.count(start) != 1 or readme.count(end) != 1:
        raise ValueError(f"{kind} marker count is invalid: {name}")
    before, rest = readme.split(start, 1)
    _, after = rest.split(end, 1)
    return f"{before}{start}\n{body}\n{end}{after}"


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
    try:
        sections = evidence_sections(root)
    except ValueError as exc:
        errors.append(str(exc))
        sections = {}
    for name in EVIDENCE_MARKERS:
        start = f"<!-- evidence:{name}:start -->"
        end = f"<!-- evidence:{name}:end -->"
        if readme.count(start) != 1 or readme.count(end) != 1:
            errors.append(f"evidence marker count is invalid: {name}")
            continue
        expected = sections.get(name)
        if expected is None:
            continue
        actual = readme.split(start, 1)[1].split(end, 1)[0].strip()
        if actual != expected:
            errors.append(f"source-derived evidence drift: {name}")
    titles, mermaid_errors = _parse_mermaid(readme)
    errors.extend(mermaid_errors)
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


def write_generated(root: Path = ROOT) -> None:
    """Rewrite source-derived inventory and evidence sections between markers."""
    root = root.resolve()
    path = root / README
    readme = path.read_text(encoding="utf-8")
    for name, table in inventory_sections(root).items():
        readme = _replace_marked_section(readme, "inventory", name, table)
    for name, body in evidence_sections(root).items():
        readme = _replace_marked_section(readme, "evidence", name, body)
    path.write_text(readme, encoding="utf-8")


def parser() -> argparse.ArgumentParser:
    result = argparse.ArgumentParser(description=__doc__)
    result.add_argument("--root", type=Path, default=ROOT)
    result.add_argument("--render", action="store_true")
    result.add_argument(
        "--write",
        action="store_true",
        help="refresh source-derived README inventory sections",
    )
    return result


def main() -> int:
    arguments = parser().parse_args()
    root = arguments.root.resolve()
    if arguments.render:
        print(rendered_inventory(root))
        return 0
    if arguments.write:
        write_generated(root)
    errors = validate(root)
    for error in errors:
        print(error, file=sys.stderr)
    return 1 if errors else 0


if __name__ == "__main__":
    raise SystemExit(main())
