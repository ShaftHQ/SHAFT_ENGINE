#!/usr/bin/env python3
"""Generate and check the one ChaosEngine harness/skill index (#6177).

`harness-index.json` is the single source for the router catalog
(`references/catalog.md`), the repository skills map block, both
marketplace manifests, `expected_skill_names`, and the retrieve-gate
harness allowlist (`hooks/retrieve_justification.py`). Zero LLM, stdlib only.

    python3 .chaos-engine/harness_index.py --check
    python3 chaos-engine/harness_index.py --write   # repo-only: source tree
"""

from __future__ import annotations

import argparse
import json
import sys
from pathlib import Path

HERE = Path(__file__).resolve().parent
INDEX_NAME = "harness-index.json"
CATALOG = "references/catalog.md"
HOSTS = ("claude", "codex", "copilot", "gemini", "grok", "opencode", "cursor", "grok-bot")
HOOK_HOSTS = ("claude", "codex", "copilot", "gemini", "grok")
HARNESS_ROOTS = (
    ".chaos-engine/",
    "chaos-engine/",
    ".chaos-engine-state/",
    "plugins/",
    ".agents/",
    ".claude/",
    ".claude-plugin/",
    ".codex/",
    ".codex-plugin/",
    ".gemini/",
    ".grok/",
    ".opencode/",
    ".cursor/",
    ".github/hooks/",
    ".github/skills/",
    ".memory/",
)
HARNESS_FILES = (
    "AGENTS.md",
    "CLAUDE.md",
    "GEMINI.md",
    ".github/copilot-instructions.md",
    ".mcp.json",
    "mempalace.yaml",
    ".graphifyignore",
)
README_START = "<!-- HARNESS-INDEX:START -->"
README_END = "<!-- HARNESS-INDEX:END -->"

_NATIVE = {"claude": "plugin", "codex": "plugin"}
_ADAPTER = {"gemini": "adapter", "grok": "adapter", "copilot": "adapter"}

# name, kind, path, description, family, codex default
SKILLS = (
    ("chaos-engine", "portable", "skills/chaos-engine/SKILL.md",
     "Canonical provider-neutral skill router and working contract. Use at the start of every task, "
     "on every host, in every main thread and delegate, before discovery, planning, edits, or answering.", "core"),
    ("work-item", "portable", "skills/work-item/SKILL.md",
     "Use when opening or rewriting a work item on any git-based SCM. Source-control agnostic; "
     "GitHub, GitLab, and Azure Boards are adapters only.", "delivery"),
    ("self-improve", "portable", "skills/self-improve/SKILL.md",
     "Use when running ChaosEngine Learning Session self-improve: dual-track harness + product "
     "lessons via learning.py after delivery or on request.", "learning"),
    ("local-agency", "portable", "skills/local-agency/SKILL.md",
     "Use when the adopter asks to delegate to local agents (OpenCode / OSS agency) against a READY "
     "local runtime instead of orchestrator-session subagents.", "delegation"),
    ("local-runtimes", "portable", "skills/local-runtimes/SKILL.md",
     "Use when choosing an optional local inference runtime for a narrow mechanical or offline job. "
     "One table routes to OmniRoute, FreeToken, Colibri, a loopback server, or the hardware probe.",
     "local-runtime"),
    ("omniroute", "route", "skills/omniroute/SKILL.md",
     "Use when local-runtimes selected the OmniRoute process for bounded work: runner, delegate "
     "continuity, capability enforcement and proof of dispatch.", "local-runtime"),
    ("freetoken", "route", "skills/freetoken/SKILL.md",
     "Standalone FreeToken process: install, READY probe and bounded dispatch. Use when the "
     "local-runtimes table points a job at FreeToken rather than a loopback server.", "local-runtime"),
    ("colibri", "route", "skills/colibri/SKILL.md",
     "Frontier MoE through a disk/RAM/VRAM hierarchy via the optional Colibri process. Use when a "
     "large-model offline job needs Colibri; not OmniRoute or FreeToken.", "local-runtime"),
    ("local-openai-compat", "route", "skills/local-openai-compat/SKILL.md",
     "Loopback OpenAI-compatible peers (Ollama, LM Studio, llamacpp on 127.0.0.1). Use when a "
     "narrow task should go to an already READY loopback server.", "local-runtime"),
    ("local-coding-delegate", "route", "skills/local-coding-delegate/SKILL.md",
     "Hardware size-class probe for local models. Use when you must learn which model size this "
     "machine can host before any runtime is picked.", "local-runtime"),
)
VENDOR = (
    ("caveman", "vendor/caveman/skills/caveman/SKILL.md",
     "Ultra-compressed communication companion. Use on every reply at ultra intensity; mandatory on "
     "the implementation path. Stop only with 'stop caveman' or 'normal mode'.", "INSTALLED_BY_DEFAULT"),
    ("ponytail", "vendor/ponytail/skills/ponytail/SKILL.md",
     "Laziest-solution-that-works companion. Use at ultra before the first edit on the "
     "implementation path to choose the smallest correct change. Stop with 'stop ponytail'.",
     "INSTALLED_BY_DEFAULT"),
    ("icm-architect", "vendor/icm-architect/skills/icm-architect/SKILL.md",
     "Advisory ICM workspace architect (folder structure as agent architecture). Use only when the "
     "task is ICM, workspace structure, or 'ICM this' design work.", "AVAILABLE"),
)
ROLES = (
    ("orchestrator", "Plan, architecture, synthesis, and final verification."),
    ("implementer", "One bounded specification before consolidated validation."),
    ("reviewer", "Independent read-only adversarial review; never edit."),
    ("tester", "Reproduce behavior; regression and acceptance evidence."),
    ("mechanical-helper", "Deterministic reversible spec-exact work; stop on ambiguity."),
)
ROUTES = (
    ("harness-learn", "references/harness-learn.md",
     "Use when repeated session traces show the git-tracked overlay should change; tune the harness in the repo, never under `~/.grok/skills`."),
    ("design-loop", "references/design-loop.md",
     "Use when a design document needs write-review-revise rounds until zero open review issues remain."),
    ("deep-research", "references/deep-research.md",
     "Use when a question needs bounded parallel research with verification and a cited final report."),
    ("learn-traces", "references/learn-traces.md",
     "Use when session traces must be mapped, reduced, and verified into lessons without a host TUI runner."),
)


def _hosts(native: dict[str, str], default: str = "catalog") -> dict[str, str]:
    return {host: native.get(host, default) for host in HOSTS}


def build_index() -> dict:
    entries: list[dict] = []
    for name, kind, path, description, family in SKILLS:
        native = {**_NATIVE, **(_ADAPTER if name == "chaos-engine" else {})}
        entries.append({
            "name": name, "kind": kind, "path": path, "description": description,
            "family": family, "hosts": _hosts(native),
        })
    for name, path, description, codex_default in VENDOR:
        entries.append({
            "name": name, "kind": "vendor", "path": path, "description": description,
            "family": "companion", "codexDefault": codex_default, "hosts": _hosts(_NATIVE),
        })
    for name, description in ROLES:
        entries.append({
            "name": name, "kind": "role", "path": "references/roles.md", "anchor": name,
            "description": description, "family": "role",
            "hosts": _hosts({"claude": "adapter", "codex": "adapter"}),
        })
    for name, path, description in ROUTES:
        entries.append({
            "name": name, "kind": "route", "path": path, "description": description,
            "family": "reference", "hosts": _hosts({}),
        })
    return {
        "schemaVersion": 1,
        "generator": "harness_index.py",
        "hosts": list(HOSTS),
        "hookHosts": list(HOOK_HOSTS),
        "harnessRoots": list(HARNESS_ROOTS),
        "harnessFiles": list(HARNESS_FILES),
        "entries": entries,
    }


def render_index(index: dict) -> str:
    return json.dumps(index, indent=2, sort_keys=True) + "\n"


def _cell(text: str) -> str:
    return text.replace("|", "/")


def render_catalog(index: dict) -> str:
    lines = [
        "# Catalog",
        "",
        "Generated by [harness_index.py](../harness_index.py) from",
        "[harness-index.json](../harness-index.json); edit the index source, then run",
        "`python3 .chaos-engine/harness_index.py --check`. Load a body on demand.",
        "router-skill-inventory-contract: a portable or vendor skill name absent from this",
        "catalog, or a catalog SKILL.md path missing or untracked, fails closed. Local runtime",
        "routes are reached through `local-runtimes`; roles through their adapters.",
        "",
        "## Catalog",
        "",
        "| name | description | path | open |",
        "| --- | --- | --- | --- |",
    ]
    for entry in index["entries"]:
        target = entry["path"] + (f"#{entry['anchor']}" if entry.get("anchor") else "")
        lines.append(
            f"| {entry['name']} | {_cell(entry['description'])} | `{target}` | [{entry['kind']}](../{target}) |"
        )
    return "\n".join(lines) + "\n"


def render_readme_block(index: dict) -> str:
    lines = [README_START, "", "Generated skill index (`chaos-engine/harness_index.py`):", ""]
    for entry in index["entries"]:
        if entry["kind"] in {"portable", "vendor"} or entry.get("family") == "local-runtime":
            lines.append(f"- [{entry['name']}](../../chaos-engine/{entry['path']}) ({entry['kind']})")
    lines.extend(["", README_END])
    return "\n".join(lines)


def expected_skill_names(index: dict) -> list[str]:
    return sorted(
        entry["name"] for entry in index["entries"]
        if entry["path"].startswith("skills/") and entry["path"].endswith("/SKILL.md")
    )


def _replace_block(text: str, block: str) -> str:
    if README_START in text and README_END in text:
        head, rest = text.split(README_START, 1)
        _, tail = rest.split(README_END, 1)
        return head + block + tail
    return text.rstrip("\n") + "\n\n" + block + "\n"


def _repo_targets(root: Path, index: dict) -> dict[Path, str]:
    targets: dict[Path, str] = {}
    readme = root / ".agents/skills/README.md"
    if readme.is_file():
        targets[readme] = _replace_block(readme.read_text(encoding="utf-8"), render_readme_block(index))
    budget = root / "scripts/ci/agent_guidance_budget.json"
    if budget.is_file():
        payload = json.loads(budget.read_text(encoding="utf-8"))
        payload.setdefault("expected_skill_names", {})["chaos-engine/skills"] = expected_skill_names(index)
        targets[budget] = json.dumps(payload, indent=2) + "\n"
    return targets


def _source_tree(root: Path) -> Path:
    for candidate in (root / "chaos-engine", root / ".chaos-engine", root):
        if (candidate / "harness_index.py").is_file():
            return candidate
    return HERE


def generated(root: Path) -> dict[Path, str]:
    index = build_index()
    source = _source_tree(root)
    targets = {source / INDEX_NAME: render_index(index), source / CATALOG: render_catalog(index)}
    if source.name == "chaos-engine":
        targets.update(_repo_targets(root, index))
    return targets


def check(root: Path) -> list[str]:
    """Return drift findings; empty when every generated artifact matches."""
    findings: list[str] = []
    source = _source_tree(root)
    for path, expected in generated(root).items():
        current = path.read_text(encoding="utf-8") if path.is_file() else None
        if current != expected:
            findings.append(f"stale generated artifact: {path.relative_to(root).as_posix()}")
    for entry in build_index()["entries"]:
        if not (source / entry["path"]).is_file():
            findings.append(f"index entry without a file: {entry['path']}")
    return findings


def write(root: Path) -> list[str]:
    written: list[str] = []
    for path, expected in generated(root).items():
        if not path.is_file() or path.read_text(encoding="utf-8") != expected:
            path.parent.mkdir(parents=True, exist_ok=True)
            path.write_text(expected, encoding="utf-8")
            written.append(path.relative_to(root).as_posix())
    return written


def load_index(tree: Path | None = None) -> dict:
    """Read the generated index beside this file; fall back to the built one."""
    path = (tree or HERE) / INDEX_NAME
    try:
        return json.loads(path.read_text(encoding="utf-8"))
    except (OSError, ValueError):
        return build_index()


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    parser.add_argument("--root", type=Path, default=HERE.parent)
    mode = parser.add_mutually_exclusive_group(required=True)
    mode.add_argument("--check", action="store_true")
    mode.add_argument("--write", action="store_true")
    args = parser.parse_args(argv)
    root = args.root.resolve()
    if args.write:
        for item in write(root):
            print(f"wrote {item}")
        return 0
    findings = check(root)
    for item in findings:
        print(item, file=sys.stderr)
    return 1 if findings else 0


if __name__ == "__main__":
    raise SystemExit(main())
