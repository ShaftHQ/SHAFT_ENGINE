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
HOSTS = ("claude", "codex", "copilot", "gemini", "grok", "opencode", "cursor", "grok-bot", "antigravity")
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
_ADAPTER = {"gemini": "adapter", "grok": "adapter", "copilot": "adapter", "antigravity": "adapter"}

# name, kind, path, description, family, codex default
SKILLS = (
    ("chaos-engine", "portable", "skills/chaos-engine/SKILL.md",
     "Canonical provider-neutral skill router and working contract. Use at the start of every task, "
     "on every host, in every main thread and delegate, before discovery, planning, edits, or answering.", "core"),
    ("work-item", "portable", "skills/work-item/SKILL.md",
     "Use when opening or rewriting a work item on any git-based SCM. Source-control agnostic; "
     "GitHub, GitLab, and Azure Boards are adapters only.", "delivery"),
    ("self-improve", "portable", "skills/self-improve/SKILL.md",
     "Use when running ChaosEngine Learning Session self-improve. Harness lessons "
     "are GitHub issues only. Product lessons may queue after delivery or on request.", "learning"),
    ("kanban", "portable", "skills/kanban/SKILL.md",
     "Use when work has several deliverables, tickets, or delegates: board, WIP 1 writer + 2 "
     "review, pull rule, Definition of Done, findings fixed or filed.", "delivery"),
    ("git-cleanup", "portable", "skills/git-cleanup/SKILL.md",
     "Use when a git worktree is dirty or another local branch or worktree exists. "
     "Ask before cleanup unless the session is unattended.", "delivery"),
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
    ("Zero-LLM first", "references/zero-llm-catalog.md",
     "Use when install, doctor, or repair can run as a script before any host chat. Prefer these deterministic paths."),
    ("Heal", "references/heal-route.md",
     "Use when a drifted install, wiped runtime, or unhealthy doctor must be repaired by file path."),
    ("Level-1 catalog", "references/level-1-catalog.md",
     "Use when a task needs a secondary skill or tool beyond the core router. The list stays short and sorted."),
    ("Context firewall", "references/context-firewall.md",
     "Use when research or a multi-file explore should run in an isolated subagent so the parent stays small."),
    ("harness-learn", "references/harness-learn.md",
     "Use when repeated session traces show the git-tracked overlay should change; tune the harness in the repo, never under `~/.grok/skills`."),
    ("design-loop", "references/design-loop.md",
     "Use when a design document needs write-review-revise rounds until zero open review issues remain."),
    ("deep-research", "references/deep-research.md",
     "Use when a question needs bounded parallel research with verification and a cited final report."),
    ("ui-delivery", "references/ui-delivery.md",
     "Use when a change touches user-visible UI, layout, styling, or themes: red-then-green e2e, measured geometry, viewport x theme matrix."),
    ("public-landing", "references/public-landing.md",
     "Use when a public calling card or product landing page must convince a human, rank in search, and orient an agent without becoming a second policy."),
    ("learn-traces", "references/learn-traces.md",
     "Use when session traces must be mapped, reduced, and verified into lessons without a host TUI runner."),
    ("Meta-optimize", "references/meta-optimize.md",
     "Use when a periodic offline review of shared logs is due. This is not a continuous session hook."),
    ("Draft skill PR", "references/draft-skill-pr.md",
     "Use when an opt-in eval-gated draft skill pull request is requested. The default is off."),
    ("Token budget", "references/token-budget-modes.md",
     "Use when triage or the environment selects an ultra-lean, balanced, or deep token budget."),
    ("Eliminate waste", "references/eliminate-waste.md",
     "Use when a hop, retry, or duplicate tool does not change the next decision and should be dropped."),
    ("Prefer CLI over MCP", "references/prefer-cli-over-mcp.md",
     "Use when both a CLI and an MCP server can do the same job. Prefer the CLI, and gh when it is configured."),
    ("No proxy", "references/no-proxy.md",
     "Use when a task would install, pin, or wrap a traffic proxy. Never install one."),
    ("GAP-EXIT2 UX", "references/host-parity-matrix.md",
     "Use when Grok or Copilot may not honor an exit-2 hard block and the compensating checklist applies."),
    ("Add-ons", "references/addons.md",
     "Use when a task fits an optional add-on (design, video, or a project pack): list, install, remove, or load one."),
    ("Complexity gate", "references/complexity-gate.md",
     "Use when a hot-spot dispatch change must treat a static-analysis Complexity ACTION_REQUIRED as a unit failure."),
    ("Durable jobs", "references/durable-jobs.md",
     "Use when work runs longer than a few minutes and must survive the agent session being killed, with no duplicate workers."),
)
ROUTE_START = "<!-- HARNESS-ROUTES:START -->"
ROUTE_END = "<!-- HARNESS-ROUTES:END -->"


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


ROUTER_USE = {
    "kanban": "several deliverables, tickets, or delegates",
    "work-item": "open or rewrite an issue or work item",
    "self-improve": "Learning Session trigger fired",
    "git-cleanup": "dirty worktree or stray branches",
    "local-agency": "delegate to local OSS agents",
    "local-runtimes": "pick a local inference runtime",
    "omniroute": "OmniRoute runtime selected",
    "freetoken": "FreeToken runtime selected",
    "colibri": "Colibri runtime selected",
    "local-openai-compat": "loopback OpenAI-compatible server",
    "local-coding-delegate": "probe local hardware size class",
    "Zero-LLM first": "install, doctor, repair by script",
    "Heal": "drifted or unhealthy install",
    "Level-1 catalog": "secondary skill or tool needed",
    "Context firewall": "isolate research or broad explore",
    "harness-learn": "traces show the overlay should change",
    "design-loop": "design doc needs review rounds",
    "deep-research": "cited multi-source research",
    "ui-delivery": "user-visible UI",
    "public-landing": "calling card or product page",
    "learn-traces": "turn traces into lessons",
    "Meta-optimize": "periodic offline log review",
    "Draft skill PR": "opt-in eval-gated skill PR",
    "Token budget": "lean, balanced, or deep",
    "Eliminate waste": "a hop or retry adds no decision",
    "Prefer CLI over MCP": "CLI and MCP both fit",
    "No proxy": "a task would add a traffic proxy",
    "GAP-EXIT2 UX": "host ignores exit-2 hard blocks",
    "Add-ons": "design, video, or project pack",
    "Complexity gate": "complexity gate",
    "Durable jobs": "outlives the session",
}


LOCAL_LLM_SKILLS = frozenset({
    "local-runtimes", "local-agency", "omniroute", "freetoken", "colibri",
    "local-openai-compat", "local-coding-delegate",
})


def render_route_table(index: dict) -> str:
    lines = ["| Route | Use when | Load |", "| --- | --- | --- |"]
    selected = [
        entry for entry in index["entries"]
        if entry.get("family") == "reference"
        or (entry["kind"] in {"portable", "route"} and entry["name"] != "chaos-engine")
    ]
    selected.sort(key=lambda entry: entry.get("family") == "reference")
    local = [entry for entry in selected if entry["name"] in LOCAL_LLM_SKILLS]
    if local:
        links = ", ".join(
            f"[{entry['name']}](../{entry['path'][len('skills/'):]})"
            for entry in sorted(local, key=lambda entry: entry["name"] != "local-runtimes")
        )
        lines.append(f"| Local LLM | optional local runtime or local agents | {links} |")
    for entry in selected:
        if entry in local:
            continue
        path = entry["path"]
        filename = path.rsplit("/", 1)[-1]
        href = "../" + path[len("skills/"):] if path.startswith("skills/") else "../../" + path
        use = ROUTER_USE[entry["name"]]
        label = {
            "harness-learn": "Harness learn",
            "design-loop": "Design loop",
            "deep-research": "Deep research",
            "ui-delivery": "UI delivery",
            "public-landing": "Public landing",
            "learn-traces": "Learn traces",
            "git-cleanup": "Git cleanup",
        }.get(entry["name"], entry["name"])
        lines.append(f"| {label} | {use} | [{filename}]({href}) |")
    return "\n".join(lines)


def _replace_marked(text: str, start: str, end: str, inner: str) -> str:
    block = f"{start}\n{inner}\n{end}"
    if start in text and end in text:
        head, rest = text.split(start, 1)
        _, tail = rest.split(end, 1)
        return head + block + tail
    return text


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
    skill = source / "skills/chaos-engine/SKILL.md"
    if skill.is_file():
        targets[skill] = _replace_marked(
            skill.read_text(encoding="utf-8"),
            ROUTE_START,
            ROUTE_END,
            render_route_table(index),
        )
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
