#!/usr/bin/env python3
"""Write the thin on-demand MCP and CLI caches from tool-index.json."""
# The full catalog lives once, in shaft-skills/shaft-developer/references/shaft-mcp-tools.md.
# These caches only point at it. The MCP cache is the service-group summary an
# agent needs before choosing SHAFT_MCP_TOOLSETS. The CLI cache is the curated
# command index, so ordinary discovery does not run --help.
#
#     python3 scripts/mcp/generate_shaft_skill_tool_caches.py
#     python3 scripts/mcp/generate_shaft_skill_tool_caches.py --check

from __future__ import annotations

import argparse
import json
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[2]
TOOL_INDEX = (
    REPO_ROOT / "shaft-mcp/src/main/resources/META-INF/shaft-mcp/tool-index.json"
)
MCP_CACHE = REPO_ROOT / "shaft-skills/shaft-developer/references/mcp-cache.md"
CLI_CACHE = REPO_ROOT / "shaft-skills/shaft-developer/references/cli-cache.md"
CATALOG_LINK = "[shaft-mcp-tools.md](shaft-mcp-tools.md)"
# Primary SHAFT_MCP_TOOLSETS key first, then the extra aliases McpToolsets accepts.
SERVICE_TOOLSETS = {
    "AutobotService": "autobot",
    "BrowserService": "browser",
    "CaptureService": "capture",
    "CodingPartnerService": "coding-partner",
    "DesignService": "design",
    "DoctorService": "doctor",
    "ElementService": "element",
    "EngineService": "engine",
    "GuideService": "guide",
    "HealerService": "healer, heal",
    "InfrastructureMcpService": "infrastructure, setup",
    "MobileService": "mobile",
    "PlannerService": "planner",
    "ShaftProjectService": "project",
    "TestAutomationService": "test-automation, report",
    "TraceService": "trace",
}
COLLISION_TOOLS = (
    "generate_test_report",
    "healer_run_failed_test",
    "capture_start",
    "capture_stop",
    "capture_status",
)
CLI_FORMS = (
    "shaft-cli tools --cached --group <group> --compact",
    "shaft-cli tools describe <tool> [--json]",
    "shaft-cli design ingest|analyze|gherkin|examples|lexicon|coverage|lint|gap-map|readiness|handoff",
    "shaft-cli report generate|open|summary|history|flake|tags|clusters|heal|mute|unmute|mutes",
    "shaft-cli doctor analyze|suggest|local-ai-status|cause-label",
    "shaft-cli browser navigate|screenshot|dom|url",
    "shaft-cli element click|type|hover",
    "shaft-cli capture start|stop|status|code|step-delete|step-reorder",
    "shaft-cli codegen [--session <path>]",
    "shaft-cli setup catalog|profiles [--json]",
    "shaft-cli setup doctor|status|verify --profile REPORTING [--json]",
    "shaft-cli setup plan --profile REPORTING --mode MANAGED --output <absolute-plan.json>",
    "shaft-cli setup install|apply|update --plan <absolute-plan.json> --approve <sha256:digest>",
    "shaft-cli setup start|stop --profile <profile>",
    "shaft-cli setup logs --profile REPORTING",
    "shaft-cli guide search",
    "shaft-cli session start|status|stop",
    "shaft-cli call <tool> [json]",
    "shaft-cli call <TOOL> [key=value ...] [--args '<json>'] [--json] [--stdio-ok]",
)


class CacheError(RuntimeError):
    """Raised when tool-index.json cannot be projected into the thin caches."""


def _tool_count(index: dict) -> int:
    """Return how many tools the index publishes."""
    tools = index.get("tools") or []
    return len(tools)


def _counts(index: dict) -> dict[str, int]:
    counts: dict[str, int] = {}
    for tool in index.get("tools") or []:
        service = str(tool.get("service") or "").strip()
        if not service:
            raise CacheError("tool-index entry is missing a service")
        counts[service] = counts.get(service, 0) + 1
    unknown = sorted(set(counts) - set(SERVICE_TOOLSETS))
    missing = sorted(set(SERVICE_TOOLSETS) - set(counts))
    if unknown or missing:
        raise CacheError(
            "service groups drifted from SHAFT_MCP_TOOLSETS "
            f"(unknown={unknown}, missing={missing})"
        )
    return counts


def render_mcp(index: dict) -> str:
    counts = _counts(index)
    count = _tool_count(index)
    lines = [
        "# MCP cache",
        "",
        (
            f"Do not paste the full {count}-tool catalog into prompts. "
            "Use `SHAFT_MCP_TOOLSETS=<groups>` for a focused session."
        ),
        "",
        "| Service | Toolset | Tools |",
        "| --- | --- | --- |",
    ]
    for service in sorted(counts):
        lines.append(f"| {service} | {SERVICE_TOOLSETS[service]} | {counts[service]} |")
    lines.extend([
        "",
        (
            f"Names and summaries: {CATALOG_LINK}. "
            "One schema: `shaft-cli tools describe <tool> [--json]`."
        ),
        "",
    ])
    return "\n".join(lines)


def render_cli(index: dict) -> str:
    names = {str(tool.get("name") or "") for tool in index.get("tools") or []}
    missing = [name for name in COLLISION_TOOLS if name not in names]
    if missing:
        raise CacheError(f"slash-alias collision tools missing from tool-index.json: {missing}")
    count = _tool_count(index)
    forms = "\n".join(f"- `{form}`" for form in CLI_FORMS)
    return "\n".join([
        "# CLI cache",
        "",
        (
            "Do not run `--help` for ordinary discovery. "
            f"Do not paste the {count}-tool catalog into prompts. "
            f"Names and summaries: {CATALOG_LINK}."
        ),
        "",
        "## Command forms",
        "",
        forms,
        "",
        (
            "Browser, element, capture, and driver commands need "
            "`shaft-cli session start` unless the call is a deliberately "
            "disposable one-shot. `report generate` rebuilds the Allure HTML "
            "from `--results` inside this process."
        ),
        "",
        "## Slash aliases are not these CLI commands",
        "",
        "| Alias | MCP tool | CLI |",
        "| --- | --- | --- |",
        "| `/report` | `generate_test_report` | `shaft-cli report` |",
        "| `/heal` | `healer_run_failed_test` | `shaft-cli report heal` |",
        (
            "| `/start`, `/stop`, `/status` | `capture_start`, `capture_stop`, "
            "`capture_status` | `shaft-cli session` or `shaft-cli setup` |"
        ),
        "",
    ])


def _load(path: Path) -> dict:
    return json.loads(path.read_text(encoding="utf-8"))


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--check", action="store_true")
    parser.add_argument("--tool-index", type=Path, default=TOOL_INDEX)
    args = parser.parse_args(argv)
    try:
        index = _load(args.tool_index)
        expected = {MCP_CACHE: render_mcp(index), CLI_CACHE: render_cli(index)}
    except (OSError, json.JSONDecodeError, CacheError) as exc:
        print(f"shaft skill tool cache: {exc}", file=sys.stderr)
        return 1
    if args.check:
        stale = [
            path.relative_to(REPO_ROOT).as_posix()
            for path, content in expected.items()
            if not path.is_file() or path.read_text(encoding="utf-8") != content
        ]
        if stale:
            print("stale shaft skill tool cache: " + ", ".join(stale), file=sys.stderr)
            return 1
        print("shaft skill tool caches are up to date.")
        return 0
    for path, content in expected.items():
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(content, encoding="utf-8", newline="\n")
        print(path.relative_to(REPO_ROOT).as_posix())
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
