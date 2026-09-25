"""Research-receipt shims for hook-capable instruction-only hosts (#6218).

Cursor and OpenCode load `AGENTS.md` without the ChaosEngine hook runtime, so
their `retrieve:` receipt is the only enforcement. This shim opens a pending
receipt at `.chaos-engine-state/research-receipt.md` before the host's first
project read. It never blocks, never overwrites an existing receipt, and never
fills the `retrieve:` field: the agent still records `used`, `skipped(<reason>)`
or `exempt(harness)`, and the Learning Session keeps flagging a pending one.

    ensure  --host cursor|opencode [--project DIR]   (Cursor passes JSON on stdin)
    install --host cursor|opencode [--project DIR]

The installer wires these automatically for detected hosts (#6230).
"""

from __future__ import annotations

import argparse
import json
import os
import sys
import time
from pathlib import Path

SINK = ".chaos-engine-state/research-receipt.md"
SHIM_HOSTS = ("cursor", "opencode")
INSTRUCTION_ONLY = ("grok-bot", "copilot-cloud")
CURSOR_HOOKS = ".cursor/hooks.json"
CURSOR_EVENTS = ("beforeReadFile", "beforeShellExecution")
OPENCODE_PLUGIN = ".opencode/plugins/chaos-engine-receipt.js"
SHIM_RELATIVE = ".chaos-engine/hooks/receipt_shim.py"
STEPS = (
    "Read live files and current instructions.",
    "Load the routed skill and directly required references.",
    "Native Memory: used, or the irrelevance reason.",
    "MemPalace: used, skipped with a reason, or degraded.",
    "Graphify: used with live-verified paths, or the irrelevance reason.",
    "Online research: source URLs and date.",
    "Compare approaches; steelman the rejected one.",
    "Plan and deferred consolidated proof commands.",
)


def _python_command() -> str:
    return "py -3" if os.name == "nt" else "python3"


def receipt_template(host: str, opened: str) -> str:
    lines = ["# Research receipt", "", f"host: {host}", f"opened: {opened}", ""]
    lines += [f"{index}. [ ] {step}" for index, step in enumerate(STEPS, start=1)]
    lines += [
        "",
        "Replace the pending line with used, skipped(<reason>) or exempt(harness).",
        "retrieve: pending",
        "",
    ]
    return "\n".join(lines)


def ensure_receipt(project: Path, host: str) -> bool:
    """Create the pending receipt when absent. Returns True when it was created."""
    sink = project / SINK
    if sink.exists() or sink.is_symlink():
        return False
    sink.parent.mkdir(parents=True, exist_ok=True)
    opened = time.strftime("%Y-%m-%dT%H:%M:%S%z")
    try:
        with sink.open("x", encoding="utf-8") as handle:
            handle.write(receipt_template(host, opened))
    except FileExistsError:
        return False
    return True


def _project_from_payload(raw: str, fallback: Path) -> Path:
    try:
        payload = json.loads(raw) if raw.strip() else {}
    except ValueError:
        return fallback
    roots = payload.get("workspace_roots") if isinstance(payload, dict) else None
    if isinstance(roots, list) and roots and isinstance(roots[0], str):
        return Path(roots[0])
    return fallback


def cursor_hooks(existing: dict[str, object]) -> dict[str, object]:
    """Merge the shim into a Cursor hooks document without touching other hooks."""
    command = f"{_python_command()} {SHIM_RELATIVE} ensure --host cursor"
    document = dict(existing) if isinstance(existing, dict) else {}
    document.setdefault("version", 1)
    hooks = document.get("hooks")
    hooks = dict(hooks) if isinstance(hooks, dict) else {}
    for event in CURSOR_EVENTS:
        entries = [entry for entry in hooks.get(event, []) if not _is_shim_entry(entry)]
        entries.append({"command": command, "timeout": 10})
        hooks[event] = entries
    document["hooks"] = hooks
    return document


def opencode_plugin() -> str:
    python = "['py', '-3']" if os.name == "nt" else "['python3']"
    return (
        "// ChaosEngine research-receipt shim (#6218). Generated; rerun\n"
        "// `receipt_shim.py install --host opencode` instead of editing.\n"
        "import { execFileSync } from 'node:child_process';\n"
        "import { existsSync } from 'node:fs';\n"
        "import { join } from 'node:path';\n\n"
        "const READ_TOOLS = new Set(['read', 'grep', 'glob', 'list', 'bash']);\n"
        f"const PYTHON = {python};\n\n"
        "export const ChaosEngineReceipt = async ({ directory }) => ({\n"
        "  'tool.execute.before': async (input) => {\n"
        "    if (!READ_TOOLS.has(input.tool)) return;\n"
        f"    if (existsSync(join(directory, '{SINK}'))) return;\n"
        "    try {\n"
        "      execFileSync(PYTHON[0], [...PYTHON.slice(1),\n"
        f"        '{SHIM_RELATIVE}', 'ensure', '--host', 'opencode', '--project', directory],\n"
        "        { cwd: directory, stdio: 'ignore', timeout: 10000 });\n"
        "    } catch {\n"
        "      // The receipt shim never blocks a tool call.\n"
        "    }\n"
        "  },\n"
        "});\n"
    )


def install(project: Path, host: str) -> Path:
    if host == "cursor":
        target = project / CURSOR_HOOKS
        try:
            existing = json.loads(target.read_text(encoding="utf-8"))
        except (OSError, ValueError):
            existing = {}
        payload = json.dumps(cursor_hooks(existing), indent=2) + "\n"
    else:
        target = project / OPENCODE_PLUGIN
        payload = opencode_plugin()
    target.parent.mkdir(parents=True, exist_ok=True)
    temporary = target.with_name(f"{target.name}.tmp-{os.getpid()}")
    temporary.write_text(payload, encoding="utf-8")
    temporary.replace(target)
    return target


def _is_shim_entry(entry: object) -> bool:
    return isinstance(entry, dict) and "receipt_shim.py" in str(entry.get("command", ""))


def uninstall(project: Path, host: str) -> bool:
    """Remove only what `install` wrote (#6230). Returns True when something changed."""
    if host == "opencode":
        target = project / OPENCODE_PLUGIN
        if target.is_file() and not target.is_symlink():
            if target.read_text(encoding="utf-8").startswith("// ChaosEngine research-receipt shim"):
                target.unlink()
                return True
        return False
    target = project / CURSOR_HOOKS
    try:
        document = json.loads(target.read_text(encoding="utf-8"))
    except (OSError, ValueError):
        return False
    hooks = document.get("hooks") if isinstance(document, dict) else None
    if not isinstance(hooks, dict):
        return False
    changed = False
    for event in CURSOR_EVENTS:
        entries = hooks.get(event)
        if not isinstance(entries, list):
            continue
        kept = [entry for entry in entries if not _is_shim_entry(entry)]
        if len(kept) != len(entries):
            changed = True
            if kept:
                hooks[event] = kept
            else:
                del hooks[event]
    if not changed:
        return False
    if not hooks and set(document) <= {"version", "hooks"}:
        target.unlink()
    else:
        target.write_text(json.dumps(document, indent=2) + "\n", encoding="utf-8")
    return True


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    parser.add_argument("command", choices=("ensure", "install"))
    parser.add_argument("--host", required=True)
    parser.add_argument("--project", type=Path)
    args = parser.parse_args(argv)
    host = args.host.strip().casefold()
    if host not in SHIM_HOSTS:
        reason = "instruction-only" if host in INSTRUCTION_ONLY else "unsupported"
        print(f"{host} is {reason}; no receipt shim ships for it", file=sys.stderr)
        return 2
    if args.command == "install":
        print(install((args.project or Path.cwd()).resolve(), host))
        return 0
    raw = "" if sys.stdin is None or sys.stdin.isatty() else sys.stdin.read()
    project = args.project or _project_from_payload(raw, Path.cwd())
    try:
        ensure_receipt(project.resolve(), host)
    except OSError:
        pass  # Never block the host on a receipt write.
    if host == "cursor":
        print(json.dumps({"permission": "allow"}))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
