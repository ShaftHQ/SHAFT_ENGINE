#!/usr/bin/env python3
"""Resolve a READY local runtime and emit ephemeral OpenCode dispatch material.

Stdlib only. Never installs OpenCode, never starts servers, never invokes FreeToken
launch/serve helpers, and never falls back to OmniRoute unless the caller passes
``--allow-cloud``.
"""

from __future__ import annotations

import argparse
import importlib.util
import json
import shutil
import sys
import tempfile
from pathlib import Path
from types import ModuleType
from urllib.parse import urlsplit
from urllib.request import Request, urlopen

SCRIPT_DIR = Path(__file__).resolve().parent
SKILLS_ROOT = SCRIPT_DIR.parents[1]

CE_BRIEF_PATH = SKILLS_ROOT.parent / "ce_brief.py"


def load_ce_brief():
    """Import chaos-engine/ce_brief.py by path (stdlib only)."""
    return _load_module("ce_brief", CE_BRIEF_PATH)


def attach_ce_brief(out: dict[str, object], project: Path | None = None) -> dict[str, object]:
    """Attach locator-only CE brief payload under ce_brief (#6068)."""
    brief = load_ce_brief().build_brief(project=project)
    out["ce_brief"] = brief
    return out


# Prefer FreeToken, then OpenAI-compat peers. Order is intentional.
RUNTIME_RANK = ("freetoken", "ollama", "lmstudio", "llamacpp", "colibri")

FREETOKEN_OPENAI_BASE = "http://127.0.0.1:1919/v1"
FREETOKEN_MODELS_URL = "http://127.0.0.1:1919/v1/models"

COLIBRI_OPENAI_BASE = "http://127.0.0.1:8000/v1"
COLIBRI_MODELS_URL = "http://127.0.0.1:8000/v1/models"

OPENAI_COMPAT_BASES = {
    "ollama": "http://127.0.0.1:11434/v1",
    "lmstudio": "http://127.0.0.1:1234/v1",
    "llamacpp": "http://127.0.0.1:8080/v1",
}


DURABLE_OPENCODE_MARKERS = (".config/opencode", ".opencode")


def is_durable_opencode_dir(directory: Path) -> bool:
    """True when a write would land in a durable OpenCode config location."""
    resolved = directory.expanduser().resolve()
    parts = {part.lower() for part in resolved.parts}
    if "opencode" in parts and (".config" in parts or "xdg" in str(resolved).lower()):
        return True
    rendered = str(resolved).replace("\\", "/").lower()
    return any(marker in rendered for marker in DURABLE_OPENCODE_MARKERS)



REQUIRED_CE_POINTERS = (
    "AGENTS.md",
    ".agents/skills/chaos-engine/SKILL.md",
)


def require_ce_pointers(project: Path | None = None) -> dict[str, object]:
    """Fail closed when project CE pointers for OpenCode are missing (#6070).

    ``--pure`` only disables plugins; it does not load ChaosEngine. OpenCode still
    needs project pointers (AGENTS.md + chaos-engine skill adapter) in the worktree.
    """
    root = Path(project).expanduser().resolve() if project is not None else Path.cwd().resolve()
    missing: list[str] = []
    for relative in REQUIRED_CE_POINTERS:
        if not (root / relative).is_file():
            missing.append(relative)
    if missing:
        return {
            "ok": False,
            "project": str(root),
            "missing": missing,
            "advice": (
                "ChaosEngine project pointers missing for OpenCode: "
                + ", ".join(missing)
                + ". Install/activate ChaosEngine in this worktree "
                "(python3 .chaos-engine/install.py doctor --project .), then retry. "
                "Note: opencode --pure only disables plugins; it does not replace CE pointers (#6070)."
            ),
        }
    return {"ok": True, "project": str(root), "missing": []}


def _project_root_from_args(args: argparse.Namespace) -> Path | None:
    """Prefer explicit --project, else argv --workdir, else None (cwd)."""
    project = getattr(args, "project", None)
    if project:
        return Path(project)
    workdir = getattr(args, "workdir", None)
    if workdir:
        return Path(workdir)
    return None



def _load_module(name: str, path: Path) -> ModuleType:
    """Load a sibling probe module by path."""
    spec = importlib.util.spec_from_file_location(name, path)
    if spec is None or spec.loader is None:
        raise RuntimeError(f"unable to load {path}")
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def freetoken_probe() -> ModuleType:
    """Import the FreeToken probe helper."""
    return _load_module("freetoken_probe", SKILLS_ROOT / "freetoken/scripts/probe.py")


def colibri_probe() -> ModuleType:
    """Import the Colibri probe helper."""
    return _load_module("colibri_probe", SKILLS_ROOT / "colibri/scripts/probe.py")


def rog_freetoken_gate() -> ModuleType:
    """Import the ROG FreeToken host gate (#6021)."""
    return _load_module("require_rog_freetoken", SCRIPT_DIR / "require_rog_freetoken.py")


def local_openai_probe() -> ModuleType:
    """Import the local OpenAI-compat probe helper."""
    return _load_module(
        "local_openai_compat_probe",
        SKILLS_ROOT / "local-openai-compat/scripts/probe.py",
    )


def loopback_openai_base(url: str) -> bool:
    """Accept only http loopback OpenAI-compat base URLs ending in /v1."""
    parsed = urlsplit(url)
    host = (parsed.hostname or "").lower()
    return (
        parsed.scheme == "http"
        and not parsed.username
        and not parsed.password
        and host in {"127.0.0.1", "localhost", "::1"}
        and parsed.path.rstrip("/") == "/v1"
        and not parsed.query
        and not parsed.fragment
    )


def agency_present(binary: str = "opencode") -> bool:
    """Whether a local agency CLI is on PATH. Never executes it."""
    return shutil.which(binary) is not None


def _ready_models(fetch_models, is_models_payload, parse_model_ids, models_url: str) -> list[str]:
    answered, body = fetch_models(models_url)
    if answered and is_models_payload(body):
        return parse_model_ids(body)
    return []


def probe_runtime(runtime: str) -> dict[str, object]:
    """Return state / base URL / models for one ranked runtime."""
    if runtime == "freetoken":
        ft = freetoken_probe()
        state = ft.probe(FREETOKEN_MODELS_URL)
        models = _ready_models(ft.fetch_models, ft.is_models_payload, ft.parse_model_ids, FREETOKEN_MODELS_URL) if state == "READY" else []
        return {
            "runtime": "freetoken",
            "state": state,
            "openai_base_url": FREETOKEN_OPENAI_BASE,
            "models": models,
            "provider_id": "freetoken",
        }

    if runtime == "colibri":
        coli = colibri_probe()
        state = coli.probe(COLIBRI_MODELS_URL)
        models = (
            _ready_models(coli.fetch_models, coli.is_models_payload, coli.parse_model_ids, COLIBRI_MODELS_URL)
            if state == "READY"
            else []
        )
        return {
            "runtime": "colibri",
            "state": state,
            "openai_base_url": COLIBRI_OPENAI_BASE,
            "models": models,
            "provider_id": "colibri",
        }

    if runtime not in OPENAI_COMPAT_BASES:
        raise ValueError(f"unknown runtime: {runtime}")

    loc = local_openai_probe()
    state = loc.probe_backend(runtime)
    models = (
        _ready_models(loc.fetch_models, loc.is_models_payload, loc.parse_model_ids, loc.default_url(runtime))
        if state == "READY"
        else []
    )
    return {
        "runtime": runtime,
        "state": state,
        "openai_base_url": OPENAI_COMPAT_BASES[runtime],
        "models": models,
        "provider_id": runtime,
    }


def _prefer_order(prefer: str | None) -> list[str]:
    order = list(RUNTIME_RANK)
    if prefer is None:
        return order
    if prefer not in RUNTIME_RANK:
        raise ValueError(f"unknown prefer runtime: {prefer}")
    return [prefer] + [item for item in order if item != prefer]


def _empty_payload(order: list[str], probed: list[dict[str, object]], allow_cloud: bool) -> dict[str, object]:
    return {
        "mode": "local-agency",
        "agency_cli": "opencode",
        "agency_on_path": agency_present(),
        "prefer_order": order,
        "probed": [{"runtime": r["runtime"], "state": r["state"]} for r in probed],
        "install": False,
        "may_ft_launch": False,
        "may_ft_serve": False,
        "may_start_server": False,
        "omniroute_fallback": False,
        "allow_cloud": allow_cloud,
        "durable_config_rewrite": False,
    }


def _select_model(chosen: dict[str, object], model: str | None) -> tuple[str | None, str | None]:
    models = list(chosen["models"])  # type: ignore[arg-type]
    if model and models and model not in models:
        return None, f"model {model!r} not listed by READY runtime {chosen['runtime']}"
    if model:
        return model, None
    if models:
        return models[0], None
    return None, f"READY runtime {chosen['runtime']} listed no models"


def resolve_local(
    *,
    prefer: str | None = None,
    model: str | None = None,
    allow_cloud: bool = False,
) -> dict[str, object]:
    """Pick the first READY local runtime. Never silent OmniRoute fallback."""
    if prefer == "freetoken":
        gate = rog_freetoken_gate().require_rog_bound()
        if not gate.get("bound"):
            order = _prefer_order(prefer)
            payload = _empty_payload(order, [], allow_cloud)
            payload["state"] = "UNHEALTHY"
            payload["chosen"] = None
            payload["rog_gate"] = gate
            payload["advice"] = gate.get(
                "advice",
                "ROG FreeToken gate failed; process-owner Shell with machineId required (#6021).",
            )
            return payload
    order = _prefer_order(prefer)
    probed = [probe_runtime(runtime) for runtime in order]
    payload = _empty_payload(order, probed, allow_cloud)
    if prefer == "freetoken":
        ft_rows = [row for row in probed if row["runtime"] == "freetoken"]
        ft_state = str(ft_rows[0]["state"]) if ft_rows else "ABSENT"
        if ft_state != "READY":
            payload["state"] = "UNHEALTHY"
            payload["chosen"] = None
            payload["rog_gate"] = {"prefer": "freetoken", "freetoken_state": ft_state}
            payload["advice"] = (
                f"FreeToken not READY on this host (state={ft_state}). "
                "Probe http://127.0.0.1:1919/v1/models on ROG via process-owner "
                "Shell with machineId; Task/box writers must not claim FreeToken "
                "(#6021). Start FreeToken yourself on ROG, or omit --prefer freetoken."
            )
            return payload
    last_error = None
    for chosen in (row for row in probed if row["state"] == "READY"):
        selected_model, error = _select_model(chosen, model)
        if error is not None:
            last_error = error
            continue
        provider_id = str(chosen["provider_id"])
        base = str(chosen["openai_base_url"])
        if not loopback_openai_base(base):
            last_error = "non-loopback OpenAI base rejected"
            continue
        payload["state"] = "READY"
        payload["chosen"] = {
            "runtime": chosen["runtime"],
            "provider_id": provider_id,
            "openai_base_url": base,
            "model": selected_model,
            "opencode_model": f"{provider_id}/{selected_model}",
        }
        return payload

    if last_error is not None:
        payload["state"] = "UNHEALTHY"
        payload["chosen"] = None
        payload["advice"] = last_error
        return payload

    payload["state"] = "ABSENT"
    payload["chosen"] = None
    advice = (
        "No READY local runtime. Start FreeToken/Ollama/LM Studio/llamacpp/Colibri "
        "yourself, or ask explicitly for cloud OmniRoute / session agents."
    )
    if allow_cloud:
        advice += " --allow-cloud set: caller may use OmniRoute deliberately."
    payload["advice"] = advice
    return payload


def opencode_config(chosen: dict[str, object]) -> dict[str, object]:
    """Build an ephemeral OpenCode config fragment (npm openai-compatible schema)."""
    provider_id = str(chosen["provider_id"])
    model = str(chosen["model"])
    base = str(chosen["openai_base_url"])
    return {
        "$schema": "https://opencode.ai/config.json",
        # Restrict this process to the local provider. OpenCode merges configs;
        # without this allowlist, durable global providers can remain reachable.
        "enabled_providers": [provider_id],
        "provider": {
            provider_id: {
                "npm": "@ai-sdk/openai-compatible",
                "name": f"{provider_id} (local)",
                "options": {
                    "baseURL": base,
                    "apiKey": "local",
                },
                "models": {
                    model: {"name": model},
                },
            }
        },
        "model": f"{provider_id}/{model}",
    }


def write_ephemeral_config(chosen: dict[str, object], directory: Path | None = None) -> Path:
    """Write OpenCode config under a temp dir. Refuse durable OpenCode paths."""
    if directory is None:
        root = Path(tempfile.mkdtemp(prefix="ce-local-agency-"))
    else:
        root = Path(directory)
        if is_durable_opencode_dir(root):
            raise ValueError("refusing durable OpenCode config directory")
    root.mkdir(parents=True, exist_ok=True)
    path = root / "opencode.json"
    path.write_text(json.dumps(opencode_config(chosen), indent=2, sort_keys=True) + "\n", encoding="utf-8")
    return path


def opencode_argv(
    chosen: dict[str, object],
    *,
    prompt: str,
    workdir: str | None = None,
    auto: bool = False,
    pure: bool = True,
    variant: str | None = "medium",
) -> list[str]:
    """Build ``opencode run`` argv. Caller sets OPENCODE_CONFIG for the process."""
    argv = ["opencode", "run"]
    if pure:
        argv.append("--pure")
    if variant:
        argv.extend(["--variant", variant])
    if auto:
        argv.append("--auto")
    argv.extend(["--model", str(chosen["opencode_model"])])
    if workdir:
        argv.extend(["--dir", workdir])
    argv.append(prompt)
    return argv


def _chosen_or_fail(payload: dict[str, object]) -> dict[str, object] | None:
    chosen = payload.get("chosen")
    if payload.get("state") == "READY" and isinstance(chosen, dict):
        return chosen
    print(json.dumps(payload, sort_keys=True))
    return None


def cmd_resolve(args: argparse.Namespace) -> int:
    payload = resolve_local(prefer=args.prefer, model=args.model, allow_cloud=args.allow_cloud)
    print(json.dumps(payload, sort_keys=True))
    return 0 if payload.get("state") == "READY" else 1


def cmd_config(args: argparse.Namespace) -> int:
    pointers = require_ce_pointers(_project_root_from_args(args))
    if not pointers.get("ok"):
        print(json.dumps({"state": "UNHEALTHY", "ce_pointers": pointers, "durable_config_rewrite": False}, sort_keys=True))
        return 2
    payload = resolve_local(prefer=args.prefer, model=args.model, allow_cloud=args.allow_cloud)
    chosen = _chosen_or_fail(payload)
    if chosen is None:
        return 1
    try:
        path = write_ephemeral_config(chosen, Path(args.dir) if args.dir else None)
    except ValueError as error:
        print(json.dumps({"state": "UNHEALTHY", "advice": str(error), "durable_config_rewrite": True}, sort_keys=True))
        return 2
    content = json.dumps(opencode_config(chosen), separators=(",", ":"), sort_keys=True)
    out = {
        "state": "READY",
        "chosen": chosen,
        "opencode_config_path": str(path),
        "env": {
            "OPENCODE_CONFIG": str(path),
            "OPENCODE_CONFIG_CONTENT": content,
        },
        "durable_config_rewrite": False,
        "may_ft_launch": False,
        "omniroute_fallback": False,
        "note": "OpenCode merges configs; enabled_providers limits this process to the local provider",
    }
    if getattr(args, "with_ce_brief", False):
        attach_ce_brief(out, project=Path(args.project) if getattr(args, "project", None) else None)
    print(json.dumps(out, sort_keys=True))
    return 0


def cmd_argv(args: argparse.Namespace) -> int:
    pointers = require_ce_pointers(_project_root_from_args(args))
    if not pointers.get("ok"):
        print(json.dumps({"state": "UNHEALTHY", "ce_pointers": pointers, "durable_config_rewrite": False}, sort_keys=True))
        return 2
    payload = resolve_local(prefer=args.prefer, model=args.model, allow_cloud=args.allow_cloud)
    chosen = _chosen_or_fail(payload)
    if chosen is None:
        return 1
    try:
        path = write_ephemeral_config(chosen, Path(args.dir) if args.dir else None)
    except ValueError as error:
        print(json.dumps({"state": "UNHEALTHY", "advice": str(error), "durable_config_rewrite": True}, sort_keys=True))
        return 2
    argv = opencode_argv(
        chosen,
        prompt=args.prompt,
        workdir=args.workdir,
        auto=args.auto,
        pure=not args.no_pure,
        variant=args.variant,
    )
    content = json.dumps(opencode_config(chosen), separators=(",", ":"), sort_keys=True)
    out = {
        "state": "READY",
        "chosen": chosen,
        "opencode_config_path": str(path),
        "env": {
            "OPENCODE_CONFIG": str(path),
            "OPENCODE_CONFIG_CONTENT": content,
        },
        "argv": argv,
        "durable_config_rewrite": False,
        "may_ft_launch": False,
        "omniroute_fallback": False,
        "note": "OpenCode merges configs; enabled_providers limits this process to the local provider",
    }
    if getattr(args, "with_ce_brief", False):
        attach_ce_brief(out, project=Path(args.project) if getattr(args, "project", None) else None)
    print(json.dumps(out, sort_keys=True))
    return 0


def cmd_brief(args: argparse.Namespace) -> int:
    """Print CE brief text or JSON (#6068)."""
    project = Path(args.project) if getattr(args, "project", None) else None
    brief = load_ce_brief().build_brief(project=project)
    if getattr(args, "json", False):
        print(json.dumps(brief, sort_keys=True))
    else:
        sys.stdout.write(str(brief["text"]))
    return 0


def cmd_chat(args: argparse.Namespace) -> int:
    """POST one chat completion to a READY local OpenAI-compat runtime (#6068)."""
    payload = resolve_local(prefer=args.prefer, model=args.model, allow_cloud=args.allow_cloud)
    chosen = _chosen_or_fail(payload)
    if chosen is None:
        return 1
    messages: list[dict[str, str]] = []
    if getattr(args, "with_ce_brief", False):
        project = Path(args.project) if getattr(args, "project", None) else None
        brief = load_ce_brief().build_brief(project=project)
        messages.append({"role": "system", "content": str(brief["text"])})
    messages.append({"role": "user", "content": args.prompt})
    body = {
        "model": str(chosen["model"]),
        "messages": messages,
    }
    base = str(chosen["openai_base_url"]).rstrip("/")
    url = f"{base}/chat/completions"
    req = Request(
        url,
        data=json.dumps(body).encode("utf-8"),
        headers={"Content-Type": "application/json", "Authorization": "Bearer local"},
        method="POST",
    )
    with urlopen(req, timeout=120) as response:
        raw = response.read()
    print(raw.decode("utf-8"))
    return 0


def parse_args(argv: list[str] | None = None) -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--prefer",
        choices=RUNTIME_RANK,
        default=None,
        help="prefer this READY runtime first (default: FreeToken, OpenAI-compat peers, then Colibri)",
    )
    parser.add_argument("--model", default=None, help="optional model id from the READY runtime")
    parser.add_argument(
        "--allow-cloud",
        action="store_true",
        help="acknowledge explicit cloud OmniRoute permission; still does not dispatch it",
    )
    sub = parser.add_subparsers(dest="command")
    sub.add_parser("resolve", help="JSON readiness + chosen local runtime (default)")
    config_p = sub.add_parser("config", help="write ephemeral OpenCode config; print env")
    config_p.add_argument("--dir", default=None, help="directory for opencode.json (default: temp)")
    config_p.add_argument("--project", default=None, help="project root for CE brief locators")
    config_p.add_argument("--with-ce-brief", action="store_true", help="attach locator-only CE brief JSON")
    argv_p = sub.add_parser("argv", help="print env + opencode run argv (does not execute)")
    argv_p.add_argument("--prompt", required=True, help="message for opencode run")
    argv_p.add_argument("--workdir", default=None, help="--dir for opencode")
    argv_p.add_argument("--dir", default=None, help="directory for ephemeral opencode.json")
    argv_p.add_argument("--auto", action="store_true", help="pass --auto to opencode")
    argv_p.add_argument("--no-pure", action="store_true", help="omit --pure")
    argv_p.add_argument(
        "--variant",
        default="medium",
        choices=("low", "medium", "high"),
        help="OpenCode --variant for tool loops (default: medium)",
    )
    argv_p.add_argument("--project", default=None, help="project root for CE brief locators")
    argv_p.add_argument("--with-ce-brief", action="store_true", help="attach locator-only CE brief JSON")
    brief_p = sub.add_parser("brief", help="print locator-only CE system brief (#6068)")
    brief_p.add_argument("--project", default=None, help="project root")
    brief_p.add_argument("--json", action="store_true", help="print brief receipt JSON")
    chat_p = sub.add_parser("chat", help="POST chat completion to READY local runtime (#6068)")
    chat_p.add_argument("--prompt", required=True, help="user message")
    chat_p.add_argument("--project", default=None, help="project root for CE brief")
    chat_p.add_argument("--with-ce-brief", action="store_true", help="set system= from CE brief")
    return parser.parse_args(argv)


def main(argv: list[str] | None = None) -> int:
    args = parse_args(argv)
    command = args.command or "resolve"
    if command == "resolve":
        return cmd_resolve(args)
    if command == "config":
        return cmd_config(args)
    if command == "argv":
        return cmd_argv(args)
    if command == "brief":
        return cmd_brief(args)
    if command == "chat":
        return cmd_chat(args)
    print(f"unknown command: {command}", file=sys.stderr)
    return 2


if __name__ == "__main__":
    raise SystemExit(main())
