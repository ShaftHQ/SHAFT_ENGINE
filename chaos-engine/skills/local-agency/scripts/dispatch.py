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

SCRIPT_DIR = Path(__file__).resolve().parent
SKILLS_ROOT = SCRIPT_DIR.parents[1]

# Prefer FreeToken, then OpenAI-compat peers. Order is intentional.
RUNTIME_RANK = ("freetoken", "ollama", "lmstudio", "llamacpp")

FREETOKEN_OPENAI_BASE = "http://127.0.0.1:1919/v1"
FREETOKEN_MODELS_URL = "http://127.0.0.1:1919/v1/models"

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
    order = _prefer_order(prefer)
    probed = [probe_runtime(runtime) for runtime in order]
    payload = _empty_payload(order, probed, allow_cloud)
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
        "No READY local runtime. Start FreeToken/Ollama/LM Studio/llamacpp "
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
) -> list[str]:
    """Build ``opencode run`` argv. Caller sets OPENCODE_CONFIG for the process."""
    argv = ["opencode", "run"]
    if pure:
        argv.append("--pure")
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
    print(json.dumps(out, sort_keys=True))
    return 0


def cmd_argv(args: argparse.Namespace) -> int:
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
    print(json.dumps(out, sort_keys=True))
    return 0


def parse_args(argv: list[str] | None = None) -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--prefer",
        choices=RUNTIME_RANK,
        default=None,
        help="prefer this READY runtime first (default: FreeToken then peers)",
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
    argv_p = sub.add_parser("argv", help="print env + opencode run argv (does not execute)")
    argv_p.add_argument("--prompt", required=True, help="message for opencode run")
    argv_p.add_argument("--workdir", default=None, help="--dir for opencode")
    argv_p.add_argument("--dir", default=None, help="directory for ephemeral opencode.json")
    argv_p.add_argument("--auto", action="store_true", help="pass --auto to opencode")
    argv_p.add_argument("--no-pure", action="store_true", help="omit --pure")
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
    print(f"unknown command: {command}", file=sys.stderr)
    return 2


if __name__ == "__main__":
    raise SystemExit(main())
