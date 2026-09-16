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


def _load_module(name: str, path: Path) -> ModuleType:
    spec = importlib.util.spec_from_file_location(name, path)
    if spec is None or spec.loader is None:
        raise RuntimeError(f"unable to load {path}")
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def freetoken_probe() -> ModuleType:
    return _load_module("freetoken_probe", SKILLS_ROOT / "freetoken/scripts/probe.py")


def local_openai_probe() -> ModuleType:
    return _load_module(
        "local_openai_compat_probe",
        SKILLS_ROOT / "local-openai-compat/scripts/probe.py",
    )


def loopback_openai_base(url: str) -> bool:
    """Accept only http loopback OpenAI-compat base URLs ending in /v1."""
    parsed = urlsplit(url)
    if parsed.scheme != "http" or parsed.username or parsed.password:
        return False
    host = (parsed.hostname or "").lower()
    if host not in {"127.0.0.1", "localhost", "::1"}:
        return False
    if parsed.path.rstrip("/") != "/v1":
        return False
    if parsed.query or parsed.fragment:
        return False
    return True


def agency_present(binary: str = "opencode") -> bool:
    """Whether a local agency CLI is on PATH. Never executes it."""
    return shutil.which(binary) is not None


def probe_runtime(runtime: str) -> dict[str, object]:
    """Return state / base URL / models for one ranked runtime."""
    if runtime == "freetoken":
        ft = freetoken_probe()
        state = ft.probe(FREETOKEN_MODELS_URL)
        models: list[str] = []
        if state == "READY":
            answered, body = ft.fetch_models(FREETOKEN_MODELS_URL)
            if answered and ft.is_models_payload(body):
                models = ft.parse_model_ids(body)
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
    models = []
    if state == "READY":
        answered, body = loc.fetch_models(loc.default_url(runtime))
        if answered and loc.is_models_payload(body):
            models = loc.parse_model_ids(body)
    return {
        "runtime": runtime,
        "state": state,
        "openai_base_url": OPENAI_COMPAT_BASES[runtime],
        "models": models,
        "provider_id": runtime,
    }


def resolve_local(
    *,
    prefer: str | None = None,
    model: str | None = None,
    allow_cloud: bool = False,
) -> dict[str, object]:
    """Pick the first READY local runtime. Never silent OmniRoute fallback."""
    order = list(RUNTIME_RANK)
    if prefer is not None:
        if prefer not in RUNTIME_RANK:
            raise ValueError(f"unknown prefer runtime: {prefer}")
        order = [prefer] + [item for item in order if item != prefer]

    probed = [probe_runtime(runtime) for runtime in order]
    chosen = next((row for row in probed if row["state"] == "READY"), None)

    payload: dict[str, object] = {
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

    if chosen is None:
        payload["state"] = "ABSENT"
        payload["chosen"] = None
        payload["advice"] = (
            "No READY local runtime. Start FreeToken/Ollama/LM Studio/llamacpp "
            "yourself, or ask explicitly for cloud OmniRoute / session agents."
        )
        if allow_cloud:
            payload["advice"] += " --allow-cloud set: caller may use OmniRoute deliberately."
        return payload

    models = list(chosen["models"])  # type: ignore[arg-type]
    selected_model = model if model else (models[0] if models else None)
    if model and models and model not in models:
        payload["state"] = "UNHEALTHY"
        payload["chosen"] = None
        payload["advice"] = f"model {model!r} not listed by READY runtime {chosen['runtime']}"
        return payload
    if selected_model is None:
        payload["state"] = "UNHEALTHY"
        payload["chosen"] = None
        payload["advice"] = f"READY runtime {chosen['runtime']} listed no models"
        return payload

    provider_id = str(chosen["provider_id"])
    base = str(chosen["openai_base_url"])
    if not loopback_openai_base(base):
        payload["state"] = "UNHEALTHY"
        payload["chosen"] = None
        payload["advice"] = "non-loopback OpenAI base rejected"
        return payload

    payload["state"] = "READY"
    payload["chosen"] = {
        "runtime": chosen["runtime"],
        "provider_id": provider_id,
        "openai_base_url": base,
        "model": selected_model,
        "opencode_model": f"{provider_id}/{selected_model}",
    }
    return payload


def opencode_config(chosen: dict[str, object]) -> dict[str, object]:
    """Build an ephemeral OpenCode config fragment (npm openai-compatible schema)."""
    provider_id = str(chosen["provider_id"])
    model = str(chosen["model"])
    base = str(chosen["openai_base_url"])
    return {
        "$schema": "https://opencode.ai/config.json",
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
    """Write OpenCode config under a temp dir. Never touches ~/.config/opencode."""
    root = Path(directory) if directory is not None else Path(tempfile.mkdtemp(prefix="ce-local-agency-"))
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


def cmd_resolve(args: argparse.Namespace) -> int:
    payload = resolve_local(prefer=args.prefer, model=args.model, allow_cloud=args.allow_cloud)
    print(json.dumps(payload, sort_keys=True))
    return 0 if payload.get("state") == "READY" else 1


def cmd_config(args: argparse.Namespace) -> int:
    payload = resolve_local(prefer=args.prefer, model=args.model, allow_cloud=args.allow_cloud)
    if payload.get("state") != "READY" or not isinstance(payload.get("chosen"), dict):
        print(json.dumps(payload, sort_keys=True))
        return 1
    chosen = payload["chosen"]
    assert isinstance(chosen, dict)
    path = write_ephemeral_config(chosen, Path(args.dir) if args.dir else None)
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
    }
    print(json.dumps(out, sort_keys=True))
    return 0


def cmd_argv(args: argparse.Namespace) -> int:
    payload = resolve_local(prefer=args.prefer, model=args.model, allow_cloud=args.allow_cloud)
    if payload.get("state") != "READY" or not isinstance(payload.get("chosen"), dict):
        print(json.dumps(payload, sort_keys=True))
        return 1
    chosen = payload["chosen"]
    assert isinstance(chosen, dict)
    path = write_ephemeral_config(chosen, Path(args.dir) if args.dir else None)
    argv = opencode_argv(
        chosen,
        prompt=args.prompt,
        workdir=args.workdir,
        auto=args.auto,
        pure=not args.no_pure,
    )
    out = {
        "state": "READY",
        "chosen": chosen,
        "opencode_config_path": str(path),
        "env": {"OPENCODE_CONFIG": str(path)},
        "argv": argv,
        "durable_config_rewrite": False,
        "may_ft_launch": False,
        "omniroute_fallback": False,
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
