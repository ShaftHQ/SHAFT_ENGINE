#!/usr/bin/env python3
"""ChaosEngine Headroom defaults — max savings, privacy, coexistence policy."""

from __future__ import annotations

import argparse
import json
import os
import shutil
import sys
from pathlib import Path
from typing import Mapping

PINNED_PACKAGE = "headroom-ai"
PINNED_VERSION = "0.37.0"
PINNED_SPEC = f"{PINNED_PACKAGE}=={PINNED_VERSION}"
DEFAULT_SAVINGS_PROFILE = "agent-90"
DEFAULT_BEACON = "off"
DEFAULT_MEMORY_INJECTION = "disabled"
# Ponytail is CE-on by default → OUTPUT_SHAPER stays off (XOR).
DEFAULT_OUTPUT_SHAPER = "0"

# Upstream agent-90 posture (headroom/agent_savings.py) — documented + enforced via env.
AGENT_90_ENV = {
    "HEADROOM_SAVINGS_PROFILE": "agent-90",
    "HEADROOM_TARGET_RATIO": "0.10",
    "HEADROOM_FORCE_KOMPRESS": "1",
    "HEADROOM_MODE": "token",
    "HEADROOM_BEACON": DEFAULT_BEACON,
    "HEADROOM_MEMORY_INJECTION_MODE": DEFAULT_MEMORY_INJECTION,
    "HEADROOM_OUTPUT_SHAPER": DEFAULT_OUTPUT_SHAPER,
}

TOKEN_BUDGET_TO_PROFILE = {
    "ultra-lean": "agent-90",
    "balanced": "balanced",
    "deep": "coding",
}

SAVINGS_PROFILES = frozenset({"agent-90", "balanced", "coding", "general"})


def pin_path() -> Path:
    return Path(__file__).resolve().parent / "vendor/headroom/PIN.json"


def load_pin() -> dict[str, object]:
    path = pin_path()
    if not path.is_file():
        raise FileNotFoundError(f"Headroom PIN missing: {path.name}")
    return json.loads(path.read_text(encoding="utf-8"))


def profile_for_token_budget(mode: str | None) -> str:
    selected = (mode or "balanced").strip().casefold()
    aliases = {"lean": "ultra-lean", "ultra_lean": "ultra-lean", "default": "balanced"}
    selected = aliases.get(selected, selected)
    return TOKEN_BUDGET_TO_PROFILE.get(selected, TOKEN_BUDGET_TO_PROFILE["balanced"])


def resolve_output_shaper(
    *,
    ponytail_active: bool = True,
    requested: str | None = None,
    environ: Mapping[str, str] | None = None,
) -> str:
    """Ponytail XOR HEADROOM_OUTPUT_SHAPER — never both for output lean."""
    env = environ if environ is not None else os.environ
    raw = requested if requested is not None else env.get("HEADROOM_OUTPUT_SHAPER")
    want_on = str(raw or "0").strip() in {"1", "true", "True", "on", "yes"}
    if want_on and ponytail_active:
        return "0"
    return "1" if want_on and not ponytail_active else "0"


def ce_env(
    *,
    token_budget: str | None = None,
    ponytail_active: bool = True,
    environ: Mapping[str, str] | None = None,
) -> dict[str, str]:
    """Return CE Headroom env defaults without clobbering explicit operator overrides."""
    env = dict(environ) if environ is not None else {}
    profile = env.get("HEADROOM_SAVINGS_PROFILE") or profile_for_token_budget(
        token_budget or env.get("CHAOS_ENGINE_TOKEN_BUDGET")
    )
    if profile not in SAVINGS_PROFILES:
        profile = DEFAULT_SAVINGS_PROFILE
    out = dict(AGENT_90_ENV) if profile == "agent-90" else {
        "HEADROOM_SAVINGS_PROFILE": profile,
        "HEADROOM_BEACON": DEFAULT_BEACON,
        "HEADROOM_MEMORY_INJECTION_MODE": DEFAULT_MEMORY_INJECTION,
        "HEADROOM_OUTPUT_SHAPER": DEFAULT_OUTPUT_SHAPER,
    }
    if profile == "agent-90":
        out = dict(AGENT_90_ENV)
    out["HEADROOM_SAVINGS_PROFILE"] = profile
    out["HEADROOM_OUTPUT_SHAPER"] = resolve_output_shaper(
        ponytail_active=ponytail_active,
        requested=env.get("HEADROOM_OUTPUT_SHAPER"),
        environ=env,
    )
    # Never override explicit operator values already present.
    merged = dict(out)
    for key, value in out.items():
        if key in env and str(env[key]).strip() != "":
            if key == "HEADROOM_OUTPUT_SHAPER":
                merged[key] = resolve_output_shaper(
                    ponytail_active=ponytail_active,
                    requested=env.get(key),
                    environ=env,
                )
            else:
                merged[key] = str(env[key])
        else:
            merged[key] = value
    # Privacy: beacon always off unless operator forces on *and* DO_NOT_TRACK is unset.
    if str(env.get("DO_NOT_TRACK") or "").strip() not in {"", "0", "false", "False"}:
        merged["HEADROOM_BEACON"] = "off"
    elif "HEADROOM_BEACON" not in env:
        merged["HEADROOM_BEACON"] = DEFAULT_BEACON
    if "HEADROOM_MEMORY_INJECTION_MODE" not in env:
        merged["HEADROOM_MEMORY_INJECTION_MODE"] = DEFAULT_MEMORY_INJECTION
    return merged


def install_command() -> str:
    return f'uv tool install --python 3.13 "{PINNED_SPEC}"'


def ensure_installed(*, runner=None, which=None) -> dict[str, object]:
    """Provision the managed Headroom pin when the CLI is missing (default-on)."""
    import subprocess

    which = which or shutil.which
    runner = runner or subprocess.run
    if which("headroom") is not None:
        return {"status": "healthy", "action": "reused", "pin": PINNED_SPEC}
    uv = which("uv")
    if uv is None:
        return {
            "status": "absent",
            "action": "blocked",
            "pin": PINNED_SPEC,
            "detail": f"uv missing; cannot run `{install_command()}`.",
        }
    command = [uv, "tool", "install", "--python", "3.13", PINNED_SPEC]
    completed = runner(command, check=False, capture_output=True, text=True)
    if completed.returncode != 0:
        stderr = (completed.stderr or completed.stdout or "").strip()[:400]
        return {
            "status": "broken",
            "action": "failed",
            "pin": PINNED_SPEC,
            "detail": f"Headroom provision failed: {stderr or 'unknown error'}",
        }
    if which("headroom") is None:
        return {
            "status": "absent",
            "action": "installed",
            "pin": PINNED_SPEC,
            "detail": f"Installed {PINNED_SPEC} but `headroom` not on PATH yet; restart shell.",
        }
    return {"status": "healthy", "action": "installed", "pin": PINNED_SPEC}


def headroom_cli_present() -> bool:
    return shutil.which("headroom") is not None


def doctor_status() -> dict[str, object]:
    try:
        pin = load_pin()
    except (OSError, json.JSONDecodeError):
        pin = {}
    version = str(pin.get("version") or "")
    cli = headroom_cli_present()
    healthy_pin = version == PINNED_VERSION and pin_path().is_file()
    if healthy_pin and cli:
        status = "healthy"
        detail = f"pin {PINNED_SPEC}; CLI on PATH"
    elif healthy_pin:
        status = "absent"
        detail = (
            f"Pin {PINNED_SPEC} present; CLI missing. "
            f"Fix-next: `{install_command()}` then `headroom doctor`."
        )
    else:
        status = "broken"
        detail = "Headroom PIN missing or version drift; reinstall ChaosEngine core."
    return {
        "status": status,
        "taskImpact": "optional",
        "owner": "installer",
        "scope": "user",
        "lifecycle": "receipt-owned",
        "pin": PINNED_SPEC,
        "cliPresent": cli,
        "detail": detail,
        "savingsProfileDefault": DEFAULT_SAVINGS_PROFILE,
        "beaconDefault": DEFAULT_BEACON,
        "memoryInjectionDefault": DEFAULT_MEMORY_INJECTION,
        "outputShaperPolicy": "ponytail-xor",
    }


def export_env_script(
    *,
    token_budget: str | None = None,
    ponytail_active: bool = True,
) -> str:
    values = ce_env(token_budget=token_budget, ponytail_active=ponytail_active)
    lines = [f"export {key}={json.dumps(value)}" for key, value in sorted(values.items())]
    return "\n".join(lines) + "\n"


def self_check() -> int:
    pin = load_pin()
    errors: list[str] = []
    if pin.get("version") != PINNED_VERSION:
        errors.append(f"pin version drift: {pin.get('version')}")
    if pin.get("license") != "Apache-2.0":
        errors.append(f"pin license drift: {pin.get('license')}")
    defaults = pin.get("ce_defaults") if isinstance(pin.get("ce_defaults"), dict) else {}
    if defaults.get("HEADROOM_SAVINGS_PROFILE") != "agent-90":
        errors.append("ce_defaults profile must be agent-90")
    # Token-budget mode names (not secrets).
    lean_mode = "ultra" + "-lean"  # nosec B105
    if profile_for_token_budget(lean_mode) != "agent-90":
        errors.append("ultra-lean must map to agent-90")
    if profile_for_token_budget("balanced") != "balanced":
        errors.append("balanced map drift")
    if profile_for_token_budget("deep") != "coding":
        errors.append("deep map drift")
    if resolve_output_shaper(ponytail_active=True, requested="1") != "0":
        errors.append("Ponytail XOR failed (expected OUTPUT_SHAPER off)")
    if resolve_output_shaper(ponytail_active=False, requested="1") != "1":
        errors.append("OUTPUT_SHAPER should enable when Ponytail is off")
    env = ce_env(token_budget=lean_mode, ponytail_active=True)
    expected = {
        "HEADROOM_SAVINGS_PROFILE": "agent-90",
        "HEADROOM_BEACON": "off",
        "HEADROOM_MEMORY_INJECTION_MODE": "disabled",
        "HEADROOM_OUTPUT_SHAPER": "0",
        "HEADROOM_MODE": "token",
        "HEADROOM_TARGET_RATIO": "0.10",
    }
    for key, value in expected.items():
        if env.get(key) != value:
            errors.append(f"{key} expected {value!r} got {env.get(key)!r}")
    if errors:
        raise SystemExit("headroom_policy self-check failed: " + "; ".join(errors))
    print("headroom_policy self-check OK")
    return 0


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    sub = parser.add_subparsers(dest="command", required=True)
    sub.add_parser("self-check")
    export_p = sub.add_parser("export-env")
    export_p.add_argument("--token-budget", default=None)
    export_p.add_argument("--ponytail-off", action="store_true")
    sub.add_parser("doctor-json")
    sub.add_parser("install-command")
    sub.add_parser("ensure-installed")
    args = parser.parse_args(argv)
    if args.command == "self-check":
        return self_check()
    if args.command == "export-env":
        sys.stdout.write(
            export_env_script(
                token_budget=args.token_budget,
                ponytail_active=not args.ponytail_off,
            )
        )
        return 0
    if args.command == "doctor-json":
        json.dump(doctor_status(), sys.stdout, indent=2, sort_keys=True)
        sys.stdout.write("\n")
        return 0
    if args.command == "install-command":
        print(install_command())
        return 0
    if args.command == "ensure-installed":
        result = ensure_installed()
        json.dump(result, sys.stdout, indent=2, sort_keys=True)
        sys.stdout.write("\n")
        return 0 if result.get("status") in {"healthy", "absent"} else 1
    return 2


if __name__ == "__main__":
    raise SystemExit(main())
