#!/usr/bin/env python3
"""Windows ``py -3`` shim.

Generated hooks call ``py -3 <script>``. The cmd launcher must not pass that
``-3`` through to CPython: it is a launcher flag, and ``python.exe`` rejects it.
``shift`` does not rewrite ``%*``, so the cmd file delegates to this script and
this script drops a leading ``-3`` before exec.

``render_py_cmd`` is the text ``install.ps1`` writes. Tests execute that text.
"""

from __future__ import annotations

import os
import sys


def strip_launcher_flag(args: list[str]) -> list[str]:
    """Drop one leading launcher-only ``-3``. Leave every other argument."""
    if args and args[0] == "-3":
        return list(args[1:])
    return list(args)


def render_py_cmd(interpreter: str, shim: str) -> str:
    """Cmd text. The interpreter sees this script, then the interpreter path, then user args.

    ``%*`` is the user argv (including a leading ``-3``). It is an argument to
    this script, not a CPython option.
    """

    def quote(value: str) -> str:
        return '"' + value.replace('"', '""') + '"'

    return (
        "@echo off\r\n"
        f"{quote(interpreter)} {quote(shim)} {quote(interpreter)} %*\r\n"
    )


LAUNCHER_SOURCE = """import os
import sys

args = sys.argv[1:]
if not args:
    sys.stderr.write("py shim missing interpreter\\n")
    raise SystemExit(2)
interpreter = args[0]
user = args[1:]
if user and user[0] == "-3":
    user = user[1:]
if os.name == "nt":
    import subprocess
    raise SystemExit(subprocess.run([interpreter, *user]).returncode)
os.execv(interpreter, [interpreter, *user])
"""


def payload_from_installer(text: str) -> str:
    """Python launcher embedded in ``install.ps1``. No source-tree copy."""
    import re

    start = text.find("# SHIM-PY-START")
    end = text.find("# SHIM-PY-END", start)
    if start < 0 or end < 0:
        raise ValueError("installer shim payload missing")
    match = re.search(r"@'(.*?)'@", text[start:end], re.S)
    if match is None:
        raise ValueError("installer shim payload missing")
    payload = match.group(1).replace("\r\n", "\n")
    if payload.startswith("\n"):
        payload = payload[1:]
    if not payload.endswith("\n"):
        payload += "\n"
    return payload


def provision_from_installer(installer: str, interpreter: str, shim_dir: str) -> str:
    """Write the launcher from the installer payload into an empty target."""
    import re

    with open(installer, encoding="utf-8") as source:
        text = source.read()
    if re.search(r"Copy-Item[^\n]*windows_py_shim", text):
        raise SystemExit("installer still copies the shim from a source checkout")
    payload = payload_from_installer(text)
    if payload != LAUNCHER_SOURCE:
        raise SystemExit("installer payload drifted from the launcher")
    destination = os.path.join(shim_dir, "windows_py_shim.py")
    os.makedirs(shim_dir, exist_ok=True)
    with open(destination, "w", encoding="utf-8", newline="\n") as handle:
        handle.write(payload)
    cmd_path = os.path.join(shim_dir, "py.cmd")
    with open(cmd_path, "w", encoding="utf-8", newline="\n") as handle:
        handle.write(render_py_cmd(interpreter, destination))
    quoted = '"' + interpreter.replace('"', '""') + '"'
    with open(os.path.join(shim_dir, "python.cmd"), "w", encoding="utf-8", newline="\n") as handle:
        handle.write("@echo off\r\n" + quoted + " %*\r\n")
    return cmd_path


def execute_installed_shim(cmd_path: str, args: list[str], env: dict[str, str]):
    """Run the generated hook through the installed cmd shim."""
    import subprocess  # nosec B404 - the installed cmd line, list argv, no shell.

    if os.name == "nt":
        child = dict(env)
        child["PATH"] = os.path.dirname(cmd_path)
        return subprocess.run(  # nosec B603 - cmd /d /c py, PATH is only the shim dir.
            ["cmd", "/d", "/c", "py", *args],
            env=child,
            capture_output=True,
            text=True,
            check=False,
        )
    with open(cmd_path, encoding="utf-8") as handle:
        return _execute_cmd_text(handle.read(), args, env)


def _execute_cmd_text(text: str, args: list[str], env: dict[str, str]):
    """Cmd semantics: ``shift`` does not rewrite ``%*``."""
    import re
    import subprocess  # nosec B404

    lines = []
    for raw in text.splitlines():
        line = raw.strip()
        if line and not line.lower().startswith("@echo") and not line.lower().startswith("rem"):
            lines.append(line)
    call = [line for line in lines if "%*" in line][-1]
    tokens = re.findall(r'"([^"]*)"', call)
    shifted = any(line.lower().startswith("shift") or " shift" in f" {line.lower()}" for line in lines)
    forwarded = list(args)
    command = [tokens[0], *forwarded] if shifted and len(tokens) < 3 else [*tokens, *forwarded]
    return subprocess.run(command, env=env, capture_output=True, text=True, check=False)  # nosec B603


def _apply_installer(argv: list[str]) -> int:
    installer = argv[argv.index("--apply-installer") + 1]
    interpreter = argv[argv.index("--interpreter") + 1]
    shim_dir = argv[argv.index("--shim-dir") + 1]
    provision_from_installer(installer, interpreter, shim_dir)
    return 0


def main(argv: list[str] | None = None) -> int:
    args = list(sys.argv[1:] if argv is None else argv)
    if len(args) < 1:
        sys.stderr.write("py shim missing interpreter\n")
        return 2
    interpreter = args[0]
    user = strip_launcher_flag(args[1:])
    if os.name == "nt":
        import subprocess  # nosec B404 - fixed interpreter from the installer, list args.

        completed = subprocess.run([interpreter, *user], check=False)  # nosec B603
        return int(completed.returncode)
    os.execv(interpreter, [interpreter, *user])  # nosec B606 - replace this shim process with the managed interpreter
    return 0  # pragma: no cover — execv does not return


if __name__ == "__main__":
    if "--apply-installer" in sys.argv:
        raise SystemExit(_apply_installer(sys.argv[1:]))
    raise SystemExit(main())
