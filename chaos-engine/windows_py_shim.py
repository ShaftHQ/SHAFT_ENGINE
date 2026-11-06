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
    raise SystemExit(main())
