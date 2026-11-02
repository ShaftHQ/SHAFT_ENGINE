#!/usr/bin/env python3
"""Probe host RAM, optional GPU memory, and OS. Recommend a size class or refuse."""

from __future__ import annotations

import argparse
import json
import os
import platform
import subprocess  # nosec B404 - only a fixed local GPU query is used.
import sys
from pathlib import Path

GIB = 1024 ** 3
REFUSE_RAM_GIB = 8
MEDIUM_RAM_GIB = 16
LARGE_RAM_GIB = 32
MEDIUM_GPU_GIB = 4
LARGE_GPU_GIB = 8


def classify(ram_bytes: int, gpu_bytes: int | None, os_name: str) -> dict:
    """Return a size-class recommendation from measured or injected hardware."""
    ram_gib = ram_bytes / GIB
    gpu_gib = None if gpu_bytes is None else gpu_bytes / GIB
    if ram_gib < REFUSE_RAM_GIB:
        recommendation = "refuse"
        reason = "less than 8 GiB RAM cannot run a useful local coder"
    elif ram_gib >= LARGE_RAM_GIB or (gpu_gib is not None and gpu_gib >= LARGE_GPU_GIB):
        recommendation = "large"
        reason = "RAM or GPU memory supports a large local coder"
    elif ram_gib >= MEDIUM_RAM_GIB or (gpu_gib is not None and gpu_gib >= MEDIUM_GPU_GIB):
        recommendation = "medium"
        reason = "RAM or GPU memory supports a medium local coder"
    else:
        recommendation = "small"
        reason = "host can run a small local coder only"
    return {
        "os": os_name,
        "ram_bytes": ram_bytes,
        "gpu_bytes": gpu_bytes,
        "recommendation": recommendation,
        "reason": reason,
    }


def _ram_bytes() -> int:
    system = platform.system()
    if system == "Windows":
        import ctypes

        class MemoryStatusEx(ctypes.Structure):
            _fields_ = [
                ("dwLength", ctypes.c_ulong),
                ("dwMemoryLoad", ctypes.c_ulong),
                ("ullTotalPhys", ctypes.c_ulonglong),
                ("ullAvailPhys", ctypes.c_ulonglong),
                ("ullTotalPageFile", ctypes.c_ulonglong),
                ("ullAvailPageFile", ctypes.c_ulonglong),
                ("ullTotalVirtual", ctypes.c_ulonglong),
                ("ullAvailVirtual", ctypes.c_ulonglong),
                ("ullAvailExtendedVirtual", ctypes.c_ulonglong),
            ]

        status = MemoryStatusEx()
        status.dwLength = ctypes.sizeof(status)
        if not ctypes.windll.kernel32.GlobalMemoryStatusEx(ctypes.byref(status)):
            raise OSError("GlobalMemoryStatusEx failed")
        return int(status.ullTotalPhys)
    if system == "Linux":
        meminfo = Path(os.sep) / "proc" / "meminfo"
        for line in meminfo.read_text(encoding="utf-8").splitlines():
            if line.startswith("MemTotal:"):
                return int(line.split()[1]) * 1024
        raise OSError("MemTotal missing from proc meminfo")
    if system == "Darwin":
        output = subprocess.run(  # nosec B603 B607 - fixed sysctl invocation.
            ["sysctl", "-n", "hw.memsize"],
            check=True,
            capture_output=True,
            text=True,
        )
        return int(output.stdout.strip())
    raise OSError(f"unsupported OS for RAM probe: {system}")


def _gpu_bytes() -> int | None:
    try:
        output = subprocess.run(  # nosec B603 B607 - fixed nvidia-smi query.
            [
                "nvidia-smi",
                "--query-gpu=memory.total",
                "--format=csv,noheader,nounits",
            ],
            check=True,
            capture_output=True,
            text=True,
            timeout=5,
        )
    except (FileNotFoundError, subprocess.SubprocessError, OSError):
        return None
    totals = []
    for line in output.stdout.splitlines():
        text = line.strip()
        if text:
            totals.append(float(text) * 1024 * 1024)
    if not totals:
        return None
    return int(max(totals))


def probe() -> dict:
    """Measure this host and classify it."""
    return classify(_ram_bytes(), _gpu_bytes(), platform.system())


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.parse_args(argv)
    json.dump(probe(), sys.stdout, indent=2)
    sys.stdout.write(os.linesep)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
