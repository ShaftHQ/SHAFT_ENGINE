"""Check every markdown link in the research references answers HTTP 2xx/3xx.

Usage: python3 linkcheck.py [folder]   (network; run locally, not in CI)
"""

from __future__ import annotations

import concurrent.futures
import json
import os
import re
import shutil
import subprocess  # nosec B404 - fixed curl argv, no shell.
import sys
from pathlib import Path

AGENT = "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 Chrome/124 Safari/537.36"


def status(url: str) -> tuple[str, str]:
    code = "000"
    for head in (["-I"], []):
        result = subprocess.run([shutil.which("curl") or "curl", "-sL", "-o", os.devnull, "-w", "%{http_code}",  # nosec B603
                                 "--max-time", "20", "-A", AGENT, *head, url], capture_output=True, text=True, check=False)
        code = result.stdout.strip() or "000"
        if code[:1] in "23":
            break
    return url, code


def main() -> int:
    folder = Path(sys.argv[1] if len(sys.argv) > 1 else Path(__file__).parents[1] / "references/research")
    urls = sorted({u for f in folder.glob("*.md") for u in re.findall(r"\]\((https://(?:[^()\s]|\([^()\s]*\))+)\)", f.read_text(encoding="utf-8"))})
    with concurrent.futures.ThreadPoolExecutor(16) as pool:
        bad = {u: c for u, c in pool.map(status, urls) if c[:1] not in "23"}
    print(json.dumps({"check": "linkcheck", "status": "fail" if bad else "pass", "urls": len(urls), "bad": bad}))
    return 1 if bad else 0


if __name__ == "__main__":
    sys.exit(main())
