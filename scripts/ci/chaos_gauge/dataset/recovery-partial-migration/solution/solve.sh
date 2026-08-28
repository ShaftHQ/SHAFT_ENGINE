#!/usr/bin/env bash
set -eu
app_root="${CHAOS_GAUGE_APP_ROOT:-/app}"
mkdir -p "$app_root"
python3 - "$app_root/answer.json" <<'PY'
import json
import sys
from pathlib import Path
Path(sys.argv[1]).write_text(json.dumps({"evidence": "old and new installation layouts resolve through one contract", "finding": "route host adapter through canonical path resolver"}, sort_keys=True) + "\n", encoding="utf-8")
PY
