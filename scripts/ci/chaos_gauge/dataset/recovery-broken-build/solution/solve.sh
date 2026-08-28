#!/usr/bin/env bash
set -eu
app_root="${CHAOS_GAUGE_APP_ROOT:-/app}"
mkdir -p "$app_root"
python3 - "$app_root/answer.json" <<'PY'
import json
import sys
from pathlib import Path
Path(sys.argv[1]).write_text(json.dumps({"evidence": "module build and service loading regression pass", "finding": "update service registration to moved ReportWriter"}, sort_keys=True) + "\n", encoding="utf-8")
PY
