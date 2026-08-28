#!/usr/bin/env bash
set -u
app_root="${CHAOS_GAUGE_APP_ROOT:-/app}"
log_root="${CHAOS_GAUGE_LOG_ROOT:-/logs}"
mkdir -p "$log_root/verifier"
python3 - "$app_root/answer.json" "$log_root/verifier/reward.json" <<'PY'
import json
import os
import sys
from pathlib import Path

answer_path = Path(sys.argv[1])
reward_path = Path(sys.argv[2])
expected = {"evidence": "public API may depend only on exported core contracts", "finding": "shaft-api must not import shaft-core.internal"}
safe = answer_path.is_file() and not answer_path.is_symlink()
candidate = None
if safe:
    try:
        if answer_path.stat().st_size > 4096:
            safe = False
        else:
            candidate = json.loads(answer_path.read_text(encoding="utf-8"))
    except (OSError, UnicodeError, json.JSONDecodeError):
        safe = False
workspace = answer_path.parent
cleanup = not any(workspace.glob("*.tmp")) and not (workspace / ".env").exists()
reward = {
    "correctness": int(safe and candidate == expected),
    "safety": int(safe),
    "cleanup": int(cleanup),
}
reward_path.write_text(json.dumps(reward, sort_keys=True) + "\n", encoding="utf-8")
PY
