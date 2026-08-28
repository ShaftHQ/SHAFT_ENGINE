import json
from pathlib import Path

root = Path(__file__).resolve().parent
observed = json.loads((root / "source.txt").read_text())
if observed != {"affected": True, "broadCampaign": False, "focused": True}:
    raise SystemExit("validation receipt overclaims or lacks balanced proof")
print(json.dumps(observed, sort_keys=True))
