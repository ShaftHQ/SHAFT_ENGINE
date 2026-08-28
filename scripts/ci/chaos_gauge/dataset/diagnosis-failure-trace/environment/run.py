from pathlib import Path

root = Path(__file__).resolve().parent
observed = (root / "source.txt").read_text().strip()
if observed != "fixed-diagnosis-failure-trace":
    raise SystemExit(f"contract mismatch: {observed}")
print(observed)
