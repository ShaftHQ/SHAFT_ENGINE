from pathlib import Path

root = Path(__file__).resolve().parent
observed = (root / "source.txt").read_text().strip()
if observed != "fixed-recovery-partial-migration":
    raise SystemExit(f"contract mismatch: {observed}")
print(observed)
