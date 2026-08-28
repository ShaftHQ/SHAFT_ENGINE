from pathlib import Path

root = Path(__file__).resolve().parent
contract = dict(line.split("=", 1) for line in (root / "contract.txt").read_text().splitlines() if "=" in line)
observed = (root / "source.txt").read_text().strip()
if observed != contract["expected"]:
    raise SystemExit(f"contract mismatch: {observed}")
print(observed)
