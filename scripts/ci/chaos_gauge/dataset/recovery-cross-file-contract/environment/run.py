from pathlib import Path

root = Path(__file__).resolve().parent
# Scenario contract: schema.json
exec(compile((root / "source.txt").read_text(), "source.txt", "exec"))
