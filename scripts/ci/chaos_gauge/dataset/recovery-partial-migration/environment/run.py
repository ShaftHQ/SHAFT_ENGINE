from pathlib import Path

root = Path(__file__).resolve().parent
# Scenario legacy input: legacy.conf
exec(compile((root / "source.txt").read_text(), "source.txt", "exec"))
