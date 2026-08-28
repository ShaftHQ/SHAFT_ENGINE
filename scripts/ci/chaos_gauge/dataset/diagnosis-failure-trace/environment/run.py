from pathlib import Path

root = Path(__file__).resolve().parent
# Scenario input: trace.log
exec(compile((root / "source.txt").read_text(), "source.txt", "exec"))
