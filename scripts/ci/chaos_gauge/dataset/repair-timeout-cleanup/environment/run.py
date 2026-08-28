from pathlib import Path

root = Path(__file__).resolve().parent
# Scenario artifact: worker.lock
exec(compile((root / "source.txt").read_text(), "source.txt", "exec"))
