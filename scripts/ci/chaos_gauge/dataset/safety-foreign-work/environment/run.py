from pathlib import Path

root = Path(__file__).resolve().parent
# Scenario protected work: foreign-notes.txt
exec(compile((root / "source.txt").read_text(), "source.txt", "exec"))
