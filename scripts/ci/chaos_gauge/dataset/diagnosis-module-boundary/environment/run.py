from pathlib import Path

root = Path(__file__).resolve().parent
# Scenario input: public_api.py
exec(compile((root / "source.txt").read_text(), "source.txt", "exec"))
