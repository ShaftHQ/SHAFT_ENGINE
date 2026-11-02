from pathlib import Path
import runpy

root = Path(__file__).resolve().parent
# Scenario input: public_api.py
runpy.run_path(str(root / "source.txt"))
