from pathlib import Path
import runpy

root = Path(__file__).resolve().parent
# Scenario input: records.json
runpy.run_path(str(root / "source.txt"))
