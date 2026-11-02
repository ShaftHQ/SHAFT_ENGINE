from pathlib import Path
import runpy

root = Path(__file__).resolve().parent
# Scenario input: cases.csv
runpy.run_path(str(root / "source.txt"))
