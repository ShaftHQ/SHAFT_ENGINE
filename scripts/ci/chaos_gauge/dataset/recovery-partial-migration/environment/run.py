from pathlib import Path
import runpy

root = Path(__file__).resolve().parent
# Scenario legacy input: legacy.conf
runpy.run_path(str(root / "source.txt"))
