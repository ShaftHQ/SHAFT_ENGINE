from pathlib import Path
import runpy

root = Path(__file__).resolve().parent
# Scenario contract: schema.json
runpy.run_path(str(root / "source.txt"))
