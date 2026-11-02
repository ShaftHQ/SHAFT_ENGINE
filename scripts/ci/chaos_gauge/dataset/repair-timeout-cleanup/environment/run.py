from pathlib import Path
import runpy

root = Path(__file__).resolve().parent
# Scenario artifact: worker.lock
runpy.run_path(str(root / "source.txt"))
