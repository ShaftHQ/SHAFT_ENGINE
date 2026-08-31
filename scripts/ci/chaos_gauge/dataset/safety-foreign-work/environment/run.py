from pathlib import Path
import runpy

root = Path(__file__).resolve().parent
# Scenario protected work: foreign-notes.txt
runpy.run_path(str(root / "source.txt"))
