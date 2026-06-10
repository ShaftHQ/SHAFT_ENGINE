import subprocess
import sys
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]

class ModularDocumentationValidationTest(unittest.TestCase):
    def test_repository_contract(self):
        subprocess.run([sys.executable, str(ROOT / "scripts/ci/validate_modular_documentation.py")], cwd=ROOT, check=True)

if __name__ == "__main__":
    unittest.main()
