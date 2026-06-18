import tempfile
import unittest
from pathlib import Path

from scripts.ci.assemble_javadocs import MODULES, assemble_javadocs


class AssembleJavadocsTest(unittest.TestCase):
    def test_assembles_all_module_sites_with_navigation(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            root = Path(temp_dir)
            (root / "pom.xml").write_text(
                '<project xmlns="http://maven.apache.org/POM/4.0.0">'
                "<modelVersion>4.0.0</modelVersion><version>1.2.3</version></project>",
                encoding="utf-8",
            )
            for module in MODULES:
                source = root / module / "target" / "reports" / "apidocs"
                source.mkdir(parents=True)
                (source / "index.html").write_text(module, encoding="utf-8")
            generator = root / "shaft-engine" / "src" / "main" / "javadoc" / "resources"
            generator.mkdir(parents=True)
            (generator / "index.html").write_text("generator", encoding="utf-8")

            destination = assemble_javadocs(root)

            index = (destination / "index.html").read_text(encoding="utf-8")
            self.assertIn("SHAFT 1.2.3 JavaDocs", index)
            for module in MODULES:
                self.assertIn(f'{module}/index.html', index)
                self.assertEqual((destination / module / "index.html").read_text(encoding="utf-8"), module)
            self.assertEqual(
                (destination / "shaft-engine" / "resources" / "index.html").read_text(encoding="utf-8"),
                "generator",
            )

    def test_rejects_missing_module_javadocs(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            root = Path(temp_dir)
            with self.assertRaisesRegex(FileNotFoundError, "shaft-engine"):
                assemble_javadocs(root)


if __name__ == "__main__":
    unittest.main()
