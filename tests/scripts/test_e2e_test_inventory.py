import tempfile
import unittest
from pathlib import Path

from scripts.ci.e2e_test_inventory import (
    classes_without_e2e_selection,
    discover_test_classes,
    parse_workflow_expressions,
    to_markdown,
)


class E2ETestInventoryTests(unittest.TestCase):
    def test_discovers_unselected_classes_after_expanding_workflow_env(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            root = Path(temp_dir)
            test_root = root / "src/test/java"
            workflow_root = root / ".github/workflows"
            (test_root / "example").mkdir(parents=True)
            workflow_root.mkdir(parents=True)
            (test_root / "example" / "BrowserActionsTests.java").write_text("class BrowserActionsTests {}", encoding="utf-8")
            (test_root / "example" / "ApiActionsTests.java").write_text("class ApiActionsTests {}", encoding="utf-8")
            (test_root / "example" / "Helper.java").write_text("class Helper {}", encoding="utf-8")
            workflow = workflow_root / "e2eTests.yml"
            workflow.write_text(
                "env:\n"
                "  GLOBAL_TESTING_SCOPE: \"!%regex[.*Api.*]\"\n"
                "jobs:\n"
                "  browser:\n"
                "    steps:\n"
                "      - run: mvn test \"-Dtest=${GLOBAL_TESTING_SCOPE}\"\n",
                encoding="utf-8",
            )

            test_classes = discover_test_classes(test_root)
            expressions = parse_workflow_expressions([workflow])
            uncovered = classes_without_e2e_selection(test_classes, expressions)

        self.assertEqual([test_class.class_name for test_class in test_classes], ["ApiActionsTests", "BrowserActionsTests"])
        self.assertEqual([test_class.class_name for test_class in uncovered], ["ApiActionsTests"])

    def test_combines_positive_regex_selectors_and_markdown_summary(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            root = Path(temp_dir)
            test_root = root / "src/test/java/example"
            workflow_root = root / ".github/workflows"
            test_root.mkdir(parents=True)
            workflow_root.mkdir(parents=True)
            (test_root / "ApiActionsTests.java").write_text("class ApiActionsTests {}", encoding="utf-8")
            (test_root / "DatabaseActionsTests.java").write_text("class DatabaseActionsTests {}", encoding="utf-8")
            workflow = workflow_root / "e2eTests.yml"
            workflow.write_text(
                "jobs:\n"
                "  api:\n"
                "    steps:\n"
                "      - run: mvn test \"-Dtest=%regex[.*Api.*], %regex[.*RequestBuilder.*]\"\n",
                encoding="utf-8",
            )

            test_classes = discover_test_classes(root / "src/test/java")
            expressions = parse_workflow_expressions([workflow])
            uncovered = classes_without_e2e_selection(test_classes, expressions)
            markdown = to_markdown(test_classes, expressions, uncovered)

        self.assertEqual([test_class.class_name for test_class in uncovered], ["DatabaseActionsTests"])
        self.assertIn("Discovered Java test classes: **2**", markdown)
        self.assertIn("`example.DatabaseActionsTests`", markdown)

    def test_ignores_abstract_test_base_classes(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            test_root = Path(temp_dir) / "src/test/java/example"
            test_root.mkdir(parents=True)
            (test_root / "MobileTest.java").write_text("public abstract class MobileTest {}", encoding="utf-8")
            (test_root / "ConcreteMobileTest.java").write_text("public class ConcreteMobileTest {}", encoding="utf-8")

            test_classes = discover_test_classes(Path(temp_dir) / "src/test/java")

        self.assertEqual([test_class.class_name for test_class in test_classes], ["ConcreteMobileTest"])


if __name__ == "__main__":
    unittest.main()
