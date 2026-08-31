import unittest
from pathlib import Path


FIXTURE = (
    Path(__file__).resolve().parents[2]
    / "shaft-engine/src/test/resources/testDataFiles/dragAndDropFixture.html"
)


class DragAndDropFixtureTest(unittest.TestCase):
    def test_uses_webdriver_pointer_events_instead_of_native_html_drag_state(self):
        html = FIXTURE.read_text(encoding="utf-8")

        self.assertNotIn('draggable="true"', html)
        self.assertIn("source.addEventListener('mousedown'", html)
        self.assertIn("target.addEventListener('mouseup'", html)
        self.assertIn("if (dragging)", html)


if __name__ == "__main__":
    unittest.main()
