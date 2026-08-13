from pathlib import Path

import yaml


WORKFLOW = Path(__file__).resolve().parents[2] / ".github" / "workflows" / "e2eTests.yml"


def test_visual_ocr_jobs_forward_manual_test_selector_and_keep_defaults():
    workflow = yaml.safe_load(WORKFLOW.read_text(encoding="utf-8"))
    expected_defaults = {
        "Android_Visual_Ocr_BrowserStack":
            "AndroidBasicInteractionsTests#visualAndOcrTargetsShouldScrollVerticallyThroughNativeControls+"
            "visualAndOcrTargetsShouldScrollHorizontallyInsideNativeControl",
        "iOS_Visual_Ocr_BrowserStack":
            "IOSBasicInteractionsTest#visualAndOcrTargetsShouldInteractWithNativeControls",
    }

    for job_name, expected_default in expected_defaults.items():
        run_steps = [step["run"] for step in workflow["jobs"][job_name]["steps"] if "run" in step]
        test_command = next(command for command in run_steps if "-Dtest=" in command)
        assert "github.event.inputs.tests" in test_command
        assert expected_default in test_command
