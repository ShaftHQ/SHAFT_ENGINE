"""#5831: soft doctor statuses must not retain install-failure heal-handoff fixNext."""

from __future__ import annotations

import importlib.util
import tempfile
import unittest
from pathlib import Path


ROOT = Path(__file__).resolve().parents[2]
INSTALL = ROOT / "chaos-engine" / "install.py"

HEAL_HANDOFF_FIX = (
    "Complete the agent heal using .chaos-engine-state/heal-handoff.md, "
    "then rerun doctor. Do not rerun the install one-liner."
)


def load(path: Path, name: str):
    spec = importlib.util.spec_from_file_location(name, path)
    if spec is None or spec.loader is None:
        raise AssertionError(f"failed to load {path}")
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


class SoftStatusFixNext5831Test(unittest.TestCase):
    def test_heal_handoff_does_not_overwrite_compatible_legacy_memory(self):
        module = load(INSTALL, "ce_5831_soft_heal")
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary)
            state = project / ".chaos-engine-state"
            state.mkdir(parents=True)
            (state / "heal-handoff.md").write_text("# leftover install failure\n", encoding="utf-8")
            components = {
                "core": {"status": "healthy", "taskImpact": "required"},
                "memory": {
                    "status": "compatible-legacy",
                    "taskImpact": "advisory",
                    "detail": "legacy memory payload still readable",
                    "fixNext": (
                        "`memory` is compatible-legacy: uninstall then reinstall the portable "
                        "bootstrap when you are ready to leave the legacy payload."
                    ),
                },
                "hooks": {
                    "status": "recovery-required",
                    "taskImpact": "required",
                    "detail": "hooks broken",
                },
            }
            module.apply_merge_handoff_fix_next(project, components)
            memory_fix = components["memory"].get("fixNext", "")
            self.assertNotIn("heal-handoff.md", str(memory_fix))
            self.assertIn("compatible-legacy", str(memory_fix))
            self.assertIn("heal-handoff.md", components["hooks"]["fixNext"])

    def test_clear_leftover_install_failure_fix_next_on_soft_statuses(self):
        module = load(INSTALL, "ce_5831_soft_clear")
        components = {
            "memory": {
                "status": "compatible-legacy",
                "taskImpact": "advisory",
                "fixNext": HEAL_HANDOFF_FIX,
            },
            "identity": {
                "status": "sync-advisory",
                "taskImpact": "advisory",
                "fixNext": HEAL_HANDOFF_FIX,
            },
            "plugins": {
                "status": "degraded",
                "taskImpact": "advisory",
                "fixNext": HEAL_HANDOFF_FIX,
            },
            "hooks": {
                "status": "recovery-required",
                "taskImpact": "required",
                "fixNext": HEAL_HANDOFF_FIX,
            },
        }
        module.clear_soft_status_install_failure_fix_next(components)
        self.assertNotIn("fixNext", components["memory"])
        self.assertNotIn("fixNext", components["identity"])
        self.assertNotIn("fixNext", components["plugins"])
        self.assertEqual(HEAL_HANDOFF_FIX, components["hooks"]["fixNext"])

    def test_component_fix_next_keeps_soft_status_message_after_clear(self):
        module = load(INSTALL, "ce_5831_soft_message")
        item = {
            "status": "compatible-legacy",
            "taskImpact": "advisory",
            "detail": "legacy v5 still works",
        }
        message = module.component_fix_next("memory", item)
        self.assertIsNotNone(message)
        self.assertIn("compatible-legacy", message)
        self.assertNotIn("heal-handoff.md", message)

    def test_soft_statuses_constant_matches_non_escalating_minus_healthy(self):
        module = load(INSTALL, "ce_5831_soft_const")
        self.assertEqual(
            module.DOCTOR_SOFT_STATUSES,
            frozenset({"compatible-legacy", "sync-advisory", "degraded"}),
        )
        self.assertTrue(module.DOCTOR_SOFT_STATUSES.issubset(module.DOCTOR_NON_ESCALATING_STATUSES))


if __name__ == "__main__":
    unittest.main()
