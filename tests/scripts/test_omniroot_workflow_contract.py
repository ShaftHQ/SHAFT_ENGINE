import unittest
from pathlib import Path


ROOT = Path(__file__).resolve().parents[2]


class OmniRootWorkflowContractTest(unittest.TestCase):
    def read(self, relative_path):
        return (ROOT / relative_path).read_text(encoding="utf-8")

    def test_execution_workflows_are_canonical_and_linked(self):
        workflows = self.read("chaos-engine/references/execution-workflows.md")
        router = self.read("chaos-engine/skills/chaos-engine/SKILL.md")
        delegation = self.read("chaos-engine/references/delegation.md")
        roles = self.read("chaos-engine/references/roles.md")

        for name in (
            "SOLO",
            "ORCHESTRATOR + SINGLE IMPLEMENTER",
            "ORCHESTRATOR + PARALLEL IMPLEMENTERS",
        ):
            self.assertIn(name, workflows)
            self.assertNotIn(f"| {name} |", router)
            self.assertNotIn(f"| {name} |", delegation)
            self.assertNotIn(f"| {name} |", roles)

        for target in (
            "execution-workflows.md",
            "omniroot/SKILL.md",
            "tdd.md#workflow",
            "roles.md",
            "delegation.md",
            "orchestrator-follow-through.md",
        ):
            self.assertIn(target, router)

    def test_workflow_selection_survives_missing_omniroute(self):
        workflows = self.read("chaos-engine/references/execution-workflows.md")
        self.assertIn("OmniRoute is absent", workflows)
        self.assertIn("host-native", workflows)
        self.assertIn("does not change the selected workflow", workflows)

    def test_omniroute_auto_use_preserves_operator_route_selection_boundary(self):
        workflows = self.read("chaos-engine/references/execution-workflows.md")
        omniroot = self.read("chaos-engine/skills/omniroot/SKILL.md")
        guide = self.read("chaos-engine/guides/omniroute.md")
        for text in (workflows, omniroot, guide):
            normalized = " ".join(text.split())
            self.assertIn("probe the fixed loopback endpoint before native fallback", normalized)
            self.assertIn("sealed operator launcher", normalized)
            self.assertIn("operator-owned priority combo", normalized)
            self.assertIn("RUNTIME_EXHAUSTED", normalized)
        self.assertIn("Built-in `spawn_agent` cannot select OmniRoute", guide)

    def test_cadence_has_one_numeric_owner(self):
        follow_through = self.read("chaos-engine/references/orchestrator-follow-through.md")
        consumers = "\n".join(
            self.read(path)
            for path in (
                "chaos-engine/skills/chaos-engine/SKILL.md",
                "chaos-engine/references/delegation.md",
                "chaos-engine/references/roles.md",
            )
        )
        for minutes in ("5 minutes", "10 minutes", "15 minutes"):
            self.assertIn(minutes, follow_through)
            self.assertNotIn(minutes, consumers)
        for duty in ("report status", "apply pressure", "consult", "remove blockers"):
            self.assertIn(duty, follow_through)


if __name__ == "__main__":
    unittest.main()
