"""Provider-neutral ChaosEngine lifecycle-kernel contracts."""

from __future__ import annotations

import importlib.util
import json
import os
import sys
import tempfile
import threading
import unittest
from pathlib import Path
from unittest import mock


ROOT = Path(__file__).resolve().parents[2]
KERNEL = ROOT / "chaos-engine/hooks/kernel.py"


def load_kernel():
    specification = importlib.util.spec_from_file_location("chaos_engine_kernel", KERNEL)
    if specification is None or specification.loader is None:
        raise RuntimeError("kernel module is unavailable")
    module = importlib.util.module_from_spec(specification)
    sys.modules[specification.name] = module
    specification.loader.exec_module(module)
    return module


class ChaosEngineKernelTest(unittest.TestCase):
    def setUp(self):
        self.kernel = load_kernel()

    def test_every_supported_host_has_an_explicit_native_contract(self):
        self.assertEqual(
            {"codex", "claude", "gemini", "grok", "copilot"},
            set(self.kernel.HOST_CAPABILITIES),
        )
        for host, capability in self.kernel.HOST_CAPABILITIES.items():
            with self.subTest(host=host):
                self.assertTrue(capability.instruction_paths)
                self.assertTrue(capability.event_aliases)
                self.assertIn("PreToolUse", capability.supported_events)
                self.assertIn("Stop", capability.supported_events)

    def test_native_payloads_normalize_to_one_event_shape(self):
        fixtures = (
            ({"hook_event_name": "PreToolUse", "session_id": "s", "tool_name": "exec_command", "tool_input": {"cmd": "git status"}}, "codex"),
            ({"hook_event_name": "PreToolUse", "session_id": "s", "tool_name": "Bash", "tool_input": {"command": "git status"}}, "claude"),
            ({"hook_event_name": "BeforeTool", "session_id": "s", "tool_name": "run_shell_command", "tool_input": {"command": "git status"}}, "gemini"),
            ({"hookEventName": "PreToolUse", "sessionId": "s", "toolName": "bash", "toolInput": {"command": "git status"}}, "grok"),
            ({"hookEventName": "preToolUse", "sessionId": "s", "toolName": "bash", "toolArgs": {"command": "git status"}}, "copilot"),
        )
        normalized = [self.kernel.normalize_event(raw, host) for raw, host in fixtures]

        self.assertEqual({event.name for event in normalized}, {"PreToolUse"})
        self.assertEqual({event.session_id for event in normalized}, {"s"})
        self.assertEqual({event.tool_name for event in normalized}, {"PowerShell"})
        self.assertEqual(
            {event.tool_input.get("command") or event.tool_input.get("cmd") for event in normalized},
            {"git status"},
        )

    def test_copilot_json_tool_args_preserve_mutation(self):
        event = self.kernel.normalize_event(
            {
                "hookEventName": "preToolUse",
                "sessionId": "s",
                "toolName": "bash",
                "toolArgs": json.dumps({"command": "git push origin HEAD"}),
            },
            "copilot",
        )

        self.assertEqual("git push origin HEAD", event.tool_input["command"])
        self.assertTrue(event.stateful_mutation)

    def test_ambiguous_snake_case_host_detection_uses_explicit_environment(self):
        raw = {"hook_event_name": "PreToolUse", "session_id": "s"}
        for host in ("codex", "claude"):
            with self.subTest(host=host), mock.patch.dict(
                os.environ, {"SHAFT_GUARD_HOST": host}, clear=False
            ):
                self.assertEqual(host, self.kernel.normalize_event(raw).host)

    def test_missing_session_never_uses_shared_state(self):
        mutation = self.kernel.evaluate(
            self.kernel.normalize_event(
                {"hook_event_name": "PreToolUse", "tool_name": "Write", "tool_input": {}},
                "codex",
            )
        )
        stopped = self.kernel.evaluate(
            self.kernel.normalize_event({"hook_event_name": "Stop"}, "codex")
        )

        self.assertEqual("deny", mutation.decision)
        self.assertEqual("CE_SESSION_REQUIRED", mutation.diagnostic_code)
        self.assertEqual("allow", stopped.decision)
        self.assertEqual("CE_SESSION_MISSING_STOP", stopped.diagnostic_code)
        self.assertEqual((), mutation.effects)
        self.assertEqual((), stopped.effects)

    def test_lifecycle_and_rule_registries_are_total_and_conflict_free(self):
        self.assertEqual([], self.kernel.validate_lifecycle())
        self.assertEqual([], self.kernel.validate_rules(self.kernel.RULES))

        conflict = self.kernel.Rule(
            code="CE_CONFLICT",
            event="PreToolUse",
            priority=self.kernel.RULES[0].priority,
            decision="allow",
            remedy=None,
            terminal=True,
            predicate=lambda _event, _snapshot: True,
        )
        errors = self.kernel.validate_rules((*self.kernel.RULES, conflict))
        self.assertTrue(any("priority" in error.casefold() for error in errors))

        cyclic = {
            "A": ("B", "Complete"),
            "B": ("A", "Blocked"),
            "Complete": (),
            "Blocked": (),
        }
        self.assertTrue(
            any("cycle" in error.casefold() for error in self.kernel.validate_lifecycle(cyclic))
        )

    def test_evaluate_preserves_phase_for_retry_and_enforces_transitions(self):
        retry = self.kernel.evaluate(
            self.kernel.normalize_event(
                {
                    "hook_event_name": "PreToolUse",
                    "phase": "Planned",
                    "tool_name": "Write",
                },
                "codex",
            )
        )
        self.assertEqual("Planned", retry.phase)
        self.assertIsNone(retry.terminal_reason)

        advanced = self.kernel.evaluate(
            self.kernel.normalize_event(
                {
                    "hook_event_name": "Stop",
                    "session_id": "s",
                    "phase": "Merged",
                    "targetPhase": "Learned",
                },
                "copilot",
            )
        )
        self.assertEqual("Learned", advanced.phase)

        invalid = self.kernel.evaluate(
            self.kernel.normalize_event(
                {
                    "hook_event_name": "Stop",
                    "session_id": "s",
                    "phase": "ReadOnly",
                    "target_phase": "Merged",
                },
                "codex",
            )
        )
        self.assertEqual("deny", invalid.decision)
        self.assertEqual("CE_INVALID_TRANSITION", invalid.diagnostic_code)
        self.assertEqual("ReadOnly", invalid.phase)
        self.assertIsNone(invalid.terminal_reason)

    def test_snapshot_memoizes_each_external_fact_once(self):
        calls = []
        providers = {"branch": lambda: calls.append("branch") or "main"}
        snapshot = self.kernel.HarnessSnapshot(
            providers=providers
        )
        providers["branch"] = lambda: "tampered"

        threads = [threading.Thread(target=lambda: snapshot.fact("branch")) for _ in range(8)]
        for thread in threads:
            thread.start()
        for thread in threads:
            thread.join()

        self.assertEqual("main", snapshot.fact("branch"))
        self.assertEqual("main", snapshot.fact("branch"))
        self.assertEqual(["branch"], calls)

    def test_effect_journal_is_append_only_idempotent_and_session_scoped(self):
        with tempfile.TemporaryDirectory() as directory:
            journal = self.kernel.EffectJournal(Path(directory) / "state-v2.jsonl")
            first = self.kernel.Effect("s1", "PreToolUse", "call", "rule", "record", {"x": 1})
            second = self.kernel.Effect("s2", "PreToolUse", "call", "rule", "record", {"x": 1})

            self.assertTrue(journal.append(first))
            self.assertFalse(journal.append(first))
            self.assertTrue(journal.append(second))
            self.assertEqual(1, len(journal.records("s1")))
            self.assertEqual(1, len(journal.records("s2")))
            self.assertEqual("s1", journal.records("s1")[0]["sessionId"])
            self.assertTrue(all(item["schemaVersion"] == 2 for item in journal.records("s1")))

            with self.assertRaises(ValueError):
                self.kernel.Effect("", "PreToolUse", "call", "rule", "record")

    def test_effect_journal_fails_closed_on_corruption_and_lock_timeout(self):
        with tempfile.TemporaryDirectory() as directory:
            path = Path(directory) / "state-v2.jsonl"
            path.write_text("not-json\n", encoding="utf-8")
            journal = self.kernel.EffectJournal(path, lock_timeout=0)
            with self.assertRaises(self.kernel.JournalCorruptionError):
                journal.records("s")

            journal._try_lock = lambda _handle: False
            with self.assertRaises(TimeoutError):
                with journal._lock():
                    self.fail("unreachable")

    def test_status_and_explain_are_versioned_secret_safe_json(self):
        report = self.kernel.evaluate(
            self.kernel.normalize_event(
                {"hook_event_name": "Stop", "session_id": "session", "api_key": "secret"},
                "gemini",
            )
        )
        rendered = json.dumps(report.to_dict(), sort_keys=True)

        self.assertEqual(1, report.to_dict()["schemaVersion"])
        self.assertNotIn("secret", rendered)
        self.assertIn("terminalReason", report.to_dict())

        effect = self.kernel.Effect(
            "session",
            "Stop",
            "call",
            "rule",
            "record",
            {"api_key": "secret", "nested": {"token": "secret"}},
        )
        self.assertNotIn("secret", json.dumps(effect.to_record(), sort_keys=True))

    def test_host_capabilities_declare_only_native_normalized_events(self):
        for host, capability in self.kernel.HOST_CAPABILITIES.items():
            with self.subTest(host=host):
                self.assertTrue(set(capability.event_aliases.values()).issubset(capability.supported_events))
        self.assertNotEqual(
            self.kernel.HOST_CAPABILITIES["gemini"].supported_events,
            self.kernel.HOST_CAPABILITIES["claude"].supported_events,
        )


if __name__ == "__main__":
    unittest.main()
