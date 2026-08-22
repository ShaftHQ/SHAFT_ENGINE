"""Provider-neutral ChaosEngine lifecycle-kernel contracts."""

from __future__ import annotations

import hashlib
import importlib.util
import json
import os
import sys
import tempfile
import threading
import time
from pathlib import Path
from unittest import TestCase, main, mock


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


class ChaosEngineKernelTest(TestCase):
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
                self.assertTrue(capability.strict_json_stdout)
                self.assertTrue(capability.live_gate)
        self.assertEqual(
            ("cloud", "ide"), self.kernel.HOST_CAPABILITIES["copilot"].static_surfaces
        )
        self.assertTrue(
            all(
                not capability.static_surfaces
                for host, capability in self.kernel.HOST_CAPABILITIES.items()
                if host != "copilot"
            )
        )

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

    def test_missing_session_shell_mutations_fail_closed(self):
        mutations = (
            "git tag release",
            "git branch feature",
            "git config user.name Chaos",
            "git diff --output=artifact.patch",
            "git diff --output artifact.patch",
            "git status $(touch pwned)",
            "git status > artifact.txt",
            "git status | tee artifact.txt",
            "git status; touch pwned",
            "git status && touch pwned",
            "git status `touch pwned`",
            "gh issue create --title bug --body body",
            "mkdir output",
            "echo changed > output.txt",
            "unknown-command --possibly-mutating",
        )
        for command in mutations:
            with self.subTest(command=command):
                event = self.kernel.normalize_event(
                    {
                        "hook_event_name": "PreToolUse",
                        "tool_name": "bash",
                        "tool_input": {"command": command},
                    },
                    "claude",
                )
                report = self.kernel.evaluate(event)
                self.assertTrue(event.stateful_mutation)
                self.assertEqual("CE_SESSION_REQUIRED", report.diagnostic_code)

        shell_read = self.kernel.normalize_event(
            {
                "hook_event_name": "PreToolUse",
                "tool_name": "bash",
                "tool_input": {"command": "git status --short"},
            },
            "claude",
        )
        self.assertTrue(shell_read.stateful_mutation)
        self.assertEqual(
            "CE_SESSION_REQUIRED", self.kernel.evaluate(shell_read).diagnostic_code
        )

    def test_missing_session_allows_only_explicit_direct_read_tools(self):
        for tool_name in ("Read", "Grep", "Glob"):
            with self.subTest(tool_name=tool_name):
                event = self.kernel.normalize_event(
                    {
                        "hook_event_name": "PreToolUse",
                        "tool_name": tool_name,
                        "tool_input": {"file_path": "README.md"},
                    },
                    "claude",
                )
                self.assertFalse(event.stateful_mutation)
                self.assertEqual("CE_OK", self.kernel.evaluate(event).diagnostic_code)

        unsafe_shapes = (
            ("functions.exec", {"source": "await tools.exec_command({cmd: 'git status'})"}),
            ("functions.exec", {"input": {"cmd": "git status"}}),
            ("run_shell_command", {"command": "git status"}),
            ("write_file", {"file_path": "result.txt", "content": "changed"}),
            ("replace", {"file_path": "result.txt", "old_string": "a", "new_string": "b"}),
            ("unknown_tool", {}),
            ("", {}),
        )
        for tool_name, tool_input in unsafe_shapes:
            with self.subTest(tool_name=tool_name, tool_input=tool_input):
                event = self.kernel.normalize_event(
                    {
                        "hook_event_name": "BeforeTool",
                        "tool_name": tool_name,
                        "tool_input": tool_input,
                    },
                    "gemini",
                )
                self.assertTrue(event.stateful_mutation)
                self.assertEqual(
                    "CE_SESSION_REQUIRED", self.kernel.evaluate(event).diagnostic_code
                )

    def test_missing_session_malformed_shell_json_fails_closed(self):
        event = self.kernel.normalize_event(
            {
                "hookEventName": "preToolUse",
                "toolName": "bash",
                "toolArgs": '{"command":"git status"',
            },
            "copilot",
        )

        report = self.kernel.evaluate(event)

        self.assertTrue(event.stateful_mutation)
        self.assertEqual("CE_SESSION_REQUIRED", report.diagnostic_code)

    def test_ambiguous_snake_case_host_detection_uses_explicit_environment(self):
        raw = {"hook_event_name": "PreToolUse", "session_id": "s"}
        for host in ("codex", "claude"):
            with self.subTest(host=host), mock.patch.dict(
                os.environ, {"CHAOS_ENGINE_HOST": host}, clear=False
            ):
                self.assertEqual(host, self.kernel.normalize_event(raw).host)

    def test_repository_guard_keeps_legacy_host_environment_outside_portable_kernel(self):
        from scripts.agents import guard as repository_guard

        raw = {"hook_event_name": "PreToolUse", "session_id": "s"}
        with mock.patch.dict(
            os.environ,
            {"SHAFT_GUARD_HOST": "claude", "CHAOS_ENGINE_HOST": "codex"},
            clear=False,
        ):
            self.assertEqual("codex", self.kernel.normalize_event(raw).host)
            self.assertEqual("claude", repository_guard.hook_host(raw))

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
        self.assertEqual("Complete", stopped.phase)
        self.assertEqual("complete", stopped.terminal_reason)
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
            predicate_code="always",
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

    def test_rule_validation_detects_wildcard_conflicts_and_unproven_remedies(self):
        wildcard = self.kernel.Rule(
            code="CE_WILDCARD",
            event="*",
            priority=50,
            decision="deny",
            remedy="Retry with a session.",
            terminal=False,
            predicate_code="always",
            remedy_code="retry_with_session",
        )
        specific = self.kernel.Rule(
            code="CE_SPECIFIC",
            event="Stop",
            priority=50,
            decision="allow",
            remedy=None,
            terminal=True,
            predicate_code="always",
        )
        unproven = self.kernel.Rule(
            code="CE_UNPROVEN",
            event="PreToolUse",
            priority=40,
            decision="deny",
            remedy="Try something else.",
            terminal=False,
            predicate_code="always",
        )

        errors = self.kernel.validate_rules((wildcard, specific, unproven))

        self.assertTrue(any("wildcard" in error.casefold() for error in errors))
        self.assertTrue(any("satisfiable remedy" in error.casefold() for error in errors))

    def test_rule_validation_rejects_equal_priority_semantic_conflicts(self):
        wildcard = self.kernel.Rule(
            code="CE_WILDCARD_RETRY",
            event="*",
            priority=50,
            decision="deny",
            remedy="Retry with a session.",
            terminal=False,
            predicate_code="missing_session",
            remedy_code="retry_with_session",
        )
        terminal = self.kernel.Rule(
            code="CE_SPECIFIC_TERMINAL",
            event="PreToolUse",
            priority=50,
            decision="deny",
            remedy=None,
            terminal=True,
            predicate_code="always",
        )
        different_remedy = self.kernel.Rule(
            code="CE_SPECIFIC_REMEDY",
            event="Stop",
            priority=50,
            decision="deny",
            remedy="Use a different recovery action.",
            terminal=False,
            predicate_code="missing_session",
            remedy_code="retry_with_session",
        )

        errors = self.kernel.validate_rules((wildcard, terminal, different_remedy))

        self.assertGreaterEqual(
            sum("wildcard" in error.casefold() for error in errors),
            2,
        )

    def test_rule_validation_uses_closed_structural_predicates(self):
        always_true = self.kernel.Rule(
            code="CE_FALSE_REMEDY",
            event="PreToolUse",
            priority=40,
            decision="deny",
            remedy="Retry with a session.",
            terminal=False,
            predicate_code="always",
            remedy_code="retry_with_session",
        )

        custom = self.kernel.Rule(
            code="CE_CUSTOM",
            event="PreToolUse",
            priority=30,
            decision="allow",
            remedy=None,
            terminal=False,
            predicate_code="run_arbitrary_python",
        )

        errors = self.kernel.validate_rules((always_true, custom))

        self.assertTrue(any("remedy" in error.casefold() for error in errors))
        self.assertTrue(any("predicate" in error.casefold() for error in errors))
        report = self.kernel.evaluate(
            self.kernel.normalize_event(
                {"hook_event_name": "PreToolUse", "session_id": "s"}, "codex"
            ),
            rules=(custom,),
        )
        self.assertEqual("CE_INVALID_RULESET", report.diagnostic_code)

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

    def test_terminal_allow_rule_cannot_bypass_lifecycle_transition(self):
        terminal_allow = self.kernel.Rule(
            code="CE_EARLY_COMPLETE",
            event="Stop",
            priority=10,
            decision="allow",
            remedy=None,
            terminal=True,
            predicate_code="missing_session",
        )
        report = self.kernel.evaluate(
            self.kernel.normalize_event(
                {
                    "hook_event_name": "Stop",
                    "phase": "Planned",
                },
                "codex",
            ),
            rules=(terminal_allow,),
        )

        self.assertEqual("deny", report.decision)
        self.assertEqual("CE_INVALID_TRANSITION", report.diagnostic_code)
        self.assertEqual("Planned", report.phase)

    def test_cancel_timeout_unknown_and_repeated_events_have_bounded_outcomes(self):
        cancelled = self.kernel.evaluate(
            self.kernel.normalize_event(
                {
                    "hook_event_name": "PreToolUse",
                    "session_id": "cancelled-session",
                    "phase": "Do",
                    "cancelled": True,
                },
                "codex",
            )
        )
        timed_out = self.kernel.evaluate(
            self.kernel.normalize_event(
                {
                    "hook_event_name": "Stop",
                    "session_id": "timeout-session",
                    "phase": "Check",
                    "timeout": True,
                },
                "copilot",
            )
        )
        malformed = self.kernel.evaluate(
            self.kernel.normalize_event(
                {"hook_event_name": "", "session_id": "malformed"}, "gemini"
            )
        )
        repeated = self.kernel.evaluate(
            self.kernel.normalize_event(
                {
                    "hook_event_name": "Stop",
                    "session_id": "repeat",
                    "phase": "Complete",
                },
                "claude",
            )
        )
        incomplete_stop = self.kernel.evaluate(
            self.kernel.normalize_event(
                {
                    "hook_event_name": "Stop",
                    "session_id": "incomplete",
                    "phase": "Do",
                },
                "codex",
            )
        )

        self.assertEqual(
            ("Blocked", "blocked", "CE_CANCELLED"),
            (cancelled.phase, cancelled.terminal_reason, cancelled.diagnostic_code),
        )
        self.assertEqual(
            ("Blocked", "blocked", "CE_TIMEOUT"),
            (timed_out.phase, timed_out.terminal_reason, timed_out.diagnostic_code),
        )
        self.assertEqual(
            ("Blocked", "blocked", "CE_UNKNOWN_EVENT"),
            (malformed.phase, malformed.terminal_reason, malformed.diagnostic_code),
        )
        self.assertEqual(
            ("Complete", "complete", "CE_TERMINAL_REPLAY"),
            (repeated.phase, repeated.terminal_reason, repeated.diagnostic_code),
        )
        self.assertEqual(
            ("Blocked", "blocked", "CE_STOP_INCOMPLETE"),
            (
                incomplete_stop.phase,
                incomplete_stop.terminal_reason,
                incomplete_stop.diagnostic_code,
            ),
        )

    def test_session_evaluation_persists_terminal_phase_and_deduplicates_replays(self):
        with tempfile.TemporaryDirectory() as directory:
            journal = self.kernel.EffectJournal(Path(directory) / "state-v2.jsonl")
            event = self.kernel.normalize_event(
                {"hook_event_name": "Stop", "session_id": "session-one"},
                "codex",
            )

            first = self.kernel.evaluate_session(event, journal)
            repeated = self.kernel.evaluate_session(event, journal)

            self.assertEqual(
                ("Complete", "CE_STOP_COMPLETE"),
                (first.phase, first.diagnostic_code),
            )
            self.assertEqual(
                ("Complete", "CE_TERMINAL_REPLAY"),
                (repeated.phase, repeated.diagnostic_code),
            )
            records = journal.records("session-one")
            self.assertEqual(1, len(records))
            self.assertEqual("Complete", records[0]["phase"])
            self.assertEqual([], journal.records("session-other"))

    def test_session_evaluation_loads_prior_phase_before_native_event(self):
        with tempfile.TemporaryDirectory() as directory:
            journal = self.kernel.EffectJournal(Path(directory) / "state-v2.jsonl")
            planned = self.kernel.normalize_event(
                {
                    "hook_event_name": "UserPromptSubmit",
                    "session_id": "session-two",
                    "target_phase": "Planned",
                },
                "claude",
            )
            self.assertEqual("Planned", self.kernel.evaluate_session(planned, journal).phase)

            native_stop = self.kernel.normalize_event(
                {"hook_event_name": "Stop", "session_id": "session-two"},
                "claude",
            )
            stopped = self.kernel.evaluate_session(native_stop, journal)

            self.assertEqual("Blocked", stopped.phase)
            self.assertEqual("CE_STOP_INCOMPLETE", stopped.diagnostic_code)
            self.assertEqual("Blocked", journal.records("session-two")[-1]["phase"])

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

    def test_snapshot_nested_fact_lookup_is_deadlock_free_and_exactly_once(self):
        calls = []
        snapshot = None

        def root():
            calls.append("root")
            return "repository"

        def branch():
            calls.append("branch")
            return f"{snapshot.fact('root')}/main"

        snapshot = self.kernel.HarnessSnapshot(providers={"root": root, "branch": branch})
        values = []
        threads = [
            threading.Thread(target=lambda: values.append(snapshot.fact("branch")))
            for _ in range(8)
        ]
        for thread in threads:
            thread.start()
        for thread in threads:
            thread.join(timeout=1)

        self.assertTrue(all(not thread.is_alive() for thread in threads))
        self.assertEqual(["repository/main"] * 8, values)
        self.assertEqual(1, calls.count("root"))
        self.assertEqual(1, calls.count("branch"))

    def test_snapshot_provider_never_runs_under_shared_lock(self):
        probe_finished = threading.Event()
        probe = None

        def provider():
            nonlocal probe
            probe = threading.Thread(
                target=lambda: (snapshot.used_facts, probe_finished.set())
            )
            probe.start()
            return probe_finished.wait(0.2)

        snapshot = self.kernel.HarnessSnapshot(providers={"branch": provider})

        self.assertTrue(snapshot.fact("branch"))
        probe.join(timeout=1)

    def test_snapshot_bounds_cross_thread_cycles_and_runs_each_provider_once(self):
        a_started = threading.Event()
        b_started = threading.Event()
        calls = []
        snapshot = None

        def a_provider():
            calls.append("a")
            a_started.set()
            b_started.wait(0.4)
            return f"a:{snapshot.fact('b')}"

        def b_provider():
            calls.append("b")
            b_started.set()
            a_started.wait(0.4)
            return f"b:{snapshot.fact('a')}"

        snapshot = self.kernel.HarnessSnapshot(
            providers={"a": a_provider, "b": b_provider}
        )
        values = []
        started = time.monotonic()
        threads = [
            threading.Thread(
                target=lambda fact_name=name: values.append(snapshot.fact(fact_name))
            )
            for name in ("a", "b")
        ]
        for thread in threads:
            thread.start()
        for thread in threads:
            thread.join(timeout=1)

        self.assertTrue(all(not thread.is_alive() for thread in threads))
        self.assertLess(time.monotonic() - started, 0.25)
        self.assertEqual(1, calls.count("a"))
        self.assertEqual(1, calls.count("b"))

    def test_snapshot_bounds_wait_for_slow_cross_thread_provider(self):
        provider_started = threading.Event()
        calls = []

        def provider():
            calls.append("slow")
            provider_started.set()
            time.sleep(0.4)
            return "done"

        snapshot = self.kernel.HarnessSnapshot(providers={"slow": provider})
        owner = threading.Thread(target=lambda: snapshot.fact("slow"))
        owner.start()
        self.assertTrue(provider_started.wait(0.2))

        started = time.monotonic()
        self.assertEqual("unknown", snapshot.fact("slow"))
        self.assertLess(time.monotonic() - started, 0.25)
        owner.join(timeout=1)
        self.assertEqual(["slow"], calls)

    def test_snapshot_bounds_first_caller_and_late_result_cannot_overwrite_unknown(self):
        release = threading.Event()
        calls = []

        def provider():
            calls.append("slow")
            release.wait(1)
            return "late"

        snapshot = self.kernel.HarnessSnapshot(
            providers={"slow": provider}, wait_timeout=0.03
        )
        started = time.monotonic()
        self.assertEqual("unknown", snapshot.fact("slow"))
        self.assertLess(time.monotonic() - started, 0.15)
        release.set()
        time.sleep(0.03)
        self.assertEqual("unknown", snapshot.fact("slow"))
        self.assertEqual(["slow"], calls)

    def test_snapshot_provider_exit_still_signals_and_commits_unknown(self):
        calls = []

        def provider():
            calls.append("boom")
            raise SystemExit(2)

        snapshot = self.kernel.HarnessSnapshot(
            providers={"fact": provider}, wait_timeout=0.1
        )
        self.assertEqual("unknown", snapshot.fact("fact"))
        self.assertEqual("unknown", snapshot.fact("fact"))
        self.assertEqual(["boom"], calls)
        self.assertEqual(("fact",), snapshot.used_facts)

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

    def test_effect_journal_rejects_malformed_v2_before_idempotency_check(self):
        with tempfile.TemporaryDirectory() as directory:
            path = Path(directory) / "state-v2.jsonl"
            effect = self.kernel.Effect("s", "PreToolUse", "call", "rule", "record")
            malformed = effect.to_record()
            malformed.pop("event")
            path.write_text(json.dumps(malformed) + "\n", encoding="utf-8")
            journal = self.kernel.EffectJournal(path)

            with self.assertRaises(self.kernel.JournalCorruptionError):
                journal.append(effect)

            malformed = effect.to_record()
            malformed["sessionId"] = 7
            path.write_text(json.dumps(malformed) + "\n", encoding="utf-8")
            with self.assertRaises(self.kernel.JournalCorruptionError):
                journal.records("s")

    def test_effect_journal_validates_legacy_v1_before_idempotency_check(self):
        with tempfile.TemporaryDirectory() as directory:
            path = Path(directory) / "state-v1.jsonl"
            effect = self.kernel.Effect("s", "PreToolUse", "call", "rule", "record")
            valid = effect.to_record()
            valid["schemaVersion"] = 1
            valid["identity"] = "act-as-mohab"
            valid["idempotencyKey"] = hashlib.sha256(
                "\0".join(
                    valid[field]
                    for field in ("sessionId", "event", "toolCallId", "rule", "effect")
                ).encode("utf-8")
            ).hexdigest()
            path.write_text(json.dumps(valid) + "\n", encoding="utf-8")
            journal = self.kernel.EffectJournal(path)

            self.assertFalse(journal.append(effect))

            for field, value in (
                ("event", None),
                ("sessionId", 7),
                ("identity", "foreign-kernel"),
                ("idempotencyKey", "0" * 64),
            ):
                with self.subTest(field=field):
                    malformed = dict(valid)
                    if value is None:
                        malformed.pop(field)
                    else:
                        malformed[field] = value
                    path.write_text(json.dumps(malformed) + "\n", encoding="utf-8")
                    with self.assertRaises(self.kernel.JournalCorruptionError):
                        journal.append(effect)

            ambiguous = dict(valid)
            ambiguous["toolCallId"] = "call\0rule"
            ambiguous["rule"] = "record"
            ambiguous["effect"] = ""
            ambiguous["idempotencyKey"] = hashlib.sha256(
                "\0".join(
                    ambiguous[field]
                    for field in ("sessionId", "event", "toolCallId", "rule", "effect")
                ).encode("utf-8")
            ).hexdigest()
            path.write_text(json.dumps(ambiguous) + "\n", encoding="utf-8")
            with self.assertRaises(self.kernel.JournalCorruptionError):
                journal.records("s")

    def test_effect_v2_key_uses_unambiguous_json_tuple_framing(self):
        left = self.kernel.Effect("a\0b", "c", "d", "e", "f")
        right = self.kernel.Effect("a", "b\0c", "d", "e", "f")

        self.assertNotEqual(left.key, right.key)

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
    main()
