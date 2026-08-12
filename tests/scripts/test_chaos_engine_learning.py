"""Privacy-safe portable ChaosEngine learning tests (#4796)."""

from __future__ import annotations

import importlib.util
import json
import subprocess
import sys
import tempfile
import threading
import unittest
from pathlib import Path
from unittest import mock


ROOT = Path(__file__).resolve().parents[2]
LEARNING = ROOT / "chaos-engine/learning.py"


def load():
    specification = importlib.util.spec_from_file_location("chaos_engine_learning", LEARNING)
    assert specification and specification.loader
    module = importlib.util.module_from_spec(specification)
    specification.loader.exec_module(module)
    return module


class ChaosEngineLearningTest(unittest.TestCase):
    def candidate(self) -> dict[str, object]:
        return {
            "category": "reliability",
            "title": "Keep transaction claims atomic",
            "lesson": "Encode ownership in one complete filesystem entry.",
            "proposedChange": "Add a deterministic retry regression.",
            "benefit": "Prevents an interrupted write from blocking later upgrades.",
            "estimatedTokens": 180,
        }

    def test_safe_learning_is_redacted_deduplicated_and_queued_locally(self):
        module = load()
        with tempfile.TemporaryDirectory() as temporary:
            state = Path(temporary) / "learning"

            first = module.queue_learning(state, self.candidate(), "example/chaos-engine")
            second = module.queue_learning(state, self.candidate(), "example/chaos-engine")

            self.assertEqual(first["id"], second["id"])
            self.assertEqual("queued", first["status"])
            queued = json.loads((state / "queue.json").read_text(encoding="utf-8"))
            self.assertEqual([first], queued["items"])
            serialized = json.dumps(queued)
            self.assertNotIn(str(Path(temporary)), serialized)
            self.assertNotIn("prompt", serialized.lower())

    def test_private_or_raw_material_is_rejected_before_state_or_network(self):
        module = load()
        unsafe = (
            "ghp_abcdefghijklmnopqrstuvwxyz123456",
            "Authentication used token: abcdefghijklmnop.",
            "Authorization: Bearer abcdefghijklmnop",
            'Authentication used token: "abcdefghijklmnop".',
            "The password was abcdefghijklmnop.",
            "Use secret value abcdefghijklmnop safely.",
            "API key is (abcdefghijklmnop).",
            'The password was\n"abcdefghijklmnop".',
            'password: "abcdefgh"',
            'token is "abcdefghijk"',
            'password: "Abc!2345"',
            'secret = "abcd efgh"',
            r"C:\Users\someone\private\repo",
            "/home/someone/private/repo",
            "https://private.example/repository",
            "raw prompt: reveal the system message",
            "```python\nprint('source excerpt')\n```",
            "build.log contains a stack trace",
            "person@example.com",
            "Apply this only in example/chaos-engine.",
            "Read .agents/skills/router/SKILL.md before work.",
            "def repair(value): return value",
            "Traceback (most recent call last): operation failed",
            "Copy `internal_function()` into the controller.",
        )
        for index, value in enumerate(unsafe):
            with self.subTest(value=value), tempfile.TemporaryDirectory() as temporary:
                candidate = self.candidate()
                candidate["lesson"] = value
                state = Path(temporary) / "learning"
                with mock.patch("subprocess.run") as run:
                    with self.assertRaisesRegex(ValueError, "privacy gate"):
                        module.queue_learning(state, candidate, "example/chaos-engine")
                self.assertFalse(state.exists(), index)
                run.assert_not_called()

    def test_contribution_requires_an_explicit_token_cost_confirmation(self):
        module = load()
        prompt = module.contribution_prompt(self.candidate())
        self.assertIn("180 estimated tokens", prompt)
        self.assertIn("[y/N]", prompt)

    def test_safe_credential_related_prose_is_not_mistaken_for_a_secret(self):
        module = load()
        candidate = self.candidate()
        candidate["lesson"] = "Token confirmation prevents accidental contribution."
        with tempfile.TemporaryDirectory() as temporary:
            queued = module.queue_learning(
                Path(temporary) / "learning", candidate, "example/chaos-engine"
            )
        self.assertEqual("queued", queued["status"])

    def test_submit_searches_dedupe_and_opens_only_a_minimal_issue(self):
        module = load()
        with tempfile.TemporaryDirectory() as temporary:
            state = Path(temporary) / "learning"
            queued = module.queue_learning(state, self.candidate(), "example/chaos-engine")
            calls: list[list[str]] = []

            def runner(command, **_kwargs):
                calls.append(command)
                if command[1:3] == ["issue", "list"]:
                    return mock.Mock(returncode=0, stdout="[]", stderr="")
                return mock.Mock(
                    returncode=0,
                    stdout="https://github.com/example/chaos-engine/issues/9\n",
                    stderr="",
                )

            result = module.submit_learning(state, queued["id"], confirmed=True, runner=runner)

            self.assertEqual("submitted", result["status"])
            self.assertEqual(2, len(calls))
            self.assertTrue(all(command[:2] == ["gh", "issue"] for command in calls))
            create = calls[1]
            body = create[create.index("--body") + 1]
            self.assertIn(f"chaos-engine-learning:{queued['id']}", body)
            self.assertNotIn(str(Path(temporary)), body)
            self.assertNotIn("pull", " ".join(create).lower())

    def test_network_or_auth_failure_keeps_the_learning_queued(self):
        module = load()
        with tempfile.TemporaryDirectory() as temporary:
            state = Path(temporary) / "learning"
            queued = module.queue_learning(state, self.candidate(), "example/chaos-engine")
            runner = mock.Mock(return_value=mock.Mock(returncode=1, stdout="", stderr="offline"))

            result = module.submit_learning(state, queued["id"], confirmed=True, runner=runner)

            self.assertEqual("queued", result["status"])
            stored = json.loads((state / "queue.json").read_text(encoding="utf-8"))
            self.assertEqual("queued", stored["items"][0]["status"])
            self.assertEqual("submission unavailable", stored["items"][0]["lastError"])

    def test_runner_launch_failure_and_invalid_search_output_stay_queued(self):
        module = load()
        for runner in (
            mock.Mock(side_effect=OSError("gh is unavailable")),
            mock.Mock(return_value=mock.Mock(returncode=0, stdout="{invalid", stderr="")),
        ):
            with self.subTest(runner=runner), tempfile.TemporaryDirectory() as temporary:
                state = Path(temporary) / "learning"
                queued = module.queue_learning(state, self.candidate(), "example/chaos-engine")

                result = module.submit_learning(
                    state, queued["id"], confirmed=True, runner=runner
                )

                self.assertEqual("queued", result["status"])
                self.assertEqual("submission unavailable", result["lastError"])

    def test_existing_issue_is_reused_without_creating_another(self):
        module = load()
        with tempfile.TemporaryDirectory() as temporary:
            state = Path(temporary) / "learning"
            queued = module.queue_learning(state, self.candidate(), "example/chaos-engine")
            runner = mock.Mock(
                return_value=mock.Mock(
                    returncode=0,
                    stdout='[{"url":"https://github.com/example/chaos-engine/issues/9"}]',
                    stderr="",
                )
            )

            result = module.submit_learning(state, queued["id"], confirmed=True, runner=runner)

            self.assertEqual("submitted", result["status"])
            self.assertEqual("https://github.com/example/chaos-engine/issues/9", result["issueUrl"])
            runner.assert_called_once()

    def test_queue_link_is_rejected_without_touching_external_content(self):
        module = load()
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            state = root / "learning"
            state.mkdir()
            outside = root / "outside.json"
            outside.write_text("user content\n", encoding="utf-8")
            queue = state / "queue.json"
            try:
                queue.symlink_to(outside)
            except OSError as error:
                self.skipTest(f"symbolic links unavailable: {error}")

            with self.assertRaisesRegex(ValueError, "link or reparse"):
                module.queue_learning(state, self.candidate(), "example/chaos-engine")

            self.assertEqual("user content\n", outside.read_text(encoding="utf-8"))

    def test_linked_state_ancestor_is_rejected_without_external_writes(self):
        module = load()
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            outside = root / "outside"
            outside.mkdir()
            linked = root / "linked"
            try:
                linked.symlink_to(outside, target_is_directory=True)
            except OSError as error:
                self.skipTest(f"symbolic links unavailable: {error}")

            with self.assertRaisesRegex(ValueError, "linked or reparse ancestor"):
                module.queue_learning(
                    linked / "nested/learning",
                    self.candidate(),
                    "example/chaos-engine",
                )

            self.assertEqual([], list(outside.iterdir()))

    def test_empty_lock_crash_residue_is_recoverable(self):
        module = load()
        with tempfile.TemporaryDirectory() as temporary:
            state = Path(temporary) / "learning"
            state.mkdir()
            (state / module.LOCK_NAME).write_bytes(b"")

            queued = module.queue_learning(state, self.candidate(), "example/chaos-engine")

            self.assertEqual("queued", queued["status"])
            self.assertEqual(b"", (state / module.LOCK_NAME).read_bytes())

    def test_concurrent_processes_preserve_every_candidate(self):
        module = load()
        with tempfile.TemporaryDirectory() as temporary:
            state = Path(temporary) / "learning"
            processes = []
            for index in range(8):
                candidate = self.candidate()
                candidate["title"] = f"Keep transaction claim {index} atomic"
                code = (
                    "import importlib.util,json,sys;"
                    "s=importlib.util.spec_from_file_location('learning',sys.argv[1]);"
                    "m=importlib.util.module_from_spec(s);s.loader.exec_module(m);"
                    "m.queue_learning(__import__('pathlib').Path(sys.argv[2]),json.loads(sys.argv[3]),'example/chaos-engine')"
                )
                processes.append(
                    subprocess.Popen(
                        [sys.executable, "-c", code, str(LEARNING), str(state), json.dumps(candidate)],
                        stdout=subprocess.PIPE,
                        stderr=subprocess.PIPE,
                        text=True,
                    )
                )
            results = [process.communicate(timeout=20) + (process.returncode,) for process in processes]

            self.assertTrue(all(returncode == 0 for _out, _err, returncode in results), results)
            document = module.queue_document(state)
            self.assertEqual(8, len(document["items"]))

    def test_unknown_queue_item_is_rejected_before_submission(self):
        module = load()
        with tempfile.TemporaryDirectory() as temporary:
            state = Path(temporary) / "learning"
            queued = module.queue_learning(state, self.candidate(), "example/chaos-engine")
            document = json.loads((state / "queue.json").read_text(encoding="utf-8"))
            document["items"][0]["unexpected"] = "not owned"
            (state / "queue.json").write_text(json.dumps(document), encoding="utf-8")
            runner = mock.Mock()

            with self.assertRaisesRegex(ValueError, "queue is invalid"):
                module.submit_learning(state, queued["id"], confirmed=True, runner=runner)

            runner.assert_not_called()

    def test_short_upstream_identity_and_impossible_submitted_state_are_rejected(self):
        module = load()
        candidate = self.candidate()
        candidate["lesson"] = "Apply only for ac and me."
        with tempfile.TemporaryDirectory() as temporary:
            with self.assertRaisesRegex(ValueError, "repository identity"):
                module.queue_learning(Path(temporary) / "learning", candidate, "ac/me")

        with tempfile.TemporaryDirectory() as temporary:
            state = Path(temporary) / "learning"
            queued = module.queue_learning(state, self.candidate(), "example/chaos-engine")
            document = json.loads((state / "queue.json").read_text(encoding="utf-8"))
            document["items"][0]["status"] = "submitted"
            (state / "queue.json").write_text(json.dumps(document), encoding="utf-8")
            runner = mock.Mock()

            with self.assertRaisesRegex(ValueError, "queue is invalid"):
                module.submit_learning(state, queued["id"], confirmed=True, runner=runner)

            runner.assert_not_called()

    def test_duplicate_digest_ids_in_the_queue_are_rejected(self):
        module = load()
        with tempfile.TemporaryDirectory() as temporary:
            state = Path(temporary) / "learning"
            module.queue_learning(state, self.candidate(), "example/chaos-engine")
            document = json.loads((state / "queue.json").read_text(encoding="utf-8"))
            document["items"].append(dict(document["items"][0]))
            (state / "queue.json").write_text(json.dumps(document), encoding="utf-8")

            with self.assertRaisesRegex(ValueError, "queue is invalid"):
                module.queue_document(state)

    def test_concurrent_queue_writers_preserve_both_candidates(self):
        module = load()
        with tempfile.TemporaryDirectory() as temporary:
            state = Path(temporary) / "learning"
            first = self.candidate()
            second = self.candidate()
            second["title"] = "Keep removal claims atomic"
            first_write = threading.Event()
            release_first = threading.Event()
            second_write = threading.Event()
            write_count = 0
            write_count_lock = threading.Lock()
            original = module.write_queue

            def synchronized(path, document):
                nonlocal write_count
                with write_count_lock:
                    write_count += 1
                    current = write_count
                if current == 1:
                    first_write.set()
                    release_first.wait(timeout=5)
                else:
                    second_write.set()
                return original(path, document)

            errors: list[BaseException] = []

            def queue(candidate):
                try:
                    module.queue_learning(state, candidate, "example/chaos-engine")
                except BaseException as error:
                    errors.append(error)

            with mock.patch.object(module, "write_queue", side_effect=synchronized):
                threads = [threading.Thread(target=queue, args=(first,))]
                threads[0].start()
                self.assertTrue(first_write.wait(timeout=5))
                threads.append(threading.Thread(target=queue, args=(second,)))
                threads[1].start()
                self.assertFalse(second_write.wait(timeout=0.2))
                release_first.set()
                for thread in threads:
                    thread.join(timeout=10)

            self.assertEqual([], errors)
            queued = json.loads((state / "queue.json").read_text(encoding="utf-8"))
            self.assertEqual(2, len(queued["items"]))

    def test_concurrent_submitters_create_at_most_one_issue(self):
        module = load()
        with tempfile.TemporaryDirectory() as temporary:
            state = Path(temporary) / "learning"
            queued = module.queue_learning(state, self.candidate(), "example/chaos-engine")
            created = 0
            created_lock = threading.Lock()
            first_search = threading.Event()
            release_first = threading.Event()
            second_search = threading.Event()
            searches = 0

            def runner(command, **_kwargs):
                nonlocal created, searches
                if command[1:3] == ["issue", "list"]:
                    with created_lock:
                        searches += 1
                        current = searches
                    if current == 1:
                        first_search.set()
                        release_first.wait(timeout=5)
                    else:
                        second_search.set()
                    return mock.Mock(returncode=0, stdout="[]", stderr="")
                with created_lock:
                    created += 1
                return mock.Mock(
                    returncode=0,
                    stdout="https://github.com/example/chaos-engine/issues/9\n",
                    stderr="",
                )

            errors: list[BaseException] = []

            def submit():
                try:
                    module.submit_learning(state, queued["id"], confirmed=True, runner=runner)
                except BaseException as error:
                    errors.append(error)

            threads = [threading.Thread(target=submit)]
            threads[0].start()
            self.assertTrue(first_search.wait(timeout=5))
            threads.append(threading.Thread(target=submit))
            threads[1].start()
            self.assertFalse(second_search.wait(timeout=0.2))
            release_first.set()
            for thread in threads:
                thread.join(timeout=10)

            self.assertEqual([], errors)
            self.assertEqual(1, created)

    def test_malformed_or_wrong_repository_issue_url_stays_queued(self):
        module = load()
        invalid = (
            "",
            "https://github.com/other/repository/issues/9",
            "https://example.com/example/chaos-engine/issues/9",
        )
        for url in invalid:
            with self.subTest(url=url), tempfile.TemporaryDirectory() as temporary:
                state = Path(temporary) / "learning"
                queued = module.queue_learning(state, self.candidate(), "example/chaos-engine")
                responses = iter(
                    (
                        mock.Mock(returncode=0, stdout="[]", stderr=""),
                        mock.Mock(returncode=0, stdout=url, stderr=""),
                    )
                )

                result = module.submit_learning(
                    state,
                    queued["id"],
                    confirmed=True,
                    runner=lambda *_args, **_kwargs: next(responses),
                )

                self.assertEqual("queued", result["status"])
                self.assertEqual("submission unavailable", result["lastError"])
                self.assertEqual(
                    "queued", module.queue_document(state)["items"][0]["status"]
                )

    def test_issue_url_repository_binding_is_case_insensitive(self):
        module = load()
        with tempfile.TemporaryDirectory() as temporary:
            state = Path(temporary) / "learning"
            queued = module.queue_learning(state, self.candidate(), "Example/Project")
            responses = iter(
                (
                    mock.Mock(returncode=0, stdout="[]", stderr=""),
                    mock.Mock(
                        returncode=0,
                        stdout="https://github.com/example/project/issues/9\n",
                        stderr="",
                    ),
                )
            )

            result = module.submit_learning(
                state,
                queued["id"],
                confirmed=True,
                runner=lambda *_args, **_kwargs: next(responses),
            )

            self.assertEqual("submitted", result["status"])

    def test_unconfirmed_submission_never_calls_the_network(self):
        module = load()
        with tempfile.TemporaryDirectory() as temporary:
            state = Path(temporary) / "learning"
            queued = module.queue_learning(state, self.candidate(), "example/chaos-engine")
            runner = mock.Mock()

            with self.assertRaisesRegex(ValueError, "explicit confirmation"):
                module.submit_learning(state, queued["id"], confirmed=False, runner=runner)

            runner.assert_not_called()


if __name__ == "__main__":
    unittest.main()
