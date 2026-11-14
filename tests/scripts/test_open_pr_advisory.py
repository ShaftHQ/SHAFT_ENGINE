"""Advisory when @me is at the open-PR cap. The command is not denied."""

from __future__ import annotations

import json
import os
import stat
import subprocess  # nosec B404 - fixed interpreter and hook.
import sys
import tempfile
import textwrap
import unittest
import uuid
from pathlib import Path

from tests.scripts.test_chaos_engine_hook import HOOK, isolated_session_environment

_GH = textwrap.dedent(
    """\
    #!/usr/bin/env python3
    import json, os, sys
    from pathlib import Path
    Path(os.environ["OPEN_PR_ARGV"]).write_text(json.dumps(sys.argv), encoding="utf-8")
    code = int(os.environ.get("OPEN_PR_EXIT", "0"))
    if code:
        sys.exit(code)
    args = sys.argv[1:]

    def flag(name, value):
        if name in args:
            index = args.index(name)
            return index + 1 < len(args) and args[index + 1] == value
        return any(item == f"{name}={value}" for item in args)

    rows = json.loads(os.environ.get("OPEN_PR_ROWS", "[]"))
    if not (flag("--author", "@me") and flag("--state", "open")):
        rows.append({"number": 999, "state": "OPEN", "author": {"login": "other"}})
    rows.append({"number": 50, "state": "CLOSED"})
    rows.append({"number": 51, "state": "MERGED"})
    print(json.dumps(rows))
    """
)

_HOST_ENV = (
    "CHAOS_ENGINE_HOST",
    "GROK_HOOK_EVENT",
    "CLAUDE_CODE_ENTRYPOINT",
    "CLAUDE_PLUGIN_ROOT",
    "CODEX_HOME",
)


class OpenPrAdvisoryTest(unittest.TestCase):
    def run_case(self, command: str, rows: list[dict], *, exit_code: int = 0, config: dict | None = None):
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            bin_dir = root / "bin"
            bin_dir.mkdir()
            gh = bin_dir / "gh"
            gh.write_text(_GH, encoding="utf-8")
            gh.chmod(gh.stat().st_mode | stat.S_IEXEC)
            if config is not None:
                dest = root / "chaos-engine" / "hooks"
                dest.mkdir(parents=True)
                (dest / "open-pr-advisory.json").write_text(json.dumps(config), encoding="utf-8")
            argv_path = root / "argv.json"
            env = isolated_session_environment()
            env["PATH"] = str(bin_dir) + os.pathsep + env.get("PATH", "")
            env["OPEN_PR_ARGV"] = str(argv_path)
            env["OPEN_PR_ROWS"] = json.dumps(rows)
            env["OPEN_PR_EXIT"] = str(exit_code)
            env["CHAOS_ENGINE_STORE_REFRESH"] = "0"
            for key in _HOST_ENV:
                env.pop(key, None)
            event = {
                "hook_event_name": "PreToolUse",
                "tool_name": "Bash",
                "session_id": f"open-pr-{uuid.uuid4().hex}",
                "cwd": str(root),
                "tool_input": {"command": command},
            }
            result = subprocess.run(  # nosec B603 - fixed interpreter and hook.
                [sys.executable, str(HOOK)],
                input=json.dumps(event),
                capture_output=True,
                text=True,
                check=False,
                env=env,
            )
            recorded = json.loads(argv_path.read_text(encoding="utf-8")) if argv_path.is_file() else None
            return result, recorded

    def assert_silent(self, result: subprocess.CompletedProcess[str]) -> None:
        self.assertEqual(0, result.returncode, result.stderr)
        payload = json.loads(result.stdout)
        self.assertNotIn("decision", payload)
        self.assertNotIn("additionalContext", payload)
        self.assertNotIn("combine", result.stdout)

    def assert_advises(self, result: subprocess.CompletedProcess[str], numbers: list[int]) -> None:
        self.assertEqual(0, result.returncode, result.stderr)
        payload = json.loads(result.stdout)
        self.assertNotIn("decision", payload)
        context = payload["additionalContext"]
        self.assertIn("combine", context)
        self.assertIn("finish", context.casefold())
        self.assertIn("merge", context.casefold())
        for number in numbers:
            self.assertIn(str(number), context)
        self.assertNotIn("50", context)
        self.assertNotIn("999", context)

    def test_at_limit_advises_and_under_limit_is_silent(self) -> None:
        under, _argv = self.run_case(
            "gh pr create --title t",
            [{"number": 11, "state": "OPEN", "isDraft": True}],
        )
        self.assert_silent(under)
        at_limit, argv = self.run_case(
            "gh pr create --title t",
            [
                {"number": 11, "state": "OPEN", "isDraft": True},
                {"number": 12, "state": "OPEN", "isDraft": False},
            ],
        )
        self.assert_advises(at_limit, [11, 12])
        self.assertIn("@me", argv)

    def test_closed_pull_request_is_not_counted(self) -> None:
        result, argv = self.run_case(
            "gh pr create --title t",
            [{"number": 11, "state": "OPEN", "isDraft": False}],
        )
        self.assertIsNotNone(argv)
        self.assert_silent(result)
        self.assertNotIn("50", result.stdout)
        self.assertNotIn("51", result.stdout)

    def test_other_author_is_not_counted(self) -> None:
        result, argv = self.run_case(
            "gh pr create --title t",
            [{"number": 11, "state": "OPEN", "isDraft": False}],
        )
        self.assertIsNotNone(argv)
        self.assertEqual(["pr", "list"], argv[1:3])
        self.assertIn("--author", argv)
        self.assertIn("@me", argv)
        self.assert_silent(result)
        self.assertNotIn("999", result.stdout)

    def test_gh_nonzero_does_not_deny(self) -> None:
        result, _argv = self.run_case(
            "gh pr create --title t",
            [
                {"number": 11, "state": "OPEN", "isDraft": True},
                {"number": 12, "state": "OPEN", "isDraft": False},
            ],
            exit_code=1,
        )
        self.assertEqual(0, result.returncode)
        payload = json.loads(result.stdout)
        self.assertNotIn("decision", payload)
        self.assertNotIn("additionalContext", payload)
        self.assertEqual(1, len(result.stderr.splitlines()))
        self.assertNotIn("deny", result.stderr.casefold())

    def test_invalid_review_wip_exits_2(self) -> None:
        result, _argv = self.run_case(
            "gh pr create --title t",
            [],
            config={"reviewWip": 0, "streams": 1},
        )
        self.assertEqual(2, result.returncode)
        self.assertIn("reviewWip", result.stdout)

    def test_streams_scale_the_cap(self) -> None:
        config = {"reviewWip": 2, "streams": 2}
        three = [
            {"number": 101, "state": "OPEN", "isDraft": False},
            {"number": 102, "state": "OPEN", "isDraft": True},
            {"number": 103, "state": "OPEN", "isDraft": False},
        ]
        under, _argv = self.run_case("gh pr create --title t", three, config=config)
        self.assert_silent(under)
        four = three + [{"number": 104, "state": "OPEN", "isDraft": False}]
        advised, _argv = self.run_case("gh pr create --title t", four, config=config)
        self.assert_advises(advised, [101, 102, 103, 104])

    def test_ready_and_reopen_use_the_same_advisory(self) -> None:
        rows = [
            {"number": 11, "state": "OPEN", "isDraft": True},
            {"number": 12, "state": "OPEN", "isDraft": False},
        ]
        for command in ("gh pr ready 8", "gh pr reopen 8"):
            with self.subTest(command=command):
                result, _argv = self.run_case(command, rows)
                self.assert_advises(result, [11, 12])


if __name__ == "__main__":
    unittest.main()
