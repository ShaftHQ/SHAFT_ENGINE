"""End-to-end portable ChaosEngine runtime contract tests (#4746)."""

import contextlib
import io
import json
import subprocess  # nosec B404 - fixed local runtime test commands.
import sys
import tempfile
import unittest
import zipfile
from pathlib import Path
from unittest import mock

from scripts.agents import act_as_mohab_cli
from scripts.agents.act_as_mohab_cli import checkpoint_status
from scripts.agents.repository_context import RepositoryContext, RepositoryContextError
from scripts.ci.assemble_act_as_mohab_plugin import assemble


ROOT = Path(__file__).resolve().parents[2]
PYTHON = str(Path(sys.executable).resolve())


class ActAsMohabRuntimeTest(unittest.TestCase):
    def setUp(self):
        self.temporary_directory = tempfile.TemporaryDirectory()
        self.addCleanup(self.temporary_directory.cleanup)
        self.base = Path(self.temporary_directory.name)
        self.package_root = self.base / "act-as-mohab"

    def assemble_runtime(self) -> Path:
        assemble(ROOT, self.package_root)
        return self.package_root / "bin/act-as-mohab.pyz"

    def test_issue_labels_returns_success_after_reconciliation(self):
        context = RepositoryContext("consumer/project", ROOT, None)
        payload = {"kind": "label-reconciliation", "applied": True}
        stdout = io.StringIO()
        with (
            mock.patch.object(act_as_mohab_cli, "context_from_arguments", return_value=context),
            mock.patch.object(act_as_mohab_cli, "reconcile_labels", return_value=payload),
            contextlib.redirect_stdout(stdout),
        ):
            result = act_as_mohab_cli.main(["issue-labels", "--apply"])

        self.assertEqual(0, result)
        self.assertEqual(payload, json.loads(stdout.getvalue()))

    def test_issue_labels_maps_github_unavailability_to_environment_error(self):
        context = RepositoryContext("consumer/project", ROOT, None)
        stderr = io.StringIO()
        with (
            mock.patch.object(act_as_mohab_cli, "context_from_arguments", return_value=context),
            mock.patch.object(
                act_as_mohab_cli,
                "reconcile_labels",
                side_effect=act_as_mohab_cli.GitHubUnavailable("offline"),
            ),
            contextlib.redirect_stderr(stderr),
        ):
            result = act_as_mohab_cli.main(["issue-labels", "--apply"])

        self.assertEqual(act_as_mohab_cli.EXIT_ENVIRONMENT_ERROR, result)
        self.assertIn("issue filing failed: offline", stderr.getvalue())

    def test_delivery_status_passes_complete_repository_context_arguments(self):
        manifest = self.base / "manifest.json"
        receipt = self.base / "receipt.json"
        manifest.write_text('{"ownedPullRequests": []}', encoding="utf-8")
        context = RepositoryContext("consumer/project", self.base, None)
        allowed = {"decision": "allow"}
        with (
            mock.patch.object(
                act_as_mohab_cli, "resolve_repository_context", autospec=True,
                return_value=context,
            ) as resolve,
            mock.patch.object(act_as_mohab_cli, "collect_delivery", return_value=[]),
            mock.patch.object(act_as_mohab_cli, "inspect_cleanup", return_value={}),
            mock.patch.object(act_as_mohab_cli, "evaluate_delivery", return_value=allowed),
            mock.patch.object(act_as_mohab_cli, "local_head", return_value="abc"),
        ):
            try:
                result = act_as_mohab_cli.main([
                    "delivery-status", "--manifest", str(manifest),
                    "--root", str(self.base), "--receipt-out", str(receipt),
                ])
            except TypeError as error:
                self.fail(f"delivery-status omitted required context arguments: {error}")

        self.assertEqual(0, result)
        resolve.assert_called_once_with(
            explicit_repo=None, pr=None, explicit_root=self.base.resolve(), cwd=self.base.resolve()
        )

    def test_runtime_exposes_all_public_commands_from_canonical_modules(self):
        runtime = self.assemble_runtime()
        self.assertTrue(runtime.is_file(), "portable runtime archive must be assembled")
        with zipfile.ZipFile(runtime) as archive:
            self.assertEqual(
                sorted(archive.namelist()),
                [
                    "__main__.py", "act_as_mohab_cli.py", "delivery_status.py", "github_client.py",
                    "issue_filing.py", "planning_contract.py", "pr_audit.py", "repository_context.py",
                    "watch_pr_checks.py",
                ],
            )

        result = subprocess.run(  # nosec B603
            [PYTHON, str(runtime), "--help"],
            cwd=self.base,
            capture_output=True,
            text=True,
            check=False,
        )
        self.assertEqual(0, result.returncode, result.stderr)
        for command in (
            "repository-context", "watch-pr-checks", "checkpoint-status", "plan-validate",
            "pr-audit", "delivery-status", "issue-plan", "issue-create", "issue-labels",
            "issue-transition", "mcp"
            , "merge-authority"
        ):
            self.assertIn(command, result.stdout)

    def test_repository_context_and_mcp_run_from_an_unrelated_repository(self):
        runtime = self.assemble_runtime()
        consumer = self.base / "unrelated-repository"
        consumer.mkdir()

        context = subprocess.run(  # nosec B603
            [
                PYTHON,
                str(runtime),
                "repository-context",
                "--repo",
                "consumer/project",
                "--root",
                str(consumer),
            ],
            cwd=consumer,
            capture_output=True,
            text=True,
            check=False,
        )
        self.assertEqual(0, context.returncode, context.stderr)
        self.assertEqual(
            {"repo": "consumer/project", "root": str(consumer.resolve()), "pr": None},
            json.loads(context.stdout),
        )
        self.assertNotIn(str(ROOT), context.stdout)

        requests = "\n".join(
            json.dumps(request)
            for request in (
                {"jsonrpc": "2.0", "id": 1, "method": "initialize", "params": {}},
                {"jsonrpc": "2.0", "id": 2, "method": "tools/list", "params": {}},
                {
                    "jsonrpc": "2.0",
                    "id": 3,
                    "method": "tools/call",
                    "params": {
                        "name": "repository_context",
                        "arguments": {"repo": "consumer/project", "root": str(consumer)},
                    },
                },
            )
        ) + "\n"
        mcp_config = json.loads((self.package_root / ".mcp.json").read_text(encoding="utf-8"))[
            "mcpServers"
        ]["chaosengine"]
        launch_cwd = (self.package_root / mcp_config["cwd"]).resolve()
        mcp = subprocess.run(  # nosec B603
            [mcp_config["command"], *mcp_config["args"]],
            cwd=launch_cwd,
            input=requests,
            capture_output=True,
            text=True,
            check=False,
        )
        self.assertEqual(0, mcp.returncode, mcp.stderr)
        responses = [json.loads(line) for line in mcp.stdout.splitlines()]
        self.assertEqual([1, 2, 3], [response["id"] for response in responses])
        tools = {tool["name"] for tool in responses[1]["result"]["tools"]}
        self.assertEqual(
            {
                "repository_context", "watch_pr_checks", "checkpoint_status", "plan_validate",
                "pr_audit",
                "delivery_status",
                "issue_plan",
            },
            tools,
        )
        payload = json.loads(responses[2]["result"]["content"][0]["text"])
        self.assertEqual("consumer/project", payload["repo"])
        self.assertEqual(str(consumer.resolve()), payload["root"])
        self.assertEqual(self.package_root.resolve(), launch_cwd)

    def test_checkpoint_status_selects_only_an_exact_head_pull_request(self):
        context = RepositoryContext("consumer/project", self.base, None)
        calls = []

        def runner(command, **_kwargs):
            calls.append(command)
            if "rev-parse" in command:
                return subprocess.CompletedProcess(command, 0, stdout="abc123\n", stderr="")
            return subprocess.CompletedProcess(
                command,
                0,
                stdout=json.dumps(
                    [[
                        {
                            "number": number,
                            "html_url": f"https://github.com/consumer/project/pull/{number}",
                            "state": "open",
                            "draft": False,
                            "head": {"sha": "old"},
                        }
                        for number in range(1, 102)
                    ]
                    + [
                        {
                            "number": 102,
                            "html_url": "https://github.com/consumer/project/pull/102",
                            "state": "closed",
                            "draft": False,
                            "head": {"sha": "abc123"},
                        },
                        {
                            "number": 103,
                            "html_url": "https://github.com/consumer/project/pull/103",
                            "state": "open",
                            "draft": True,
                            "head": {"sha": "abc123"},
                        },
                    ]]
                ),
                stderr="",
            )

        status = checkpoint_status(
            context,
            runner=runner,
            executable_resolver=lambda name: name,
        )

        self.assertEqual("abc123", status["head"])
        self.assertEqual(103, status["pullRequest"]["number"])
        self.assertIn("repos/consumer/project/commits/abc123/pulls", calls[1])
        self.assertIn("--paginate", calls[1])

        def no_exact_open_pull_request(command, **_kwargs):
            if "rev-parse" in command:
                return subprocess.CompletedProcess(command, 0, stdout="abc123\n", stderr="")
            return subprocess.CompletedProcess(
                command,
                0,
                stdout=json.dumps(
                    [[
                        {
                            "number": 1, "html_url": "https://example.test/pull/1",
                            "state": "closed", "draft": False, "head": {"sha": "abc123"},
                        },
                        {
                            "number": 2, "html_url": "https://example.test/pull/2",
                            "state": "open", "draft": False, "head": {"sha": "different"},
                        },
                    ]]
                ),
                stderr="",
            )

        self.assertIsNone(
            checkpoint_status(
                context,
                runner=no_exact_open_pull_request,
                executable_resolver=lambda name: name,
            )["pullRequest"]
        )

        with self.assertRaises(RepositoryContextError):
            checkpoint_status(
                context,
                runner=lambda *_args, **_kwargs: (_ for _ in ()).throw(OSError("unavailable")),
                executable_resolver=lambda name: name,
            )

    def test_checkpoint_status_slurps_and_validates_every_api_page(self):
        context = RepositoryContext("consumer/project", self.base, None)
        exact = {
            "number": 104,
            "html_url": "https://github.com/consumer/project/pull/104",
            "state": "open",
            "draft": False,
            "head": {"sha": "abc123"},
        }

        def runner(command, **_kwargs):
            if "rev-parse" in command:
                return subprocess.CompletedProcess(command, 0, stdout="abc123\n", stderr="")
            # Without --slurp gh emits one JSON document per page.  With it,
            # those same pages become one JSON array that can be validated.
            stdout = json.dumps([[], [exact]]) if "--slurp" in command else f"[]\n{json.dumps([exact])}\n"
            return subprocess.CompletedProcess(command, 0, stdout=stdout, stderr="")

        try:
            status = checkpoint_status(context, runner=runner, executable_resolver=lambda name: name)
        except RepositoryContextError as error:
            self.fail(f"multi-page checkpoint lookup must succeed: {error}")
        self.assertEqual(104, status["pullRequest"]["number"])

        for malformed in ([{"not": "a page"}], [[1]], [[{"state": "open", "head": []}]]):
            def malformed_runner(command, **_kwargs):
                if "rev-parse" in command:
                    return subprocess.CompletedProcess(command, 0, stdout="abc123\n", stderr="")
                return subprocess.CompletedProcess(command, 0, stdout=json.dumps(malformed), stderr="")

            with self.subTest(malformed=malformed), self.assertRaises(RepositoryContextError):
                checkpoint_status(
                    context,
                    runner=malformed_runner,
                    executable_resolver=lambda name: name,
                )

    def test_mcp_returns_errors_and_continues_after_untrusted_request_shapes(self):
        runtime = self.assemble_runtime()
        requests = "\n".join(
            (
                "not-json",
                "[1]",
                "1",
                json.dumps({"jsonrpc": "2.0", "id": 2, "method": "tools/call", "params": []}),
                json.dumps(
                    {
                        "jsonrpc": "2.0",
                        "id": 3,
                        "method": "tools/call",
                        "params": {"name": "repository_context", "arguments": []},
                    }
                ),
                json.dumps({"jsonrpc": "2.0", "id": 4, "method": "unknown", "params": {}}),
                json.dumps({"jsonrpc": "2.0", "method": "unknown-notification", "params": {}}),
                json.dumps(
                    {
                        "jsonrpc": "2.0",
                        "id": 5,
                        "method": "tools/call",
                        "params": {"name": "unknown_tool", "arguments": {}},
                    }
                ),
                json.dumps({"jsonrpc": "2.0", "id": 6, "method": "tools/list", "params": {}}),
            )
        ) + "\n"

        result = subprocess.run(  # nosec B603
            [PYTHON, str(runtime), "mcp"],
            cwd=self.base,
            input=requests,
            capture_output=True,
            text=True,
            check=False,
        )

        self.assertEqual(0, result.returncode, result.stderr)
        self.assertNotIn("Traceback", result.stderr)
        responses = [json.loads(line) for line in result.stdout.splitlines()]
        self.assertEqual(8, len(responses))
        self.assertTrue(all("error" in response for response in responses[:6]))
        self.assertTrue(responses[6]["result"]["isError"])
        self.assertIn("tools", responses[7]["result"])

    def test_mcp_enforces_json_rpc_envelopes_and_standard_error_codes(self):
        runtime = self.assemble_runtime()
        requests = "\n".join(
            (
                "not-json",
                "1",
                json.dumps({"jsonrpc": "1.0", "id": 2, "method": "tools/list"}),
                json.dumps({"jsonrpc": "2.0", "id": 3, "method": 7}),
                json.dumps({"jsonrpc": "2.0", "id": {}, "method": "tools/list"}),
                json.dumps({"jsonrpc": "2.0", "id": [], "method": "tools/list"}),
                json.dumps({"jsonrpc": "2.0", "id": True, "method": "tools/list"}),
                json.dumps({"jsonrpc": "2.0", "id": 4, "method": "unknown"}),
                json.dumps({"jsonrpc": "2.0", "id": 5, "method": "tools/list", "params": []}),
                json.dumps({"jsonrpc": "2.0", "method": "unknown-notification"}),
                json.dumps({"jsonrpc": "2.0", "id": 6, "method": "tools/list"}),
                "",
            )
        )
        result = subprocess.run(  # nosec B603
            [PYTHON, str(runtime), "mcp"], cwd=self.base, input=requests,
            capture_output=True, text=True, check=False,
        )

        self.assertEqual(0, result.returncode, result.stderr)
        self.assertNotIn("Traceback", result.stderr)
        responses = [json.loads(line) for line in result.stdout.splitlines()]
        self.assertEqual([
            -32700, -32600, -32600, -32600, -32600, -32600, -32600, -32601, -32602,
        ], [
            response.get("error", {}).get("code") for response in responses[:-1]
        ])
        self.assertEqual([None, None, None], [response["id"] for response in responses[4:7]])
        self.assertEqual(6, responses[-1]["id"])
        self.assertIn("tools", responses[-1]["result"])


if __name__ == "__main__":
    unittest.main()
