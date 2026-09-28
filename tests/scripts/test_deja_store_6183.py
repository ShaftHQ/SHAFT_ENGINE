"""Opt-in deja transcript store contracts (#6183)."""

from __future__ import annotations

import importlib.util
import json
import os
import stat
import subprocess
import tempfile
import unittest
import unittest.mock
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
PLATFORMS = {
    "windows-x64",
    "windows-arm64",
    "linux-x64",
    "linux-arm64",
    "macos-x64",
    "macos-arm64",
}
INSTALLER_SOURCES = (
    "chaos-engine/install.py",
    "chaos-engine/bootstrap.py",
    "chaos-engine/hosts.py",
    "chaos-engine/dependencies.py",
    "chaos-engine/official_self_heal.py",
    "chaos-engine/install.sh",
    "chaos-engine/install.ps1",
)
NO_TRANSCRIPT_HOSTS = {"grok-bot", "copilot-cloud"}


def load(relative: str, name: str):
    path = ROOT / relative
    spec = importlib.util.spec_from_file_location(name, path)
    if spec is None or spec.loader is None:
        raise RuntimeError(f"cannot load {path}")
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def _fake_deja(directory: Path, payload: str) -> None:
    path = directory / "deja"
    path.write_text(
        "#!/usr/bin/env python3\n"
        "import sys\n"
        f"sys.stdout.write({payload!r})\n",
        encoding="utf-8",
    )
    path.chmod(path.stat().st_mode | stat.S_IEXEC)


class DejaStoreContractTest(unittest.TestCase):
    @classmethod
    def setUpClass(cls) -> None:
        cls.install = load("chaos-engine/install.py", "ce_install_deja_6183")
        cls.retrieve = load("chaos-engine/retrieve.py", "ce_retrieve_deja_6183")
        cls.policy = load("chaos-engine/mcp_policy.py", "ce_policy_deja_6183")

    def test_deja_store_is_opt_in_and_off_by_default(self) -> None:
        self.assertNotIn("deja", self.install.DEFAULT_BUNDLE_COMPONENTS)
        self.assertIn("deja", self.install.OPT_IN_BUNDLE_COMPONENTS)
        options = self.install.default_bundle_options()
        self.assertFalse(options["deja"])
        self.assertTrue(options["memory"])
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary)
            written = self.install.write_bundle_options(project, options)
            document = json.loads(written.read_text(encoding="utf-8"))
        self.assertFalse(document["optIn"]["deja"]["defaultOn"])
        self.assertFalse(document["enabled"]["deja"])
        self.assertNotIn("deja", document["defaultOn"])
        with unittest.mock.patch.dict(os.environ, {"CHAOS_ENGINE_WITH_DEJA": "1"}, clear=False):
            self.assertTrue(self.install.normalize_bundle_options(None)["deja"])
        with unittest.mock.patch.dict(
            os.environ,
            {"CHAOS_ENGINE_WITH_DEJA": "1", "CHAOS_ENGINE_WITHOUT_DEJA": "1"},
            clear=False,
        ):
            self.assertFalse(self.install.normalize_bundle_options(None)["deja"])
        enabled = self.install.parser().parse_args(
            ["install", "--project", "p", "--source", "s", "--commit", "c", "--with-deja"]
        )
        disabled = self.install.parser().parse_args(
            ["install", "--project", "p", "--source", "s", "--commit", "c", "--without-deja"]
        )
        with unittest.mock.patch.dict(os.environ, {"CHAOS_ENGINE_WITH_DEJA": "", "CHAOS_ENGINE_WITHOUT_DEJA": ""}, clear=False):
            self.assertTrue(self.install.bundle_from_install_args(enabled)["deja"])
            self.assertFalse(self.install.bundle_from_install_args(disabled)["deja"])

    def test_retrieve_store_deja_emits_standard_receipt_with_caps(self) -> None:
        hits = [
            {
                "path": f"chaos-engine/file{index}.py",
                "line": index + 1,
                "excerpt": "x" * 400,
            }
            for index in range(12)
        ]
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            project = root / "proj"
            project.mkdir()
            bindir = root / "bin"
            bindir.mkdir()
            _fake_deja(bindir, json.dumps({"results": hits}))
            path = str(bindir) + os.pathsep + os.environ.get("PATH", "")
            with unittest.mock.patch.dict(os.environ, {"PATH": path}, clear=False):
                code = self.retrieve.main(
                    ["--store", "deja", "--project", str(project), "--host", "claude", "past decision"]
                )
        self.assertEqual(0, code)
        # main prints JSON; re-call the function for the structured receipt.
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            project = root / "proj"
            project.mkdir()
            bindir = root / "bin"
            bindir.mkdir()
            _fake_deja(bindir, json.dumps({"results": hits}))
            path = str(bindir) + os.pathsep + os.environ.get("PATH", "")
            with unittest.mock.patch.dict(os.environ, {"PATH": path}, clear=False):
                receipt = self.retrieve.retrieve(
                    "past decision", store="deja", project=project, host="claude"
                )
        self.assertEqual("used", receipt["status"])
        self.assertLessEqual(len(receipt["hits"]), 8)
        self.assertGreater(len(receipt["hits"]), 0)
        self.assertLessEqual(receipt["bytes"], 800)
        self.assertLessEqual(len(receipt["excerpt"].encode("utf-8")), 800)
        self.assertTrue(receipt["untrusted"])
        self.assertEqual("live-files", receipt["verify"])
        for hit in receipt["hits"]:
            self.assertFalse(str(hit["path"]).startswith("/"))
            self.assertNotRegex(str(hit["path"]), r"^[A-Za-z]:")

    def test_retrieve_store_deja_skips_cleanly_when_binary_or_history_missing(self) -> None:
        matrix = json.loads((ROOT / "scripts/ci/agent_harness_parity.json").read_text(encoding="utf-8"))
        row = next(item for item in matrix["capabilities"] if item.get("id") == "retrieve_store_deja")
        self.assertEqual("equivalent", row["mode"])
        self.assertEqual("chaos-engine/references/retrieve-first.md", row["owner"])
        self.assertEqual(
            matrix["hosts"],
            ["claude", "codex", "copilot", "gemini", "grok", "opencode", "cursor", "grok-bot"],
        )
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary)
            with unittest.mock.patch.dict(os.environ, {"PATH": str(project / "missing")}, clear=False):
                code = self.retrieve.main(
                    ["--store", "deja", "--project", str(project), "--host", "claude", "past run"]
                )
                receipt = self.retrieve.retrieve(
                    "past run", store="deja", project=project, host="claude"
                )
                self.assertEqual(0, code)
                self.assertEqual("degraded", receipt["status"])
                self.assertEqual("missing-binary", receipt["reason"])
                for host in matrix["hosts"]:
                    self.assertTrue(row.get(host))
                    outcome = self.retrieve.retrieve(
                        "past run", store="deja", project=project, host=host
                    )
                    if host in NO_TRANSCRIPT_HOSTS or str(host).endswith("-cloud"):
                        self.assertEqual(
                            ("skipped", "no-history"), (outcome["status"], outcome["reason"]), host
                        )
                    else:
                        self.assertEqual(
                            ("degraded", "missing-binary"),
                            (outcome["status"], outcome["reason"]),
                            host,
                        )
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            project = root / "proj"
            project.mkdir()
            bindir = root / "bin"
            bindir.mkdir()
            _fake_deja(bindir, json.dumps({"results": [], "reason": "no-history"}))
            path = str(bindir) + os.pathsep + os.environ.get("PATH", "")
            with unittest.mock.patch.dict(os.environ, {"PATH": path}, clear=False):
                empty = self.retrieve.retrieve(
                    "past run", store="deja", project=project, host="claude"
                )
                code = self.retrieve.main(
                    ["--store", "deja", "--project", str(project), "--host", "claude", "past run"]
                )
        self.assertEqual(0, code)
        self.assertEqual("skipped", empty["status"])
        self.assertEqual("no-history", empty["reason"])

    def test_no_deja_mcp_hooks_or_user_home_skills_in_any_host_config(self) -> None:
        for relative in INSTALLER_SOURCES:
            text = (ROOT / relative).read_text(encoding="utf-8", errors="ignore")
            self.assertNotIn("deja install", text, relative)
            self.assertNotIn("deja update", text, relative)
        with tempfile.TemporaryDirectory() as temporary:
            home = Path(temporary)
            claude = home / ".claude"
            claude.mkdir()
            (claude / "settings.json").write_text(
                json.dumps(
                    {
                        "mcpServers": {
                            "deja": {"command": "deja", "args": ["mcp"]},
                            "other": {"command": "other"},
                        },
                        "hooks": {
                            "SessionStart": [
                                {"hooks": [{"type": "command", "command": "deja search"}]}
                            ]
                        },
                    }
                ),
                encoding="utf-8",
            )
            skill = claude / "skills" / "deja"
            skill.mkdir(parents=True)
            (skill / "SKILL.md").write_text("deja skill\n", encoding="utf-8")
            codex = home / ".codex"
            codex.mkdir()
            (codex / "config.toml").write_text(
                '[mcp_servers.deja]\ncommand = "deja"\n\n[mcp_servers.other]\ncommand = "other"\n',
                encoding="utf-8",
            )
            repaired = self.policy.repair_user_deja(home)
            self.assertTrue(repaired.get("stripped") or repaired.get("skillsRemoved"))
            settings = json.loads((claude / "settings.json").read_text(encoding="utf-8"))
            self.assertNotIn("deja", settings["mcpServers"])
            self.assertIn("other", settings["mcpServers"])
            self.assertNotIn("deja", (claude / "settings.json").read_text(encoding="utf-8"))
            self.assertFalse(skill.exists())
            toml = (codex / "config.toml").read_text(encoding="utf-8")
            self.assertNotIn("mcp_servers.deja", toml)
            self.assertIn("mcp_servers.other", toml)
        tracked = []
        for relative in (".mcp.json", ".claude", ".codex", ".github/hooks", "plugins", "chaos-engine/hooks"):
            path = ROOT / relative
            if path.is_file():
                tracked.append(path)
            elif path.is_dir():
                tracked.extend(item for item in path.rglob("*") if item.is_file())
        for path in tracked:
            if path.suffix not in {".json", ".toml", ".yml", ".yaml"}:
                continue
            text = path.read_text(encoding="utf-8", errors="ignore")
            self.assertNotIn('"deja"', text.casefold(), str(path))
            self.assertNotRegex(text, r"(?i)mcp_servers\.deja")

    def test_deja_invocation_sets_offline_and_embed_off_and_has_no_absolute_paths(self) -> None:
        calls: list[tuple[list[str], dict]] = []

        def fake_run(args, **kwargs):
            calls.append((list(args), kwargs))
            completed = subprocess.CompletedProcess(args, 0, stdout='{"results":[]}', stderr="")
            return completed

        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            project = root / "proj"
            project.mkdir()
            bindir = root / "bin"
            bindir.mkdir()
            (bindir / "deja").write_text("#!/bin/sh\n", encoding="utf-8")
            (bindir / "deja").chmod(0o755)
            with unittest.mock.patch.dict(os.environ, {"PATH": str(bindir)}, clear=False):
                with unittest.mock.patch.object(self.retrieve.subprocess, "run", fake_run):
                    self.retrieve.retrieve("query", store="deja", project=project, host="claude")
                    self.retrieve.retrieve(
                        "query", store="deja", project=project, host="claude", mode="how"
                    )
                    self.retrieve.retrieve(
                        "query", store="deja", project=project, host="claude", mode="fix"
                    )
        self.assertEqual(3, len(calls))
        search_args, search_kwargs = calls[0]
        self.assertEqual("deja", search_args[0])
        self.assertEqual("search", search_args[1])
        self.assertIn("how", calls[1][0])
        self.assertIn("fix", calls[2][0])
        self.assertIn("--limit", search_args)
        self.assertEqual("8", search_args[search_args.index("--limit") + 1])
        self.assertIn(".", search_args)
        for args, kwargs in calls:
            self.assertNotIn("install", args)
            self.assertNotIn("update", args)
            self.assertNotIn("proxy", " ".join(args).casefold())
            for arg in args:
                self.assertFalse(str(arg).startswith("/"), arg)
                self.assertNotRegex(str(arg), r"^[A-Za-z]:")
            env = kwargs["env"]
            self.assertEqual("1", env["DEJA_OFFLINE"])
            self.assertEqual("1", env["DEJA_EMBED_OFF"])

    def test_deja_dependency_pinned_with_sha256_for_all_platforms(self) -> None:
        spec = json.loads((ROOT / "chaos-engine/dependencies.json").read_text(encoding="utf-8"))
        dependency = spec["dependencies"]["deja"]
        self.assertEqual("0.21.2", dependency["minimumVersion"])
        self.assertIn("deja", dependency["executables"])
        component = spec["components"]["deja"]
        self.assertEqual("optional", component["taskImpact"])
        self.assertEqual("user", component["scope"])
        self.assertEqual("user-managed-cache", component["lifecycle"])
        self.assertNotIn("deja", spec["tools"])
        runtime = spec["runtimes"]["deja"]
        self.assertEqual("0.21.2", runtime["version"])
        self.assertEqual(PLATFORMS, set(runtime["artifacts"]))
        for name, artifact in runtime["artifacts"].items():
            self.assertTrue(artifact["url"].startswith("https://"), name)
            self.assertIn("/v0.21.2/", artifact["url"])
            self.assertNotIn(".mcpb", artifact["url"])
            self.assertRegex(artifact["sha256"], r"^[0-9a-f]{64}$")
        notices = (ROOT / "chaos-engine/THIRD_PARTY_NOTICES.md").read_text(encoding="utf-8")
        self.assertIn("deja-vu", notices)
        self.assertIn("MIT", notices)
        self.assertIn("0.21.2", notices)
        guide = (ROOT / "chaos-engine/references/retrieve-first.md").read_text(encoding="utf-8")
        self.assertIn("deja (opt-in)", guide)
        self.assertIn("What did a past agent session run or decide here?", guide)


if __name__ == "__main__":
    unittest.main()
