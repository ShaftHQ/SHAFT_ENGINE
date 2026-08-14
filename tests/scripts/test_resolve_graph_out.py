"""Freshness tests for the shared Graphify cache resolver (#4639)."""

import json
import os
import re
import shutil
import subprocess  # nosec B404 - tests run fixed local Git and Python commands.
import sys
import tempfile
import unittest
from pathlib import Path


ROOT = Path(__file__).resolve().parents[2]
SCRIPT = ROOT / "tools/repository-map/resolve_graph_out.py"


class ResolveGraphOutTest(unittest.TestCase):
    def setUp(self):
        self.temporary = tempfile.TemporaryDirectory()
        self.addCleanup(self.temporary.cleanup)
        self.sandbox = Path(self.temporary.name)
        self.primary = self.sandbox / "primary"
        self.primary.mkdir()
        self.git("init", cwd=self.primary)
        self.git("config", "user.email", "graphify-test@example.invalid", cwd=self.primary)
        self.git("config", "user.name", "Graphify Test", cwd=self.primary)
        (self.primary / "source.py").write_text("print('indexed')\n", encoding="utf-8")
        self.git("add", "source.py", cwd=self.primary)
        self.git("commit", "-m", "indexed source", cwd=self.primary)
        self.graph_out = self.primary / "graphify-out"
        self.graph_out.mkdir()
        (self.graph_out / "manifest.json").write_text("{}\n", encoding="utf-8")

    def git(self, *args, cwd):
        git_executable = shutil.which("git")
        self.assertIsNotNone(git_executable)
        return subprocess.run(  # nosec B603 - resolved Git executable and controlled fixture arguments.
            [git_executable, *args],
            cwd=cwd,
            check=True,
            capture_output=True,
            text=True,
        )

    def resolver(self, *args, cwd=None, env=None):
        return subprocess.run(  # nosec B603 - current interpreter and repository-owned resolver.
            [sys.executable, str(SCRIPT), *args],
            cwd=cwd or self.primary,
            env=env,
            check=False,
            capture_output=True,
            text=True,
        )

    def test_absolute_environment_override_selects_external_cache(self):
        external = self.sandbox / "external-cache"
        environment = os.environ.copy()
        environment["SHAFT_GRAPHIFY_OUT"] = str(external)

        completed = self.resolver(env=environment)

        self.assertEqual(0, completed.returncode, completed.stderr)
        self.assertEqual(str(external.resolve()), completed.stdout.strip())

    def test_relative_environment_override_fails_closed(self):
        environment = os.environ.copy()
        environment["SHAFT_GRAPHIFY_OUT"] = "relative/cache"

        completed = self.resolver(env=environment)

        self.assertEqual(1, completed.returncode)
        self.assertIn("SHAFT_GRAPHIFY_OUT must be absolute", completed.stderr)

    def test_blank_environment_override_fails_closed(self):
        environment = os.environ.copy()
        environment["SHAFT_GRAPHIFY_OUT"] = "   "

        completed = self.resolver(env=environment)

        self.assertEqual(1, completed.returncode)
        self.assertIn("SHAFT_GRAPHIFY_OUT must not be blank", completed.stderr)

    def test_non_empty_cache_without_revision_marker_is_stale(self):
        completed = self.resolver("--check")

        self.assertEqual(1, completed.returncode)
        self.assertIn("stale -", completed.stderr)
        self.assertIn("no indexed revision marker", completed.stderr)
        self.assertNotIn("absent -", completed.stderr)

    def test_non_empty_cache_without_manifest_is_stale_not_absent(self):
        (self.graph_out / "manifest.json").unlink()
        (self.graph_out / "partial-cache").write_text("partial\n", encoding="utf-8")

        completed = self.resolver("--check")

        self.assertEqual(1, completed.returncode)
        self.assertIn("stale -", completed.stderr)
        self.assertIn("no manifest.json", completed.stderr)
        self.assertNotIn("absent -", completed.stderr)

    def test_recorded_cache_passes_at_the_same_revision(self):
        recorded = self.resolver("--record-current")
        checked = self.resolver("--check")

        self.assertEqual(0, recorded.returncode, recorded.stderr)
        self.assertEqual(0, checked.returncode, checked.stderr)
        marker = json.loads(
            (self.graph_out / ".shaft-source-revision.json").read_text(encoding="utf-8")
        )
        self.assertEqual(1, marker["schema_version"])
        self.assertEqual(
            self.git("rev-parse", "HEAD", cwd=self.primary).stdout.strip(),
            marker["indexed_revision"],
        )
        self.assertEqual(64, len(marker["manifest_sha256"]))

    def test_boolean_marker_schema_is_stale(self):
        self.assertEqual(0, self.resolver("--record-current").returncode)
        marker_path = self.graph_out / ".shaft-source-revision.json"
        marker = json.loads(marker_path.read_text(encoding="utf-8"))
        marker["schema_version"] = True
        marker_path.write_text(json.dumps(marker), encoding="utf-8")

        completed = self.resolver("--check")

        self.assertEqual(1, completed.returncode)
        self.assertIn("marker schema is unsupported", completed.stderr)

    def test_later_tracked_source_revision_makes_cache_stale(self):
        self.assertEqual(0, self.resolver("--record-current").returncode)
        indexed = self.git("rev-parse", "HEAD", cwd=self.primary).stdout.strip()
        (self.primary / "later.py").write_text("print('later')\n", encoding="utf-8")
        self.git("add", "later.py", cwd=self.primary)
        self.git("commit", "-m", "later tracked source", cwd=self.primary)
        requested = self.git("rev-parse", "HEAD", cwd=self.primary).stdout.strip()

        completed = self.resolver("--check")

        self.assertEqual(1, completed.returncode)
        self.assertIn("stale -", completed.stderr)
        self.assertIn(f"indexed={indexed}", completed.stderr)
        self.assertIn(f"requested={requested}", completed.stderr)

    def test_linked_worktree_is_checked_against_its_own_revision(self):
        self.assertEqual(0, self.resolver("--record-current").returncode)
        linked = self.sandbox / "linked"
        self.git("worktree", "add", "-b", "feature", str(linked), cwd=self.primary)
        (linked / "feature.py").write_text("print('feature')\n", encoding="utf-8")
        self.git("add", "feature.py", cwd=linked)
        self.git("commit", "-m", "feature source", cwd=linked)

        completed = self.resolver("--check", cwd=linked)
        record_attempt = self.resolver("--record-current", cwd=linked)

        self.assertEqual(1, completed.returncode)
        self.assertIn("stale -", completed.stderr)
        self.assertIn("requested=", completed.stderr)
        self.assertEqual(1, record_attempt.returncode)
        self.assertIn("primary checkout", record_attempt.stderr)

    def test_linked_worktree_cannot_record_into_an_overridden_cache(self):
        linked = self.sandbox / "linked"
        self.git("worktree", "add", "-b", "feature", str(linked), cwd=self.primary)
        overridden = linked / "cache"
        overridden.mkdir()
        (overridden / "manifest.json").write_text("{}\n", encoding="utf-8")
        environment = os.environ.copy()
        environment["SHAFT_GRAPHIFY_OUT"] = str(overridden)

        completed = self.resolver("--record-current", cwd=linked, env=environment)

        self.assertEqual(1, completed.returncode)
        self.assertIn("primary checkout", completed.stderr)
        self.assertFalse((overridden / ".shaft-source-revision.json").exists())

    def test_manifest_changed_after_marker_is_stale(self):
        self.assertEqual(0, self.resolver("--record-current").returncode)
        (self.graph_out / "manifest.json").write_text('{"changed": {}}\n', encoding="utf-8")

        completed = self.resolver("--check")

        self.assertEqual(1, completed.returncode)
        self.assertIn("manifest changed after revision marker", completed.stderr)

    def test_pr_gate_and_guidance_use_the_freshness_check(self):
        workflow = (ROOT / ".github/workflows/pr-gate.yml").read_text(encoding="utf-8")
        guidance = (
            ROOT / "chaos-engine/references/graphify.md"
        ).read_text(encoding="utf-8")
        readme = (ROOT / "tools/repository-map/README.md").read_text(encoding="utf-8")

        self.assertIn("tests.scripts.test_resolve_graph_out", workflow)
        self.assertIn("tests.scripts.test_graphify_maintenance", workflow)
        self.assertIn("'tests/scripts/test_graphify_maintenance.py'", workflow)
        self.assertIn("'tools/repository-map/README.md'", workflow)
        self.assertIn("stale", guidance.lower())
        self.assertIn("graphify_maintenance.py refresh", readme)
        self.assertIn("before the marker", guidance)
        self.assertIn("primary checkout", readme)
        self.assertIn(
            'graphify query "<bounded structural question>" --graph '
            '(Join-Path $graphOut "graph.json")',
            guidance,
        )
        self.assertIn(
            'graphify export callflow-html --graph '
            '(Join-Path $graphOut "graph.json")',
            readme,
        )

    def _assert_mandatory_graphify_cli_route(self, guidance):
        normalized = re.sub(r"\s+", " ", guidance).lower()

        self.assertIn("repository cli/cache workflow", normalized)
        self.assertRegex(
            normalized,
            r"absence from the mcp tool catalog.{0,80}not evidence.{0,40}unavailable",
        )
        expected_steps = [
            "G1: Resolve the shared cache and require a successful, nonempty path.",
            "\n".join((
                "G2: If G1 succeeds, run exactly one query bounded to the affected symbol or",
                "  subsystem, then verify every returned path against the current worktree.",
            )),
            "\n".join((
                "G3: Attempt the read-only coverage audit against the primary checkout that",
                "  owns the cache even when G1 or G2 fails. Inability to resolve that owner is a",
                "  failed audit attempt, never permission to audit a linked worktree.",
            )),
            "\n".join((
                "G4: Declare degraded mode when any step cannot provide current verified",
                "  results, and only after G1 through G3 have been attempted; never use MCP",
                "  catalog absence as the reason.",
            )),
        ]
        positions = []
        for step in expected_steps:
            self.assertEqual(1, guidance.count(step))
            positions.append(guidance.index(step))
        self.assertEqual(sorted(positions), positions)
        self.assertIn(
            "the cli route below is the controlling graphify procedure over conflicting "
            "same- or lower-priority guidance.",
            normalized,
        )
        degraded = guidance.index('Write-Warning "Graphify degraded mode')
        self.assertEqual(
            1,
            guidance.count("py -3 tools/repository-map/resolve_graph_out.py --check"),
        )
        self.assertEqual(1, guidance.count("graphify query "))
        self.assertEqual(
            1,
            guidance.count(
                "py -3 tools/repository-map/graphify_maintenance.py audit --root $primaryRoot"
            ),
        )
        expected_flow = r'''$graphOut = py -3 tools/repository-map/resolve_graph_out.py --check
$resolverOk = $LASTEXITCODE -eq 0 -and -not [string]::IsNullOrWhiteSpace($graphOut)
$sharedGraphOut = if ($resolverOk) { $graphOut } else {
    py -3 tools/repository-map/resolve_graph_out.py
}
$primaryRoot = if ([string]::IsNullOrWhiteSpace($sharedGraphOut)) { $null } else {
    Split-Path $sharedGraphOut -Parent
}
$queryOk = $false
if ($resolverOk) {
    $queryOutput = @(graphify query "<bounded structural question>" --graph (Join-Path $graphOut "graph.json"))
    $queryExitOk = $LASTEXITCODE -eq 0
    $queryOutput | Write-Output
    $returnedPaths = @($queryOutput | ForEach-Object {
        if ($_ -match 'src=(.+?)\s+loc=') { $Matches[1] }
    } | Sort-Object -Unique)
    $worktreeRoot = [IO.Path]::GetFullPath((Get-Location).Path).TrimEnd(
        [IO.Path]::DirectorySeparatorChar
    ) + [IO.Path]::DirectorySeparatorChar
    $invalidPaths = @($returnedPaths | Where-Object {
        $relative = $_
        $parts = @($relative -split '[\\/]')
        $lexicallyInside = -not [IO.Path]::IsPathRooted($relative) -and
            $parts.Count -gt 0 -and -not ($parts | Where-Object { $_ -in @('', '.', '..') })
        $resolved = if ($lexicallyInside) {
            Resolve-Path -LiteralPath (Join-Path (Get-Location) $relative) -ErrorAction SilentlyContinue
        } else { $null }
        $inside = $null -ne $resolved -and ($resolved.ProviderPath + [IO.Path]::DirectorySeparatorChar).StartsWith(
            $worktreeRoot, [StringComparison]::OrdinalIgnoreCase
        )
        $hasReparsePoint = $false
        $lexicalPath = (Get-Location).Path
        foreach ($part in $parts) {
            $lexicalPath = Join-Path $lexicalPath $part
            $item = Get-Item -LiteralPath $lexicalPath -Force -ErrorAction SilentlyContinue
            $hasReparsePoint = $hasReparsePoint -or (
                $null -ne $item -and ($item.Attributes.value__ -band 1024) -ne 0
            )
        }
        -not $inside -or $hasReparsePoint
    })
    $queryOk = $queryExitOk -and $invalidPaths.Count -eq 0
}
$auditOk = $false
if ($null -ne $primaryRoot) {
    py -3 tools/repository-map/graphify_maintenance.py audit --root $primaryRoot
    $auditOk = $LASTEXITCODE -eq 0
}
if (-not ($resolverOk -and $queryOk -and $auditOk)) {
    Write-Warning "Graphify degraded mode: use targeted live-file verification."
}'''
        powershell_blocks = re.findall(r"```powershell\n(.*?)\n```", guidance, re.DOTALL)
        self.assertEqual([expected_flow], powershell_blocks)
        resolver = guidance.index(
            "py -3 tools/repository-map/resolve_graph_out.py --check"
        )
        bounded_query = guidance.index(
            'graphify query "<bounded structural question>" --graph '
            '(Join-Path $graphOut "graph.json")'
        )
        audit = guidance.index(
            "py -3 tools/repository-map/graphify_maintenance.py audit --root $primaryRoot"
        )
        self.assertLess(resolver, bounded_query)
        self.assertLess(bounded_query, audit)
        self.assertLess(audit, degraded)

    def test_missing_mcp_catalog_entry_cannot_bypass_the_graphify_cli_route(self):
        guidance = (
            ROOT / "chaos-engine/references/graphify.md"
        ).read_text(encoding="utf-8")

        self._assert_mandatory_graphify_cli_route(guidance)

    def _assert_graphify_contention_is_a_degraded_continuation(self, files):
        graphify = re.sub(r"\s+", " ", files["graphify"])
        retrieval = re.sub(r"\s+", " ", files["retrieval"])
        entrypoint = re.sub(r"\s+", " ", files["entrypoint"])

        for required in (
            "The primary checkout is the sole Graphify refresh owner.",
            "A linked-worktree revision mismatch or an active refresh lock is an expected "
            "degraded state after G1 through G4, not an implementation blocker.",
            "must not refresh, wait or retry-loop, clear or replace the lock, freshness "
            "marker, or cache, or switch, reset, or overwrite the primary checkout",
            "Continue with native Memory, MemPalace, and targeted live `rg`.",
            "Only the primary owner may schedule one later refresh when the primary "
            "checkout and shared cache are uncontested.",
        ):
            self.assertIn(required, graphify)

        self.assertIn(
            "Graphify contention is the narrow exception to implementation waiting",
            retrieval,
        )
        self.assertIn(
            "a complete degraded Graphify receipt permits implementation to continue",
            entrypoint,
        )

    def test_graphify_contention_degrades_without_shared_state_mutation(self):
        files = {
            "graphify": (ROOT / "chaos-engine/references/graphify.md").read_text(
                encoding="utf-8"
            ),
            "retrieval": (
                ROOT / "chaos-engine/references/retrieve-first.md"
            ).read_text(encoding="utf-8"),
            "entrypoint": (
                ROOT / "chaos-engine/skills/chaos-engine/SKILL.md"
            ).read_text(encoding="utf-8"),
        }

        self._assert_graphify_contention_is_a_degraded_continuation(files)

        mutations = {
            "linked refresh allowed": files["graphify"].replace(
                "must not refresh, wait or retry-loop",
                "may refresh or wait and retry-loop",
            ),
            "contention blocks implementation": files["graphify"].replace(
                "not an implementation blocker",
                "an implementation blocker",
            ),
            "primary ownership removed": files["graphify"].replace(
                "The primary checkout is the sole Graphify refresh owner.",
                "Any checkout may own a Graphify refresh.",
            ),
        }
        for name, graphify_mutation in mutations.items():
            with self.subTest(name=name), self.assertRaises(AssertionError):
                self._assert_graphify_contention_is_a_degraded_continuation(
                    {**files, "graphify": graphify_mutation}
                )

    def test_graphify_cli_route_contract_rejects_bypass_mutations(self):
        guidance = (
            ROOT / "chaos-engine/references/graphify.md"
        ).read_text(encoding="utf-8")
        mutations = {
            "optional route": guidance.replace(
                "The CLI route below is the controlling Graphify procedure",
                "The CLI route below is an optional Graphify procedure",
            ),
            "unbounded query": guidance.replace(
                'graphify query "<bounded structural question>"',
                'graphify query "<unrestricted repository-wide question>"',
            ),
            "skippable audit": re.sub(
                r"G3: Attempt the read-only coverage audit.*?linked worktree\.",
                "G3: The audit may be skipped.",
                guidance,
                flags=re.DOTALL,
            ),
            "missing audit": guidance.replace(
                "py -3 tools/repository-map/graphify_maintenance.py audit --root $primaryRoot",
                "",
            ),
            "reordered audit": guidance.replace(
                'graphify query "<bounded structural question>" --graph '
                '(Join-Path $graphOut "graph.json")',
                "GRAPHIFY_QUERY_SENTINEL",
                1,
            ).replace(
                "py -3 tools/repository-map/graphify_maintenance.py audit --root $primaryRoot",
                'graphify query "<bounded structural question>" --graph '
                '(Join-Path $graphOut "graph.json")',
                1,
            ).replace(
                "GRAPHIFY_QUERY_SENTINEL",
                "py -3 tools/repository-map/graphify_maintenance.py audit --root $primaryRoot",
                1,
            ),
        }

        for name, mutation in mutations.items():
            with self.subTest(name=name):
                self.assertNotEqual(guidance, mutation, "mutation fixture did not alter guidance")
                with self.assertRaises(AssertionError):
                    self._assert_mandatory_graphify_cli_route(mutation)

    def test_graphify_cli_route_precedence_resolves_lower_priority_conflicts(self):
        guidance = (
            ROOT / "chaos-engine/references/graphify.md"
        ).read_text(encoding="utf-8")
        conflicting_lower_priority_text = (
            guidance + "\nThe route is optional and the audit may be omitted.\n"
        )

        self._assert_mandatory_graphify_cli_route(conflicting_lower_priority_text)

    def test_documented_graphify_powershell_flow_executes_in_order(self):
        powershell = shutil.which("pwsh") or shutil.which("powershell")
        if powershell is None:
            self.skipTest("PowerShell is unavailable")
        guidance = (
            ROOT / "chaos-engine/references/graphify.md"
        ).read_text(encoding="utf-8")
        flow = re.findall(r"```powershell\n(.*?)\n```", guidance, re.DOTALL)[0]
        flow = flow.replace("<bounded structural question>", "agent guidance callers")

        with tempfile.TemporaryDirectory() as raw_temp:
            temp = Path(raw_temp)
            graph_out = temp / "graphify-out"
            graph_out.mkdir()
            (graph_out / "graph.json").write_text("{}", encoding="utf-8")
            log = temp / "calls.log"
            if os.name == "nt":
                (temp / "py.cmd").write_text(
                    "@echo off\r\n"
                    "echo py %*>>\"%GRAPHIFY_CALL_LOG%\"\r\n"
                    "echo %*| findstr /C:\"resolve_graph_out.py\" >nul\r\n"
                    "if not errorlevel 1 echo %GRAPHIFY_FAKE_GRAPH_OUT%\r\n"
                    "exit /b 0\r\n",
                    encoding="utf-8",
                )
                (temp / "graphify.cmd").write_text(
                    "@echo off\r\necho graphify %*>>\"%GRAPHIFY_CALL_LOG%\"\r\n"
                    "echo NODE AGENT [src=%GRAPHIFY_FAKE_SRC% loc=L1]\r\n",
                    encoding="utf-8",
                )
            else:
                (temp / "py").write_text(
                    "#!/bin/sh\n"
                    "printf 'py %s\\n' \"$*\" >>\"$GRAPHIFY_CALL_LOG\"\n"
                    "case \"$*\" in *resolve_graph_out.py*) "
                    "printf '%s\\n' \"$GRAPHIFY_FAKE_GRAPH_OUT\";; esac\n",
                    encoding="utf-8",
                )
                (temp / "graphify").write_text(
                    "#!/bin/sh\nprintf 'graphify %s\\n' \"$*\" "
                    ">>\"$GRAPHIFY_CALL_LOG\"\n"
                    "printf 'NODE AGENT [src=%s loc=L1]\\n' \"$GRAPHIFY_FAKE_SRC\"\n",
                    encoding="utf-8",
                )
                (temp / "py").chmod(0o755)
                (temp / "graphify").chmod(0o755)
            env = os.environ.copy()
            env["PATH"] = str(temp) + os.pathsep + env.get("PATH", "")
            env["GRAPHIFY_CALL_LOG"] = str(log)
            env["GRAPHIFY_FAKE_GRAPH_OUT"] = str(graph_out)
            env["GRAPHIFY_FAKE_SRC"] = "AGENTS.md"

            completed = subprocess.run(  # nosec B603 - fixed local PowerShell executable and test-owned script.
                [powershell, "-NoProfile", "-NonInteractive", "-Command", flow],
                cwd=ROOT,
                env=env,
                capture_output=True,
                text=True,
                timeout=30,
                check=False,
            )
            self.assertEqual(0, completed.returncode, completed.stderr)
            self.assertNotIn(
                "Graphify degraded mode", completed.stdout + completed.stderr
            )
            calls = log.read_text(encoding="utf-8").splitlines()
            env["GRAPHIFY_FAKE_SRC"] = "missing/from/graph.java"
            missing_path = subprocess.run(  # nosec B603 - fixed local PowerShell executable and test-owned script.
                [powershell, "-NoProfile", "-NonInteractive", "-Command", flow],
                cwd=ROOT,
                env=env,
                capture_output=True,
                text=True,
                timeout=30,
                check=False,
            )
            self.assertEqual(0, missing_path.returncode, missing_path.stderr)
            self.assertIn(
                "Graphify degraded mode", missing_path.stdout + missing_path.stderr
            )
            outside = temp / "outside.txt"
            outside.write_text("outside", encoding="utf-8")
            env["GRAPHIFY_FAKE_SRC"] = str(outside)
            escaped_path = subprocess.run(  # nosec B603 - fixed local PowerShell executable and test-owned script.
                [powershell, "-NoProfile", "-NonInteractive", "-Command", flow],
                cwd=ROOT,
                env=env,
                capture_output=True,
                text=True,
                timeout=30,
                check=False,
            )
            self.assertEqual(0, escaped_path.returncode, escaped_path.stderr)
            self.assertIn(
                "Graphify degraded mode", escaped_path.stdout + escaped_path.stderr
            )
        self.assertEqual(3, len(calls), calls)
        self.assertIn("resolve_graph_out.py --check", calls[0])
        self.assertRegex(
            calls[1],
            r'^graphify query ["\']?agent guidance callers["\']? --graph ',
        )
        self.assertIn("graphify_maintenance.py audit --root", calls[2])
        self.assertIn(str(temp), calls[2])

    def test_nightly_refresh_delegates_to_the_canonical_maintenance_owner(self):
        wrapper = (ROOT / "tools/agent-infra/graphify-refresh.cmd").read_text(encoding="utf-8")
        workflow = (ROOT / ".github/workflows/pr-gate.yml").read_text(encoding="utf-8")
        commands = [
            line.strip()
            for line in wrapper.splitlines()
            if line.strip().lower().startswith(
                ("py ", "if errorlevel", "if not errorlevel")
            )
        ]

        self.assertEqual(
            [
                'py -3 "%~dp0shaft_knowledge_refresh.py" --root "%SHAFT_ROOT%" --sentinel "%SHAFT_SENTINEL%" --validate-only >nul 2>&1',
                "if errorlevel 1 exit /b 1",
                'py -3 "%~dp0shaft_knowledge_refresh.py" --root "%SHAFT_ROOT%" --sentinel "%SHAFT_SENTINEL%" > "%SHAFT_LOG%" 2>&1',
            ],
            commands,
        )
        self.assertNotIn("call graphify", wrapper.lower())
        self.assertIn("'tools/agent-infra/graphify-refresh.cmd'", workflow)

    @unittest.skipUnless(os.name == "nt", "Windows command wrapper regression")
    def test_nightly_wrapper_rejects_positive_and_negative_owner_failures(self):
        wrapper_dir = self.sandbox / "tools/agent-infra"
        wrapper_dir.mkdir(parents=True)
        wrapper = wrapper_dir / "graphify-refresh.cmd"
        shutil.copy2(ROOT / "tools/agent-infra/graphify-refresh.cmd", wrapper)
        fake_bin = self.sandbox / "fake-bin"
        fake_bin.mkdir()
        (fake_bin / "py.cmd").write_text(
            """@echo off
if "%GRAPHIFY_REFRESH_EXIT%"=="0" type nul > "%GRAPHIFY_MARKER%"
exit /b %GRAPHIFY_REFRESH_EXIT%
""",
            encoding="utf-8",
        )
        marker = self.sandbox / "recorded.marker"
        (self.sandbox / ".agent-infra/logs").mkdir(parents=True)
        base_env = os.environ.copy()
        base_env["PATH"] = str(fake_bin) + os.pathsep + base_env["PATH"]
        base_env["USERPROFILE"] = str(self.sandbox)
        base_env["GRAPHIFY_MARKER"] = str(marker)
        cmd_executable = shutil.which("cmd.exe")
        self.assertIsNotNone(cmd_executable)
        for refresh_exit in (1, -1):
            with self.subTest(refresh_exit=refresh_exit):
                marker.unlink(missing_ok=True)
                env = base_env.copy()
                env["GRAPHIFY_REFRESH_EXIT"] = str(refresh_exit)
                result = subprocess.run(  # nosec B603 - fixed local command wrapper with controlled PATH fixture.
                    [cmd_executable, "/d", "/c", str(wrapper)],
                    cwd=ROOT,
                    env=env,
                    check=False,
                    capture_output=True,
                    text=True,
                )
                expected_exit = refresh_exit if refresh_exit >= 0 else 2**32 + refresh_exit
                self.assertEqual(expected_exit, result.returncode)
                self.assertFalse(marker.exists())

        success_env = base_env.copy()
        success_env["GRAPHIFY_REFRESH_EXIT"] = "0"
        success = subprocess.run(  # nosec B603 - fixed local command wrapper with controlled PATH fixture.
            [cmd_executable, "/d", "/c", str(wrapper)],
            cwd=ROOT,
            env=success_env,
            check=False,
            capture_output=True,
            text=True,
        )
        self.assertEqual(0, success.returncode, success.stderr)
        self.assertTrue(marker.exists())

    def test_install_guidance_uses_upgradeable_official_graphify_tool(self):
        readme = (ROOT / "tools/repository-map/README.md").read_text(encoding="utf-8")

        self.assertIn("uv tool install graphifyy", readme)
        self.assertIn("uv tool upgrade graphifyy", readme)
        self.assertNotIn("graphifyy==0.9.17", readme)


if __name__ == "__main__":
    unittest.main()
