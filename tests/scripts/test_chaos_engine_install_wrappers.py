"""ChaosEngine one-liner wrappers derive source from the invocation URL (#5224)."""

from __future__ import annotations

import ast
import io
import os
import re
import shutil
import subprocess  # nosec B404 - tests invoke local pwsh/bash on repo wrappers only.
import tempfile
import tokenize
import unittest
from pathlib import Path


ROOT = Path(__file__).resolve().parents[2]
POWERSHELL = ROOT / "chaos-engine/install.ps1"
SHELL = ROOT / "chaos-engine/install.sh"
RAW_URL = re.compile(
    r"https://raw\.githubusercontent\.com/"
    r"([A-Za-z0-9_.-]+)/([A-Za-z0-9_.-]+)/"
    r"(.+?)/install\.(ps1|sh)\b",
    re.IGNORECASE,
)
PLACEHOLDER = "owner/repository"
WINDOWS_ONE_LINER = (
    'irm "https://raw.githubusercontent.com/ShaftHQ/SHAFT_ENGINE/main/chaos-engine/install.ps1" | iex'
)
POSIX_ONE_LINER = (
    'curl -fsSL "https://raw.githubusercontent.com/ShaftHQ/SHAFT_ENGINE/main/chaos-engine/install.sh"'
    + ' | bash -s -- "https://raw.githubusercontent.com/ShaftHQ/SHAFT_ENGINE/main/chaos-engine/install.sh"'
)
USER_WINDOWS_COMMAND = (
    'irm "https://raw.githubusercontent.com/ShaftHQ/SHAFT_ENGINE/main/chaos-engine/install.ps1" | iex'
)
FORK_WINDOWS_COMMAND = (
    'irm "https://raw.githubusercontent.com/Other/SHAFT_ENGINE/main/chaos-engine/install.ps1" | iex'
)
ROOT_WINDOWS_COMMAND = (
    'irm "https://raw.githubusercontent.com/Example/chaos-engine/main/install.ps1" | iex'
)


def parse_raw_url(text: str) -> dict[str, str] | None:
    match = RAW_URL.search(text)
    if match is None:
        return None
    owner, repo, rest, script = match.groups()
    repository = f"{owner}/{repo}"
    if repository.casefold() == PLACEHOLDER:
        return None
    parts = rest.split("/")
    if len(parts) >= 3 and parts[0] == "refs" and parts[1] in {"heads", "tags"}:
        ref = "/".join(parts[:3])
        prefix_parts = parts[3:]
    else:
        ref = parts[0]
        prefix_parts = parts[1:]
    prefix = "/".join(prefix_parts)
    bootstrap_path = "bootstrap.py" if not prefix else f"{prefix}/bootstrap.py"
    return {
        "repository": repository,
        "ref": ref,
        "prefix": prefix,
        "script": f"install.{script}",
        "bootstrap_url": (
            f"https://raw.githubusercontent.com/{repository}/{ref}/{bootstrap_path}"
        ),
    }


class ChaosEngineInstallWrapperTest(unittest.TestCase):
    def test_python_grammar_extracts_nested_fork_and_root_urls(self):
        nested = parse_raw_url(USER_WINDOWS_COMMAND)
        self.assertEqual(
            {
                "repository": "ShaftHQ/SHAFT_ENGINE",
                "ref": "main",
                "prefix": "chaos-engine",
                "script": "install.ps1",
                "bootstrap_url": (
                    "https://raw.githubusercontent.com/ShaftHQ/SHAFT_ENGINE/"
                    "main/chaos-engine/bootstrap.py"
                ),
            },
            nested,
        )
        self.assertEqual(
            "Other/SHAFT_ENGINE",
            parse_raw_url(FORK_WINDOWS_COMMAND)["repository"],
        )
        root = parse_raw_url(ROOT_WINDOWS_COMMAND)
        self.assertEqual("Example/chaos-engine", root["repository"])
        self.assertEqual("", root["prefix"])
        self.assertEqual(
            "https://raw.githubusercontent.com/Example/chaos-engine/main/bootstrap.py",
            root["bootstrap_url"],
        )
        self.assertIsNone(
            parse_raw_url(
                'irm "https://raw.githubusercontent.com/owner/repository/main/'
                'chaos-engine/install.ps1" | iex'
            )
        )

    def test_wrappers_contain_the_raw_url_grammar_and_reject_the_placeholder(self):
        powershell = POWERSHELL.read_text(encoding="utf-8")
        shell = SHELL.read_text(encoding="utf-8")
        self.assertIn("ConvertFrom-ChaosEngineRawUrl", powershell)
        self.assertIn("parse_chaos_engine_raw_url", shell)
        self.assertIn("raw.githubusercontent.com", shell)
        self.assertIn(PLACEHOLDER, powershell)
        self.assertIn(PLACEHOLDER, shell)
        self.assertIn("Get-PSCallStack", powershell)
        self.assertIn("TryGetValues", powershell)
        self.assertIn("Test-ChaosEngineSourceTree", powershell)
        self.assertIn("is_chaos_engine_source_tree", shell)
        self.assertIn("skills/chaos-engine/SKILL.md", powershell)
        self.assertIn("skills/chaos-engine/SKILL.md", shell)
        self.assertNotIn('$response.Headers["Retry-After"]', powershell)
        self.assertNotIn("$response.Headers['Retry-After']", powershell)
        self.assertNotIn("shaft", powershell.casefold())
        self.assertNotIn("shaft", shell.casefold())

    def test_wrappers_prefer_invocation_url_over_placeholder_env(self):
        powershell = POWERSHELL.read_text(encoding="utf-8")
        shell = SHELL.read_text(encoding="utf-8")
        self.assertIn("CHAOS_ENGINE_REPOSITORY", powershell)
        self.assertIn("CHAOS_ENGINE_REPOSITORY", shell)
        self.assertRegex(powershell, r"ConvertFrom-ChaosEngineRawUrl")
        self.assertNotRegex(
            powershell,
            r"if \(\[string\]::IsNullOrWhiteSpace\(\$repository\)\) \{\s*"
            r'throw "Set CHAOS_ENGINE_REPOSITORY',
        )
        self.assertNotIn(
            'fail "Set CHAOS_ENGINE_REPOSITORY to the upstream owner/repository',
            shell,
        )

    def test_documented_one_liners_put_owner_repository_in_the_url(self):
        readme = (ROOT / "chaos-engine/README.md").read_text(encoding="utf-8")
        install = (ROOT / "chaos-engine/INSTALL.md").read_text(encoding="utf-8")
        self.assertIn(WINDOWS_ONE_LINER, readme)
        self.assertIn(WINDOWS_ONE_LINER, install)
        self.assertIn(POSIX_ONE_LINER, readme)
        self.assertIn(POSIX_ONE_LINER, install)
        for document in (readme, install):
            self.assertEqual(1, document.count(WINDOWS_ONE_LINER))
            self.assertEqual(1, document.count(POSIX_ONE_LINER))
            command_lines = "\n".join(
                line for line in document.splitlines()
                if "raw.githubusercontent.com/ShaftHQ/SHAFT_ENGINE" in line
            )
            self.assertNotIn("--interactive", command_lines)
            self.assertNotIn("$(printf", document)
            self.assertNotRegex(document, r"\bhaftHQ\b")
            self.assertNotRegex(document, r"(?<!S)HAFT_ENGINE")
            self.assertNotIn("$env:CHAOS_ENGINE_REPOSITORY/main", document)

    def test_shaft_profile_keeps_the_real_url_without_an_env_preamble(self):
        profile = (
            ROOT / "chaos-engine/profiles/shaft/entrypoint.md"
        ).read_text(encoding="utf-8")
        self.assertIn(
            "https://raw.githubusercontent.com/ShaftHQ/SHAFT_ENGINE/main/"
            "chaos-engine/install.ps1",
            profile,
        )
        self.assertNotIn("with `CHAOS_ENGINE_REPOSITORY=", profile)

    def test_powershell_parser_matches_the_python_grammar_when_pwsh_exists(self):
        powershell = POWERSHELL.read_text(encoding="utf-8")
        self.assertIn("ConvertFrom-ChaosEngineRawUrl", powershell)
        pwsh = shutil.which("pwsh") or shutil.which("powershell")
        if pwsh is None:
            self.skipTest("pwsh is not on PATH")
        script = r"""
Set-StrictMode -Version Latest
$ErrorActionPreference = 'Stop'
. $args[0] -ParseOnly
$samples = @(
    'irm "https://raw.githubusercontent.com/ShaftHQ/SHAFT_ENGINE/main/chaos-engine/install.ps1" | iex',
    'irm "https://raw.githubusercontent.com/Other/SHAFT_ENGINE/main/chaos-engine/install.ps1" | iex',
    'irm "https://raw.githubusercontent.com/Example/chaos-engine/main/install.ps1" | iex',
    'irm "https://raw.githubusercontent.com/owner/repository/main/chaos-engine/install.ps1" | iex'
)
foreach ($sample in $samples) {
    $parsed = ConvertFrom-ChaosEngineRawUrl $sample
    if ($null -eq $parsed) {
        Write-Output 'NONE'
        continue
    }
    Write-Output ($parsed.Repository + '|' + $parsed.Ref + '|' + $parsed.Prefix + '|' + $parsed.BootstrapUrl)
}
"""
        with tempfile.NamedTemporaryFile("w", suffix=".ps1", delete=False, encoding="utf-8") as handle:
            handle.write(script)
            driver = handle.name
        try:
            completed = subprocess.run(  # nosec B603 - local pwsh plus the repo wrapper
                [pwsh, "-NoProfile", "-File", driver, str(POWERSHELL)],
                check=False,
                capture_output=True,
                text=True,
            )
        finally:
            Path(driver).unlink(missing_ok=True)
        self.assertEqual(0, completed.returncode, completed.stderr + completed.stdout)
        lines = [line.strip() for line in completed.stdout.splitlines() if line.strip()]
        self.assertEqual(
            [
                "ShaftHQ/SHAFT_ENGINE|main|chaos-engine|"
                + "https://raw.githubusercontent.com/ShaftHQ/SHAFT_ENGINE/main/chaos-engine/bootstrap.py",
                "Other/SHAFT_ENGINE|main|chaos-engine|"
                + "https://raw.githubusercontent.com/Other/SHAFT_ENGINE/main/chaos-engine/bootstrap.py",
                "Example/chaos-engine|main||"
                + "https://raw.githubusercontent.com/Example/chaos-engine/main/bootstrap.py",
                "NONE",
            ],
            lines,
        )

    def test_powershell_resolver_prefers_invocation_url_over_placeholder_env(self):
        powershell = POWERSHELL.read_text(encoding="utf-8")
        self.assertIn("Resolve-ChaosEngineSource", powershell)
        pwsh = shutil.which("pwsh") or shutil.which("powershell")
        if pwsh is None:
            self.skipTest("pwsh is not on PATH")
        script = r"""
Set-StrictMode -Version Latest
$ErrorActionPreference = 'Stop'
. $args[0] -ParseOnly
$env:CHAOS_ENGINE_REPOSITORY = 'owner/repository'
$parsed = Resolve-ChaosEngineSource -Texts @(
    'irm "https://raw.githubusercontent.com/ShaftHQ/SHAFT_ENGINE/main/chaos-engine/install.ps1" | iex'
)
Write-Output ($parsed.Repository + '|' + $parsed.Ref + '|' + $parsed.Prefix)
"""
        with tempfile.NamedTemporaryFile("w", suffix=".ps1", delete=False, encoding="utf-8") as handle:
            handle.write(script)
            driver = handle.name
        try:
            completed = subprocess.run(
                [pwsh, "-NoProfile", "-File", driver, str(POWERSHELL)],
                check=False,
                capture_output=True,
                text=True,
            )
        finally:
            Path(driver).unlink(missing_ok=True)
        self.assertEqual(0, completed.returncode, completed.stderr + completed.stdout)
        self.assertEqual(
            ["ShaftHQ/SHAFT_ENGINE|main|chaos-engine"],
            [line.strip() for line in completed.stdout.splitlines() if line.strip()],
        )

    def test_powershell_irm_pipeline_uses_url_not_leftover_env(self):
        pwsh = shutil.which("pwsh") or shutil.which("powershell")
        if pwsh is None:
            self.skipTest("pwsh is not on PATH")
        script_path = str(POWERSHELL).replace("'", "''")
        command = (
            "$env:CHAOS_ENGINE_RESOLVE_ONLY='1'; "
            "$env:CHAOS_ENGINE_REPOSITORY='Evil/Repo'; "
            "$url = 'https://raw.githubusercontent.com/ShaftHQ/SHAFT_ENGINE/main/chaos-engine/install.ps1'; "
            f"Get-Content -Raw -LiteralPath '{script_path}' | iex"
        )
        completed = subprocess.run(
            [pwsh, "-NoProfile", "-Command", command],
            check=False,
            capture_output=True,
            text=True,
        )
        self.assertEqual(0, completed.returncode, completed.stderr + completed.stdout)
        self.assertIn(
            "ShaftHQ/SHAFT_ENGINE|main|remote|"
            "https://raw.githubusercontent.com/ShaftHQ/SHAFT_ENGINE/main/chaos-engine/bootstrap.py",
            completed.stdout,
        )
        self.assertNotIn("Evil/Repo", completed.stdout)

    def test_powershell_header_helper_reads_http_response_headers(self):
        pwsh = shutil.which("pwsh") or shutil.which("powershell")
        if pwsh is None:
            self.skipTest("pwsh is not on PATH")
        script = r"""
Set-StrictMode -Version Latest
$ErrorActionPreference = 'Stop'
. $args[0] -ParseOnly
$message = [System.Net.Http.HttpResponseMessage]::new([System.Net.HttpStatusCode]::TooManyRequests)
[void]$message.Headers.TryAddWithoutValidation('Retry-After', '5')
Write-Output (Get-ChaosEngineHeader $message.Headers 'Retry-After')
Write-Output (Get-ChaosEngineHeader $message.Headers 'X-Missing')
"""
        with tempfile.NamedTemporaryFile("w", suffix=".ps1", delete=False, encoding="utf-8") as handle:
            handle.write(script)
            driver = handle.name
        try:
            completed = subprocess.run(
                [pwsh, "-NoProfile", "-File", driver, str(POWERSHELL)],
                check=False,
                capture_output=True,
                text=True,
            )
        finally:
            Path(driver).unlink(missing_ok=True)
        self.assertEqual(0, completed.returncode, completed.stderr + completed.stdout)
        self.assertEqual(["5"], [line.strip() for line in completed.stdout.splitlines() if line.strip()])

    def test_powershell_resolve_only_uses_sibling_bootstrap(self):
        pwsh = shutil.which("pwsh") or shutil.which("powershell")
        if pwsh is None:
            self.skipTest("pwsh is not on PATH")
        with tempfile.TemporaryDirectory() as temporary:
            wrapper = Path(temporary) / "install.ps1"
            wrapper.write_text(POWERSHELL.read_text(encoding="utf-8"), encoding="utf-8")
            (Path(temporary) / "bootstrap.py").write_text("marker = 1\n", encoding="utf-8")
            skill = Path(temporary) / "skills" / "chaos-engine" / "SKILL.md"
            skill.parent.mkdir(parents=True)
            skill.write_text("# ChaosEngine\n", encoding="utf-8")
            completed = subprocess.run(
                [
                    pwsh,
                    "-NoProfile",
                    "-File",
                    str(wrapper),
                ],
                check=False,
                capture_output=True,
                text=True,
                env={
                    **os.environ,
                    "CHAOS_ENGINE_RESOLVE_ONLY": "1",
                    "CHAOS_ENGINE_REPOSITORY": "Example/Project",
                },
            )
        self.assertEqual(0, completed.returncode, completed.stderr + completed.stdout)
        self.assertIn("Example/Project|main|local|", completed.stdout)


    def test_powershell_scratch_directory_with_stale_sibling_bootstrap_is_rejected(self):
        pwsh = shutil.which("pwsh") or shutil.which("powershell")
        if pwsh is None:
            self.skipTest("pwsh is not on PATH")
        with tempfile.TemporaryDirectory() as temporary:
            wrapper = Path(temporary) / "install.ps1"
            wrapper.write_text(POWERSHELL.read_text(encoding="utf-8"), encoding="utf-8")
            (Path(temporary) / "bootstrap.py").write_text("marker = 1\n", encoding="utf-8")
            completed = subprocess.run(
                [
                    pwsh,
                    "-NoProfile",
                    "-File",
                    str(wrapper),
                ],
                check=False,
                capture_output=True,
                text=True,
                env={
                    **os.environ,
                    "CHAOS_ENGINE_RESOLVE_ONLY": "1",
                    "CHAOS_ENGINE_REPOSITORY": "Example/Project",
                },
            )
        self.assertEqual(0, completed.returncode, completed.stderr + completed.stdout)
        self.assertIn("Example/Project|main|remote|", completed.stdout)
        self.assertNotIn("|local|", completed.stdout)

    def test_shell_wrapper_uses_positional_url_over_env(self):
        shell = shutil.which("bash") or shutil.which("sh")
        if shell is None:
            self.skipTest("sh is not on PATH")
        script = SHELL.as_posix()
        if os.name == "nt" and script[1:3] == ":/":
            wsl = "/mnt/" + script[0].lower() + script[2:]
            probe = subprocess.run(  # nosec B603 - local bash path probe, no user input
                [shell, "-c", f"test -f '{wsl}' && echo wsl || test -f '{script}' && echo win || echo missing"],
                check=False,
                capture_output=True,
                text=True,
            )
            if "wsl" in probe.stdout:
                script = wsl
            elif "win" not in probe.stdout:
                self.skipTest(f"{shell} cannot read {SHELL}")
        url = "https://raw.githubusercontent.com/ShaftHQ/SHAFT_ENGINE/main/chaos-engine/install.sh"
        with tempfile.TemporaryDirectory(ignore_cleanup_errors=True) as temporary:
            completed = subprocess.run(
                [
                    shell,
                    "-c",
                    "CHAOS_ENGINE_RESOLVE_ONLY=1 CHAOS_ENGINE_REPOSITORY=Evil/Repo "
                    + f"'{script}' '{url}'",
                ],
                check=False,
                capture_output=True,
                text=True,
                cwd=temporary,
            )
        self.assertEqual(0, completed.returncode, completed.stderr + completed.stdout)
        self.assertEqual(
            "ShaftHQ/SHAFT_ENGINE|main|chaos-engine|"
            + "https://raw.githubusercontent.com/ShaftHQ/SHAFT_ENGINE/main/chaos-engine/bootstrap.py",
            completed.stdout.strip().splitlines()[-1],
        )

    def test_remaining_wrapper_strings_use_explicit_concatenation(self):
        """#5233: adjacent literals in POSIX_ONE_LINER or the positional-url test go red."""
        source = Path(__file__).read_text(encoding="utf-8")
        self.assertTrue(
            has_implicit_string_concat("X = (\n    'a'\n    'b'\n)\n"),
            "detector must catch adjacent literals",
        )
        self.assertFalse(has_implicit_string_concat("X = (\n    'a'\n    + 'b'\n)\n"))
        self.assertFalse(has_implicit_string_concat("X = 'ab'\n"))
        posix = source_segment_for_assignment(source, "POSIX_ONE_LINER")
        positional = source_segment_for_function(
            source, "test_shell_wrapper_uses_positional_url_over_env"
        )
        self.assertFalse(
            has_implicit_string_concat(posix),
            "POSIX_ONE_LINER must use explicit + or one line",
        )
        self.assertFalse(
            has_implicit_string_concat(positional),
            "test_shell_wrapper_uses_positional_url_over_env must use explicit + or one line",
        )

    def test_install_md_joins_the_installer_lf_attributes_sentence(self):
        """#5234: do not leave a one-word `The` paragraph."""
        document = (ROOT / "chaos-engine/INSTALL.md").read_text(encoding="utf-8")
        self.assertIn(
            "into the adopter payload. The installer also merges receipt-bound LF attributes",
            document,
        )
        self.assertNotRegex(document, r"The\s*\n\s*\n\s*installer also merges")

    def test_posix_leftover_bootstrap_is_not_copied_without_source_tree_skill(self):
        """#5232: leftover bootstrap.py without SKILL.md must take the fetch path."""
        self._run_posix_bootstrap_copy_gate(source_tree=False)

    def test_posix_source_tree_bootstrap_is_copied_when_skill_exists(self):
        """#5232: a source-tree fixture with SKILL.md does copy sibling bootstrap.py."""
        self._run_posix_bootstrap_copy_gate(source_tree=True)

    def _run_posix_bootstrap_copy_gate(self, *, source_tree: bool) -> None:
        shell = shutil.which("bash") or shutil.which("sh")
        if shell is None:
            self.skipTest("sh is not on PATH")
        if shutil.which("python3") is None and shutil.which("python") is None:
            self.skipTest("python3 is required")
        url = (
            "https://raw.githubusercontent.com/Example/Project/main/"
            "chaos-engine/install.sh"
        )
        leftover = 'raise SystemExit("LEFTOVER_BOOTSTRAP")\n'
        fetched = 'raise SystemExit("FETCHED_BOOTSTRAP")\n'
        with tempfile.TemporaryDirectory(ignore_cleanup_errors=True) as temporary:
            root = Path(temporary)
            wrapper = root / "install.sh"
            wrapper.write_bytes(
                SHELL.read_bytes().replace(b"\r\n", b"\n").replace(b"\r", b"\n")
            )
            (root / "bootstrap.py").write_bytes(leftover.encode("utf-8"))
            if source_tree:
                skill = root / "skills" / "chaos-engine" / "SKILL.md"
                skill.parent.mkdir(parents=True)
                skill.write_text("# ChaosEngine\n", encoding="utf-8", newline="\n")
            bindir = root / "bin"
            bindir.mkdir()
            write_posix_stub(
                bindir / "curl",
                "\n".join(
                    (
                        "#!/bin/sh",
                        'out=""',
                        'while [ "$#" -gt 0 ]; do',
                        '  case "$1" in',
                        '    -o) out="$2"; shift 2 ;;',
                        "    --retry|--retry-delay) shift 2 ;;",
                        '    -*) shift ;;',
                        '    *) shift ;;',
                        "  esac",
                        "done",
                        '[ -n "$out" ] || exit 1',
                        f"printf '%s\\n' '{fetched.strip()}' > \"$out\"",
                    )
                ),
            )
            write_posix_stub(
                bindir / "wget",
                "\n".join(
                    (
                        "#!/bin/sh",
                        'echo "wget must not run in leftover-bootstrap tests" >&2',
                        "exit 1",
                    )
                ),
            )
            project = root / "project"
            project.mkdir()
            script = shell_visible_path(shell, wrapper)
            unix_bin = shell_visible_path(shell, bindir)
            unix_project = shell_visible_path(shell, project)
            completed = subprocess.run(  # nosec B603 - local bash plus repo wrapper
                [
                    shell,
                    "-c",
                    f"PATH='{unix_bin}':\"$PATH\" && cd '{unix_project}' && "
                    f"sh '{script}' '{url}'",
                ],
                check=False,
                capture_output=True,
                text=True,
            )
        output = completed.stdout + completed.stderr
        if source_tree:
            self.assertIn("LEFTOVER_BOOTSTRAP", output)
            self.assertNotIn("FETCHED_BOOTSTRAP", output)
        else:
            self.assertIn("FETCHED_BOOTSTRAP", output)
            self.assertNotIn("LEFTOVER_BOOTSTRAP", output)


_STRINGY = {tokenize.STRING}
if hasattr(tokenize, "FSTRING_START"):
    _STRINGY.add(tokenize.FSTRING_START)


def has_implicit_string_concat(source: str) -> bool:
    pending = False
    for tok in tokenize.generate_tokens(io.StringIO(source).readline):
        if tok.type in _STRINGY:
            if pending:
                return True
            pending = True
        elif tok.type == tokenize.OP and tok.string == "+":
            pending = False
        elif tok.type in (
            tokenize.NL,
            tokenize.NEWLINE,
            tokenize.INDENT,
            tokenize.DEDENT,
            tokenize.COMMENT,
            tokenize.ENCODING,
        ):
            continue
        else:
            pending = False
    return False


def source_segment_for_assignment(source: str, name: str) -> str:
    tree = ast.parse(source)
    for node in tree.body:
        if not isinstance(node, ast.Assign):
            continue
        for target in node.targets:
            if isinstance(target, ast.Name) and target.id == name:
                segment = ast.get_source_segment(source, node)
                if segment is None:
                    raise AssertionError(f"could not read assignment {name}")
                return segment
    raise AssertionError(f"assignment {name} is missing")


def source_segment_for_function(source: str, name: str) -> str:
    tree = ast.parse(source)
    for node in ast.walk(tree):
        if isinstance(node, ast.FunctionDef) and node.name == name:
            segment = ast.get_source_segment(source, node)
            if segment is None:
                raise AssertionError(f"could not read function {name}")
            return segment
    raise AssertionError(f"function {name} is missing")


def write_posix_stub(path: Path, body: str) -> None:
    text = body.replace("\r\n", "\n").replace("\r", "\n").rstrip() + "\n"
    path.write_bytes(text.encode("utf-8"))
    path.chmod(0o755)


def shell_visible_path(shell: str, path: Path) -> str:
    script = path.resolve().as_posix()
    if os.name != "nt" or script[1:3] != ":/":
        return script
    wsl = "/mnt/" + script[0].lower() + script[2:]
    git_bash = "/" + script[0].lower() + script[2:]
    probe = subprocess.run(  # nosec B603 - local bash path probe, no user input
        [
            shell,
            "-c",
            f"test -e '{wsl}' && echo wsl || test -e '{git_bash}' && echo git "
            f"|| test -e '{script}' && echo win || echo missing",
        ],
        check=False,
        capture_output=True,
        text=True,
    )
    if "wsl" in probe.stdout:
        return wsl
    if "git" in probe.stdout:
        return git_bash
    if "win" in probe.stdout:
        return script
    raise unittest.SkipTest(f"{shell} cannot see {path}")


if __name__ == "__main__":
    unittest.main()
