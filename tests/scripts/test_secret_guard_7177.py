"""#7177: secret guard hooks block generated reports and secrets before commit and push."""
from __future__ import annotations

import base64
import importlib.util
import os
import shutil
import subprocess
import tempfile
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
GUARD = ROOT / "chaos-engine" / "secret_guard.py"
HOOKS = ROOT / "chaos-engine" / "git_hooks.py"
TEMPLATE = ROOT / "shaft-engine/src/main/resources/examples/tools/git-hooks"
SECRET = "Sup3rSecretAccessKey99"
GIT = shutil.which("git") or "git"


def load(path: Path, name: str):
    spec = importlib.util.spec_from_file_location(name, path)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def run(cwd: Path, *args: str, stdin: str | None = None) -> subprocess.CompletedProcess:
    env = dict(os.environ, GIT_AUTHOR_NAME="t", GIT_AUTHOR_EMAIL="t@e", GIT_COMMITTER_NAME="t",
               GIT_COMMITTER_EMAIL="t@e", CHAOS_ENGINE_SECRET_GUARD="1")
    return subprocess.run([GIT, *args], cwd=cwd, input=stdin, capture_output=True, text=True, env=env)


def report_with(value: str) -> str:
    blob = base64.b64encode(f'{{"accessKey":"{value}"}}'.encode()).decode()
    return f'<html><script>d("data/attachments/a.txt","{blob}")</script></html>'


class SecretGuardTestCase(unittest.TestCase):
    def setUp(self):
        self.guard = load(GUARD, "secret_guard_7177")
        self.temp = tempfile.TemporaryDirectory()
        self.repo = Path(self.temp.name) / "project"
        self.repo.mkdir()
        run(self.repo, "init", "-q", "-b", "feature")
        secrets = self.repo / "src/main/resources/properties"
        secrets.mkdir(parents=True)
        (secrets / "secrets.properties").write_text(f"browserStack.accessKey={SECRET}\n", encoding="utf-8")
        (self.repo / ".gitignore").write_text("secrets.properties\n", encoding="utf-8")
        run(self.repo, "add", ".gitignore")
        run(self.repo, "commit", "-q", "-m", "init")

    def tearDown(self):
        self.temp.cleanup()

    def guard_cli(self, *args: str, stdin: str = "") -> subprocess.CompletedProcess:
        return subprocess.run(["python3", str(GUARD), *args], cwd=self.repo, input=stdin,
                              capture_output=True, text=True)


class FindingsTest(SecretGuardTestCase):
    def test_known_value_inside_a_renamed_report_blob_is_found_without_echo(self):
        values = self.guard.secret_values(self.repo, [self.guard.DEFAULT_SECRET_FILES.split(",")[0]])
        found = self.guard.findings("docs/evidence.txt", report_with(SECRET).encode(), values, [])
        self.assertEqual(["known secret value"], found)
        self.assertNotIn(SECRET, " ".join(found))

    def test_report_paths_tokens_and_assignments_are_named(self):
        (self.repo / ".secret-guard-paths").write_text("# reports\nallure[^/]*\\.html$\n(^|/)allure-(results|report)/\n")
        blocked = self.guard.blocked_paths(self.repo)
        self.assertEqual(["generated report or secret file"],
                         self.guard.findings("docs/x/run-allure.html", b"<html/>", set(), [], blocked))
        self.assertEqual(["generated report or secret file"],
                         self.guard.findings("target/allure-results/a-result.json", b"{}", set(), [], blocked))
        self.assertEqual([], self.guard.findings("docs/x/run-allure.html", b"<html/>", set(), []))
        self.assertEqual(["generated report or secret file"],
                         self.guard.findings("config/browserstack.yml", b"", set(), []))
        token = b"eyJhbGciOiJIUzI1NiJ9.eyJzdWIiOiIxMjM0NTY3ODkw.sig"
        self.assertEqual(["JWT-like token"], self.guard.findings("a.txt", token, set(), []))
        self.assertEqual(["secret assignment"],
                         self.guard.findings("a.yml", b"clientSecret: abcd1234efgh5678ijkl", set(), []))
        self.assertEqual([], self.guard.findings("a.java", b'password = readPassword("x")', set(), []))
        self.assertEqual([], self.guard.findings("a.txt", b"plain text", set(), []))

    def test_allow_globs_skip_heuristics_but_never_known_values(self):
        (self.repo / ".secret-guard-allow").write_text("# fixtures\nsrc/test/*\n", encoding="utf-8")
        allow = self.guard.allowed_globs(self.repo)
        token = b"eyJhbGciOiJIUzI1NiJ9.eyJzdWIiOiIxMjM0NTY3ODkw.sig"
        self.assertEqual([], self.guard.findings("src/test/T.java", token, set(), allow))
        self.assertEqual(["known secret value"],
                         self.guard.findings("src/test/T.java", SECRET.encode(), {SECRET.encode()}, allow))

    def test_secret_values_read_properties_yaml_and_json(self):
        (self.repo / "browserstack.yml").write_text("userName: someone\naccessKey: yamlValue12345\n", encoding="utf-8")
        (self.repo / "keys.json").write_text('{"a": {"b": ["jsonValue12345", "short"]}}', encoding="utf-8")
        values = self.guard.secret_values(self.repo, ["browserstack.yml", "keys.json", "missing.properties", ""])
        self.assertEqual({b"yamlValue12345", b"jsonValue12345"}, values)

    def test_a_broken_report_blob_is_skipped(self):
        self.assertEqual([], self.guard.findings("a.txt", b'd("x","' + b"A" * 17 + b'")', set(), []))


class PushRulesTest(SecretGuardTestCase):
    def test_direct_push_to_main_is_refused_and_deletes_are_skipped(self):
        zero = "0" * 40
        with self.assertRaises(PermissionError):
            self.guard.pushed_revisions(f"refs/heads/x {'a' * 40} refs/heads/main {zero}\n")
        self.assertEqual([], self.guard.pushed_revisions(f"(delete) {zero} refs/heads/x {'b' * 40}\n\n"))
        self.assertEqual(["a" * 40, "--not", "--remotes"],
                         self.guard.pushed_revisions(f"refs/heads/x {'a' * 40} refs/heads/x {zero}\n"))
        self.assertEqual(["a" * 40, "^" + "b" * 40],
                         self.guard.pushed_revisions(f"refs/heads/x {'a' * 40} refs/heads/x {'b' * 40}\n"))


class CommandLineTest(SecretGuardTestCase):
    def test_staged_clean_file_passes_and_staged_report_is_blocked(self):
        (self.repo / "Readme.md").write_text("hello\n", encoding="utf-8")
        run(self.repo, "add", "Readme.md")
        self.assertEqual(0, self.guard_cli("--staged").returncode)
        (self.repo / "evidence.txt").write_text(report_with(SECRET), encoding="utf-8")
        run(self.repo, "add", "evidence.txt")
        blocked = self.guard_cli("--staged")
        self.assertEqual(1, blocked.returncode)
        self.assertIn("evidence.txt: known secret value", blocked.stderr)
        self.assertNotIn(SECRET, blocked.stderr + blocked.stdout)

    def test_rev_audit_and_push_mode_scan_new_blobs_only(self):
        base = run(self.repo, "rev-parse", "HEAD").stdout.strip()
        (self.repo / "evidence.txt").write_text(report_with(SECRET), encoding="utf-8")
        run(self.repo, "add", "evidence.txt")
        run(self.repo, "commit", "-q", "-m", "leak")
        head = run(self.repo, "rev-parse", "HEAD").stdout.strip()
        audit = self.guard_cli("--rev", f"{base}..{head}")
        self.assertEqual(1, audit.returncode)
        self.assertIn("evidence.txt", audit.stderr)
        self.assertEqual(0, self.guard_cli("--rev", f"{head}..{head}").returncode)
        pushed = self.guard_cli("--push", stdin=f"refs/heads/feature {head} refs/heads/feature {base}\n")
        self.assertEqual(1, pushed.returncode)
        refused = self.guard_cli("--push", stdin=f"refs/heads/feature {head} refs/heads/main {base}\n")
        self.assertIn("no direct push to refs/heads/main", refused.stderr)


class HookInstallTest(SecretGuardTestCase):
    def setUp(self):
        super().setUp()
        self.hooks = load(HOOKS, "git_hooks_7177")

    def commit_leak(self) -> subprocess.CompletedProcess:
        (self.repo / "evidence.txt").write_text(report_with(SECRET), encoding="utf-8")
        run(self.repo, "add", "evidence.txt")
        return run(self.repo, "commit", "-q", "-m", "leak")

    def test_install_sets_hooks_path_and_blocks_a_leaking_commit(self):
        result = self.hooks.install_secret_guard(self.repo, GUARD)
        target = (self.repo / ".git" / "chaos-engine-hooks").resolve()
        self.assertEqual("installed", result["status"])
        self.assertEqual(str(target), run(self.repo, "config", "core.hooksPath").stdout.strip())
        self.assertIsNone(result["chained"])
        for name in ("pre-commit", "pre-push"):
            self.assertTrue(os.access(target / name, os.X_OK))
        leak = self.commit_leak()
        self.assertNotEqual(0, leak.returncode)
        self.assertIn("secret_guard BLOCKED", leak.stderr)
        self.assertNotIn(SECRET, leak.stderr)
        (self.repo / "evidence.txt").write_text("clean\n", encoding="utf-8")
        run(self.repo, "add", "evidence.txt")
        self.assertEqual(0, run(self.repo, "commit", "-q", "-m", "clean").returncode)

    def test_existing_hooks_path_is_chained_with_shims_and_push_stdin_is_replayed(self):
        own = self.repo / "own-hooks"
        own.mkdir()
        marker = self.repo.parent / "marker.txt"
        for name, body in (("pre-commit", "echo commit >> {m}"), ("pre-push", "cat >> {m}"),
                           ("commit-msg", "echo msg >> {m}")):
            path = own / name
            path.write_text("#!/bin/sh\n" + body.format(m=marker) + "\n", encoding="utf-8")
            path.chmod(0o755)
        run(self.repo, "config", "core.hooksPath", "own-hooks")
        result = self.hooks.install_secret_guard(self.repo, GUARD)
        self.assertEqual("own-hooks", result["chained"])
        self.assertEqual(["commit-msg"], result["shims"])
        again = self.hooks.install_secret_guard(self.repo, GUARD)
        self.assertEqual("own-hooks", again["chained"])
        (self.repo / "a.txt").write_text("a\n", encoding="utf-8")
        run(self.repo, "add", "a.txt")
        self.assertEqual(0, run(self.repo, "commit", "-q", "-m", "a").returncode)
        remote = self.repo.parent / "remote.git"
        run(self.repo.parent, "init", "-q", "--bare", str(remote))
        run(self.repo, "remote", "add", "origin", str(remote))
        self.assertEqual(0, run(self.repo, "push", "-q", "origin", "feature").returncode)
        log = marker.read_text(encoding="utf-8")
        self.assertIn("commit", log)
        self.assertIn("msg", log)
        self.assertIn("refs/heads/feature", log)
        refused = run(self.repo, "push", "-q", "origin", "feature:main")
        self.assertNotEqual(0, refused.returncode)
        self.assertIn("no direct push to refs/heads/main", refused.stderr)

    def test_real_default_hooks_are_chained_and_opt_out_or_no_repo_skips(self):
        default = self.repo / ".git" / "hooks" / "pre-commit"
        default.write_text("#!/bin/sh\nexit 0\n", encoding="utf-8")
        default.chmod(0o755)
        result = self.hooks.install_secret_guard(self.repo, GUARD)
        self.assertEqual(str(default.parent.resolve()), result["chained"])
        default.unlink()
        run(self.repo, "config", "--unset", "core.hooksPath")
        self.assertIsNone(self.hooks.install_secret_guard(self.repo, GUARD)["chained"])
        self.assertEqual("", run(self.repo, "config", "--get", self.hooks.CHAIN_KEY).stdout)
        os.environ["CHAOS_ENGINE_SECRET_GUARD"] = "0"
        try:
            self.assertEqual("skipped", self.hooks.install_secret_guard(self.repo, GUARD)["status"])
        finally:
            del os.environ["CHAOS_ENGINE_SECRET_GUARD"]
        plain = Path(self.temp.name) / "plain"
        plain.mkdir()
        self.assertEqual("skipped", self.hooks.install_secret_guard(plain, GUARD)["status"])
        nested = self.repo / "nested"
        nested.mkdir()
        self.assertEqual("not the root of its git repository",
                         self.hooks.install_secret_guard(nested, GUARD)["reason"])


class TemplateParityTest(unittest.TestCase):
    def test_shaft_template_ships_the_same_guard_and_hooks(self):
        self.assertEqual(GUARD.read_bytes(), (TEMPLATE / "secret_guard.py").read_bytes())
        for name, flag in (("pre-commit", "--staged"), ("pre-push", "--push")):
            text = (TEMPLATE / name).read_text(encoding="utf-8")
            self.assertIn(f'secret_guard.py" {flag}', text)
            self.assertTrue(os.access(TEMPLATE / name, os.X_OK))
            self.assertIn("SECRET_GUARD_PATHS=", text)
        self.assertIn("allure-(results|report)", (ROOT / ".secret-guard-paths").read_text(encoding="utf-8"))
        for gitignore in (ROOT / "shaft-engine/src/main/resources/examples").glob("*/*/.gitignore"):
            self.assertIn("*allure*.html", gitignore.read_text(encoding="utf-8"), str(gitignore))


if __name__ == "__main__":
    unittest.main()
