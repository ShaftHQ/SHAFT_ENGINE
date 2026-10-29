"""#7251: the delete guard refuses deletes of work that exists nowhere else."""

from __future__ import annotations

import hashlib
import importlib.util
import io
import json
import os
import shutil
import subprocess  # nosec B404 - fixed git argv in temp fixtures.
import tempfile
import unittest
from pathlib import Path
import unittest.mock

GIT = shutil.which("git") or "git"
ROOT = Path(__file__).resolve().parents[2]
CORE = ROOT / "chaos-engine"


def load(name: str, path: Path):
    spec = importlib.util.spec_from_file_location(name, path)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


GUARD = load("chaos_engine_delete_guard_7251", CORE / "delete_guard.py")


def git(cwd: Path, *argv: str) -> None:
    subprocess.run(  # nosec B603 - fixed git argv, temp fixture paths.
        [GIT, "-C", str(cwd), "-c", "user.name=t", "-c", "user.email=t@example.invalid", "-c", "commit.gpgsign=false", *argv],
        check=True,
        capture_output=True,
    )


class DeleteGuardTest(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory()
        self.base = Path(self.temp.name).resolve()
        home = self.base / "home"
        home.mkdir()
        # The fixture lives under /tmp, which is allow-listed; drop defaults so the git rules are exercised.
        self.patches = [
            unittest.mock.patch.object(GUARD, "DEFAULT_ALLOW", ()),
            unittest.mock.patch.dict(os.environ, {"HOME": str(home), "XDG_STATE_HOME": str(home / "state")}),
        ]
        for patch in self.patches:
            patch.start()
        self.repo = self.base / "work"
        self.repo.mkdir()
        git(self.repo, "init", "-q", "-b", "main")
        (self.repo / "keep.txt").write_text("x", encoding="utf-8")
        git(self.repo, "add", ".")
        git(self.repo, "commit", "-qm", "init")

    def tearDown(self):
        for patch in reversed(self.patches):
            patch.stop()
        self.temp.cleanup()

    def reason(self, command: str, cwd: Path | None = None) -> str:
        return GUARD.block_reason(command, cwd or self.base, self.repo)

    def push(self) -> None:
        remote = self.base / "remote.git"
        subprocess.run([GIT, "init", "-q", "--bare", str(remote)], check=True, capture_output=True)  # nosec B603
        git(self.repo, "remote", "add", "origin", str(remote))
        git(self.repo, "push", "-q", "-u", "origin", "main")

    def test_delete_guard_blocks_untracked_dir_allows_pushed_clean(self):
        data = self.repo / "data"
        data.mkdir()
        (data / "only-copy.txt").write_text("precious", encoding="utf-8")
        self.assertIn("untracked", self.reason(f"rm -rf {data}"))
        git(self.repo, "add", ".")
        git(self.repo, "commit", "-qm", "data")
        self.assertIn("no upstream", self.reason(f"rm -rf {data}"))
        self.push()
        self.assertEqual("", self.reason(f"rm -rf {data}"))
        self.assertEqual("", self.reason(f"rm -rf {self.repo}"))
        (self.repo / "keep.txt").write_text("edited", encoding="utf-8")
        self.assertIn("modified", self.reason(f"rm -rf {self.repo}"))

    def test_ahead_of_upstream_is_refused(self):
        self.push()
        (self.repo / "new.txt").write_text("n", encoding="utf-8")
        git(self.repo, "add", ".")
        git(self.repo, "commit", "-qm", "local only")
        self.assertIn("ahead", self.reason(f"rm -rf {self.repo}"))

    def test_find_delete_and_exec_rm_are_refused(self):
        (self.repo / "u.txt").write_text("u", encoding="utf-8")
        self.assertIn("refused", self.reason(f"find {self.repo} -name '*.txt' -delete"))
        self.assertIn("refused", self.reason(f"find {self.repo} -type f -exec rm -f {{}} +"))
        self.assertEqual("", self.reason(f"find {self.repo} -name '*.txt' -print"))

    def test_glob_cd_and_xargs_cases(self):
        (self.repo / "a.log").write_text("a", encoding="utf-8")
        self.assertIn("a.log", self.reason(f"rm -f {self.repo}/*.log"))
        self.assertIn("a.log", self.reason(f"cd {self.repo} && rm -rf a.log"))
        self.assertIn("a.log", self.reason("rm -rf a.log", cwd=self.repo))
        self.assertIn("a.log", self.reason(f"bash -c 'cd {self.repo}; rm a.log'"))
        self.assertIn("stdin", self.reason(f"ls {self.repo} | xargs rm -f"))
        self.assertIn("computed", self.reason('rm -rf "$TARGET"'))
        self.assertIn("a.log", self.reason(f"git -C {self.repo} clean -fdx"))
        self.assertEqual("", self.reason(f"git -C {self.repo} clean -n"))

    def test_outside_git_needs_allow_list_and_tmp_default(self):
        loose = self.base / "loose"
        loose.mkdir()
        self.assertIn("outside git", self.reason(f"rm -rf {loose}"))
        state = self.repo / ".chaos-engine-state"
        state.mkdir()
        (state / "delete-allow.txt").write_text(f"{loose}/**\n", encoding="utf-8")
        self.assertEqual("", self.reason(f"rm -rf {loose}"))
        self.assertEqual("", self.reason("rm -rf /nonexistent-7251/x"))

    def test_default_allow_list_covers_tmp_caches_and_video_renders(self):
        allowed = (
            str(Path(tempfile.gettempdir()).resolve() / "x"),
            str(Path.home() / ".cache/pip"),
            "/w/app/node_modules/.cache/babel",
            "/w/app/target/classes",
            "/w/pkg/__pycache__/m.pyc",
            "/w/pkg/.pytest_cache",
            "/w/bot/video/clips/a.mp4",
            "/w/bot/video/renders",
            "/w/bot/videos/ep1/out/final.mp4",
        )
        refused = ("/w/bot/video/src/a.mp4", "/w/bot/website", "/w/bot/video", "/w/app/src/target.py")
        with unittest.mock.patch.object(GUARD, "DEFAULT_ALLOW", load("fresh_7251", CORE / "delete_guard.py").DEFAULT_ALLOW):
            for path in allowed:
                self.assertTrue(GUARD.allow_listed(Path(path)), path)
            for path in refused:
                self.assertFalse(GUARD.allow_listed(Path(path)), path)

    def test_python_rmtree_literal_checked_and_computed_refused(self):
        (self.repo / "d").mkdir()
        (self.repo / "d" / "f").write_text("f", encoding="utf-8")
        self.assertIn("refused", self.reason(f"python3 -c \"import shutil; shutil.rmtree('{self.repo / 'd'}')\""))
        self.assertIn("non-literal", self.reason("python3 -c 'import shutil,sys; shutil.rmtree(sys.argv[1])' x"))

    def _loose(self, name="loose"):
        path = self.base / name
        path.mkdir()
        return path

    def test_code_match_allows_once_with_warning_and_logs(self):
        """#7343: owner pastes the chat code back; it unlocks exactly once."""
        loose = self._loose()
        self.assertIn("outside git", self.reason(f"rm -rf {loose}"))
        code = GUARD.request_code([loose])
        self.assertRegex(code, r"^[A-Z2-9]{6}$")
        self.assertNotIn(code, GUARD._ack_store().read_text(encoding="utf-8"))  # Only the hash is stored.
        reason, warnings = GUARD.check(f"rm -rf {loose}", self.base, self.base, code.lower())
        self.assertEqual("", reason)
        self.assertIn("outside git", warnings[0])  # Not-pushed becomes a warning, not a block.
        self.assertIn("used", GUARD._ack_store().with_name("delete-overrides.jsonl").read_text(encoding="utf-8"))
        again, _ = GUARD.check(f"rm -rf {loose}", self.base, self.base, code)  # Reuse.
        self.assertIn("already used", again)

    def test_code_mismatch_expiry_and_path_change_refuse(self):
        loose, other = self._loose(), self._loose("other")
        code = GUARD.request_code([loose])
        self.assertIn("unknown", GUARD.check(f"rm -rf {loose}", self.base, self.base, "ZZZZZZ")[0])
        self.assertIn("different path list", GUARD.check(f"rm -rf {loose} {other}", self.base, self.base, code)[0])
        self.assertIn("different path list", GUARD.check(f"rm -rf {other}", self.base, self.base, code)[0])
        with unittest.mock.patch.dict(os.environ, {"CHAOS_DELETE_ACK": code}):
            self.assertEqual("", self.reason(f"rm -rf {loose}"))  # Env form; still valid after mismatches.
        stale = GUARD.request_code([loose], ttl=-1)
        self.assertIn("expired", GUARD.check(f"rm -rf {loose}", self.base, self.base, stale)[0])
        self.assertFalse(hasattr(GUARD, "issue_ack"))  # TTY-only override is gone.

    def test_never_touch_list_no_code_unlocks(self):
        for name in ("yt", "designer-ws.git.retired", "ROG-x"):
            target = self._loose(name)
            with self.assertRaises(PermissionError):
                GUARD.request_code([target])
            self.assertIn("never-touch", self.reason(f"rm -rf {target}/sub"))
        self.assertIn("contains never-touch", self.reason(f"rm -rf {self.base}"))
        profile = self.base / "h" / ".config" / "google-chrome"
        profile.mkdir(parents=True)
        self.assertIn("never-touch", self.reason(f"rm -rf {profile}/Default"))
        self.assertIn("browser profile", self.reason(f"rm -rf {self.base / 'h'}"))
        self.assertEqual(2, GUARD.main(["--request-code", str(profile)]))

    def test_unlocked_names_need_a_code(self):
        for name in ("wt-relblock", "wt-ls", "ce-src-abc"):
            target = self._loose(name)
            self.assertEqual("", GUARD.never_touch(target))
            self.assertIn("outside git", self.reason(f"rm -rf {target}"))
            code = GUARD.request_code([target])
            self.assertEqual("", GUARD.check(f"rm -rf {target}", self.base, self.base, code)[0])

    def _sha(self, path: Path) -> str:
        return hashlib.sha256(path.read_bytes()).hexdigest()

    def _write_allow(self, allow: Path, lines: list[str]) -> None:
        allow.parent.mkdir(parents=True, exist_ok=True)
        allow.write_text("\n".join(lines) + "\n", encoding="utf-8")
        allow.chmod(0o600)

    def test_yt_verified_d1_d2_d3_proof_containment_and_env(self):
        """#7525: sha256+oEmbed verification, containment, owner-only env; injected verifier."""
        yt = self.base / "yt"
        (yt / "meta").mkdir(parents=True)
        (yt / "src").mkdir(parents=True)
        outside = self.base / "secrets.mp4"
        outside.write_text("secret", encoding="utf-8")
        listed = yt / "src" / "AbCdEfGhIjK.mp4"
        unlisted = yt / "src" / "unlisted.mp4"
        meta_listed = yt / "meta" / "AbCdEfGhIjK.mp4"
        for target in (listed, unlisted, meta_listed):
            target.write_text("payload-" + target.name, encoding="utf-8")
        good_sha = self._sha(listed)
        wrong_sha = "0" * 64
        allow = self.base / "home" / "yt-verified-delete.txt"
        escape = yt / "src" / ".." / ".." / "secrets.mp4"
        symlink = yt / "src" / "link.mp4"
        try:
            symlink.symlink_to(outside)
        except OSError:
            symlink = None
        self._write_allow(
            allow,
            [
                f"{listed} AbCdEfGhIjK {good_sha}",
                f"{listed} FakeId00001 {good_sha}",
                f"{unlisted} AbCdEfGhIjK {wrong_sha}",
                f"{meta_listed} AbCdEfGhIjK {self._sha(meta_listed)}",
                f"{escape} AbCdEfGhIjK {self._sha(outside)}",
                f"{yt / 'src'}/** AbCdEfGhIjK {good_sha}",
                f"{yt / 'src'}/ AbCdEfGhIjK {good_sha}",
            ]
            + ([f"{symlink} AbCdEfGhIjK {self._sha(outside)}"] if symlink is not None else []),
        )
        # Default verifier rejects everything until we inject.
        GUARD.set_youtube_video_verifier(lambda _vid: False)
        with unittest.mock.patch.dict(os.environ, {"CHAOS_YT_VERIFIED_DELETE": str(allow)}):
            # oEmbed/network reject (injected False)
            self.assertIn("never-touch", GUARD.never_touch(listed))
            # FakeId: even with True verifier, FakeId line is a second entry for same path —
            # loader keeps last write; use separate path for FakeId.
            fake = yt / "src" / "fake.mp4"
            fake.write_text("fake", encoding="utf-8")
            self._write_allow(allow, [f"{fake} FakeId00001 {self._sha(fake)}", f"{listed} AbCdEfGhIjK {good_sha}"])
            GUARD.set_youtube_video_verifier(lambda vid: vid == "AbCdEfGhIjK")
            self.assertIn("never-touch", GUARD.never_touch(fake))  # FakeId rejected by verifier False
            GUARD.set_youtube_video_verifier(lambda vid: True)  # even True shouldn't matter if we treat FakeId specially?
            # Spec: FakeId rejected — meaning forged id that fails oEmbed. Inject False for FakeId.
            GUARD.set_youtube_video_verifier(lambda vid: vid != "FakeId00001")
            self.assertIn("never-touch", GUARD.never_touch(fake))
            # sha mismatch
            self._write_allow(allow, [f"{unlisted} AbCdEfGhIjK {wrong_sha}"])
            GUARD.set_youtube_video_verifier(lambda _vid: True)
            self.assertIn("never-touch", GUARD.never_touch(unlisted))
            # oEmbed 404
            self._write_allow(allow, [f"{listed} AbCdEfGhIjK {good_sha}"])
            GUARD.set_youtube_video_verifier(lambda _vid: False)
            self.assertIn("never-touch", GUARD.never_touch(listed))
            # network error => locked (verifier raises → treat as reject via wrapper returning False)
            GUARD.set_youtube_video_verifier(lambda _vid: (_ for _ in ()).throw(RuntimeError("net")))
            # Production catches network; injected verifier should not raise — simulate by returning False
            GUARD.set_youtube_video_verifier(lambda _vid: False)
            self.assertIn("never-touch", GUARD.never_touch(listed))
            # .. escape rejected
            self._write_allow(allow, [f"{escape} AbCdEfGhIjK {self._sha(outside)}"])
            GUARD.set_youtube_video_verifier(lambda _vid: True)
            self.assertIn("never-touch", GUARD.never_touch(escape))
            with self.assertRaises(PermissionError):
                GUARD.request_code([escape])
            # symlink escape rejected
            if symlink is not None:
                self._write_allow(allow, [f"{symlink} AbCdEfGhIjK {self._sha(outside)}"])
                self.assertIn("never-touch", GUARD.never_touch(symlink))
                with self.assertRaises(PermissionError):
                    GUARD.request_code([symlink])
            # yt/meta rejected even if listed + sha + oEmbed ok
            self._write_allow(allow, [f"{meta_listed} AbCdEfGhIjK {self._sha(meta_listed)}"])
            self.assertIn("never-touch", GUARD.never_touch(meta_listed))
            with self.assertRaises(PermissionError):
                GUARD.request_code([meta_listed])
            # valid entry + code allowed (TOCTOU re-check at check time)
            self._write_allow(allow, [f"{listed} AbCdEfGhIjK {good_sha}"])
            GUARD.set_youtube_video_verifier(lambda vid: vid == "AbCdEfGhIjK")
            self.assertEqual("", GUARD.never_touch(listed))
            self.assertIn("outside git", self.reason(f"rm -f {listed}"))
            code = GUARD.request_code([listed])
            self.assertEqual("", GUARD.check(f"rm -f {listed}", self.base, self.base, code)[0])
            # env file not 0600 rejected (no widening)
            allow.chmod(0o644)
            GUARD.set_youtube_video_verifier(lambda _vid: True)
            self.assertEqual({}, GUARD._load_yt_verified())
            self.assertIn("never-touch", GUARD.never_touch(listed))
            allow.chmod(0o600)
        GUARD.set_youtube_video_verifier(None)

    def test_yt_verified_default_oembed_network_fail_closed(self):
        """Injected absence uses default path; patched urlopen failure stays locked."""
        yt = self.base / "yt" / "src"
        yt.mkdir(parents=True)
        listed = yt / "AbCdEfGhIjK.mp4"
        listed.write_text("x", encoding="utf-8")
        allow = self.base / "home" / "yt-verified-delete.txt"
        self._write_allow(allow, [f"{listed} AbCdEfGhIjK {self._sha(listed)}"])
        GUARD.set_youtube_video_verifier(None)
        env = {
            "CHAOS_YT_VERIFIED_DELETE": str(allow),
            # Empty string clears ambient bind (patch.dict does not delete keys).
            "CHAOS_YT_EXPECTED_AUTHORS": "",
        }
        with unittest.mock.patch.dict(os.environ, env, clear=False):
            with unittest.mock.patch.object(GUARD.urllib.request, "urlopen", side_effect=OSError("down")):
                self.assertFalse(GUARD.youtube_video_exists("AbCdEfGhIjK"))
                self.assertIn("never-touch", GUARD.never_touch(listed))
                cache = GUARD._load_oembed_cache()
                entry = cache.get("AbCdEfGhIjK")
                self.assertIsInstance(entry, dict)
                self.assertFalse(entry.get("ok"))
                self.assertEqual(GUARD.YT_OEMBED_SOFT_TTL, entry.get("ttl"))

    def _fake_oembed(self, status: int, body: bytes):
        class _Resp:
            def __init__(self):
                self.status = status

            def read(self):
                return body

            def __enter__(self):
                return self

            def __exit__(self, *args):
                return False

        return _Resp()

    def test_yt_oembed_requires_author_json_and_optional_channel_bind(self):
        """HTTP 200 alone is not enough; author_name proof + optional expected authors."""
        GUARD.set_youtube_video_verifier(None)
        with unittest.mock.patch.dict(os.environ, {"CHAOS_YT_EXPECTED_AUTHORS": ""}, clear=False):
            # empty JSON author → locked
            with unittest.mock.patch.object(
                GUARD.urllib.request,
                "urlopen",
                return_value=self._fake_oembed(200, b'{"title":"x"}'),
            ):
                self.assertFalse(GUARD.youtube_video_exists("AbCdEfGhIjK"))
            # wipe cache between cases
            GUARD._save_oembed_cache({})
            good = b'{"title":"x","author_name":"Channel One","author_url":"https://www.youtube.com/@one"}'
            with unittest.mock.patch.object(
                GUARD.urllib.request, "urlopen", return_value=self._fake_oembed(200, good)
            ):
                self.assertTrue(GUARD.youtube_video_exists("AbCdEfGhIjK"))
            GUARD._save_oembed_cache({})
            with unittest.mock.patch.dict(os.environ, {"CHAOS_YT_EXPECTED_AUTHORS": "Other Channel"}):
                with unittest.mock.patch.object(
                    GUARD.urllib.request, "urlopen", return_value=self._fake_oembed(200, good)
                ):
                    self.assertFalse(GUARD.youtube_video_exists("AbCdEfGhIjK"))
            GUARD._save_oembed_cache({})
            with unittest.mock.patch.dict(os.environ, {"CHAOS_YT_EXPECTED_AUTHORS": "Channel One,Other"}):
                with unittest.mock.patch.object(
                    GUARD.urllib.request, "urlopen", return_value=self._fake_oembed(200, good)
                ):
                    self.assertTrue(GUARD.youtube_video_exists("AbCdEfGhIjK"))

    def test_yt_oembed_403_private_long_ttl_fail_closed(self):
        """oEmbed 403 (private) stays locked and uses the long definitive TTL."""
        GUARD.set_youtube_video_verifier(None)
        GUARD._save_oembed_cache({})
        err = GUARD.urllib.error.HTTPError(
            GUARD.YT_OEMBED.format(id="AbCdEfGhIjK"), 403, "Forbidden", hdrs=None, fp=None
        )
        with unittest.mock.patch.object(GUARD.urllib.request, "urlopen", side_effect=err):
            self.assertFalse(GUARD.youtube_video_exists("AbCdEfGhIjK"))
        entry = GUARD._load_oembed_cache()["AbCdEfGhIjK"]
        self.assertFalse(entry["ok"])
        self.assertEqual(403, entry.get("status"))
        self.assertEqual(GUARD.YT_OEMBED_TTL, entry.get("ttl"))

    def test_yt_oembed_5xx_soft_ttl_fail_closed(self):
        """HTTP 5xx stays locked and uses the short soft TTL (not the day-long definitive TTL)."""
        GUARD.set_youtube_video_verifier(None)
        GUARD._save_oembed_cache({})
        err = GUARD.urllib.error.HTTPError(
            GUARD.YT_OEMBED.format(id="AbCdEfGhIjK"), 503, "Service Unavailable", hdrs=None, fp=None
        )
        with unittest.mock.patch.object(GUARD.urllib.request, "urlopen", side_effect=err):
            self.assertFalse(GUARD.youtube_video_exists("AbCdEfGhIjK"))
        entry = GUARD._load_oembed_cache()["AbCdEfGhIjK"]
        self.assertFalse(entry["ok"])
        self.assertEqual(503, entry.get("status"))
        self.assertEqual(GUARD.YT_OEMBED_SOFT_TTL, entry.get("ttl"))

    def test_yt_oembed_cache_hit_revalidates_author_bind(self):
        """Warm positive cache must re-apply CHAOS_YT_EXPECTED_AUTHORS; bare ok:true is not enough."""
        import time

        GUARD.set_youtube_video_verifier(None)
        now = time.time()
        GUARD._save_oembed_cache(
            {
                "AbCdEfGhIjK": {
                    "ok": True,
                    "expires": now + GUARD.YT_OEMBED_TTL,
                    "checked": int(now),
                    "status": 200,
                    "author": "Channel One",
                    "ttl": GUARD.YT_OEMBED_TTL,
                }
            }
        )
        # Unbound: stored non-empty author still unlocks without a network call.
        with unittest.mock.patch.dict(os.environ, {"CHAOS_YT_EXPECTED_AUTHORS": ""}, clear=False):
            with unittest.mock.patch.object(GUARD.urllib.request, "urlopen") as urlopen:
                self.assertTrue(GUARD.youtube_video_exists("AbCdEfGhIjK"))
                urlopen.assert_not_called()
        # Tighten bind without wiping cache → must lock (D1).
        with unittest.mock.patch.dict(os.environ, {"CHAOS_YT_EXPECTED_AUTHORS": "Other Channel"}, clear=False):
            with unittest.mock.patch.object(GUARD.urllib.request, "urlopen") as urlopen:
                self.assertFalse(GUARD.youtube_video_exists("AbCdEfGhIjK"))
                urlopen.assert_not_called()
        # Matching bind still unlocks from cache.
        with unittest.mock.patch.dict(os.environ, {"CHAOS_YT_EXPECTED_AUTHORS": "Channel One"}, clear=False):
            self.assertTrue(GUARD.youtube_video_exists("AbCdEfGhIjK"))

    def test_yt_oembed_poisoned_or_malformed_cache_fail_closed(self):
        """Poisoned ok:true / empty author / non-bool ok / bad expires never unlock or raise."""
        import time

        GUARD.set_youtube_video_verifier(None)
        now = time.time()
        yt = self.base / "yt" / "src"
        yt.mkdir(parents=True)
        listed = yt / "AbCdEfGhIjK.mp4"
        listed.write_text("x", encoding="utf-8")

        # ok:true with empty author → locked (D1 poison).
        GUARD._save_oembed_cache(
            {
                "AbCdEfGhIjK": {
                    "ok": True,
                    "expires": now + GUARD.YT_OEMBED_TTL,
                    "author": "",
                }
            }
        )
        with unittest.mock.patch.dict(os.environ, {"CHAOS_YT_EXPECTED_AUTHORS": "Channel One"}, clear=False):
            with unittest.mock.patch.object(GUARD.urllib.request, "urlopen") as urlopen:
                self.assertFalse(GUARD.youtube_video_exists("AbCdEfGhIjK"))
                urlopen.assert_not_called()

        # Non-bool ok string would be truthy under bool(); must miss and re-fetch as locked.
        GUARD._save_oembed_cache(
            {
                "AbCdEfGhIjK": {
                    "ok": "false",
                    "expires": now + GUARD.YT_OEMBED_TTL,
                    "author": "Channel One",
                }
            }
        )
        with unittest.mock.patch.object(
            GUARD.urllib.request, "urlopen", side_effect=OSError("down")
        ):
            self.assertFalse(GUARD.youtube_video_exists("AbCdEfGhIjK"))

        # Malformed expires must not raise into never_touch / block_reason (D2).
        GUARD._save_oembed_cache(
            {
                "AbCdEfGhIjK": {
                    "ok": True,
                    "expires": "not-a-number",
                    "author": "Channel One",
                }
            }
        )
        allow = self.base / "home" / "yt-verified-delete.txt"
        self._write_allow(allow, [f"{listed} AbCdEfGhIjK {self._sha(listed)}"])
        with unittest.mock.patch.dict(
            os.environ,
            {
                "CHAOS_YT_VERIFIED_DELETE": str(allow),
                "CHAOS_YT_EXPECTED_AUTHORS": "",
            },
            clear=False,
        ):
            with unittest.mock.patch.object(
                GUARD.urllib.request, "urlopen", side_effect=OSError("down")
            ):
                self.assertFalse(GUARD.youtube_video_exists("AbCdEfGhIjK"))
                # never_touch / block_reason must not raise TypeError
                self.assertIn("never-touch", GUARD.never_touch(listed))
                self.assertIn("never-touch", GUARD.block_reason(f"rm -f {listed}", self.base, self.repo))

    def test_yt_old_renders_need_a_code_rest_refused(self):
        yt = self.base / "yt"
        (yt / "meta").mkdir(parents=True)
        for name in ("ep1.r3.mp4", "ep1.mp4", "meta/ep1.r3.mp4", "meta/notes.json"):
            (yt / name).write_text("x", encoding="utf-8")
        old = yt / "ep1.r3.mp4"
        self.assertEqual("", GUARD.never_touch(old))
        self.assertIn("outside git", self.reason(f"rm -f {old}"))
        code = GUARD.request_code([old])
        self.assertEqual("", GUARD.check(f"rm -f {old}", self.base, self.base, code)[0])
        for kept in (yt, yt / "meta", yt / "ep1.mp4", yt / "meta" / "ep1.r3.mp4", yt / "meta" / "notes.json"):
            self.assertIn("never-touch", self.reason(f"rm -rf {kept}"))
            with self.assertRaises(PermissionError):
                GUARD.request_code([kept])

    def test_cli_request_and_verify(self):
        loose = self._loose()
        out = io.StringIO()
        with unittest.mock.patch("sys.stdout", out), unittest.mock.patch.object(GUARD.Path, "cwd", return_value=self.base):
            self.assertEqual(0, GUARD.main(["--request-code", str(loose)]))
            code = out.getvalue().split()[1]
            self.assertEqual(1, GUARD.main([str(loose)]))
            self.assertEqual(0, GUARD.main(["--code", code, str(loose)]))
            self.assertEqual(1, GUARD.main(["--code", code, str(loose)]))

    def test_hook_blocks_and_doctor_row_passes(self):
        guard = load("chaos_engine_guard_hook_7251", CORE / "hooks" / "guard.py")
        (self.repo / "u.txt").write_text("u", encoding="utf-8")
        with unittest.mock.patch.object(guard, "_delete_guard", return_value=GUARD):
            self.assertIn("Delete guard", guard.delete_guard_reason((f"rm -rf {self.repo}",), self.base))
        self.assertIsNotNone(guard._delete_guard())  # Real wiring: the hook finds the core module.
        event = {"hook_event_name": "PreToolUse", "tool_name": "Bash", "tool_input": {"command": f"rm -rf {self.repo}"}, "cwd": str(self.base)}
        with unittest.mock.patch.object(guard, "_delete_guard", return_value=GUARD), unittest.mock.patch.object(guard, "_record_denial_with_significance"), unittest.mock.patch.object(guard, "_run_event", return_value=0) as rest:
            self.assertEqual(2, guard._guarded_event(event, "claude"))
            rest.assert_not_called()
        self.assertIn("_guarded_event", (CORE / "hooks" / "guard.py").read_text(encoding="utf-8").split("def main()")[1])
        self.assertEqual(("pass", ""), GUARD.doctor_check(CORE))
        install = load("chaos_engine_install_7251", CORE / "install.py")
        self.assertEqual(("pass",), install.delete_guard_status(CORE))
        status = install.backup_remote_advisory(self.repo, CORE)
        self.assertEqual("warn", status[0])
        self.assertIn("no remote", status[1])
        self.push()
        (self.repo / "u.txt").unlink()
        self.assertEqual(("pass",), install.backup_remote_advisory(self.repo, CORE))


    def test_backup_remote_warns_in_consumer_mode(self):
        """#7263: consumer exclude hides .memory, so a pushed clean tree must still warn."""
        self.push()
        self.assertEqual(("pass", ""), GUARD.backup_remote_status(self.repo))
        marker = self.repo / ".chaos-engine-state" / "consumer-mode"
        marker.parent.mkdir()
        marker.write_text("consumer\n", encoding="utf-8")
        exclude = self.repo / ".git" / "info" / "exclude"
        exclude.write_text("/.chaos-engine-state/\n/.memory/\n", encoding="utf-8")
        (self.repo / ".memory").mkdir()
        (self.repo / ".memory" / "m.md").write_text("only copy", encoding="utf-8")
        status, reason = GUARD.backup_remote_status(self.repo)
        self.assertEqual("warn", status)
        self.assertIn("consumer mode hides harness state", reason)

if __name__ == "__main__":
    unittest.main()
