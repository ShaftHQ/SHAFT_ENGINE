"""Bounded GitHub CLI transport tests (#4769)."""

import json
import subprocess  # nosec B404 - subprocess objects are test fixtures only.
import tempfile
import unittest

from scripts.agents.github_client import GitHubClient, GitHubUnavailable


class GitHubClientTest(unittest.TestCase):
    def test_rest_slurps_and_validates_every_page(self):
        calls = []

        def runner(command, **_kwargs):
            calls.append(command)
            return subprocess.CompletedProcess(command, 0, json.dumps([[{"id": 1}], [{"id": 2}]]), "")

        client = GitHubClient("consumer/project", runner=runner, executable="gh")
        self.assertEqual([{"id": 1}, {"id": 2}], client.rest_pages("pulls/7/reviews"))
        self.assertIn("repos/consumer/project/pulls/7/reviews", calls[0])
        self.assertIn("--paginate", calls[0])
        self.assertIn("--slurp", calls[0])

    def test_rest_field_projection_is_local_so_gh_never_combines_slurp_and_jq(self):
        calls = []

        def runner(command, **_kwargs):
            calls.append(command)
            pages = [
                {"check_runs": [{"id": 1}]},
                {"check_runs": [{"id": 2}]},
            ]
            return subprocess.CompletedProcess(command, 0, json.dumps(pages), "")

        client = GitHubClient("consumer/project", runner=runner, executable="gh")
        result = client.rest_page_result("commits/deadbeef/check-runs", jq=".check_runs")

        self.assertEqual([{"id": 1}, {"id": 2}], result["items"])
        self.assertNotIn("--jq", calls[0])

    def test_transport_timeout_error_and_malformed_page_fail_closed(self):
        cases = (
            lambda command, **kwargs: (_ for _ in ()).throw(subprocess.TimeoutExpired(command, 3)),
            lambda command, **kwargs: subprocess.CompletedProcess(command, 1, "", "denied"),
            lambda command, **kwargs: subprocess.CompletedProcess(command, 0, "not-json", ""),
            lambda command, **kwargs: subprocess.CompletedProcess(command, 0, json.dumps([{"not": "page"}]), ""),
        )
        for runner in cases:
            with self.subTest(runner=runner), self.assertRaises(GitHubUnavailable):
                GitHubClient("consumer/project", runner=runner, executable="gh").rest_pages("issues/7/comments")

    def test_repository_identity_and_endpoint_are_validated(self):
        for repository, endpoint in (("bad", "issues/1/comments"), ("owner/repo", "https://evil.test"), ("owner/repo", "../users")):
            with self.subTest(repository=repository, endpoint=endpoint), self.assertRaises(ValueError):
                GitHubClient(repository, runner=lambda *_a, **_k: None, executable="gh").rest_pages(endpoint)

    def test_comment_once_checks_exact_body_posts_with_file_and_reads_back(self):
        calls = []
        responses = [
            subprocess.CompletedProcess([], 0, "[]", ""),
            subprocess.CompletedProcess([], 0, "https://github.com/consumer/project/issues/7#issuecomment-9\n", ""),
            subprocess.CompletedProcess([], 0, json.dumps([[{"body": "proof", "html_url": "https://github.com/consumer/project/issues/7#issuecomment-9"}]]), ""),
        ]
        def runner(command, **_kwargs):
            calls.append(command)
            return responses.pop(0)
        with tempfile.NamedTemporaryFile("w", encoding="utf-8") as body_file:
            body_file.write("proof")
            body_file.flush()
            result = GitHubClient("consumer/project", runner=runner, executable="gh").comment_once(7, body_file.name)
        self.assertTrue(result["created"])
        self.assertIn("--body-file", calls[1])
        self.assertEqual("https://github.com/consumer/project/issues/7#issuecomment-9", result["url"])

    def test_comment_once_reuses_exact_existing_body_and_fails_on_mismatch(self):
        existing = [[{"body": "proof", "html_url": "https://github.com/consumer/project/issues/7#issuecomment-8"}]]
        client = GitHubClient("consumer/project", runner=lambda *_a, **_k: subprocess.CompletedProcess([], 0, json.dumps(existing), ""), executable="gh")
        with tempfile.NamedTemporaryFile("w", encoding="utf-8") as body_file:
            body_file.write("proof")
            body_file.flush()
            self.assertFalse(client.comment_once(7, body_file.name)["created"])

        responses = [subprocess.CompletedProcess([], 0, "[]", ""), subprocess.CompletedProcess([], 0, "url", ""), subprocess.CompletedProcess([], 0, json.dumps([[{"body": "other", "html_url": "url"}]]), "")]
        client = GitHubClient("consumer/project", runner=lambda *_a, **_k: responses.pop(0), executable="gh")
        with tempfile.NamedTemporaryFile("w", encoding="utf-8") as body_file:
            body_file.write("proof")
            body_file.flush()
            with self.assertRaisesRegex(GitHubUnavailable, "readback"):
                client.comment_once(7, body_file.name)


if __name__ == "__main__":
    unittest.main()
