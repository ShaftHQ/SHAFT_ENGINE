"""Bounded GitHub CLI transport tests (#4769)."""

import json
import subprocess
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


if __name__ == "__main__":
    unittest.main()
