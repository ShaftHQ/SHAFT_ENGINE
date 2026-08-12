"""Small bounded GitHub CLI transport with strict paginated JSON shapes."""

from __future__ import annotations

import json
import re
import shutil
import subprocess  # nosec B404 - fixed list-argument gh calls, never a shell.
from pathlib import Path
from typing import Any


REPOSITORY = re.compile(r"^[A-Za-z0-9_.-]+/[A-Za-z0-9_.-]+$")
ENDPOINT = re.compile(r"^[A-Za-z0-9_.~!$&'()*+,;=:@%/-]+$")


class GitHubUnavailable(RuntimeError):
    """GitHub transport or response could not prove a complete result."""


class GitHubClient:
    def __init__(self, repository: str, *, root: Path | None = None, runner=None, executable: str | None = None):
        if not REPOSITORY.fullmatch(repository):
            raise ValueError("repository must be an explicit owner/name slug")
        self.repository = repository
        self.root = Path.cwd() if root is None else Path(root)
        self.runner = subprocess.run if runner is None else runner
        self.executable = executable or shutil.which("gh")
        if not self.executable:
            raise GitHubUnavailable("GitHub CLI is not available")

    def get(self, endpoint: str) -> dict:
        self._validate_endpoint(endpoint)
        command = [self.executable, "api", f"repos/{self.repository}/{endpoint}"]
        try:
            result = self.runner(
                command, cwd=self.root, capture_output=True, text=True, timeout=30, check=False,
            )
        except (OSError, subprocess.SubprocessError) as error:
            raise GitHubUnavailable(f"GitHub API unavailable: {error}") from error
        if result.returncode:
            raise GitHubUnavailable(result.stderr.strip() or "GitHub API returned an error")
        try:
            payload = json.loads(result.stdout)
        except json.JSONDecodeError as error:
            raise GitHubUnavailable(f"GitHub API returned invalid JSON: {error}") from error
        if not isinstance(payload, dict):
            raise GitHubUnavailable("GitHub API returned an invalid object")
        return payload

    @staticmethod
    def _validate_endpoint(endpoint: str) -> None:
        if (
            not ENDPOINT.fullmatch(endpoint)
            or "://" in endpoint
            or endpoint.startswith(("/", "."))
            or "/../" in f"/{endpoint}/"
        ):
            raise ValueError("endpoint must be a repository-relative GitHub API path")

    def rest_pages(self, endpoint: str) -> list[dict]:
        return self.rest_page_result(endpoint)["items"]

    def rest_page_result(self, endpoint: str, *, jq: str | None = None) -> dict:
        self._validate_endpoint(endpoint)
        command = [
            self.executable, "api", f"repos/{self.repository}/{endpoint}",
            "--paginate", "--slurp",
        ]
        if jq:
            command.extend(("--jq", jq))
        try:
            result = self.runner(
                command, cwd=self.root, capture_output=True, text=True,
                timeout=30, check=False,
            )
        except (OSError, subprocess.SubprocessError) as error:
            raise GitHubUnavailable(f"GitHub API unavailable: {error}") from error
        if result.returncode:
            raise GitHubUnavailable(result.stderr.strip() or "GitHub API returned an error")
        try:
            pages = json.loads(result.stdout or "[]")
        except json.JSONDecodeError as error:
            raise GitHubUnavailable(f"GitHub API returned invalid JSON: {error}") from error
        if not isinstance(pages, list) or any(not isinstance(page, list) for page in pages):
            raise GitHubUnavailable("GitHub pagination was incomplete or malformed")
        flattened = [item for page in pages for item in page]
        if any(not isinstance(item, dict) for item in flattened):
            raise GitHubUnavailable("GitHub page contains an invalid item")
        return {"items": flattened, "pageCount": len(pages), "complete": True}

    def graphql_pages(self, query: str, variables: dict[str, Any]) -> dict:
        """Run a GraphQL connection query whose output is one connection page per response."""
        if not isinstance(query, str) or "$endCursor" not in query or "pageInfo" not in query:
            raise ValueError("GraphQL pagination query must bind $endCursor and request pageInfo")
        command = [self.executable, "api", "graphql", "--paginate", "--slurp", "-f", f"query={query}"]
        for name, value in sorted(variables.items()):
            if not re.fullmatch(r"[A-Za-z_][A-Za-z0-9_]*", name):
                raise ValueError("invalid GraphQL variable name")
            command.extend(("-F", f"{name}={value}"))
        try:
            result = self.runner(
                command, cwd=self.root, capture_output=True, text=True, timeout=30, check=False,
            )
        except (OSError, subprocess.SubprocessError) as error:
            raise GitHubUnavailable(f"GitHub GraphQL unavailable: {error}") from error
        if result.returncode:
            raise GitHubUnavailable(result.stderr.strip() or "GitHub GraphQL returned an error")
        try:
            pages = json.loads(result.stdout or "[]")
        except json.JSONDecodeError as error:
            raise GitHubUnavailable(f"GitHub GraphQL returned invalid JSON: {error}") from error
        if not isinstance(pages, list) or not pages or any(not isinstance(page, dict) for page in pages):
            raise GitHubUnavailable("GitHub GraphQL pagination was incomplete or malformed")
        return {"pages": pages, "pageCount": len(pages), "complete": True}
