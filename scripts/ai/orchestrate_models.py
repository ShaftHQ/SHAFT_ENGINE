#!/usr/bin/env python3
"""Coordinate paid-agent planning and Ollama-backed local model summarization.

This script turns a short task request into inspectable Markdown artifacts by
asking a paid/cloud-capable coding agent for a smart implementation plan and
then asking a local Ollama model to summarize the plan for follow-up work.
"""

from __future__ import annotations

import argparse
import dataclasses
import datetime as dt
import re
import shutil
import subprocess
import sys
from pathlib import Path
from typing import Callable, Sequence


@dataclasses.dataclass(frozen=True)
class AgentCandidate:
    """A detected command-line agent."""

    name: str
    command: str
    path: str


@dataclasses.dataclass(frozen=True)
class CommandResult:
    """Captured subprocess result."""

    command: Sequence[str]
    returncode: int
    stdout: str
    stderr: str


class OrchestrationError(RuntimeError):
    """Raised for user-actionable orchestration failures."""


PAID_AGENT_COMMANDS: dict[str, str] = {
    "codex": "codex",
    "claude": "claude",
}

# Keep non-interactive command templates isolated so maintainers can adjust them
# when upstream CLI syntax changes. As of current public docs, Codex supports
# `codex exec "<prompt>"`, Claude Code supports `claude -p "<prompt>"`, and
# Ollama supports `ollama run <model> "<prompt>"`.
PAID_AGENT_TEMPLATES: dict[str, Callable[[str, str], list[str]]] = {
    "codex": lambda command, prompt: [command, "exec", prompt],
    "claude": lambda command, prompt: [command, "-p", prompt],
}

SECRET_PATTERNS: tuple[tuple[re.Pattern[str], str], ...] = (
    (
        re.compile(r"(?im)\b([A-Z0-9_]*(?:TOKEN|KEY|SECRET|PASSWORD|CREDENTIAL)[A-Z0-9_]*)\s*=\s*([^\s]+)"),
        r"\1=<redacted>",
    ),
    (re.compile(r"(?im)(Authorization\s*:\s*Bearer\s+)([^\s]+)"), r"\1<redacted>"),
    (
        re.compile(r"(?is)-----BEGIN [A-Z ]*PRIVATE KEY-----.*?-----END [A-Z ]*PRIVATE KEY-----"),
        "<redacted private key>",
    ),
    (re.compile(r"\b(?:sk-[A-Za-z0-9_-]{12,}|gh[pousr]_[A-Za-z0-9_]{12,})\b"), "<redacted>"),
)


def redact_secrets(text: str) -> str:
    """Redact common token, credential, bearer-token, and private-key patterns."""
    redacted = text or ""
    for pattern, replacement in SECRET_PATTERNS:
        redacted = pattern.sub(replacement, redacted)
    return redacted


def log(message: str) -> None:
    """Print a redacted progress message."""
    print(redact_secrets(message), flush=True)


def prompt_for_task(input_func: Callable[[str], str] = input) -> str:
    """Prompt the user for a non-empty task request."""
    log("What would you like to work on today?")
    task = input_func("> ").strip()
    if not task:
        raise OrchestrationError("Task request cannot be empty.")
    return task


def make_slug(text: str, max_length: int = 60) -> str:
    """Return a filesystem and branch safe slug for human text."""
    slug = re.sub(r"[^a-z0-9]+", "-", text.lower()).strip("-")
    slug = re.sub(r"-+", "-", slug)
    if not slug:
        slug = "task"
    if len(slug) > max_length:
        slug = slug[:max_length].rstrip("-") or "task"
    return slug


def run_command(command: Sequence[str], cwd: Path | None = None) -> CommandResult:
    """Run a command and capture stdout/stderr without invoking a shell."""
    completed = subprocess.run(command, cwd=cwd, text=True, capture_output=True, check=False)
    return CommandResult(
        command=tuple(command),
        returncode=completed.returncode,
        stdout=completed.stdout,
        stderr=completed.stderr,
    )


def is_git_repo() -> bool:
    """Return whether the current directory is inside a Git work tree."""
    result = run_command(["git", "rev-parse", "--is-inside-work-tree"])
    return result.returncode == 0 and result.stdout.strip() == "true"


def current_branch() -> str | None:
    """Return the current Git branch name, or None if unavailable/detached."""
    if not is_git_repo():
        return None
    result = run_command(["git", "branch", "--show-current"])
    branch = result.stdout.strip()
    return branch or None


def has_uncommitted_changes() -> bool:
    """Return whether Git reports uncommitted changes."""
    if not is_git_repo():
        return False
    result = run_command(["git", "status", "--porcelain"])
    return bool(result.stdout.strip())


def branch_exists(branch_name: str) -> bool:
    """Return whether a local branch already exists."""
    result = run_command(["git", "rev-parse", "--verify", "--quiet", branch_name])
    return result.returncode == 0


def create_task_branch(task: str) -> str | None:
    """Create and switch to a deterministic task branch when Git is available."""
    if not is_git_repo():
        log("Git repository was not detected. Branch creation was skipped.")
        return None
    if has_uncommitted_changes():
        log(
            "Warning: uncommitted changes are present. "
            "They will be preserved while creating the task branch."
        )
    base_name = f"ai-task/{make_slug(task)}"
    branch_name = base_name
    if branch_exists(branch_name):
        timestamp = dt.datetime.now(dt.UTC).strftime("%Y%m%d-%H%M%S")
        branch_name = f"{base_name}-{timestamp}"
    result = run_command(["git", "switch", "-c", branch_name])
    if result.returncode != 0:
        raise OrchestrationError(
            f"Failed to create branch {branch_name}:\n{redact_secrets(result.stderr or result.stdout)}"
        )
    log(f"Created branch: {branch_name}")
    return branch_name


def detect_paid_agents(which: Callable[[str], str | None] = shutil.which) -> list[AgentCandidate]:
    """Detect supported paid/cloud-capable planning agents on PATH."""
    agents: list[AgentCandidate] = []
    for name, command in PAID_AGENT_COMMANDS.items():
        path = which(command)
        if path:
            agents.append(AgentCandidate(name=name, command=command, path=path))
    return agents


def choose_candidate(
    candidates: list[AgentCandidate],
    label: str,
    requested_name: str | None = None,
    non_interactive: bool = False,
    input_func: Callable[[str], str] = input,
) -> AgentCandidate:
    """Choose a candidate automatically, by requested name, or interactively."""
    if not candidates:
        raise OrchestrationError(f"No {label}s were detected.")
    if requested_name:
        for candidate in candidates:
            if candidate.name == requested_name or candidate.command == requested_name:
                return candidate
        detected = ", ".join(candidate.name for candidate in candidates)
        raise OrchestrationError(
            f"Requested {label} '{requested_name}' was not detected. Detected: {detected or 'none'}."
        )
    if len(candidates) == 1:
        candidate = candidates[0]
        log(f"Detected {label}: {candidate.name}")
        log(f"Using {candidate.name} automatically because it is the only available {label}.")
        return candidate
    if non_interactive:
        names = ", ".join(candidate.name for candidate in candidates)
        raise OrchestrationError(f"Multiple {label}s were detected ({names}); pass an explicit selection.")
    log(f"Multiple {label}s were detected:")
    for index, candidate in enumerate(candidates, start=1):
        log(f"  {index}) {candidate.name} ({candidate.path})")
    choice = input_func(f"Select the {label} [1-{len(candidates)}]:\n> ").strip()
    try:
        choice_index = int(choice)
    except ValueError as exc:
        raise OrchestrationError(f"Invalid {label} selection: {choice}") from exc
    if choice_index < 1 or choice_index > len(candidates):
        raise OrchestrationError(f"Invalid {label} selection: {choice}")
    return candidates[choice_index - 1]


def repository_context() -> str:
    """Return concise repository-aware planning constraints without secret data."""
    paths = [
        "AGENTS.md",
        ".github/instructions/framework-source.instructions.md",
        ".github/instructions/java-tests.instructions.md",
    ]
    existing = [path for path in paths if Path(path).exists()]
    if not existing:
        return "No repository-specific instruction files were detected."
    return (
        "This repository may contain durable instructions. Before editing matching files, read and obey: "
        + ", ".join(existing)
        + ". Preserve safety, secret-handling, testing, and pull-request instructions."
    )


def build_paid_agent_prompt(user_request: str, repo_context: str) -> str:
    """Build the paid-agent prompt while preserving the exact user request."""
    return f"""You are the planning agent for an implementation workflow.

Convert the user's request into a complete implementation plan.
Preserve the user's requested meaning and do not add unrelated scope.
Expand ambiguous human language into concrete implementation steps, validation steps, edge cases, expected files,
and acceptance criteria.

Repository constraints:
{repo_context}

The output must be Markdown. It must include:
- objective;
- assumptions;
- repository constraints;
- step-by-step implementation plan;
- examples of expected behavior;
- files likely to change;
- tests or checks to run;
- failure handling;
- final review checklist.

Write only the plan content, not unrelated commentary.

Original user request:
<<<USER_REQUEST
{user_request}
USER_REQUEST
>>>"""


def run_paid_agent(agent: AgentCandidate, prompt: str) -> CommandResult:
    """Invoke a paid agent and return captured output."""
    template = PAID_AGENT_TEMPLATES.get(agent.name)
    if template is None:
        raise OrchestrationError(f"Unsupported paid agent template: {agent.name}")
    command = template(agent.command, prompt)
    log(f"Running paid agent: {agent.name}")
    result = run_command(command)
    log(redact_secrets(result.stdout))
    if result.stderr:
        log(redact_secrets(result.stderr))
    if result.returncode != 0:
        raise OrchestrationError(f"Paid agent '{agent.name}' failed with exit code {result.returncode}.")
    if not result.stdout.strip():
        raise OrchestrationError(f"Paid agent '{agent.name}' produced empty output.")
    log("Paid agent completed successfully.")
    return result


def parse_ollama_list(output: str) -> list[str]:
    """Parse model names from `ollama list` table output."""
    models: list[str] = []
    for raw_line in output.splitlines():
        line = raw_line.strip()
        if not line or line.upper().startswith("NAME"):
            continue
        models.append(line.split()[0])
    return models


def detect_ollama_models() -> list[str]:
    """Detect installed Ollama models by running `ollama list`."""
    if shutil.which("ollama") is None:
        raise OrchestrationError(
            "Ollama was not found on PATH. Install Ollama and pull at least one model, for example:\n"
            "  ollama pull qwen3.5:9b\nThen rerun this script."
        )
    result = run_command(["ollama", "list"])
    if result.returncode != 0:
        raise OrchestrationError(f"Failed to run 'ollama list':\n{redact_secrets(result.stderr or result.stdout)}")
    models = parse_ollama_list(result.stdout)
    if not models:
        raise OrchestrationError(
            "No local Ollama models were detected. Pull a model, for example:\n"
            "  ollama pull qwen3.5:9b"
        )
    return models


def choose_ollama_model(
    models: list[str],
    requested_model: str | None = None,
    non_interactive: bool = False,
    input_func: Callable[[str], str] = input,
) -> str:
    """Choose an Ollama model automatically, by name, or interactively."""
    if not models:
        raise OrchestrationError("No local Ollama models were detected.")
    if requested_model:
        if requested_model in models:
            return requested_model
        raise OrchestrationError(
            f"Requested local model '{requested_model}' was not detected. Detected: {', '.join(models)}."
        )
    if len(models) == 1:
        log(f"Detected local Ollama model: {models[0]}")
        log(f"Using {models[0]} automatically because it is the only available local model.")
        return models[0]
    if non_interactive:
        raise OrchestrationError(
            f"Multiple local Ollama models were detected ({', '.join(models)}); pass --local-model."
        )
    log("Multiple local Ollama models were detected:")
    for index, model in enumerate(models, start=1):
        log(f"  {index}) {model}")
    choice = input_func(f"Select the local model that should consume the plan [1-{len(models)}]:\n> ").strip()
    try:
        choice_index = int(choice)
    except ValueError as exc:
        raise OrchestrationError(f"Invalid local model selection: {choice}") from exc
    if choice_index < 1 or choice_index > len(models):
        raise OrchestrationError(f"Invalid local model selection: {choice}")
    return models[choice_index - 1]


def build_local_agent_prompt(plan_markdown: str) -> str:
    """Build the prompt for a local model to summarize the smart plan."""
    return f"""You are the local implementation assistant.

Read the implementation plan below. Produce a concise but complete feature summary for the user and future implementers.
Do not claim that code was changed unless the plan explicitly includes completed code changes. Include:
- what the requested feature will do;
- key workflow steps;
- required tools;
- generated artifacts;
- risks and safety constraints;
- suggested implementation checklist.

Implementation plan:
<<<PLAN
{plan_markdown}
PLAN
>>>"""


def run_ollama_model(model: str, prompt: str) -> CommandResult:
    """Invoke an Ollama model and return captured output."""
    log(f"Running local model: {model}")
    result = run_command(["ollama", "run", model, prompt])
    log(redact_secrets(result.stdout))
    if result.stderr:
        log(redact_secrets(result.stderr))
    if result.returncode != 0:
        raise OrchestrationError(f"Local model '{model}' failed with exit code {result.returncode}.")
    if not result.stdout.strip():
        raise OrchestrationError(f"Local model '{model}' produced empty output.")
    log("Local model completed successfully.")
    return result


def write_markdown_artifact(path: Path, metadata: dict[str, str], body: str) -> None:
    """Write a Markdown artifact with metadata and redacted body."""
    path.parent.mkdir(parents=True, exist_ok=True)
    artifact_metadata = dict(metadata)
    title = artifact_metadata.pop("title", "Generated Artifact")
    lines = [f"# {title}", ""]
    for key, value in artifact_metadata.items():
        lines.append(f"- {key}: {redact_secrets(str(value))}")
    lines.extend(["", "---", "", redact_secrets(body).strip(), ""])
    path.write_text("\n".join(lines), encoding="utf-8")


def truncate_metadata(value: str, max_length: int = 300) -> str:
    """Truncate long metadata values for readable artifact headers."""
    if len(value) <= max_length:
        return value
    return value[: max_length - 3] + "..."


def utc_now() -> str:
    """Return an ISO-8601 UTC timestamp."""
    return dt.datetime.now(dt.UTC).replace(microsecond=0).isoformat().replace("+00:00", "Z")


def build_parser() -> argparse.ArgumentParser:
    """Build the command-line parser."""
    parser = argparse.ArgumentParser(
        description="Create a paid-agent smart plan and Ollama local-model feature summary.",
        epilog=(
            "Examples:\n"
            "  python scripts/ai/orchestrate_models.py --task 'Fix flaky login test'\n"
            "  python scripts/ai/orchestrate_models.py --paid-agent codex --local-model qwen3.5:9b\n"
            "  python scripts/ai/orchestrate_models.py --current-branch --task 'Document CLI setup'"
        ),
        formatter_class=argparse.RawDescriptionHelpFormatter,
    )
    parser.add_argument("--task", help="Task request to plan. If omitted, the script prompts interactively.")
    parser.add_argument("--paid-agent", choices=sorted(PAID_AGENT_COMMANDS), help="Paid planning agent to use.")
    parser.add_argument("--local-model", help="Installed Ollama model to use for the local feature summary.")
    parser.add_argument(
        "--current-branch",
        action="store_true",
        help="Use the current branch instead of creating a task branch.",
    )
    parser.add_argument(
        "--dry-run",
        action="store_true",
        help="Validate selection and write deterministic sample artifacts without invoking agents.",
    )
    parser.add_argument(
        "--output-dir",
        type=Path,
        help="Directory for generated Markdown artifacts. Defaults to .agent-runs/<task-slug>.",
    )
    parser.add_argument("--non-interactive", action="store_true", help="Fail instead of prompting for missing choices.")
    return parser


def orchestrate(args: argparse.Namespace) -> int:
    """Run the orchestration workflow."""
    if args.non_interactive and not args.task:
        raise OrchestrationError("--non-interactive requires --task.")
    task = args.task.strip() if args.task else prompt_for_task()
    if not task:
        raise OrchestrationError("Task request cannot be empty.")

    branch = current_branch()
    if args.current_branch:
        log(f"Using current branch: {branch or 'unavailable'}")
    else:
        created_branch = create_task_branch(task)
        branch = created_branch or branch

    slug = make_slug(task)
    output_dir = args.output_dir or Path(".agent-runs") / slug

    paid_agents = detect_paid_agents()
    if not paid_agents:
        raise OrchestrationError(
            "No paid agents were found on PATH. Install or authenticate a supported paid agent "
            "such as 'codex' or 'claude', then rerun."
        )
    paid_agent = choose_candidate(paid_agents, "paid agent", args.paid_agent, args.non_interactive)
    log(f"Selected paid agent: {paid_agent.name} ({paid_agent.path})")

    paid_prompt = build_paid_agent_prompt(task, repository_context())
    if args.dry_run:
        paid_output = "# Dry Run Smart Plan\n\nThis is a deterministic dry-run plan. No paid agent was invoked."
    else:
        paid_output = run_paid_agent(paid_agent, paid_prompt).stdout

    smart_plan_path = output_dir / "smart-agent-plan.md"
    write_markdown_artifact(
        smart_plan_path,
        {
            "title": "Smart Agent Implementation Plan",
            "Original request": truncate_metadata(task),
            "Paid agent": paid_agent.name,
            "Generated at": utc_now(),
            "Branch": branch or "unavailable",
        },
        paid_output,
    )
    log(f"Saved smart plan: {smart_plan_path}")

    if args.dry_run:
        models = [args.local_model or "dry-run-model"]
    else:
        try:
            models = detect_ollama_models()
        except OrchestrationError as exc:
            log("No local feature summary was generated.")
            log(f"Smart plan remains available at: {smart_plan_path}")
            raise exc
    local_model = choose_ollama_model(models, args.local_model, args.non_interactive)
    log(f"Selected local model: {local_model}")

    plan_markdown = smart_plan_path.read_text(encoding="utf-8")
    local_prompt = build_local_agent_prompt(plan_markdown)
    if args.dry_run:
        local_output = (
            "# Dry Run Feature Summary\n\n"
            "This is a deterministic dry-run summary. No local model was invoked."
        )
    else:
        local_output = run_ollama_model(local_model, local_prompt).stdout

    feature_summary_path = output_dir / "local-agent-feature-summary.md"
    write_markdown_artifact(
        feature_summary_path,
        {
            "title": "Local Agent Feature Summary",
            "Source plan": smart_plan_path.name,
            "Local model": local_model,
            "Generated at": utc_now(),
            "Branch": branch or "unavailable",
        },
        local_output,
    )
    log(f"Saved feature summary: {feature_summary_path}")

    log(
        "Implementation planning artifacts are ready for review.\n\n"
        f"Branch: {branch or 'unavailable'}\n"
        f"Smart agent plan: {smart_plan_path}\n"
        f"Local feature summary: {feature_summary_path}\n\n"
        "Open this branch in your preferred IDE to inspect the generated plan and summary."
    )
    return 0


def main(argv: Sequence[str] | None = None) -> int:
    """Entry point."""
    parser = build_parser()
    args = parser.parse_args(argv)
    try:
        return orchestrate(args)
    except OrchestrationError as exc:
        log(f"Error: {exc}")
        return 1


if __name__ == "__main__":
    sys.exit(main())
