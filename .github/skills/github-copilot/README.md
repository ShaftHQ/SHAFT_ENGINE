# GitHub Copilot Skills

Skills and instructions for using [GitHub Copilot](https://github.com/features/copilot) with the SHAFT test automation framework.

## About GitHub Copilot

GitHub Copilot is an AI pair programmer that helps you write code faster. It draws context from comments and code to suggest individual lines and whole functions.

## Available Skills

| Skill | File | Description |
|---|---|---|
| CI Failure Investigator | [ci-failure-investigator.md](./ci-failure-investigator.md) | Ingests CI workflow logs, summarizes root causes, maps failures to source files, and proposes minimal fix strategies |
| Flaky Test Stabilizer | [flaky-test-stabilizer.md](./flaky-test-stabilizer.md) | Detects intermittent test anti-patterns, proposes stabilization plans, and generates regression checks |
| Release & Dependency Guard | [release-dependency-guard.md](./release-dependency-guard.md) | Validates release-version consistency, detects dependency drift, and generates release readiness reports |

## Notes

- SHAFT AI policy is centralized in `/AGENTS.md` and `/docs/ai/`; `.github/copilot-instructions.md` is only a bridge, and path-scoped instruction files must not be recreated.
- Skills in this directory focus on specific, reusable prompts and workflows beyond the base configuration
