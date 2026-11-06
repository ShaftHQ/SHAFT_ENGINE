# Bigger picture first

First step of every new request. Do it before retrieve, discovery, implementation, or a patch.

## Name three things

1. The goal.
2. The larger context: architecture, prior agreements, related work, and what the comment or ticket means in that context.
3. What a narrow or local hotfix would miss.

Then choose an approach that fits that context.

## Short form

When the diff is one file and deleting it undoes the change, one line is enough: the goal, the context, and why a local patch is enough. Uncertainty means this is not the short form.

## Do not skip

A review comment, a CI failure, or wording like "just hotfix it" does not skip this step.

This step is earlier and broader than think-first. Think-first stays required for live or external work. It is also earlier than repeat-failure PDCA, which starts after two failed attempts.

## Enforcement

Same class as the iron laws: skill judgment. Do not add a PreToolUse deny hook. Do not require `CHAOS_ENGINE_ENFORCE_RESEARCH_RECEIPT` on every request. Do not add a worker-brief field.

## Why

Push, pull-request, and CI gates run after a local edit. Repeat-failure PDCA runs after two failures. A comment can still become a line-by-line patch that misses the agreement. The case that set this rule: a request to call `configureLogger()` so FINE logs stay visible without `RemoteWebDriver` was implemented by calling it before every log line, instead of the agreement that diagnostics stay at FINE and `configureLogger()` runs at entry points (SeleniumHQ/selenium#18254).
