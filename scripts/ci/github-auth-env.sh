#!/usr/bin/env bash
# Normalize GitHub authentication environment variables for local agents and CI.
#
# Source this file before using `gh`, `curl`, or other GitHub API clients:
#   source scripts/ci/github-auth-env.sh
#
# GitHub CLI prefers GH_TOKEN, while many CI/agent environments expose
# GITHUB_TOKEN. Keep both names populated when either one is already available.
# This script never prints token values.

if [ -z "${GH_TOKEN:-}" ] && [ -n "${GITHUB_TOKEN:-}" ]; then
  export GH_TOKEN="${GITHUB_TOKEN}"
fi

if [ -z "${GITHUB_TOKEN:-}" ] && [ -n "${GH_TOKEN:-}" ]; then
  export GITHUB_TOKEN="${GH_TOKEN}"
fi

# Make gh fail fast in non-interactive environments instead of prompting for
# browser/device authentication when no usable token is present.
export GH_PROMPT_DISABLED="${GH_PROMPT_DISABLED:-1}"
