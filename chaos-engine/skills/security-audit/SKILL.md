---
name: security-audit
description: >-
  Use when auditing a repository we own. A second pass confirms.
license: MIT
permissions:
  - read
---

# Security audit

On demand only. This skill is not part of the always-loaded router.

License check 2026-11-10: the studied Cloudflare skill was MIT. This file is original and does not copy that skill.

## Scope

Audit only a repository the owner names and that we are allowed to change. If the target is not a repository we own, stop. Do not produce attack steps.

## Findings

A finding is unconfirmed until a second pass by someone other than the writer verifies it. The writer cannot confirm their own finding.

Do not include exploit procedures, payloads, or covert access steps.

Classifier: [verify.py](verify.py).
