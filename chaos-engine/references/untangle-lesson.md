---
description: Use when a review needs hidden-link, needless-link, and surplus vocabulary without rewriting the repository or deleting tests.
---

# Untangle

Use this when a review of a change already in scope needs structure vocabulary. Do not start a repository-wide untangle.

## Hidden links

Hidden links are copies that must change together, a module that reads state another module sets, a feature split across layers, or a call order callers must just know.

## Needless links

Needless links are callers that depend on internals, stable code that imports more volatile code, a general function that takes one caller's vocabulary, or a shallow module whose interface changes with its internals.

## Surplus

Surplus is a pass-through wrapper, an extension point that nothing varies, or dead code.

## Special cases

A one-value branch, a caller-only parameter, a swallowed exception, a retry or sleep, or a disabled test is a requirement the current shape missed. A disabled test stays until a behavior test replaces it.

## Decision log

Write the fix, then the problem lines it removes. Do not start editing from the problem list. Stop at the requested change. Do not continue until nothing worth fixing remains.

## Rejected

Do not untangle a whole repository. Do not remove tests. The upstream step "remove tests that would not catch a real bug" is rejected. Iron law 3 stands: never weaken, delete, or rewrite a test to reach green.

Keep a comment that states a constraint the code cannot show.
