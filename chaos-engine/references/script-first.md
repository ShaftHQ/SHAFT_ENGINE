# Script first

Load this when an investigation would otherwise become a long chain of
overlapping tool calls. Prefer one deterministic program or focused test over
fifteen hops that restate the same question.

This reimplements a published code-mode idea as portable guidance: write or
run one program against the workspace instead of orchestrating each primitive
by hand. No upstream runtime is required. Named sources and licenses live in
[THIRD_PARTY_NOTICES](../THIRD_PARTY_NOTICES.md) and [RESEARCH](../RESEARCH.md).

## When a script is the smaller move

Use a script or focused test when you would otherwise:

- call the same search or read with slightly different arguments;
- parse a large log, report, or JSON document by hand;
- apply the same mechanical edit in several files;
- prove a behavior that a short executable already can fail.

Mechanical one-file edits still inspect the target, apply the change, and
report it. Do not invent a script for a single obvious line.

## How to do it

1. Name the question the program must answer or the transform it must apply.
2. Write the smallest runnable probe or edit next to the work, or use an
   existing test.
3. Run it once. Read the result. Delete a throwaway probe when it has no
   lasting value.
4. If the script fails for setup or environment reasons, fix that first. A
   broken harness is not evidence about the product.

## Boundaries

- Prefer the standard library and tools already in the project.
- Do not add a dependency to avoid a few lines.
- Do not replace an owned test with an untracked scratch file when the test
  is the proof the task needs.
- Token savings never drop negation, safety, or required attribution.
