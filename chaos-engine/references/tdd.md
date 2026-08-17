## Test-driven development

Features, fixes, refactors, and behavior changes use strict
RED-GREEN-REFACTOR:

1. RED: add one focused test of observable behavior and run it. Accept only an
   expected assertion failure caused by missing behavior; a pass or setup,
   syntax, or environment error is not RED. If production code was written
   first, revert that new code and restart.
2. GREEN: write only enough production code to pass. Rerun the focused test,
   then the nearest plausible regression check. Fix code, not a valid test.
3. REFACTOR: improve names or duplication only while green, then rerun checks.

Prefer real code; mock only an unavoidable external boundary. A test that
asserts nothing, prints instead of asserting, or mocks the behavior under test
is not a test. Never backfill tests after implementation and call it TDD, or
claim an unrun check. Documentation, guidance, configuration, generated code,
and discarded prototypes may skip test-first; validate their structure or
affected flow instead. If RED or GREEN cannot run, report the blocker and do
not claim verified behavior.

Mocks, or the urge to skip RED: [TDD failure modes](tdd-failure-modes.md).

Caveman, Ponytail, and TDD adaptations retain their MIT notices under
`references/*.LICENSE`.
