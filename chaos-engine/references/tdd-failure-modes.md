# TDD failure modes

Load this when a test involves a mock, when you are about to add a method to a
production class for a test's benefit, or when you catch yourself building an
argument for writing the code first.

The entrypoint states the cycle and lists the red-flag phrases. It does not
answer them, and it does not say how to catch any of this while you can still
act on it. That is what is here: a rebuttal per excuse, and a gate per failure.

## Rationalizations, answered

Recognizing the phrase is not enough, because each one arrives wearing a
reason. These are the reasons and what is wrong with them.

| Excuse | Answer |
| --- | --- |
| "Tests after achieve the same thing" | Tests-after answer "what does this do?" Tests-first answer "what should this do?" The order decides what the test asserts. |
| "I will verify it works afterwards" | A test written against finished code passes on its first run. You never saw it fail, so you know nothing about what it catches. |
| "I already tested the edge cases by hand" | Ad-hoc, unrecorded, not repeatable. It cannot re-run when the code changes, which is the moment it would have earned its keep. |
| "Deleting hours of work is wasteful" | The hours are gone either way. The waste is keeping code no check covers. |
| "Keep it as a reference and write the tests first" | You will adapt what is on screen. That is testing after, with extra steps. |
| "I need to explore first" | Fine. Exploration is a prototype, and a prototype is thrown away before the first real test. |
| "It is too simple to break" | Simple code breaks. The test costs less than the argument about whether it was needed. |
| "This one is hard to write a test for" | Listen to it. Hard to test is hard to use, and the design is what is wrong. |
| "TDD is dogma, I am being pragmatic" | Test-first finds the defect before the commit instead of after the deploy. Pragmatism is the argument for it. |
| "The code around this has no tests either" | You are improving that area. Cover what you touch. |
| "This case is different because..." | Finish the sentence out loud. If it does not name a rule, it is the excuse, not a case. |

## Test-only methods in production

A method that only tests call is production surface with no production caller. It
reads as real API to everyone downstream, it can be invoked by mistake, and it
usually parks a resource's teardown on a class that does not own that
resource's lifecycle. Test cleanup belongs in test utilities.

**Gate, before adding any method to a production class.** Is it called only
from tests? Then do not add it. Does this class own the lifecycle of what the
method disposes? If not, it is the wrong class even for production code.

## Mocking without understanding

Mocking broadly to be safe is how a test starts passing for the wrong reason.
The usual shape: the mocked call carried a side effect the test's premise
depended on, so the condition under test can no longer arise and the assertion
proves nothing. Mocking the slow thing at too high a level removes the
behavior along with the slowness.

**Gate, before mocking anything.** Name the real method's side effects, then
say which of them this test depends on. If it depends on any, mock further down
— the external or slow operation itself — never the call the test's premise
runs through. If you cannot say what the test depends on, run it against the
real implementation first and watch what has to happen, then add the least
mocking that keeps it honest. "I will mock this to be safe" and "this might be
slow" are the phrasings that precede this failure.

## Incomplete mocks

A mock assembled from the fields your assertion reads encodes your assumptions
about a structure rather than the structure. Code downstream consumes a field
you did not know about, and the mock is silent about it: the suite stays green
and the integration fails. Mirror the whole structure as it really exists, not
the part this one test looks at.

**Gate, before writing a mock response.** Find the real shape, from a live
example or the contract that defines it, and include every field the system may
read downstream. If you are unsure, include all documented fields. Building a
mock at all means owning the entire structure.

## Asserting on a mock

The entrypoint already rules this out, so treat an assertion on a mock as a
symptom and go after the cause: you arrive here by adding mocks without first
watching the test fail against real code, which means the fix is upstream of
the assertion rather than in it. Delete the assertion or unmock the collaborator
and test the real behavior.

Tells: an assertion matching a mock's own identifier, a mock whose removal turns
the test red for no behavioral reason, a mock you cannot justify out loud.

## When the mock is the problem

Warning signs: setup longer than the test, another mock added for every
collaborator on the way to green, a mock missing methods the real component
has, or a test that breaks whenever the mock changes.

Ask whether the mock is needed at all. A test against real collaborators is
often smaller than the mock it would have replaced.
