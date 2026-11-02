# Analyze and decide

## 1 Requirements

- Restate goal in one line. List acceptance criteria as checks a test can run.
- Write non-goals. Unstated scope grows; stated non-goals stop it.
- Ask only questions that change the plan and sources cannot answer.
- Each criterion maps to one test or one measurement before code.

## 2 Quality attributes

- Pick top 3 of: reliability, performance, security, maintainability,
  operability, compatibility, usability, portability (ISO/IEC 25010).
- Write each as scenario: source, stimulus, environment, response, measure.
  Example: "10x burst, steady state, p95 < 300 ms, no errors."
- Unmeasurable attribute = wish. Drop or make measurable.

## 3 Trade-offs

- At least 2 real options, one being "smallest change / do nothing".
- Score against the 2-3 attributes from card 2, plus cost to reverse.
- Two-way door (cheap undo): decide fast, move on. One-way door: card 4.
- Name what each option makes worse. No option has no downside.

## 4 ADR

Hard to reverse, public contract, or cross-team? Write it in the PR body or
`docs/adr/NNNN-title.md` if the repo has the folder:

```text
Title | Status: proposed/accepted/superseded by N
Context: forces, constraints, quality scenarios
Decision: one sentence, active voice
Options: rejected ones + why
Consequences: good, bad, follow-ups (filed)
```

Review rounds: [design loop](../../../references/design-loop.md). Never edit an
accepted ADR; supersede it.

## 5 C4 sketch

Only when 2+ deployable units or external systems change. Level 1 context
(people, systems) + level 2 containers (apps, stores, queues) as a Mermaid or
text list. Skip levels 3-4; code is the component diagram.
