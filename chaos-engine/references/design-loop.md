# Design loop

Portable write-review-revise contract for a design document. Host-specific
spawn tools, scratch helpers, and pager labels stay on that host.

## Use when

The owner asks to design, write a design doc, architecture doc, or technical
spec.

## Durable rules

- The orchestrator coordinates only. A writer subagent authors the document. A
  reviewer subagent authors findings. The orchestrator does not write the doc
  or invent review issues.
- Loop until the reviewer reports **0 open issues**. Nits count. There is no
  iteration cap, but a writer `wontfix` that the reviewer reopens is a
  stalemate: escalate to the owner once.
- Every design document includes **Key Decisions** (with rationale) and a
  **PR Plan** of independently reviewable pull requests.
- Owner answers to open questions and stalemates are final.
- Keep one design-doc path, one summary path, and one review path for the
  whole loop. Every claimed spawn is a real dispatch in the same turn.

This contract does not execute the PR plan; delivery stays on
[work-github-playbook](work-github-playbook.md).
