# Consult-first

Load this reference only when uncertainty, blast radius, reversibility, or stakes justify a deeper
decision. It is a thinking aid, not a mutation gate.

Record only what changes the implementation:

1. Deliverable and observable proof.
2. Fatal unknowns, cheapest first.
3. Invariants and trust boundaries.
4. Two credible approaches and why one wins.
5. Files, owners, callers, and migration states affected.
6. Validation scope or alternative evidence.
7. Delivery and rollback shape.

Use a table only when several callers, states, or acceptance criteria would otherwise be missed.
Issue comments, RED tests, delegation, and independent review are optional evidence selected by the
task and user. Unavailable planning infrastructure never blocks local work.

Prefer the smallest structural owner fix over symptom lists. Prefer a removable design when options
are otherwise equal. Current files and authoritative primary sources outrank old plans.
