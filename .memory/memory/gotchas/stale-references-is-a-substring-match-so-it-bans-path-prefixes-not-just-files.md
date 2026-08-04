## Do this

Before naming a new file or choosing a directory anywhere under
`.agents/skills/act-as-mohab/references/`, read the `stale_references` array in
`scripts/ci/agent_guidance_budget.json` and check your intended path against it
as a **substring**, not as a path:

```bash
py -3 -c "import json;print('\n'.join(json.load(open('scripts/ci/agent_guidance_budget.json'))['stale_references']))"
```

If any entry is a substring of the path you were about to link, pick a
different name. A flat sibling sharing a prefix is the cheap escape: the banned
`references/tdd/` does not occur in `references/tdd-failure-modes.md`, because
the separator differs. Do this before you write the file, not after the
validator goes red -- the diagnosis is expensive (below), and by then the name
is already in a commit message, a branch, and an issue.

## Why, and why the red is misleading

`validate_stale_references` in `scripts/ci/validate_agent_guidance.py` is
literally `if stale_reference in content` -- a raw substring test, with no path
resolution and no existence check. It runs over every file matched by
`reference_scan_globs`: `.agents/skills/act-as-mohab/references/**/*.md`,
`.agents/skills/README.md`, `.agents/skills/*/SKILL.md` and the host adapters.

So an entry ending in `/` retires an entire namespace permanently, including
files nobody has created yet. That is not apparent from the key's name, which
reads like a list of dead files.

The failure is reported against the file holding the **link**, not the new file
whose name caused it. Add `references/tdd/anything.md` and the red names
`SKILL.md`, `README.md` and `routing.md` -- three files you did not think you
were breaking, none of them the one to rename. Expect to lose time here unless
you recognise the shape.

## The entries are not reproduced here on purpose

Read them from the budget file with the command above. Copying the list into
memory would give it a second home to drift in, and several entries name paths
that active memory is separately forbidden to mention
(`test_active_memory_has_no_retired_harness_contracts` in
`tests/scripts/test_agent_harness_portability.py` matches retired harness
contracts by regex and fails the object that names one -- this memory tripped
exactly that on its first draft).

Two entries are worth knowing without looking: `references/tdd/` and
`references/test-driven-development.md`. They are why #4502 / PR #4503 shipped
one flat `tdd-failure-modes.md` rather than a `tdd/` subdirectory holding two
files. That layout was forced, not preferred -- if you are revisiting it, this
is the constraint to re-check first.
