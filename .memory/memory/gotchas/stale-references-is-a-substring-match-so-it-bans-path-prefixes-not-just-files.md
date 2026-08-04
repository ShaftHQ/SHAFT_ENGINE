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

## Live entries that constrain naming right now

`references/tdd/`, `references/test-driven-development.md`,
`references/caveman.md`, `references/ponytail.md`, `references/shaft-mastery.md`,
`playbooks/README.md`, `.agents/routing-bridges.txt`.

The first two are why #4502 / PR #4503 shipped one flat `tdd-failure-modes.md`
instead of a `tdd/` subdirectory holding two files. That was a forced layout,
not a preference -- if you are revisiting that decision, this is the constraint
to re-check first.
