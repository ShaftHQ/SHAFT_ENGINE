# {Workspace name} - the pipeline

The flow in one line: {plan it, make it, check it, ship it, in your workspace's words}.

| Stage | Job |
|---|---|
| `01_{name}` | {five words} |
| `02_{name}` | {five words} |
| `03_{name}` | {five words} |

Each stage's own `CONTEXT.md` holds its inputs, outputs and check.

Factory (stable, every run): `_shared/{voice.md, rules.md, ...}`
Product (new each run): each stage's `output/`

Status is whatever exists: a stage is complete when its `output/` holds an artifact and any `check.md` there says pass. A placeholder that only keeps the empty folder in git does not count.
