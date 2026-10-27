# {Workspace name}

{One sentence: what this workspace is and what leaves it.}

## Where things live

| Folder | What it holds |
|---|---|
| `stages/` | the pipeline, in execution order |
| `_shared/` | factory: rules and reference that never change per run |
| `_templates/` | blank starters: new work is a copy, not a blank page |
| `_tools/` | scripts the stages call; `links.py` checks every link and input path |
| `setup/` | one-time factory configuration |

## Route by what just happened

| If | Go to | Then stop at |
|---|---|---|
| starting a new run | [01_{name}](stages/01_{name}/CONTEXT.md) | the stage's check |
| {previous stage} passed its check | next numbered stage | the stage's check |
| asked for status | scan `stages/*/output/` and any `check.md` | report what exists |
| setting up for a new user | [questionnaire](setup/questionnaire.md) | answers written to `_shared/` |

## The one rule

Nothing moves to the next stage until the last one's check passes.
