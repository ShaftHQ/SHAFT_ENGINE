# {Workspace name}

{One sentence: what this workspace is and what leaves it.}

Built on ICM: folders carry sequencing, hierarchy carries context, files carry state. The structure is the documentation — if something needs explaining, the explanation goes in that folder's CONTEXT.md, not in your head.

## Where things live

| Folder | What it holds |
|---|---|
| `stages/` | the pipeline, in execution order |
| `_shared/` | factory: rules and reference that never change per run |
| `_templates/` | blank starters — new work is a copy, not a blank page |
| `setup/` | one-time factory configuration |

## Route by what just happened

| If | Go to | Then stop at |
|---|---|---|
| starting a new run | `stages/01_.../CONTEXT.md` | human reads the output |
| {previous stage} output approved | next numbered stage | human reads the output |
| asked for status | scan `stages/*/output/` | report what exists |
| setting up for a new user | `setup/questionnaire.md` | answers written to `_shared/` |

## The one rule

Nothing moves to the next stage until a person has read the output of the last one.
