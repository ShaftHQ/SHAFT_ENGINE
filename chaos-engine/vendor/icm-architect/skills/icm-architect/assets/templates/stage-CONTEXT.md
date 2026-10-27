# {NN}_{stage-name} - {the job in five words}

One job: {the single thing this stage does}.

## Inputs
- Working (this run): `../{prev-stage-folder}/output/{file}` (the previous folder's real name; renumbering the pipeline means editing this path)
- Reference (every run): `../../_shared/{rules-file}.md`
- Reference (every run): `references/{stage-specific-guide}.md`
- Example (optional): `examples/` (one real input and one good output from a run worth matching)

Do NOT load: {anything an eager agent would wrongly pull in: other stages' references, prior runs, the whole `_shared/` folder}.

## Process
1. If an input is missing or empty, stop and say which.
2. {Transform, following the reference constraints. A step that is code names its command, run from the workspace root: Run `python _tools/{script}.py {args}`.}
3. {Hard limits worth restating: length, count, format.}

## Outputs
- `{artifact}.md` -> `output/`

{For a shelf of notes instead of a stage: the notes this folder holds and the sections each must carry, from `_meta/schema.md`.}

## Check
{Who or what checks, as one concrete act: a person reads it aloud / `_tools/{script}.py` confirms the totals / a small model answers {question}. A check that isn't a person writes `check.md` beside the output, first line `pass` or `fail: <why>`. Edit the output in place; the next stage reads whatever is here.}
