---
name: video-pipeline-runbook
description: Use when a render, ASR, or QC run takes minutes, so the work must survive interruptions, rebuild only what changed, and stay off a loaded machine.
---

# Video pipeline runbook

How to run the [technical-video pipeline](technical-video.md) without redoing
work. Five stages, in order; one CPU-heavy job at a time.

## 1. Detached build

- Launch every render as a core [durable job](../../../../references/durable-jobs.md),
  never inside one long agent turn and never as a bare background command:
  `python3 .chaos-engine/tool.py job start build --part-dir cache --part-dir out -- nice -n 19 python build.py --only master`.
  A second worker is refused while the lease is live. Sessions die at about
  45-50 min, and the job keeps running.
- The build records each finished step with
  `python3 .chaos-engine/tool.py job checkpoint build <step>` and skips a step
  that `job checkpoint build <step> --check` (or its cached output) already
  shows done. Every scene, cache, and encode writes `<file>.part`, then
  renames it into place.
- `STATUS.md` stays the human log: one timestamped line per step (what
  finished, what is next, open findings). It is never the liveness signal. A
  live build can go 30 min without a STATUS line.
- On resume, run `python3 .chaos-engine/tool.py job status build` first.
  `live` means leave it alone. Otherwise `python3 .chaos-engine/tool.py job resume build`
  kills the old run's survivors, deletes its `.part` orphans, and restarts it.
  A watchdog runs only `job resume build` (recipe in durable-jobs.md). Never
  restart a finished step, and never start a second `--only` build beside a
  live one.
- Incremental: cache each scene render under a hash of its inputs (source
  files, shared CSS and tokens, timing, parameters). `--only <target,...>`
  rebuilds just the affected outputs; a one-target fix never rebuilds all.
- Never ship stale inputs: the build entry point regenerates every derived
  file (scene pages, boards, line WAVs) or first runs
  `design_qc.py fresh --source 'make_*.py' --source 'boards/*.json' --target 'scenes/*.html'`
  and stops on failure. A rebuild script never calls a later stage alone.
- Wait with one blocking call: `python3 .chaos-engine/tool.py job wait build --timeout 540 --tail 5`
  (exit 7 = still live; call it again). No log polling loops.
- Cap encoder threads below the core count (`-threads`, x264 `threads`) and
  keep tests and other heavy jobs off the machine while it renders.

## 2. Fast gates

Right after each target encodes, gate it in seconds and stop that target on
failure: `vooverlap`, `levels` (verify, then CRF retry per D14), `static`,
`blackfreeze`, `flatframes`. Example plan for `design_qc.py all fast-plan.json`:

```json
{"checks": [{"check": "vooverlap", "args": ["edl.json"]},
            {"check": "levels", "args": ["out/master.mp4"]},
            {"check": "static", "args": ["out/master-clean.mp4"]}]}
```

## 3. Idle full QC

Voice pre-check is two commands in two processes: synthesize every line,
then transcribe every line WAV (D17). When no render runs, `design_qc.py idle --wait 600` passes, then ASR
(`vowords`, `vopauses`, `tts`) and the full `all qc-plan.json --cache qc-cache.json`:
a step whose check, arguments and input files match a cached pass is
skipped, so only changed outputs are re-checked (list indirect inputs in a
step's `inputs`). ASR or QC on a loaded machine produces false alarms; a
finding is confirmed on the line WAV or a second run before anyone
investigates it.

## 4. Review

A numeric reviewer score is not a gate: LLM judges are weak absolute
scorers and video models describe frames that are not there. The gate is
full QC plus a findings ledger with no open verified finding of severity
moderate or worse.

- Use one reviewer per output file (several files in one prompt get mixed up),
  fresh each round, never the builder. The prompt carries a **measured facts**
  block (duration, scene holds, text sizes, gaps, loudness) and an **owner
  decisions** block (intentional choices not to report), and the severity
  scale: blocker (wrong or broken, cannot ship), major (a viewer misses the
  point), moderate (visible flaw a viewer notices), minor (polish). Each
  finding: mm:ss, what is on screen or heard, severity. A score is optional
  and advisory.
- Verify every finding with a tool before any edit:
  `design_qc.py frames out.mp4 --at 1:36 --at 2:18 --out sheet.png --spectrum spec.png`
  (frames at t-1, t, t+1; spectrum of the same window), plus ASR word timings
  for speech. Record it in `ledger.json`:
  `{"rounds": [{"round": 1, "output": "short", "findings": [{"id": "f1", "t": "0:15", "severity": "moderate", "claim": "caption covers chips", "verdict": "true", "evidence": "sheet r1: overlap at y 1290", "status": "fixed", "fix_evidence": "sheet r2: 140 px gap"}]}]}`.
- A true finding is fixed red, then green: first add a failing check (a
  `claims` `must`/`must_not` entry, a frame assertion), then fix until it
  passes. Iterate on scene stills (D09), not full rebuilds; then
  `--only` the affected outputs.
- `design_qc.py findings ledger.json --cap 3 --markdown review-log.md` is
  the gate. `next`: `verify` (a finding lacks a verdict or evidence), `fix`,
  `deliver`, or `owner`. Run another round only after a verified moderate or
  worse fix, at most 3 rounds per output; at the cap, deliver to the owner
  with the review log instead of looping.

## 5. Deliver

Upload when full QC passes and `findings` says `deliver` (or `owner` at the
cap), with the QC report and review log beside the files. A file sent
earlier is named and labelled DRAFT with its open failures; superseded
drafts move to an archive folder.
