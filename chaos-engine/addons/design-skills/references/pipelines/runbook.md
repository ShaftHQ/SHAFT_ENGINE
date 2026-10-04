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
- Cap encoder threads below the core count (`-threads`, x264 `threads`) and
  keep tests and other heavy jobs off the machine while it renders.

## 2. Fast gates

Right after each target encodes, gate it in seconds and stop that target on
failure: `vooverlap`, `levels` (verify, then CRF retry per D14), `static`,
`blackfreeze`. Example plan for `design_qc.py all fast-plan.json`:

```json
{"checks": [{"check": "vooverlap", "args": ["edl.json"]},
            {"check": "levels", "args": ["out/master.mp4"]},
            {"check": "static", "args": ["out/master-clean.mp4"]}]}
```

## 3. Idle full QC

When no render runs, `design_qc.py idle --wait 600` passes, then ASR
(`vowords`, `tts`) and the full `all qc-plan.json`. ASR or QC on a loaded
machine produces false alarms; a finding is confirmed on the line WAV or a
second run before anyone investigates it.

## 4. Review

A fresh reviewer that did not build the video watches and listens to every
output end to end and lists blockers with timestamps. Fixes go back to stage
1 with `--only`.

## 5. Deliver

Upload only when full QC and review pass, with the QC report beside the
files. A file sent earlier is named and labelled DRAFT with its open
failures; superseded drafts move to an archive folder.
