---
description: Use when work runs longer than a few minutes and must survive the agent session being killed, with no duplicate workers.
---

# Durable jobs

Hosts kill agent sessions at a hard limit; about 50 minutes has been
observed (see [CI status economy](ci-status-economy.md)). A durable job runs
detached from the session under a supervisor that keeps a **lease** (PID,
process group, command, heartbeat) and a **checkpoint log**. Any later
session or watchdog reads the lease instead of guessing.

## Session-budget rules

- Assume any session can die at about 45-50 min, at any point in a turn.
- Anything longer than a few minutes runs as a durable job:
  `python3 .chaos-engine/tool.py job start <name> -- <command>`. Never as a
  foreground command, and never as a bare `nohup ... &`.
- Checkpoint before every long step: the job records finished steps with
  `tool.py job checkpoint <name> <step>`, and outputs are written atomically.
- Never hold work only in the session. Hand off through the job lease, the
  checkpoint log, and a status file such as `STATUS.md`, which is for people.
- Before any resume, run `tool.py job status <name>`. `live` means leave it
  alone. A doc's mtime or `pgrep` is never a liveness signal: a live worker can
  go quiet for half an hour.

## Commands

| Command | Effect |
| --- | --- |
| `tool.py job start NAME [--heartbeat 30] [--stale S] [--part-dir D]... [--max-runs 3] -- CMD...` | Detach a supervisor (`setsid`; Windows detached process group) that runs CMD in its own process group and heartbeats the lease. Refused (exit 3) while the lease is live. |
| `tool.py job status NAME [--json]` | `live`, `stale`, `done`, `failed`, `stopped` or `absent`, plus heartbeat age and last step. Exit 0 live/done, 3 stale, 4 failed, 5 stopped, 6 absent. |
| `tool.py job wait NAME [--timeout 540] [--tail 5]` | Block until the job is not live, then print the status line and the last log lines. Exit as `status`, or 7 when still live at the timeout. Use it instead of polling loops; keep the timeout under the host's tool-call limit. |
| `tool.py job resume NAME` | Watchdog entry. No-op while live or done, and after an owner's `stop`. A stale or failed run restarts with the recorded command, at most `--max-runs` times in total. |
| `tool.py job stop NAME` | Terminate the whole run and record `stopped`. |
| `tool.py job checkpoint NAME STEP [--note T]` / `--check` | Record a finished step, or exit 0 only if STEP is recorded (skip it on resume). |

State lives in `.chaos-engine-state/jobs/<name>/` (`lease.json`,
`checkpoint.jsonl`, `output.log`) of the project, or under
`$CHAOS_ENGINE_JOBS_DIR`. Without `--root` or the variable, the CLI walks up
from the current directory to the nearest `.chaos-engine-state/jobs` (else
the nearest directory holding `.chaos-engine/`), so a sub-directory never
sees a live job as `absent`.

## Live versus stale

A lease is stale only when the supervisor PID is dead, or when its heartbeat
is older than `--stale` (default 4x `--heartbeat`). On Linux the PID must also
carry the run's id, so a reused PID never looks alive. Taking over a stale,
failed or finished run first kills every surviving process of that run, found
by the run id that every child inherits (this catches children of dead parents
that left the process group). It then deletes `*.part` files under the run's
`--part-dir`s. A live job's files are never touched.

## Atomic outputs

Write every output to `<file>.part`, then rename it into place, so a kill
loses only the step in flight:

- Python: `with atomic_output(path) as part: part.write_bytes(data)`, using
  `atomic_output` from [`jobs.py`](../jobs.py).
- Shell: `cmd > out.mp4.part && mv out.mp4.part out.mp4` (for ffmpeg, add
  `-f mp4` because the `.part` name hides the format).

The job command must be incremental: on restart it skips steps whose output
or checkpoint exists. Resume never redoes finished work by itself.

## Watchdog recipe

A watchdog (cron, a host routine, a scheduled agent) runs exactly one
command per job, from the directory that started it (`$PROJECT` below), and
never launches the pipeline directly:

```bash
cd "$PROJECT" && python3 .chaos-engine/tool.py job resume render
```

- Cron, every 10 minutes:
  `*/10 * * * * cd "$PROJECT" && python3 .chaos-engine/tool.py job resume render >> .chaos-engine-state/jobs/watchdog.log 2>&1`
- Agent routine: run `tool.py job status render --json`. Report only on a
  change to `done`, `failed` or `stopped`, or on a "needs a human" line from
  `resume`. Otherwise run `tool.py job resume render` and stay silent. Never
  kill by process name, and never start a second copy "to be safe".
