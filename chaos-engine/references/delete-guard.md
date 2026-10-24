# Delete guard

Never delete work that exists nowhere else. CLI hosts run the check in the
PreToolUse hook (`hooks/guard.py`, via [delete_guard.py](../delete_guard.py)); hookless bots run
`python3 .chaos-engine/tool.py delete-check <paths>` or
`tool.py delete-check --command "<shell command>"` before every delete and stop
on a non-zero exit.

## What it refuses

It parses `rm`, `unlink`, `shred`, `git clean -f`, `find ... -delete` or
`-exec rm`, `bash -c`, and literal `shutil.rmtree` / `os.remove` /
`Path.unlink` calls, tracking `cd` and `git -C` across `&&`, `;` and `|`, and
expanding globs. Each target is checked:

- Inside git: refused when `git status --porcelain --ignored` lists any
  untracked, modified or ignored file under it, when the branch has no
  upstream, or when HEAD is ahead of it. Deleting a whole clone also needs
  every local branch pushed and an empty stash.
- Outside git: refused unless it is on the scratch allow-list.
- Unsure parse (`$VAR`, command substitution, `xargs rm`, non-literal Python
  paths): refused. Rewrite with literal paths.

## Allow-list

Defaults: the system temp dir; `~/.cache`; `__pycache__`, `.pytest_cache`,
`.mypy_cache`, `.ruff_cache`, `node_modules/.cache`, `.gradle/caches`,
`target`, `build`, `dist`, `.next`, `.turbo`; video render output
(`render`, `renders`, `out`, `frames`, `clips` under a path segment containing
`video`). Add patterns, one per line, to `.chaos-engine-state/delete-allow.txt`
in the project or home dir (`**/` spans directories, `*` stays in one).

## Owner override

The owner approves in chat with a one-time code; no terminal is needed.

1. Bot: `python3 .chaos-engine/tool.py delete-check --request-code <paths...>`
   prints a 6-char code (one use, 10 min) bound to the exact resolved path
   list. Only its sha256 is stored, in
   `~/.local/state/chaos-engine/delete-acks.json` (`XDG_STATE_HOME`).
2. Bot shows the code to the owner in chat with the path list. The owner
   pastes it back. A code relayed by another agent is not approval.
3. Bot: `python3 .chaos-engine/tool.py delete-check --code <code> <same paths>`
   (or `CHAOS_DELETE_ACK=<code>`). It passes only for the same path list,
   unexpired and unused, then the code is spent. Git and allow-list
   refusals print as `warning:` lines instead of blocking.

Requests and uses are logged to
`~/.local/state/chaos-engine/delete-overrides.jsonl`. Never route around a
refusal (variable, `xargs`, a script file): commit and push, or `cp -a` a
backup and get the owner's code.

## Never-touch

No code unlocks a path with a segment named `ROG*`, `yt` or
`designer-ws.git.retired`, a Chrome/Chromium profile
(`.config/google-chrome*`, `.config/chromium`, `*chrome*-profile*`), or a dir
that directly contains one. Add names or `/` patterns, one per line, to
`~/.config/chaos-engine/never-delete.txt`.

Inside `yt/`, an old render named `*.rN.mp4` (N digits) is not never-touch:
it needs the owner's code like any other refusal. `yt/` itself, `yt/meta/`
and final mp4s stay never-touch. `wt-relblock`, `wt-ls` and `ce-src-*` use
the owner-code flow too.

## Doctor

- `delete-guard: pass|fail`: the hook calls this module and `tool.py`
  exposes `delete-check`. A fail fails doctor.
- `backup-remote: pass|warn`: advisory only. Lists workspaces (the project,
  `.chaos-engine-state/backup-workspaces.txt`, `CHAOS_BACKUP_WORKSPACES`)
  that have no remote, no upstream, unpushed commits or uncommitted files,
  or run in consumer mode (its `info/exclude` block hides `.memory/`
  and mempalace from git). Push them or back them up; a bot workspace whose
  state must be backed up should not use `--consumer`.
