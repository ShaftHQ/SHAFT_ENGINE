# Delete guard (#7251)

Never delete work that exists nowhere else. CLI hosts run the check in the
PreToolUse hook (`hooks/guard.py`); hookless bots run
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

Only the owner mints a token, at an interactive terminal, by typing the path:
`python3 .chaos-engine/delete_guard.py --issue-ack <path>` (single path,
1 h). The agent then runs the delete with `CHAOS_DELETE_ACK=<token>`. Each
override is logged to `~/.local/state/chaos-engine/delete-overrides.jsonl`.
An agent never runs `--issue-ack` or writes the ack store itself.

## Doctor

- `delete-guard: pass|fail`: the hook calls this module and `tool.py`
  exposes `delete-check`. A fail fails doctor.
- `backup-remote: pass|warn`: advisory only. Lists workspaces (the project,
  `.chaos-engine-state/backup-workspaces.txt`, `CHAOS_BACKUP_WORKSPACES`)
  that have no remote, no upstream, unpushed commits or uncommitted files.
  Push them or back them up.
