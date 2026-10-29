# Delete guard

Never delete work that exists nowhere else. CLI hosts run the check in the
PreToolUse hook (`hooks/guard.py`, via [delete_guard.py](../delete_guard.py)); hookless bots run
`python3 .chaos-engine/tool.py delete-check <paths>` or
`tool.py delete-check --command "<shell command>"` before every delete and stop
on a non-zero exit.

## What it refuses

It parses `rm`, `unlink`, `shred`, `mv` (each source path), `rsync --delete*` /
`--remove-source-files`, `git clean -f`, `find ... -delete` or `-exec rm`,
`bash -c`, literal `shutil.rmtree` / `os.remove` / `os.unlink` / `os.rmdir`,
and `Path("...").unlink|rmdir|rmtree` constructor forms (including `python -c`
and heredoc bodies), tracking `cd` and `git -C` across `&&`, `;` and `|`, and
expanding globs. Each target is checked:

- Inside git: refused when `git status --porcelain --ignored` lists any
  untracked, modified or ignored file under it, when the branch has no
  upstream, or when HEAD is ahead of it. Deleting a whole clone also needs
  every local branch pushed and an empty stash.
- Outside git: refused unless it is on the scratch allow-list.
- Unsure parse (`$VAR`, command substitution, `xargs rm`, non-literal Python
  paths): refused. Rewrite with literal paths.
- Symlink operands: plain `rm`/`unlink`/`mv` of the link inode are link-only.
  Tree ops that follow links (`find -delete`/`-exec rm`, `rsync --delete*`/
  `--remove-source-files`, `rm -r` with a trailing slash, `shutil.rmtree`/
  `Path.rmtree`, `mv src/`) resolve the realpath and apply the checks to the
  target; resolution failure refuses closed.

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

Inside `yt/`, two exact-file exceptions are not never-touch and need the
owner's code like any other refusal:

- an old render named `*.rN.mp4` (N digits) outside `yt/meta/`
- a file listed in the verified-upload allowlist that passes live checks.
  Allowlist path: `CHAOS_YT_VERIFIED_DELETE` (must be owner-only mode `0600`
  and owned by the current user) or else
  `~/.config/chaos-engine/yt-verified-delete.txt` under the same owner-only
  rule. The source used is logged. Each line is
  `absolute/path.mp4 <11-char-video-id> <sha256>`. At `request_code` and
  `check` time the guard requires: (1) exact path with no `..` and no
  symlink components; (2) `realpath(path)` equals the normalized listed
  path, lies under the `yt` root, and is not under `yt/meta`; (3) file
  sha256 matches the entry; (4) YouTube oEmbed
  (`/oembed?url=...watch?v=<id>&format=json`) returns HTTP 200 **and** a
  JSON body with a non-empty `author_name` (status alone is not enough).
  Optional `CHAOS_YT_EXPECTED_AUTHORS` (comma-separated oEmbed
  `author_name` values) binds the unlock to those channels; when unset,
  any non-empty author counts. Results are cached under state: positives
  and definitive HTTP 4xx (including **403 = private / unlisted-private**,
  not a box IP block — public ids still return 200 from the same host)
  use a long TTL; network errors and HTTP 5xx use a short negative TTL so
  a brief outage does not lock public files for a day. A warm **positive**
  cache hit still re-checks the stored non-empty `author` against the
  current `CHAOS_YT_EXPECTED_AUTHORS` bind (bare `ok: true` is never enough);
  malformed cache entries (`expires` / `ok` / types) are treated as misses
  and never raise into `never_touch`. Private videos stay locked without an
  owner YouTube Data API credential (none required for public proof).
  noembed and thumbnail checks are not used: noembed proxies oEmbed, and
  private thumbs look like missing. Globs, directories, escapes, and
  malformed proofs are ignored. The allowlist is local/env-only; never
  commit real paths or titles. Env override cannot widen unlocks without
  those checks.

`yt/` itself, `yt/meta/**`, and any unlisted or unverified yt file stay
never-touch with no code override. `wt-relblock`, `wt-ls` and `ce-src-*` use
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
- `backups-watch: pass|warn`: advisory only. Documents the optional
  backups-directory watcher ([backups_watch.py](../backups_watch.py)).
  Doctor never starts it. Start by hand:
  `python3 .chaos-engine/tool.py backups-watch --path <backups-root>`
  (or set `CHAOS_BACKUPS_WATCH_PATH`). Uses `inotifywait` when installed,
  otherwise a poll loop. Logs JSON lines to
  `~/.local/state/chaos-engine/backups-watch.log` with timestamp, event,
  path, watcher pid/cmdline, and agent env when present. Tests may call
  `tool.py backups-watch --self-test` or `--once --force-poll`; they must
  not leave a daemon running.

## Lessons

- **Backup dir = push to remote before parking.** A folder under a local
  backups parking directory that has never been pushed (or copied to a
  verified remote) is not a backup — it is a single copy. Push or sync it
  before ending the session. Unpushed backup folders surface as a
  `backup-remote: warn` (or an explicit warning in the parking checklist);
  do not treat "it is under backups/" as durable. Incident 2026-10-29: an
  unpushed `carl-cleanup-*` parking folder vanished with no delete-guard
  log because a raw `mv`/`rm` path had bypassed the old shell-word trigger.
