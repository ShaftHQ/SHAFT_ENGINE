# L77 NO-TOKEN-IN-REMOTE

## Rule

Doctor and pre-push **FAIL** when any git remote URL embeds credentials:

- token prefixes: `ghp_`, `github_pat_`, related `gh*_` / `glpat-` shapes
- `x-access-token:…@` userinfo
- `scheme://user:password@host`

Never print the URL or token value — report remote **names** and reason codes
only. Fix: rewrite remotes to credential-free HTTPS or SSH; authenticate via
`GH_TOKEN` / `gh auth` / OS credential helper
(repo `scripts/ci/github-auth-env.sh`; keep remotes credential-free).

Module: [`remote_credential_guard.py`](../../../rules/remote_credential_guard.py).

## Verify

- [ ] Fake `ghp_` / `github_pat_` / `user:pass@` fixtures → FAIL (values not in output)
- [ ] Clean `https://github.com/org/repo.git` / `git@github.com:org/repo.git` → PASS
- [ ] Doctor row `remote-credential: fail` when embedded; pre-push lists `L77-no-token-in-remote`

## Why

Embedded PATs in `git remote -v` leak via logs, screenshots, and agent tooling.
Auth belongs in the environment, not the remote URL.
