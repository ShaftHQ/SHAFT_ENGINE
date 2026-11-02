# Client-name scrub (public repos)

Public remotes must not carry a client or engagement code name in branch
names, commit messages, PR bodies, or **added lines of a PR diff**. Load
markers from a **local** denylist only — never commit the real tokens.

## Configure

- Env: `CHAOS_ENGINE_CLIENT_NAME_DENYLIST=token1,token2`
- Or untracked file: `.chaos-engine-state/client-name-denylist.txt` (one token
  per line; `#` comments allowed). The state directory is gitignored.

When the denylist is empty / unset, the gate is a no-op. When it is set, the
gate **fails closed** on a match.

## Surfaces

1. Current branch name
2. Commit subjects/bodies since merge-base with the remote default branch
3. Open PR body for the current head (when `gh` can see one)
4. Added lines of `merge-base...HEAD` (unified diff `+` lines only; removals
   are ignored so scrub PRs that delete a marker are not blocked by `-` lines)

Library: [`client_name_scrub.py`](../client_name_scrub.py). Overlay pre-push
calls it on live pushes. Tests inject a **synthetic** placeholder via env; they
must not embed real client markers.

## Related

Mobile-retro harness helpers: [mobile-retro-harness.md](mobile-retro-harness.md).
