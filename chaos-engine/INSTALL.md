# Install or upgrade ChaosEngine

Give the following single command to Codex, Claude, Grok, Gemini, or another
coding agent while its working directory is the project you want to manage:

> Install or upgrade ChaosEngine in this project from the latest commit of the
> configured upstream. Fetch and inspect that upstream's
> `chaos-engine/bootstrap.py`, run it with Python 3, `--project .`, and the
> explicit `--repository owner/repository`; then run Python 3 with
> `.chaos-engine/install.py status --project .`. Do not stop until the command reports the resolved
> 40-character commit and healthy core, host adapters, and local tools. Treat
> the installed ChaosEngine skill as the canonical harness and route any
> existing agent guidance through it without deleting unrelated user content.

The command is agent-oriented so the agent selects the available Python 3
executable on Windows, macOS, or Linux and can report a blocked network or
authentication boundary. For a direct terminal flow, save the bootstrap and
run:

```text
python bootstrap.py --project . --repository owner/repository
```

Add `--branch branch` to override the repository's configured default branch. The
bootstrap resolves that mutable branch through the GitHub API, downloads the
exact commit archive, rejects unsafe archive entries, and records repository,
branch, and commit provenance in `.chaos-engine/manifest.json`. Re-running the
same command upgrades to the latest resolved commit; an offline or invalid
download leaves the last verified installation unchanged.

The consumer folder may be a GitHub checkout, another Git checkout, or a
non-Git directory. ChaosEngine installs project-locally and does not infer its
upstream from the consumer repository.
