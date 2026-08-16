<p align="center">
  <img alt="ChaosEngine symbol" src="assets/brand/symbol-light.svg" width="180">
</p>

# Install or upgrade ChaosEngine

ChaosEngine installs project-locally and is provider-, model-, language-, and project-agnostic.
Run one command from the root of the project that should use it.

## Windows PowerShell

```powershell
py -3 -c "import urllib.request;exec(urllib.request.urlopen('https://raw.githubusercontent.com/ShaftHQ/SHAFT_ENGINE/main/chaos-engine/bootstrap.py',timeout=30).read())"
```

## macOS or Linux

```sh
python3 -c "import urllib.request;exec(urllib.request.urlopen('https://raw.githubusercontent.com/ShaftHQ/SHAFT_ENGINE/main/chaos-engine/bootstrap.py',timeout=30).read())"
```

Run the same command to upgrade. Python 3 and network access to GitHub are the only bootstrap
prerequisites. Inspect the linked bootstrap first when local policy requires source review.

The bootstrap resolves the upstream default branch to an immutable commit, downloads only the
bounded `chaos-engine/` tree, validates its shape and sizes, and installs transactionally. Failure
leaves the previous verified installation unchanged. The portable distribution records hashed
provenance without copying the source repository identity into adopter guidance.

## Installed surface

One command installs and activates the complete portable harness:

- canonical outcome-first skill and focused references;
- neutral project profile plus thin host instruction adapters;
- role and plugin manifests for detected clients;
- disabled-by-default diagnostic hook files and empty lifecycle hook configuration;
- Memory, MemPalace, Graphify, and MCP configuration;
- pinned project-local tool runtimes and dependency receipts;
- status, doctor, rollback, uninstall, and cache operations;
- ignore and line-ending rules that preserve unrelated project configuration.

The core uses only the capability names **most intelligent**, **default**, and **mechanical**.
Provider-specific files are adapters generated at the edge; they do not own or duplicate policy.
Local diagnostics are fail-open and never deny tools, mutation, delivery, or completion.

Restart clients that were open during installation so they load the project-local adapters.

## Operate

```text
python .chaos-engine/install.py status
python .chaos-engine/install.py doctor
python .chaos-engine/install.py rollback
python .chaos-engine/install.py uninstall
```

Use `py -3` instead of `python` on Windows when needed. `status` is passive and network-free.
`doctor` runs bounded active probes. Ordinary tasks treat Memory, MemPalace, and Graphify as
advisory even when explicit maintenance reports recovery is needed.

Generated runtimes, indexes, caches, receipts, reports, and `graphify-out/` remain untracked.
Canonical configuration and adapters remain trackable.

## Advanced source selection

Forks and repository contributors may download `bootstrap.py` and pass explicit overrides:

```text
python bootstrap.py --project . --repository owner/repository --branch branch --distribution portable
```

The target may be any Git checkout or non-Git directory. The installer never infers the harness
source from the target project. Non-portable contributor profiles require explicit distribution
selection and are never installed by the public command.

Optional user-managed components, such as Maven Tools MCP, are discovered when their verified cache
already exists; their absence does not make the portable installation unhealthy.

