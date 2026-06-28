# Repository Map

SHAFT_ENGINE uses Graphify for an optional local repository map. The checked-in files only teach agents how to build and query it; generated `graphify-out/` files stay local.

## Install

Use the official package name, `graphifyy`; the CLI command is still `graphify`.

```powershell
# preferred when uv is installed
uv tool install graphifyy==0.8.50

# Windows fallback
py -3 -m pip install --user graphifyy==0.8.50
```

If `pip` installs `graphify.exe` outside PATH, open a new terminal after updating the user PATH, or run commands as `py -3 -m graphify ...`.

## Build

Run from the repository root:

```powershell
graphify .
```

On PowerShell, use `graphify .`, not `/graphify .`.

## Query

Use Graphify to choose files before broad manual search:

```powershell
graphify query "Where is the core SHAFT WebDriver facade and related test coverage?"
graphify path "SHAFT.GUI.WebDriver" "DriverFactory"
graphify export callflow-html
```

After Graphify identifies likely files, read exact files with `rg` and small excerpts before editing. Keep using Memory for durable decisions and gotchas; do not rely on Graphify output as the source of truth.

The default `.graphifyignore` keeps semantic document/media formats out of the graph so a code/config map can be built without LLM API keys. Remove those ignore entries only when semantic document extraction is explicitly needed and a suitable key is available.

## Git Hygiene

`graphify-out/` is intentionally ignored. Do not commit generated graph reports, HTML, JSON, caches, or binary exports unless a maintainer explicitly asks for a reviewed snapshot.
