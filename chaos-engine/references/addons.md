---
name: addons
description: Use when a task fits an optional add-on (design, video, or a project pack): list, install, remove, or load one.
---

# Add-ons

The core installs alone. Design and other optional add-ons are never
selected from project files. A user opts in with a flag; that choice persists
across upgrades until removed.

The one-liner reidentifies pack identity on every run, using the same
`*/ce-pack` entrypoint signal as host skills. The pack home gets that pack's
distribution add-on (and the user skills it requires). A project that only
matches the pack `installWhen` gets the user skills and stays on the portable
distribution. Any other project stays portable, and a previously recorded
distribution add-on is dropped. `--without-<name>` on that run still wins.

## List

```
python3 .chaos-engine/install.py addons --project .
```

Prints each add-on as `name  state  flag  use-when`. Add `--json` for tools.

## Install or remove

| Installer | Add | Remove |
| --- | --- | --- |
| `install.sh` / `bootstrap.py` / `install.py install` | `--with-<name>` | `--without-<name>` |
| `install.ps1` (shipped add-ons; others via the environment) | `-With<PascalName>` | `-Without<PascalName>` |
| Environment (any installer) | `CHAOS_ENGINE_ADDONS=<name>,<name>` | |

Unknown names fail closed and list the valid ones. Requirements are added
automatically; removing an add-on that another selected add-on requires is
refused and names the dependent. A later upgrade without flags keeps recorded
optional add-ons. Pack identity is chosen again on that run. When the source
lacks a recorded external add-on (the official
one-liner downloads only the core tree), the installed copy is kept and the
installer prints `addon kept: <name> (project-local; set CHAOS_ENGINE_ADDON_PATHS or reinstall with its source)`. That is expected for project-owned add-ons (for example `automatest-coach`) when this install run did not pass their source path; doctor also surfaces them as `stale-source`. Extra add-on
directories can be named in `CHAOS_ENGINE_ADDON_PATHS`.

## Private add-ons

A private add-on or project pack never carries the owner's or a client's
name, prices, or trainee data in shipped files. Use role placeholders (for
example "the owner") and keep a validator in the pack that fails on them.

## Project-pack exception

Optional add-ons stay opt-in. Pack identity does not. A checkout with exactly
one `*/ce-pack` entrypoint is that pack's home and is installed as the pack
distribution. A different project whose files match `installWhen` receives
only the user skills. When the selected pack declares `postInstall` (a repository script and flags), the
bootstrap downloads that script from the same commit and runs it in the
project once the core installs. Some packs use this to run their own tool installer. Pass `--skip-pack-post-install` to skip it. If it fails, the
core install is kept and the exit code is non-zero.

## Load

Installed add-ons live at `.chaos-engine/addons/<name>/`. Open the add-on's
router (`SKILL.md` or the file named in its `addon.json`) and load one card
at a time. An add-on that selects a project distribution (a project pack)
changes the installed profile instead of adding files there.

## Fallbacks

- No design add-on: UI work uses [ui-delivery](ui-delivery.md); design
  documents use [design-loop](design-loop.md).
- An add-on route named in a task but not installed: say so and give the
  install flag; do not improvise its rules.

The ordered English films after the accepted install film are the
[showcase film slate](details/showcase-film-slate.md). That page does not shoot or
regenerate the install film.
