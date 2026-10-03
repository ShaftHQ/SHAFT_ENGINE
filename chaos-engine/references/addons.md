---
name: addons
description: Use when a task fits an optional add-on (design, video, or a project pack): list, install, remove, or load one.
---

# Add-ons

The core installs alone. Add-ons are optional bundles of skills and
references that are never installed by default and never auto-selected from
project files. A user opts in with a flag; the choice persists across
upgrades until removed.

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
refused and names the dependent. A later upgrade without flags keeps the
recorded set.

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
