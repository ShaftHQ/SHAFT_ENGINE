# ChaosEngine profiles and packs

Host adapters load the portable core, then at most one project pack. The core
ships only the neutral `portable` profile; it contains no repository-specific
routes, branch names, permissions, or companion-project facts.

- The public `portable` distribution in the catalog (repo-only `chaos-engine/distributions.json`)
  installs the neutral [profile](portable/entrypoint.md) and
  [configuration](portable/profile.json).
- A source repository may ship a project pack beside the core at
  `<dir>/ce-pack/` (`profile.json` + `entrypoint.md`). Its `profile.json`
  declares its own `distribution` and an `installWhen` predicate; the installer
  selects that distribution only when the target project matches, and installs
  the pack under `.chaos-engine/packs/<name>/`. Otherwise the portable
  distribution is used and no project pack is copied.
- Language packs live in the core under `packs/` (for example the
  [java pack](../packs/java/pack.md)) and stay inert unless the project needs them.
- Hard cut: the old `.chaos-engine/profiles/<name>/` layout is no longer read.
  Install and doctor print the migration notice until a reinstall replaces it.
