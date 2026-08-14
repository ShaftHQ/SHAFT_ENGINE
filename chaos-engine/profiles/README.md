# ChaosEngine project profiles

Host adapters select exactly one project profile after loading the portable
core. Profiles contain repository-specific routes, branch names, permissions,
and companion-project facts; the core contains none of those values.

- The public `portable` distribution in the [catalog](../distributions.json)
  installs the neutral [profile](portable/entrypoint.md) and
  [configuration](portable/profile.json).
- A source repository may offer an explicitly selected repository profile for
  its own contributors; that profile is never part of the public default.
