# ChaosEngine project profiles

Host adapters select exactly one project profile after loading the portable
core. Profiles contain repository-specific routes, branch names, permissions,
and companion-project facts; the core contains none of those values.

- The public `portable` distribution installs the neutral profile.
- A source repository may offer an explicitly selected repository profile for
  its own contributors; that profile is never part of the public default.
