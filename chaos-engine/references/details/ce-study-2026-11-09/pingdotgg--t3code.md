# pingdotgg/t3code

- URL: https://github.com/pingdotgg/t3code
- Lists: One note for both lists. Charlie Hills row 23 (49K stars on the 6 Nov 2026 snapshot) and weekly top 10 row 01 for 2-8 Nov 2026 (+15.4k). Not a second repo.
- GitHub description: GitHub returned an empty description. The README lead calls T3 Code an agent-harness control surface for the agents on your machine, with a mobile app.
- README: README headings seen: what it is selling, installation, command line, desktop app, and Windows and macOS package-manager installs.

## Reusable patterns

- The control surface is separate from the agent. Phone and desktop drive a local agent; they are not a second agent.
- Install paths are per operating system and stay outside the agent prompt.

## Adopt

- ChaosEngine local-agency already delegates to a local agent. A mobile or desktop control surface would be a new product. It is not one of the four follow-ups named here, so no issue is opened for it and no skill is added.

## Skip

- Do not vendor the app, the mobile client, or the installers.
- Do not add a remote control path. Local tools only unless the user approves a named cloud agent or an extra machine.

## License check

- Checked 2026-11-10. GraphQL licenseInfo.spdxId is MIT. gh api repos/pingdotgg/t3code/license returned HTTP 200 and path LICENSE.
- No skill edit from this note.

Adopt the idea. Do not copy third-party code. Do not vendor the repository.
