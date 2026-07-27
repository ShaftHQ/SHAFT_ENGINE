# IntelliJ SHAFT Capture Demo -- Runbook

Tracking issue: [#4299](https://github.com/ShaftHQ/SHAFT_ENGINE/issues/4299).

> **Status: scaffolding only.** This runbook, the local-scope Windows-MCP
> installer, and the reset-scripted demo project are landed and verified.
> The actual recorded take -- plugin install, Assistant recording,
> DuckDuckGo search + assertion, codegen, video post-processing, Drive
> upload -- has **not** been performed yet. That step needs a fresh Claude
> Code session started *after* `install-windows-mcp.ps1` has run, since MCP
> servers are only discovered at session start (see Prerequisites). Update
> this status line once a real take has been recorded end to end.

## Goal

Produce a real, AI-driven (no human touching the mouse/keyboard) screen
recording of: installing the SHAFT IntelliJ plugin's optional upgrade,
asking the SHAFT Assistant to record a browser flow (DuckDuckGo search for
`shaft_engine`, assert the first result contains `shafthq`), stopping the
recording, running codegen, and reviewing the generated test + page object.
The recording is then post-processed (duplicate near-static frames removed)
and uploaded to Google Drive.

## Prerequisites

| Requirement | Verified as | Notes |
|---|---|---|
| `ffmpeg` | `ffmpeg version 8.1.1-essentials_build` | Already installed on this machine; `gdigrab` (Windows screen capture) ships built in, no extra install. |
| `uv`/`uvx` | `uv 0.11.29` | Used to run Windows-MCP without a manual pip install (`uvx windows-mcp ...`). |
| Windows-MCP | via `install-windows-mcp.ps1` (below) | github.com/CursorTouch/Windows-MCP. **Local scope only** -- never add it to this repo's checked-in `.mcp.json`; its own README says it "operates with full system access and can perform irreversible operations." |
| JDK for `shaft-intellij`'s Gradle daemon | `~/.jdks/ms-21.0.11` | **Must** be JDK 21 or earlier. JDK 25 crashes the Gradle Daemon on Windows (`EXCEPTION_ACCESS_VIOLATION` in G1 GC) -- `shaft-intellij/settings.gradle.kts` fails fast with an actionable message if it detects a newer JDK (issue #3784). See `.claude/skills/shaft-mastery/references/intellij-plugin.md`. |

## 1. Install Windows-MCP (local scope, scoped-down tools)

```powershell
powershell -NoProfile -ExecutionPolicy Bypass -File tools\intellij-plugin-recording\install-windows-mcp.ps1
```

This runs:

```
claude mcp add windows-mcp --scope local -- uvx windows-mcp serve --tools "Click,Type,Scroll,Move,Shortcut,Wait,WaitFor,Screenshot,Snapshot,App"
```

- **`--scope local`** writes to the current user's own `~/.claude.json`
  project entry, never to this repo's `.mcp.json`. Verify with
  `git status --short .mcp.json` (must be empty) after running.
- **Tool allow-list**: `Click, Type, Scroll, Move, Shortcut, Wait, WaitFor,
  Screenshot, Snapshot, App` -- every UI-Interaction / Desktop-Capture /
  Application-Management tool the server offers, nothing more. `Snapshot`
  (the Windows UI Automation accessibility-tree read) is included on
  purpose: it's why Windows-MCP was chosen over vision/pixel-based clicking
  for a Swing-based IDE. **Explicitly excluded**: `PowerShell` (raw shell
  execution), `Registry`, `Process` (can kill arbitrary processes),
  `FileSystem` (arbitrary read/write/delete), `Scrape`, `MultiSelect`/
  `MultiEdit` (bulk-edit blast radius), `Clipboard`, `Notification`.
- The script is idempotent: if `windows-mcp` is already registered at local
  scope it reports that and exits 0 rather than re-registering.
- **Restart Claude Code after running this** -- MCP servers are only
  discovered when a session starts; a session that was already running
  when you registered the server will not see its tools.

## 2. Launch the sandboxed IDE

```powershell
$env:JAVA_HOME = "$env:USERPROFILE\.jdks\ms-21.0.11"
cd shaft-intellij
.\gradlew.bat runIde
```

This builds the local plugin and launches a **sandboxed** IntelliJ instance
with it pre-loaded -- not your real IDE install. First run downloads the
IntelliJ Platform distribution into the Gradle cache and can take several
minutes; subsequent runs are much faster.

Verified 2026-07-28: build went `initializeIntellijPlatformPlugin` ->
`compileJava` -> `prepareSandbox` -> `runIde`, and produced a real, visible,
on-screen window (`Get-Process java | Select MainWindowTitle` showed
`"Welcome to IntelliJ IDEA (Administrator)"` with a non-zero window handle).
Stop it with `taskkill /PID <gradlew PID> /T /F` -- `runIde` blocks the
Gradle task for as long as the IDE stays open, so it will not exit on its
own when you're done.

## 3. Reset the demo project before every take

```powershell
powershell -NoProfile -ExecutionPolicy Bypass -File tools\intellij-plugin-recording\reset-demo-project.ps1
```

Deletes and re-copies `demo-project-template/` into
`%USERPROFILE%\shaft-demo-workspace\selenium-testng-demo` (override with
`-TargetPath`). Deliberately outside this repo, so opening it in IntelliJ
never touches SHAFT_ENGINE's own VCS state. Run this before the *first*
take and before every re-recording attempt -- it wipes `.idea/`, `target/`,
and any codegen output a previous attempt left behind.

The template is a minimal Maven project: `selenium-java` + `testng` on the
classpath, one trivial passing `SampleTest`, and deliberately **no** SHAFT
dependency and **no** real Selenium test yet -- the demo's whole point is
that the plugin's Assistant adds the DuckDuckGo test + page object live,
during the recording.

Verified 2026-07-28: ran the script, then `mvn -Dtest=SampleTest
-Dallure.automaticallyOpen=false -DheadlessExecution=true test` (surefire:
`Tests run: 1, Failures: 0, Errors: 0`); then deliberately dirtied the
working copy (stray `.idea/`, `target/`, a fake leftover
`DuckDuckGoTest.java`) and re-ran the script -- confirmed it restores
exactly the two template files.

## 4. The recording procedure (not yet performed -- see status banner)

1. Start the screen recording (see step 5) *before* touching the IDE.
2. In the sandboxed IDE, open the reset demo project
   (`%USERPROFILE%\shaft-demo-workspace\selenium-testng-demo`).
3. Install the SHAFT plugin (`Settings > Plugins`, or via the SHAFT tool
   window if pre-bundled by `runIde`) and complete first-run setup --
   including the optional upgrade-to-SHAFT step. Pre-handle the first-run
   popups called out in Known Pitfalls below so they don't clutter the
   recording.
4. Ask the SHAFT Assistant to start a recording (`start recording`).
5. In the managed browser: open DuckDuckGo, search `shaft_engine`, assert
   the first result contains the text `shafthq`.
6. Tell the Assistant to stop the recording (`stop`).
7. Ask it to run codegen against the capture and approve generation
   (`approve` / `okay` / `generate`).
8. Let playback and codegen finish; show the generated test class and page
   object class.
9. Stop the screen recording.

The existing `tools/intellij-plugin-recording/README.md` documents the
non-video-capture version of steps 4-8 (`Capture Scenario` section) in more
detail and is the pattern this procedure follows; this runbook adds the
video capture, the plugin *install* step (not just an already-built plugin),
and the DuckDuckGo/`shafthq` assertion specifics.

## 5. Screen recording and post-processing (commands verified, not yet run for a real take)

Record with `gdigrab` (Windows' built-in screen-capture demuxer, no extra
install):

```powershell
ffmpeg -f gdigrab -framerate 15 -i desktop raw-capture.mp4
```

(Ctrl+C to stop.) Back up the raw file before post-processing -- do not
process in place.

Remove near-duplicate consecutive frames (e.g. the mouse moving with
nothing else changing) with `mpdecimate`, then renumber timestamps with
`setpts` so the result plays at a consistent rate:

```powershell
ffmpeg -i raw-capture.mp4 -vf "mpdecimate,setpts=N/FRAME_RATE/TB" -an processed-capture.mp4
```

Sanity-checked on a 2-second test clip of a mostly-static desktop
(2026-07-28): input was 30 frames / 2.00s; `mpdecimate` output was 13 frames
/ 0.87s -- confirms the filter drops duplicate frames as intended. A real
multi-minute take with genuine idle/reading stretches should compress more
in both frame count and file size than this trivial sanity check; verify
size and playability again on the real recording before uploading (see
Deliverables checkpoint 6 in issue #4299).

## 6. Upload

Upload the processed `.mp4` to Google Drive root via
`mcp__claude_ai_Google_Drive__create_file` with `base64Content` +
`contentMimeType: "video/mp4"` + `disableConversionToGoogleType: true` (so
it isn't converted to a Google-native type). **Do not claim the upload is
shared successfully without independently re-fetching the file's metadata**
(`get_file_metadata`) -- issue #3285's own attempt found automated sharing
unreliable even though the upload itself succeeded.

## Known pitfalls (from issue #3285 -- read before re-attempting)

Issue #3285 was an earlier, near-identical manual attempt at this same
DuckDuckGo/POM demo (no video capture). It is **closed**, reportedly fixed
via [#3286](https://github.com/ShaftHQ/SHAFT_ENGINE/pull/3286) with a paired
docs sync in `shafthq.github.io#708`. That fix has **not been
re-independently verified** as part of this scaffolding-only PR (no live
Assistant session was driven here) -- treat the following as "watch for
this recurring, confirm it's actually gone" rather than "guaranteed fixed":

- **Source-edit approval gate**: the built-in optimization prompt could not
  proceed in Agent mode unless `Allow source edits` was approved, with no
  read-only audit path when source edits were disabled.
- **DuckDuckGo POM request mis-routing**: asking Agent mode for DuckDuckGo
  Page Object Model code (with source edits enabled) routed the prompt
  through `sequence` and started `driver_initialize` -- a live browser
  execution -- instead of returning or adding focused Java POM code.
- **Broad unrelated repo changes**: that same run generated broad,
  unrelated repo changes and artifacts (provider/config/test edits plus
  generated report/resource folders) before it was cancelled and manually
  reverted. **If this recurs during the real take: stop immediately, do not
  let anything unintended get committed**, and re-open a fresh issue
  referencing #3285 and #4299.
- **First-run IDE popup clutter**: JetBrains project trust defaults the
  Windows Defender exclusion checkbox on, and terminal/Cucumber suggestion
  popups can overlap the setup panel. Dismiss/pre-handle before starting the
  actual recording take.
- **Windows Firewall prompt**: an earlier pass saw a Firewall prompt for an
  OpenJDK process during driver initialization. Pre-approve or otherwise
  handle this before recording so it doesn't interrupt or appear in the
  footage.
- **Google Drive auto-share unreliability**: #3285's upload succeeded but
  automated sharing failed for both workspace-domain sharing and the local
  Git email -- the resulting link may need the owner to adjust access
  manually. Don't claim sharing succeeded without checking
  `get_file_metadata`.
- **Non-interactive setup fallback**: the plugin's own copied setup command
  did not offer a non-interactive skills-install path in the visible UI
  flow; `scripts/mcp/install-shaft-mcp.ps1 -Client codex
  --install-shaft-skills --json` is the deterministic fallback used to keep
  setup scriptable, but the real take should still show the UI-driven
  install (that's the point of an onboarding demo) and use the script only
  as a sanity-check, not a silent substitute for the recorded flow.

## Files in this directory

| File | Purpose |
|---|---|
| `RUNBOOK.md` | This document. |
| `README.md` | Pre-existing non-video-capture Assistant e2e capture workflow; this runbook extends it with recording/upload. |
| `install-windows-mcp.ps1` | Idempotent local-scope Windows-MCP registration with the scoped tool allow-list. |
| `record-onboarding.ps1` | Pre-existing plugin build/verify script (`gradle -p shaft-intellij check buildPlugin verifyPlugin`). |
| `demo-project-template/` | Minimal Maven Selenium+TestNG scaffold opened during the recording. |
| `reset-demo-project.ps1` | Restores a pristine copy of the template before every take. |
