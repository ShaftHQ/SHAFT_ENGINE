# IntelliJ SHAFT capture demo runbook

Tracking issue: [#4299](https://github.com/ShaftHQ/SHAFT_ENGINE/issues/4299).

## Withdrawn reference take

Executed on Windows on 2026-08-11 with sandboxed IntelliJ 2024.3 and
SHAFT 10.3.20260809.

- Withdrawn video: [SHAFT-IntelliJ-Assistant-demo-issue-4299-final.mp4](https://drive.google.com/file/d/1Tyuo6HJ9MNRd0AKaFrESIP1JpU0mlJFM/view)
- Processed media: 2560 x 1440, 15 fps, 152.33 seconds, 11,553,949 bytes.
- Preserved concatenated raw media: 1,022 seconds, 71,296,536 bytes. The
  withdrawn take used four bounded segments, assembled as documented below.
- Rejection reason: the Assistant visibly reports that `capture_generate_replay`
  could not finish because the promoted generated source is missing. The later
  appended manual replay is a different execution and does not repair that failed
  Assistant flow. This file is retained only as a defect artifact and is not
  acceptance evidence for #4299.

The Drive file ID and preview were verified after upload. General access is
**Anyone with the link -> Viewer**. An independent permission read returned
`type=anyone`, `role=reader`, and `allowFileDiscovery=false`; an unauthenticated
request returned HTTP 200 with the expected filename. Those checks prove access
to the withdrawn artifact, not product acceptance. The connector's earlier
organization-share attempt returned `permission.domain: invalid or not
applicable`, so the owner completed the general-access change in Drive's UI.
Upload success alone does not prove public access; verify permission metadata
and signed-out access separately for the replacement take.

## Safety boundary

Use a UI connector limited to visible IDE/browser operations. The reference
take used the bundled Windows Computer Use connector as the equivalent
approved by #4299.

- Allowed: activate applications, inspect visible state, click, type, scroll,
  press shortcuts, and take screenshots.
- Do not automate terminals, authentication dialogs, password managers,
  security/privacy prompts, registry changes, process termination, or
  arbitrary filesystem operations.
- The SHAFT Assistant used its configured Gemini provider. Do not automate a
  coding-agent UI through Computer Use.
- `install-windows-mcp.ps1` remains an optional user-local alternative with
  shell, registry, process, filesystem, scraping, clipboard, and bulk-edit
  tools excluded.

## Prerequisites

- JDK 21 for the `shaft-intellij` Gradle daemon.
- Maven, Chrome, `ffmpeg`, and `ffprobe` with Windows `gdigrab`.
- A configured SHAFT Assistant provider key. Enter it only in the masked IDE
  field and clear the clipboard afterward.

## 1. Reset the external demo

```powershell
powershell -NoProfile -ExecutionPolicy Bypass -File tools\intellij-plugin-recording\reset-demo-project.ps1
```

The default target is
`%USERPROFILE%\shaft-demo-workspace\selenium-testng-demo`. It is outside the
repository and must be reset before every take.

## 2. Build and launch the sandbox

Select any installed JDK 21 as `JAVA_HOME`, then use the repository wrapper:

```powershell
$env:JAVA_HOME = '<absolute path to a JDK 21 installation>'
shaft-intellij\gradlew.bat -p shaft-intellij check buildPlugin verifyPlugin
shaft-intellij\gradlew.bat -p shaft-intellij runIde
```

Open the reset demo project. On the JetBrains trust dialog, clear the Windows
Defender exclusion checkbox. Dismiss nonessential IDE suggestions before
recording.

## 3. Complete SHAFT setup

In the SHAFT tool window:

1. Configure the Assistant provider and test the connection.
2. Install/verify `shaft-mcp`, `shaft-cli`, and SHAFT skills.
3. Use the deterministic repository installer shown by the setup panel if
   the UI installer is unavailable.
4. Approve the optional SHAFT project upgrade.

The reference take also verified the non-AI upgrader:

```powershell
py -3 shaft-upgrader\upgrade_to_modular_shaft.py --project "$env:USERPROFILE\shaft-demo-workspace\selenium-testng-demo" --yes --no-ai
```

Both baseline and upgraded Maven compilation must pass before recording.

## 4. Record the real browser flow

```powershell
$videoDir = "$env:USERPROFILE\shaft-demo-workspace\issue-4299-video"
$main = "$videoDir\main.mp4"
powershell -NoProfile -ExecutionPolicy Bypass -File tools\intellij-plugin-recording\capture-desktop.ps1 -OutputPath $main -FrameRate 15 -MaximumDurationSeconds 900
```

The script prints a stop-file path. Creating it sends `q` to ffmpeg so the
MP4 is finalized cleanly.

In the SHAFT Assistant:

1. Ask `start recording https://duckduckgo.com`.
2. In the managed headed browser, enter `shaft_engine` and submit.
3. Verify the first visible result is ShaftHQ/SHAFT_ENGINE.
4. Add **Text contains** on the first result title with value `ShaftHQ`.
   Generated `contains` assertions are case-sensitive.
5. Ask `stop` and preserve the returned capture JSON under `recordings/`.

The reference capture completed six events: tab open, navigation, click,
type, submission/navigation, and `TEXT_CONTAINS` verification.

## 5. Generate, create the Page Object, and replay

```powershell
shaft-cli codegen --session recordings\<capture>.json --output-dir . --package tests.generated --class-name RecordedFlowTest --overwrite
```

Expected generated artifacts:

- `src/test/java/tests/generated/RecordedFlowTest.java`
- `src/test/resources/testDataFiles/recorded-flow-test.json`

Ask the Assistant to refactor the browser operations into
`src/test/java/pages/DuckDuckGoPage.java`, leaving the generated test as its
consumer. Show all three files in the recording.

Remove only the demo project's prior test evidence, then force a headed,
focused replay:

```powershell
Remove-Item -LiteralPath allure-results,allure-report,allure-single -Recurse -Force -ErrorAction SilentlyContinue
mvn clean test '-Dtest=tests.generated.RecordedFlowTest' '-DheadlessExecution=false' '-Dallure.automaticallyOpen=false'
allure generate allure-results --single-file --clean --output allure-single
```

Inspect the Surefire XML rather than trusting Maven's banner. Acceptance is
exactly 1 newly written test, 0 failures, 0 errors. Open the new
`allure-single\index.html` in the headed browser for the closing proof. The
reference take used externalized values `shaft_engine` and `ShaftHQ` and a
first-result locator targeting `article#r1-0 [data-testid=result-title-a]`.

Friction found during the take is tracked in
[#4715](https://github.com/ShaftHQ/SHAFT_ENGINE/issues/4715),
[#4716](https://github.com/ShaftHQ/SHAFT_ENGINE/issues/4716), and
[#4717](https://github.com/ShaftHQ/SHAFT_ENGINE/issues/4717).

## 6. Stop, assemble, and post-process

```powershell
New-Item -ItemType File -Path "$main.stop" -Force
```

If the main capture contains every accepted step, process it directly:

```powershell
powershell -NoProfile -ExecutionPolicy Bypass -File tools\intellij-plugin-recording\postprocess-desktop-capture.ps1 -InputPath $main -OutputPath "$videoDir\final.mp4"
```

The reference take instead kept the accepted long setup and added three short
segments so a failed closing proof did not invalidate it. Keep the desktop
resolution unchanged and capture each supplement through the same script at
15 fps; each command prints the marker to create from the orchestration shell
after the named proof is visible:

```powershell
$success = "$videoDir\success-15fps.mp4"
powershell -NoProfile -ExecutionPolicy Bypass -File tools\intellij-plugin-recording\capture-desktop.ps1 -OutputPath $success -FrameRate 15 -MaximumDurationSeconds 120

$allureProof = "$videoDir\allure-proof.mp4"
powershell -NoProfile -ExecutionPolicy Bypass -File tools\intellij-plugin-recording\capture-desktop.ps1 -OutputPath $allureProof -FrameRate 15 -MaximumDurationSeconds 120

$pageObjectProof = "$videoDir\page-object-proof.mp4"
powershell -NoProfile -ExecutionPolicy Bypass -File tools\intellij-plugin-recording\capture-desktop.ps1 -OutputPath $pageObjectProof -FrameRate 15 -MaximumDurationSeconds 120
```

After finalizing all four files through their printed stop markers, assemble
them without re-encoding and post-process that preserved combined raw file:

```powershell
$segments = @(
    $main,
    $success,
    $allureProof,
    $pageObjectProof
)
$concatFile = "$videoDir\concat.txt"
$concatLines = $segments | ForEach-Object {
    if ($_ -match "'") { throw "Segment path contains an unsupported single quote: $_" }
    "file '$($_.Replace('\', '/'))'"
}
[System.IO.File]::WriteAllLines($concatFile, $concatLines, [System.Text.UTF8Encoding]::new($false))
ffmpeg -hide_banner -loglevel warning -f concat -safe 0 -i $concatFile -c copy -n "$videoDir\combined-raw.mp4"
powershell -NoProfile -ExecutionPolicy Bypass -File tools\intellij-plugin-recording\postprocess-desktop-capture.ps1 -InputPath "$videoDir\combined-raw.mp4" -OutputPath "$videoDir\final.mp4"
```

The postprocessor refuses in-place/overwrite operations, preserves raw media,
removes near-duplicate frames with `mpdecimate`, renumbers timestamps, writes
H.264/yuv420p fast-start output, and reports both media summaries. Inspect
representative frames and the final frame before upload.

## 7. Upload and verify

Upload the final MP4 to Drive root as `video/mp4`. Re-fetch metadata or open
the returned URL and verify filename, preview, and file ID. Sharing is a
separate operation; verify the resulting permission independently and use the
manual owner step above if the connector cannot create it.

## Repository files

| File | Purpose |
|---|---|
| `capture-desktop.ps1` | Graceful bounded Windows desktop capture. |
| `postprocess-desktop-capture.ps1` | Raw-preserving duplicate-frame removal. |
| `reset-demo-project.ps1` | Recreates the external Selenium+TestNG demo. |
| `demo-project-template/` | Minimal resettable project seed. |
| `record-onboarding.ps1` | Plugin build/verification helper. |
| `install-windows-mcp.ps1` | Optional user-local Windows-MCP registration. |
