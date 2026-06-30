# IntelliJ Plugin Live Recording Workflow

Run this exact flow when asked to create a real SHAFT IntelliJ IDEA plugin video recording.

1. Build the latest local plugin ZIP:
   - `gradle -p shaft-intellij check buildPlugin verifyPlugin`
2. Install the built ZIP from `shaft-intellij` build output into an isolated IntelliJ sandbox and open:
   - `Tools -> SHAFT -> Open SHAFT`
3. On first-run MCP setup:
   - `Install / Update SHAFT MCP`
   - `Family: CODEX`
   - `Runtime: CLI`
   - `Test connection`
4. Configure Assistant:
   - `Route: LOCAL`
   - `Provider/Family: CODEX`
   - `Runtime: CLI`
   - `Mode: AGENT`
   - Enable `Allow edits`
5. In Assistant prompt, use exact first sentence:
   - `open chrome and login to saucedemo`
   - Use credentials:
     - `standard_user`
     - `secret_sauce`
   - Ask the agent to finish with exactly:
     - `SAUCEDEMO_LOGIN_DONE`
6. Verify completion marker in logs/output:
   - `SAUCEDEMO_LOGIN_DONE`
7. Record video from IDE launch through marker:
   - Format: `MP4`
   - Codec: `H.264`
   - Frame rate: `30 fps`
   - Audio: **off** (no audio)
   - View: full desktop
   - Command:
     - `ffmpeg -y -f gdigrab -framerate 30 -i desktop -c:v libx264 -preset veryfast -pix_fmt yuv420p -movflags +faststart target/intellij-plugin-recording/shaft-intellij-onboarding.mp4`
8. Upload final MP4 to Google Drive root and paste that link in the PR body.

Notes:
- On this Windows machine, Chrome exists at `C:\Program Files\Google\Chrome\Application\chrome.exe` even when `chrome` is not on `PATH`.
- Keep the run isolated from the user's daily IDE profile; use the Gradle IntelliJ sandbox or a disposable IDE config directory.
- Before starting capture, launch the selected IntelliJ runtime once and dismiss any Windows Firewall prompt for OpenJDK manually; automation must not choose Windows Security permissions.
- If the live run exposes blockers, fix blockers in the PR and collect nonblocker follow-ups in one GitHub issue.
