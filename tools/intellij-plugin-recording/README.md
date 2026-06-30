# IntelliJ Plugin Live Recording Workflow

Run this exact flow when asked to create a real SHAFT IntelliJ IDEA plugin video recording.

1. Build the latest local plugin ZIP:
   - `gradle -p shaft-intellij check buildPlugin verifyPlugin`
2. Install the built ZIP from `shaft-intellij` build output into an isolated IntelliJ sandbox and open:
   - `Tools -> SHAFT -> Open SHAFT`
3. On first-run MCP setup:
   - `Family: CODEX`
   - `Runtime: CLI`
   - `Install / Update SHAFT MCP`
   - This installs the plugin MCP command and configures SHAFT MCP for the selected agent.
   - `Test connection`
4. Configure Assistant:
   - `Route: LOCAL`
   - `Provider/Family: CODEX`
   - `Runtime: CLI`
   - `Mode: AGENT`
   - Enable `Allow edits`
   - Visible agent output should look like the selected agent's normal Markdown response.
   - Do not show raw JSON, `autobot_local_agent_run`, or SHAFT status metadata for normal local agent chat.
5. In Assistant prompt, use exact first sentence:
   - `open duckduckgo and search for SHAFT Engine`
   - Ensure the browser window is in focus.
   - Ask the agent to finish with exactly:
     - `DUCKDUCKGO_SHAFT_DONE`
6. Verify completion marker in logs/output:
   - `DUCKDUCKGO_SHAFT_DONE`
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
- When recording a branch before merge, launch the IDE with `SHAFT_MCP_INSTALLER_REF` set to the current branch so first-run install exercises the branch installer rather than `main`.
- If the live run exposes blockers, fix blockers in the PR and collect nonblocker follow-ups in one GitHub issue.
