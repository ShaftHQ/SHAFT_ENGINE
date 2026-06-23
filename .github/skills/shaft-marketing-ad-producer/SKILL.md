---
name: shaft-marketing-ad-producer
description: Use when planning or producing SHAFT ads needing scripts, scenes, demo capture, Allure or MCP recorder footage, captions, music, code visuals, edits, and reusable assets.
---

# SHAFT Marketing Ad Producer

Plan SHAFT ads without fake claims.

## Stack

- Strategy/routes/scenes: `creative-production:positioning-explorer`, `creative-production:offer-explorer`, `creative-production:ads-explorer`, `creative-production:scene-explorer`, `creative-production:moodboard-explorer`.
- Visual polish: `creative-production:generative-polish`, `imagegen`. Keep exact copy, code, logos, and UI screenshots deterministic.
- Browser capture: `browse` or `playwright`; use `chrome:control-chrome` only for current Chrome state.
- Desktop capture: `computer-use:computer-use`; use OBS, Windows capture, or `ffmpeg` only if installed.
- SHAFT proof: MCP recorder/report tools such as `capture_start`, `capture_stop`, `capture_code_blocks`, `playwright_record_start`, `playwright_record_stop`, `playwright_recording_code_blocks`, mobile recording, `generate_test_report`, and Doctor/Allure tools.
- Editing/audio: existing video tools plus approved audio-generation or licensed music. If no generator exists, write a music brief and ask for or reuse an approved track.

## Workflow

1. Brief: audience, channel, duration, feature, proof, aspect ratio, and planning-only vs production.
2. Evidence: read SHAFT README/docs or MCP guidance. Use real Allure, Capture, MCP recorder, Doctor, or code surfaces for production; label mockups.
3. Script/scenes: make a timestamped table with visual/action, text, voiceover, source, animation, music beat, and asset path.
4. Assets: render code snippets as deterministic HTML/CSS screenshots. Never use generated-image text for final copy or code.
5. Capture: avoid secrets and private browser data. Record approved tabs/windows, preferably demo projects or sanitized reports.
6. Edit: assemble clips, captions, overlays, snippets, music, and exports with the smallest existing toolchain. Save commands/projects.
7. Reuse: save reusable output in the marketing repo, not SHAFT_ENGINE. If no repo path is given, ask first. Use `brand/theme.json`, `brand/assets/`, `snippets/`, `music/`, and `campaigns/<yyyy-mm>-<slug>/{brief.md,script.md,storyboard.md,captures/,renders/,exports/,manifest.json}`.

## Output

Planning: campaign slate, storyboard table, asset checklist, capture plan, and theme notes.

Production: scripts/storyboards, captures, code-card renders, captions, music source/brief, edit manifest, final exports, and marketing-repo paths.

Guardrails: no unsupported benchmarks, 3rd-party logos, security claims, generated final code text, secrets, or tokens.
