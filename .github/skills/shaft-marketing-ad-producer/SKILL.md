---
name: shaft-marketing-ad-producer
description: Use when planning or producing SHAFT ads needing scripts, scenes, demo capture, reports, captions, music, code visuals, edits, or reusable assets.
---

# SHAFT Marketing Ad Producer

Plan SHAFT ads without fake claims.

## Stack

- Strategy/scenes: use the relevant `creative-production:*` explorer.
- Polish: `creative-production:generative-polish`, `imagegen`; keep exact copy, code, logos, and UI screenshots deterministic.
- Capture: `browse` or `playwright`; use Chrome only for current profile state, and desktop/OBS/`ffmpeg` only if installed.
- SHAFT proof: use MCP recorder, report, Doctor, Allure, and code-block tools.
- Editing/audio: use existing video tools and approved or licensed audio; otherwise write a music brief.

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

Production: scripts/storyboards, captures, code-card renders, captions, music source/brief, edit manifest, exports, and marketing-repo paths.

Guardrails: no unsupported benchmarks, 3rd-party logos, security claims, generated final code text, secrets, or tokens.
