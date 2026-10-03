---
name: screen-capture
description: Use when a video needs real terminal or browser footage recorded as reproducible, secret-free, scripted takes.
---

# D11 Screen capture

## Use when

The video shows a real terminal session (an install, a command, a test run)
or a real browser flow. Output is never faked or typed into a mockup.

## Tooling

- Terminal: VHS (MIT; needs ttyd and ffmpeg) runs `.tape` scripts, so a retake
  is a re-run. Fallback: asciinema recording rendered with agg (both GPL-3.0,
  used as tools, never shipped).
- Browser: Playwright `recordVideo` at the delivery size.

## Tape template

```
Output capture/install.mp4
Require git
Set Width 3840
Set Height 2160
Set FontSize 44
Set FontFamily "JetBrains Mono"
Set Theme { "background": "#0f1115", "foreground": "#e8eaf0" }
Set TypingSpeed 45ms
Env PS1 "$ "
Hide
Type "cd $(mktemp -d)" Enter
Show
Type "<the real command>" Enter
Wait+Screen@10m /installed|healthy/
Sleep 2s
```

## Rules

- Clean environment for every take: throwaway HOME and project directory,
  a rootless container or VM where available, a neutral prompt and hostname.
- Wait on real output (`Wait /regex/`), never on fixed sleeps.
- Scrub secrets before recording: unset tokens, use a test account, and avoid
  personal paths, emails, and hostnames on screen.
- Keep the full log of each take next to the video.
- A network failure means a retake, not an edited fake.
- Windows footage comes from a real Windows machine (PowerShell), recorded
  with the same tape discipline or the OS recorder at a fixed size.

## Workflow

1. Write the tape per storyboard scene; dry-run with `Output` to GIF.
2. Record at 2160p; save the log.
3. Review frames for secrets and readability; retake as needed.
4. Long waits become speed ramps in D12, labelled on screen.

## Verify

- `vhs` exits 0 and every `Wait` matched; the command's own exit code is 0.
- A health command (for example a doctor or status command) in the take
  reports success.
- Log scan finds no tokens, emails, usernames, or private paths
  (`rg -n "gh[pousr]_|@|/home/|/Users/" take.log` reviewed); sampled frames
  checked with OCR when tesseract is installed.

## Sources

charmbracelet/vhs (MIT); asciinema and agg (GPL-3.0, tools only); Playwright
video recording documentation.
