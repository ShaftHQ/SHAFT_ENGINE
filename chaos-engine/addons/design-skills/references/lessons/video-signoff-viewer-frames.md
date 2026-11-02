---
name: video-signoff-viewer-frames
description: Use when signing any video (chapter, Short, podcast clip) for publish or schedule; require full-frame viewer acceptance and automated caption/chrome gates.
---

# L39 Video sign-off: viewer frames + regression gates

## Use when

A talk chapter, Short, podcast clip or other captioned video is about to be
**SIGN**ed, scheduled or uploaded. Extends [L02](mixed-script-captions.md),
[L06](caption-video-qc.md), [L13](perceptual-acceptance.md),
[L14](band-coverage-gate.md), [L35](video-caption-handoff.md) and
[V07](../video-craft/shorts-safe-area-captions.md).

## Rules

1. **Viewer frames beat hashes.** Signing from sha256 / ffprobe / MAD /
   chrome-title-match / gates.json alone is invalid. The reviewer must open
   a contact sheet of **≥1 full-resolution frame per 5 s** of content (plus
   first 3 s and last 3 s before end-card) and reject anything a phone-sized
   viewer cannot read.
2. **No non-viewer on-screen text.** Reject production meta, debug, prompts,
   placeholders and editor notes (examples: `HEAVY VFX`, `TODO`,
   `PLACEHOLDER`, `lorem`, `DEBUG`, raw brief lines). Gate: `nonviewertext`
   scans text/ASS/SRT/JSON/HTML exports only (**no OCR**). An empty or
   image-only directory fails closed (`no-text-inputs`) — export chrome or
   ASS text first. Brand wordmark and the locked ear-lock title may stay;
   genre/meta chrome that reads as instructions fails unless explicitly
   allow-listed with `design-qc: allow` on that line.
3. **No stacked timed captions.** Gate `captionlayers` fails overlapping ASS
   Dialogue times, more than one muxed subtitle stream, or burn-in plus soft/
   sidecar captions both enabled in a manifest. It does **not** read the ASS
   `Layer` column or detect two time-disjoint burned systems — catch those
   with the contact-sheet eye check (rule 1). YouTube auto-captions/dub on
   the watch page are a **post-upload** check: open the watch page with
   captions off and confirm no second readable layer.
4. **Mixed-script isolates must survive to the burn format.** Every Latin run
   inside an RTL caption/hook/label line needs LRI…PDI (or HTML `<bdi>`).
   Gate: `bidisolate` (and existing `bidi` for neutrals). **Do not strip
   isolates** in the renderer "because ASS is weak" without a proven
   substitute that still passes `bidisolate` on the **shipped** caption
   file and a cropped-frame check (L02 rule 12).
5. **Caption alignment fits the band.** Two-line cues must fit the karaoke
   / lower-third band (fontsize × lines × leading ≤ band height − pad).
   Gate: `asscapfit` resolves **each Dialogue's Style** Fontsize (multi-style
   karaoke included) + `\\N` count vs `--band-h`.
6. **Safe area + phone legibility.** Text inside platform safe zones; caption
   ink readable when the frame is scaled to ~390 CSS px wide.
7. **Standard review brief (mandatory).** Every video sign-off / independent
   adversarial review brief **must** include the block below and attach the
   gate JSON receipts. A SIGN without those receipts is void.

### Review-brief block (copy into every video SIGN brief)

```text
## L39 viewer acceptance (mandatory)
- [ ] Contact sheet: ≥1 full-res frame / 5s + first 3s + last 3s (content)
- [ ] design_qc.py bidisolate <cues|ass|srt|hooks>
- [ ] design_qc.py bidi <cues|ass|srt|hooks>   # neutrals / trailing punct
- [ ] design_qc.py captionlayers <ass|mp4|manifest.json>
- [ ] design_qc.py asscapfit <file.ass> --band-h <px>
- [ ] design_qc.py nonviewertext <chrome.txt|ass|srt|dir-of-text>  # no OCR; empty/image-only dir or direct PNG fails
- [ ] Reject any on-screen text not intended for viewers
- [ ] No overlapping ASS Dialogue times / burn+soft both on; legible at phone size
- [ ] Safe-area check; after upload: watch page with captions OFF
- [ ] When wardrobe/sync/meta apply: wardrobe / capsync (L49 or L44 --single-model) / rendermeta
- [ ] Disputed sync cues: design_qc.py adjudic adjudication.json (L52)
- [ ] Digital-human / avatar claiming the subject: design_qc.py likenessvoice <mp4> --observations <json> (L56)
- [ ] Likeness: fresh independent reviewer, blind side-by-side vs real photo/video refs; any doubt = FAIL
- [ ] Voice: same reviewer, blind vs real reference audio; any doubt = FAIL; no SIGN without both
- [ ] Rerender: design_qc.py rendermeta <mp4> --meta upload.meta (L50; regen every render)
- [ ] Before slotting extend/LQ: design_qc.py extendfeas <inventory.json> (L51)
- [ ] Sync: independent anchors + frozen METHOD.md fixed sample set (L49/L44 + L53)
- [ ] Wardrobe: per-button samples when spec.buttons present (L48)
- [ ] Before YouTube upload: design_qc.py ytprecheck --observations <json> (L57)
- [ ] After schedule/upload: design_qc.py ytidbackfill <inventory.json> (L58)
- [ ] Retain final mp4 until public URL: design_qc.py retainmp4 --observations <json> (L59)
- [ ] Before rebuild/schedule: design_qc.py scheduledup --inventory <live.json> --title <t> (L61)
- [ ] On SIGN: copy finals to signed-renders/<UNIT>/ under the workspace root (L62 never-touch)
- [ ] Human ASS cues: craft_qc.py karaokehumanfrag <ass> (L63)
- [ ] Uncaptioned speech: craft_qc.py uncaptionedspeech --cues cues.json --asr asr.json (L64)
- [ ] Likeness refs carry real-source provenance (L60); AI kinds FAIL
Receipts: attach the JSON stdout of each design_qc command.
Metric-only / hash-only SIGN is a FAIL even when prior FAIL items closed.
```

## Regression inheritance (why old fixes missed batch1)

| Defect | Where the old fix lived | Why worker/reviewer did not inherit it | Delegation cause? |
| --- | --- | --- | --- |
| Mixed-script bidi (LRI…PDI) | CE L02 + `design_qc.py bidi` (neutrals only); Shorts `shorts-pilot/*/tools/render_r2.py` + `caption_qa.py` keep isolates | Podcast `render_vfx_chapter.py` **strips** LRI/PDI before writing ASS; `bidi` never required isolates and did not scan `.ass` | Yes — builder lane used podcast script defaults; reviewer brief checked chrome title string, not `bidisolate` |
| Caption alignment / stacked lines | V07 / L06 frame midpoints; Shorts caption_qa line rules | No ASS band-fit gate; two-line Fontsize 36 in BAND_H=130 looked "stacked" on phone; contact sheets existed but sign-off Read focused on title pill crops | Yes — reviewer trusted chrome/title region, not full lower-third legibility |
| Stacked caption layers | L03 mask burned banners; V07 pre-placement scan; L14 band coverage | No `captionlayers` gate; YouTube auto-captions/dub after language=Arabic can add a soft layer on the watch page; brief never required post-upload watch-page check | Partially — render miss + upload settings; review of render ≠ review of what YouTube plays |

## Verify

- [ ] `design_qc.py bidisolate cues.srt` fails RTL+Latin without LRI; passes with LRI…PDI; bare digits in RTL pass.
- [ ] `design_qc.py captionlayers clip.ass` fails overlapping Dialogue times; passes clamped cues.
- [ ] `design_qc.py asscapfit clip.ass --band-h 130` fails oversized karaoke Style even when Default is tiny; passes fitting Style.
- [ ] `design_qc.py nonviewertext chrome.txt` fails `HEAVY VFX` / `TODO` / `PLACEHOLDER`; empty/image-only dir **and direct PNG file path** fail closed; brand lines pass.
- [ ] SIGN receipt quotes the review-brief block and attaches gate JSON.

## Why

Batch1 podcast chapters passed independent adversarial SIGN on hashes,
duration, chrome-title-vs-prior-FAIL and upload meta, then the owner rejected
visible nonsense chrome and unreadable stacked captions. Lessons and Shorts
gates already knew the fixes; they were prose or another pipeline's defaults,
not hard fails on the podcast path or in the independent sign-off brief.

## Sources

Field retro 2026-10-30: independent sign-off packs for podcast chapters and
one follow-on chrome re-render; SHAs `029a3f9`, `38dd572`, `48e8722`,
`4f1a43c`. Frame evidence from local extracts of the signed mp4s.
Companions: L02, L13, L35, V07.
