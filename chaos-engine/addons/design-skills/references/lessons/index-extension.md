# Lesson index extension (L40–L65)

Rows for Phase 1 memory-abstraction lessons plus 2026-10-31 retro cards. Kept separate from
`index.md` so a sibling video sign-off lesson PR can own L39 + the
primary index tip without a merge clash. The design-skills completeness
test reads this file together with `index.md`.

| Card | Use when |
| --- | --- |
| [L40 YouTube upload language order](youtube-upload-language-order.md) | Studio upload: video language → schedule → title/desc language → captions |
| [L41 Spoken-lock scope words](spoken-lock-scope-words.md) | ear-locked title must keep scope nouns; `title_scope_gate` |
| [L42 Chrome follows title](chrome-follows-title.md) | retitle → re-render burned-in chrome; body-only overlays; clean endcard |
| [L43 Long vs Short dedupe](lq-vs-short-dup.md) | long shares source with Shorts: titles differ; overlap only if ≥3× superset |
| [L44 Caption min-cue + independent sync](caption-min-cue-independent-sync.md) | cue <0.8s fail unless merged; capsync vs independent ASR/FA |
| [L45 Delete-check refusal final](delete-check-refusal-final.md) | delete-check refusal is final; no raw-rm bypass; session-manifest self-delete |
| [L46 Render freshness sha256](render-freshness-sha256.md) | fix-round mp4 sha256 ≠ prior; REUSE_MP4 fails |
| [L47 Avatar frozen-tail](avatar-frozen-tail.md) | frame-diff last 2s + after speech; static fails |
| [L48 Avatar wardrobe gate](avatar-wardrobe-gate.md) | avatar/I2V wardrobe vs ref; collar crop; buttons; 0.5 s samples |
| [L49 Caption sync multi-model consensus](caption-sync-multimodel-consensus.md) | ≥2 ASR/FA + RMS; no builder rows; 0 measured or >150 ms = FAIL |
| [L50 Render meta rev/sha](render-meta-rev-sha.md) | meta.render_rev/sha must match current render |
| [L51 Extend feasibility before slot](extend-feasibility-before-slot.md) | dup ≥3× + G6 clear before scheduling |
| [L52 Per-cue sync adjudication](per-cue-sync-adjudication.md) | disputed cues: discard snap FA; false-lead; silence bridge; adjudic.json SoT |
| [L53 Freeze METHOD before review](freeze-method-before-review.md) | METHOD.md+script before review; AMBIG rules; cap 2 fix rounds |
| [L54 Checkpoint generated assets](checkpoint-generated-assets.md) | checkpoint/commit renders immediately; explicit paths |
| [L55 Signed script header](signed-script-header.md) | mark script header signed on design sign-off |
| [L56 Digital-human likeness + voice](avatar-likeness-voice-gate.md) | blind likeness+voice vs real refs; fresh independent reviewer; doubt=FAIL |
| [L57 YouTube Studio pre-upload check](youtube-studio-preupload-check.md) | content-library receipt before upload; dup Shorts fail closed |
| [L58 Inventory yt_id backfill](inventory-yt-id-backfill.md) | after schedule/upload backfill yt_id; no stale nulls |
| [L59 Retain mp4 until public URL](retain-mp4-until-public-url.md) | keep final mp4 until public watch URL proof |
| [L60 Likeness real-ref provenance](avatar-likeness-voice-gate.md) | harden L56: real_* kind + sha256 + source; ban AI oracle |
| [L61 Schedule dup before rebuild](schedule-dup-before-rebuild.md) | title+window vs live inventory before rebuild/schedule |
| [L62 Signed-renders store](signed-renders-store.md) | copy-on-SIGN; signed-renders never-touch |
| [L63 Karaoke human frag](karaoke-human-frag.md) | no <0.8s karaoke fragments inside human cues |
| [L64 Uncaptioned speech](uncaptioned-speech.md) | ASR speech ≥300ms needs ≥50% human-cue cover |
