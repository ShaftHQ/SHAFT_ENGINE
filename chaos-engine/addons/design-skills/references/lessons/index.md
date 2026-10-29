# Lesson cards (talk video, captions, release, hosted CMS)

Field lessons from talk Shorts, visual/media pipeline retros, and hosted-CMS embeds. These are rules
with checks, not taste. Load the matching D-card or craft card first, then
one lesson card.

| Card | Use when |
| --- | --- |
| [L01 hook speech match](hook-speech-match.md) | a hook card or title card sits over speech |
| [L02 mixed-script captions](mixed-script-captions.md) | captions or labels mix RTL (Arabic) and Latin |
| [L03 talk edit and layers](talk-edit-and-layers.md) | tightening a talking-head edit, crossfading overlays |
| [L04 signed-output release](signed-output-release.md) | a pipeline change could alter already approved renders |
| [L05 cross-surface takeaways](cross-surface-takeaways.md) | one technique must carry across Shorts, long video, decks and web |
| [L06 caption and video QC](caption-video-qc.md) | splice joins, frame sampling, fade tails, ASR hallucinations |
| [L07 Arabic text and mirroring](arabic-rtl-type-mirroring.md) | Arabic text on any surface; mirroring layout, icons and arrows for RTL |
| [L08 visual-first approach](visual-first-approach.md) | choosing or iterating a visual/media approach before multi-pass; visual gate before metrics |
| [L09 privacy names gate](privacy-names-gate.md) | captions/ASR personal names + vocatives; mute notes alone never PASS |
| [L10 end-card band concat](endcard-band-concat.md) | burn overlay bands on content only; gate live end card vs clean plate |
| [L11 title punct by script](title-punct-by-script.md) | metadata title/desc question marks: Latin `?` vs Arabic `؟` |
| [L12 question-mark baseline](qmark-baseline.md) | mixed-font trailing `?`/`؟` shares body baseline (`anchor=ls`) |
| [L13 perceptual acceptance](perceptual-acceptance.md) | accepting a visual/media package; metric floors; reviewer must view output vs real reference |
| [L14 band coverage gate](band-coverage-gate.md) | burned band must cover source on-screen text every content frame |
| [L15 mute span gate](mute-span-gate.md) | hard mute ≤2s/span and ≤10% content; cue >2s → contiguous cut; else recut |
| [L16 end-card filter_complex](endcard-filter-complex.md) | concat via filter_complex; seek by video-stream duration; pix_fmt |
| [L17 caption micro-gap snap](caption-micro-gap-snap.md) | snap sub-~80ms cue gaps after cuts |
| [L18 ASR names in run_gates](asr-names-run-gates.md) | wire privacy/ASR scan into run_gates; fail-closed if words missing |
| [L19 hard-cut coherence](hard-cut-coherence.md) | each join one thought; prefer contiguous alternate over bad cut |
| [L20 avatar voice first](avatar-voice-first.md) | voice is first dealbreaker; I2V still ≠ voice-matched avatar |
| [L21 viral pacing FLAG](viral-pacing-flag.md) | late punchline on viral → FLAG (advisory), never FAIL |
| [L22 ear-lock type templates](earlock-type-templates.md) | ear-lock title; viral vs qa G16 line-1 prefixes |

| [L23 hosted-CMS no-dup chrome](hosted-cms-no-dup-chrome.md) | embed under site chrome; sticky header offset on first section |
| [L24 hosted-CMS assets](hosted-cms-assets.md) | relative assets → https or data after CSP probe |
| [L25 CMS owns meta](hosted-cms-owns-meta.md) | no og/twitter/html lang in Code-block paste |
| [L26 hostile CMS resets](hostile-cms-resets.md) | hostile CSS + heading luminance + squircle tile |
| [L27 full-width embed](full-width-embed.md) | Code-block section spans content grid |
| [L28 live-verify CMS embed](live-verify-cms-embed.md) | headless public URL matrix @390/1440 |
| [L29 plan-first multi-axis](plan-first-multi-axis.md) | plan+accept before multi-axis visual build |
| [L30 explainer embed length](explainer-embed-length.md) | sibling Watch explainers ~60–100 s parity |
| [L31 paste then re-verify](paste-then-reverify.md) | artifact paste + immediate livecms |

External resources and verdicts: [design resources](../research/design-resources-2026-10.md).
