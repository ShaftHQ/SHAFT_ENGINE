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
| [L27 full-width embed](full-width-embed.md) | Span content grid + phone gutter parity |
| [L28 live-verify CMS embed](live-verify-cms-embed.md) | headless public URL matrix @360/390/430/1440 |
| [L29 plan-first multi-axis](plan-first-multi-axis.md) | plan+accept before multi-axis visual build |
| [L30 explainer embed length](explainer-embed-length.md) | sibling Watch explainers ~60–100 s parity |
| [L31 paste then re-verify](paste-then-reverify.md) | artifact paste + immediate livecms |
| [L32 Arabic tech code-switch](arabic-tech-codeswitch.md) | EN tech terms in Arabic UI; no و next to bdi Latin in headings |
| [L33 LESS Custom CSS grid/calc](less-css-grid-calc.md) | longhand grid + escaped calc under LESS 1.3.3 |
| [L34 Paste robots noindex](paste-robots-noindex.md) | no robots noindex/nofollow or X-Robots-Tag in Code-block paste |
| [L35 Video caption handoff](video-caption-handoff.md) | human captions, script filter, speech cue gap, TP/fps/VFX/onset |
| [L36 CMS paste fragment only](cms-paste-fragment-only.md) | Code-block paste = fragment; dir/lang + CSS on wrapper; header Δ≤1px vs `/` |
| [L37 CMS paste no HTML comments](cms-paste-no-html-comments.md) | no `<!--` in paste; ops notes leak live |
| [L38 Caption phonetic + mute gate](caption-phonetic-mute-gate.md) | Arabic phonetic tech dumps + Egyptian allowlist; mute must be silent; no burned [muted] |
| [L39 Video sign-off viewer frames](video-signoff-viewer-frames.md) | full-frame SIGN; bidisolate/captionlayers/asscapfit/nonviewertext |
| [L44 Caption min-cue + independent sync](caption-min-cue-independent-sync.md) | cue <0.8s fail unless merged; capsync vs independent ASR/FA onsets |
| [L45 Delete-check refusal final](delete-check-refusal-final.md) | delete-check refusal is final; no raw-rm bypass; session-manifest self-delete |

| [L65 motion-graphics gate](motion-graphics-gate.md) | kinetic MG; ASR path provenance; sequential match |
| [L66 web UI gate](web-ui-gate.md) | live web publish bidi/WCAG/390+1440 |
| [L70 coach pre-publish](coach-pre-publish.md) | Coach PASS before live publish |
| [L71 avatar likeness ref](avatar-likeness-ref-conditioned.md) | reference-conditioned; glasses+age |
| [L72 design fix-loop](design-fix-loop-localized.md) | after cap-2 localize, no full regen |
| [L74 gate proven red](gate-proven-red.md) | FAIL known-bad fixture first |
| [L75 no probe live](no-probe-live.md) | write probes on scratch only |
| [L76 preview real width](preview-at-real-width.md) | Coach PASS at real container width |

External resources and verdicts: [design resources](../research/design-resources-2026-10.md).
