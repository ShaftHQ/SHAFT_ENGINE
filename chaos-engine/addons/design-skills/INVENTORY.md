# Design skills inventory

Every upstream source the cards draw on, pinned by commit. Cards re-express
rules in their own words with attribution; no upstream file is copied into
this add-on. Refresh a pin deliberately: read the upstream diff, update the
card, then update the SHA and date here.

Pinned: 2026-10-03.

Cards: 23 step cards (D01-D23), 12 craft cards (C01-C12), 16 web-craft cards (W01-W16, references/web-craft), 4 video-craft cards (V07-V10, references/video-craft), 1 stills card (I01, references/stills), 1 data-viz card (G01, references/dataviz), 1 recon-craft card (R01, references/recon-craft), 38 lesson
cards (L01-L38 incl. L35 video caption handoff + L36-L37 CMS paste + L38 caption phonetic/mute; sources listed per card), the
innovation bank, 3 site-builder cards and 1 slides card plus S03 native RTL and S04 pptxgenjs (references/slides). D23 draws on no
upstream repository; its sources are platform and W3C documentation
listed in the card.

| Upstream | Commit | Licence | Use | Cards |
| --- | --- | --- | --- | --- |
| [anthropics/skills](https://github.com/anthropics/skills) | `dbd4588f9e1033efb41dad4bef2f7947c8993d44` | Apache-2.0 (per-skill `LICENSE.txt`; `pptx` is proprietary and not used) | re-expressed: frontend-design, canvas-design, algorithmic-art, brand-guidelines, theme-factory | D02, D03, D07 |
| [pbakaus/impeccable](https://github.com/pbakaus/impeccable) | `e103efe779e2dd01274dabae83531fef00bf2563` | Apache-2.0 (NOTICE) | re-expressed anti-pattern rules | D03 |
| [leonxlnx/taste-skill](https://github.com/leonxlnx/taste-skill) | `6010f91a9935f1c167dfc500fb1a475681892e6b` | MIT | re-expressed taste rules (v2 checks 2026-10-24) | D03, D07 |
| [vercel-labs/web-interface-guidelines](https://github.com/vercel-labs/web-interface-guidelines) | `434b7f91364665f2f733b310ec54809bf8f37937` | MIT | audit rule set, referenced by pin | D04 |
| [Nutlope/hallmark](https://github.com/Nutlope/hallmark) | `7ec868225c6caa040558c321ee0fefa0e73f83a8` | MIT | gate tiers, built-from-the-job, label tells re-expressed (theme catalog not used) | W01 |
| [emilkowalski/skills](https://github.com/emilkowalski/skills) | `2d228630ba1446365f614a108cb8252f1feda202` | MIT | UI motion, review-animations and apple-design interaction rules re-expressed | W02, W10, D08 |
| [cathrynlavery/diagram-design](https://github.com/cathrynlavery/diagram-design) | `0b9b10cab3dbf695642fa1a6c4b4f60df9083689` | MIT | diagram rules re-expressed | W05 |
| [zarazhangrui/frontend-slides](https://github.com/zarazhangrui/frontend-slides) | `9906a34d640d2111f724544cbc50f7f130569ae1` | MIT | rendered style previews | slides deck-system |
| [heygen-com/hyperframes](https://github.com/heygen-com/hyperframes) | `1a238171ec55896a9cb8452c33e4e0fe1063a60c` | Apache-2.0 | renderer and skill notes, referenced; vertical, captions, overlays, review and sound rules adapted | D01, D09, V07, V08 |
| [jakubkrehel/skills](https://github.com/jakubkrehel/skills) | `019ab7c199154f4509b55f12d84a4f5b578dc606` | MIT | better-typography, better-colors, better-accessibility rules adapted | W06, W08, W09 |
| [almubarmij/arabic-ai-skills](https://github.com/almubarmij/arabic-ai-skills) | `08d3b9d14c2d1719c0b96f703bced8dccfe2076a` | MIT | cite only (low trust), Arabic type and mirroring | L07 |
| [sultanalsafran/agent-skills](https://github.com/sultanalsafran/agent-skills) | `fc05be36316e5e5ea2ee0b5ec51519b411d54665` | MIT | arabic-presentations rules re-expressed; script not fetched or run | S03 |
| [wondelai/skills](https://github.com/wondelai/skills) | `c172996495bed0fcd26896a9416b2093fd7073f0` | MIT | refactoring-ui spacing, neutrals and 8-check score adapted | W07 |
| [owl-listener/designer-skills](https://github.com/owl-listener/designer-skills) | `9a6930cf84a822eb458624bd11c61aac5bbdf224` | MIT | layout-grid and dark-mode rules adapted | W07, W08 |
| [jakubkrehel/oklch-skill](https://github.com/jakubkrehel/oklch-skill) | `0e81f040e16d48fcc61d3e1b7ea548542371321f` | MIT | OKLCH thresholds and P3 fallback adapted | W08 |
| [addyosmani/web-quality-skills](https://github.com/addyosmani/web-quality-skills) | `afa8da942115f2961fdbfa80807ea0b232ff6c00` | MIT | focus ring and scroll-margin, inspiration | W09 |
| [cloudai-x/threejs-skills](https://github.com/cloudai-x/threejs-skills) | `0e181866f095b5f06d005133876e926515390d95` | MIT | post-processing notes, inspiration | W11 |
| [calesthio/generative-media-skills](https://github.com/calesthio/generative-media-skills) | `8c85352d5d75d4dcbe58480bd138e37b9742bab1` | MIT | grade order, loudness map, captions, title QC adapted; validators read, not run | V09, V10 |
| [higgsfield-ai/skills](https://github.com/higgsfield-ai/skills) | `f83af0bc1d937c8119099a11f8ebbf5e6fb99819` | MIT | thumbnail rules only; paid CLI not used | I01 |
| [jimliu/baoyu-skills](https://github.com/jimliu/baoyu-skills) | `1567581c26ec29f4216c6e6835415bf30343b0e3` | MIT | regenerate-not-patch, inspiration | I01 |
| [lewislulu/html-ppt-skill](https://github.com/lewislulu/html-ppt-skill) | `fd1629067909ff55b36b905476de4e5f26062a1f` | MIT | speaker-notes rule, inspiration; scripts not run | S04 |
| [anthropics/knowledge-work-plugins](https://github.com/anthropics/knowledge-work-plugins) | `33d3dea41a341b341d4f5da2e8801ae0c89be2b8` | Apache-2.0 | data-visualization chart choice adapted | G01 |
| [k-dense-ai/scientific-agent-skills](https://github.com/k-dense-ai/scientific-agent-skills) | `593377bb048752abb0152deab9fbb1083a6087df` | MIT | figure integrity and print export adapted | G01 |
| [jamalmohafil/claude-skills](https://github.com/jamalmohafil/claude-skills) | `ca8c888abeffa8616b611032510c631d43a263ef` | MIT | arabic-design checklist adapted | L07 |
| [itady74/ux-writing-arabic](https://github.com/itady74/ux-writing-arabic) | `412f6318084d33b4946f31f06849a34bbb49051a` | none | cite only, no text copied | L07 |
| [greensock/gsap-skills](https://github.com/greensock/gsap-skills) | `aed9cfd3277740755f6bfc1155c7aa645403b760` | MIT | referenced by pin | W03 |
| [haidrrrry/claude-remotion-skill](https://github.com/haidrrrry/claude-remotion-skill) | `1dcbe5e3fc6cf970bd10d3cc05f0a8a5d19d0383` | MIT | engine-neutral motion rules re-expressed | D08 |
| [bryanwhl/ffmpeg-video-editor](https://github.com/bryanwhl/ffmpeg-video-editor) | `d5c3ee6e9896fdabccd4e8b5d68149252bbb682c` | MIT | ffmpeg recipes re-expressed | D12 |
| [naveedharri/video-editor](https://github.com/naveedharri/video-editor) | `9386307a9604c4a315bd56877d94d5a410b27648` | MIT | QA-before-preview idea | D12 |
| [charmbracelet/vhs](https://github.com/charmbracelet/vhs) | `24fa2254a9806091e6ee6a980e9f3bcfe0a9ba53` | MIT | capture tool | D11 |
| [ManimCommunity/manim](https://github.com/ManimCommunity/manim) | `5dc0d3b8dfe23b1dbf2284cca111c5d146a44db0` | MIT | animation tool | D10 |
| [motion-canvas/motion-canvas](https://github.com/motion-canvas/motion-canvas) | `7b91435c301d530351dcf5ebb91dd139c002e405` | MIT | alternative animation tool | D09, D10 |
| [thewh1teagle/kokoro-onnx](https://github.com/thewh1teagle/kokoro-onnx) | `3596b26764286a7de9d90c363e988d50578918e5` | MIT (Kokoro-82M weights Apache-2.0) | default TTS | D17 |
| [ggml-org/whisper.cpp](https://github.com/ggml-org/whisper.cpp) | `60c0be6ac8fa71b1a2ae2dd938a31a34a508e774` | MIT | transcription and alignment | D17, D18 |
| [Rikorose/DeepFilterNet](https://github.com/Rikorose/DeepFilterNet) | `d375b2d8309e0935d165700c91da9de862a99c31` | MIT or Apache-2.0 | speech enhancement | D16 |
| [xinntao/Real-ESRGAN](https://github.com/xinntao/Real-ESRGAN) | `a4abfb2979a7bbff3f69f58f58ae324608821e27` | BSD-3-Clause | upscaling model | D19 |
| [xinntao/Real-ESRGAN-ncnn-vulkan](https://github.com/xinntao/Real-ESRGAN-ncnn-vulkan) | `37026f49824c5cf84062e7c6a5dd71445dcf610f` | MIT | GPU upscaler binary | D19 |
| [Netflix/vmaf](https://github.com/Netflix/vmaf) | `0497a0f299706b3124241f6ffc7fffc81d5665eb` | BSD-2-Clause-Patent | quality metric | D19 |
| [latent-spaces/brag](https://github.com/latent-spaces/brag) | `069432cac935ed1895859df201c31436c07fb74e` | MIT (bundled music/SFX not reused) | launch-video rules re-expressed | D01, D08, D11, D20, D22 |
| [remotion-dev/remotion](https://github.com/remotion-dev/remotion) | `e385a83dbde54179c0457ad90b7d7c3a4b6ab44a` | custom (company licence may be required) | documented alternative only | D09 |
| [Tejashmakwana/motionmaxxing](https://github.com/Tejashmakwana/motionmaxxing) | `8c8ec0f2a6f6c9a0da15cd24f7b1298ab298368c` | Apache-2.0 (NOTICE: Tejas Makwana; GSAP, fonts and showcase media excluded and not used) | re-expressed: never-ship list, craft laws, gates G2/G3/G5, measured eases; map in references/research/motionmaxxing-map.md | D08, D09, D20, craft cards |

| [Jakeschincariol/replica-skill](https://github.com/Jakeschincariol/replica-skill) | `77c9436fb3d18c3d58169efb8caf4fe906b0dc51` (patterns-only; not vendored) | MIT | method re-expressed only: recon map, features.csv parity, evidence-linked complaint themes; no clone/build/deploy skills copied | R01 |

Evaluated 2026-10-24, reference only and not vendored: UI UX Pro Max
(MIT), design-motion-principles (MIT), the Google DESIGN.md spec
(Apache-2.0) and the TypeUI DESIGN.md Extractor (MIT). Verdicts:
[design resources](references/research/design-resources-2026-10.md).

## Not used

- The Replica eleven-skill clone pipeline (architect/build/deploy); CE keeps method patterns only (R01).
- The impeccable CLI and detector (remote API, telemetry) and the transitions.dev
  library (no redistribution). Only the W01/W02 rules are re-expressed.
- Unicorn Studio (proprietary; free tier is branded) and its script (no
  licence): hover-reveal studied as reference only. Also reference only:
  archify, scroll-world (jakubkrehel/skills better-typography is adapted
  in W06 since batch 3). Skipped: stitch-skills and website cloners
  (third-party trade dress).
- Restricted (batch 3): land-book.com is human browsing only (its terms ban
  bots and AI use); socialproofexamples.com, navbar.gallery,
  pricingpages.design, ogimage.gallery and posts.design are cite only
  (no licence; screenshots are their owners'). posts.design and
  reallygoodemails MCPs need accounts and are not connected (note only).
  adfolio.design was bot-walled and not studied. Gamed installs skipped:
  designed-by-ai, uizze, runcomfy, feishu/lark wrappers, rknall
  svg-logo-designer (no licence).
- Batch 3 skips: nextlevelbuilder/ui-ux-pro-max (search scripts, CSV style
  catalogs, CLI installer); the superpowers ai-video-generation forks
  (cloned forks, inflated install counts, paid generation APIs);
  remotion-dev/skills (company licence; we render with ffmpeg); the
  hyperframes media-use and remove-background parts (sign-in, model
  download); the arabic-presentations repair script.
- getdesign.md catalog entries (third-party brand trade dress, no reuse licence).
- Piper voices with non-commercial dataset licences (for example lessac).
- RNNoise community model files without a licence.
- Generated imagery containing text, code, or UI.
- Copyrighted books (Murch, Thomas and Johnston, Müller-Brockmann):
  paraphrased ideas only.
