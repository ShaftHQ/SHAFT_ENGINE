# Design skills inventory

Every upstream source the cards draw on, pinned by commit. Cards re-express
rules in their own words with attribution; no upstream file is copied into
this add-on. Refresh a pin deliberately: read the upstream diff, update the
card, then update the SHA and date here.

Pinned: 2026-10-03.

| Upstream | Commit | Licence | Use | Cards |
| --- | --- | --- | --- | --- |
| [anthropics/skills](https://github.com/anthropics/skills) | `8a1541c4a3ffa5a20a5a91de0dcf3f0bab1d1ef4` | Apache-2.0 (per-skill `LICENSE.txt`) | re-expressed: frontend-design, canvas-design, algorithmic-art, brand-guidelines, theme-factory | D02, D03, D07 |
| [pbakaus/impeccable](https://github.com/pbakaus/impeccable) | `e103efe779e2dd01274dabae83531fef00bf2563` | Apache-2.0 (NOTICE) | re-expressed anti-pattern rules | D03 |
| [leonxlnx/taste-skill](https://github.com/leonxlnx/taste-skill) | `ce26fc25c0e5e8cab638f883de62d9a86ee5e45b` | MIT | re-expressed taste rules | D03, D07 |
| [vercel-labs/web-interface-guidelines](https://github.com/vercel-labs/web-interface-guidelines) | `e3d624baaf29dc1fc645aff3e38f03e564d2d6b1` | MIT | audit rule set, referenced by pin | D04 |
| [heygen-com/hyperframes](https://github.com/heygen-com/hyperframes) | `b7343e3a95791bdc3448eddd6f00b4e73128b345` | Apache-2.0 | renderer and skill notes, referenced | D01, D09 |
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
| [remotion-dev/remotion](https://github.com/remotion-dev/remotion) | `e385a83dbde54179c0457ad90b7d7c3a4b6ab44a` | custom (company licence may be required) | documented alternative only | D09 |

## Not used

- Piper voices with non-commercial dataset licences (for example lessac).
- RNNoise community model files without a licence.
- Generated imagery containing text, code, or UI.
- Copyrighted books (Murch, Thomas and Johnston, Müller-Brockmann):
  paraphrased ideas only.
