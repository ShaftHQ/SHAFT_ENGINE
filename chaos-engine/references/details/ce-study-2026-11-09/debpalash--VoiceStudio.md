# debpalash/VoiceStudio

- URL: https://github.com/debpalash/VoiceStudio
- Lists: Charlie Hills snapshot, 6 Nov 2026, row 15 (102K stars on that snapshot).
- GitHub description: An open-source local voice tool: voice cloning, voice design, video dubbing, dictation, transcription, and audiobook creation.
- README: README headings seen: what you can do, Pro and Cloud, platforms. The lead describes cloning from a reference recording, timed dubbing, and generating a voice from a description.

## Reusable patterns

- Voice work is split into clone, design, dub, and transcribe, and the local path is named separately from Pro and Cloud.
- A reference recording is an input, not something the tool invents.

## Adopt

- A voice feature stays on a local tool. A cloud voice API needs a named approval. The design add-on already has voice-over gates. Do not extend them from this repository.
- Voice cloning as a product is follow-up https://github.com/ShaftHQ/SHAFT_ENGINE/issues/7881, not a skill body.

## Skip

- Do not copy models, weights, prompts, or dubbing code.
- Do not vendor an AGPL program into this MIT repository.
- Do not clone a person's voice.

## License check

- Checked 2026-11-10. GraphQL licenseInfo.spdxId is AGPL-3.0. gh api repos/debpalash/VoiceStudio/license returned HTTP 200 and path LICENSE.
- The SHAFT_ENGINE root LICENSE is MIT. AGPL-3.0 is incompatible with copying this program into that tree. Skill edit blocked. Vendoring blocked.

