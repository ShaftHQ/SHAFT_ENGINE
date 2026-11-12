---
description: Use when a spoken introduction is scripted, synthesized, or reviewed before upload.
---

# Narration review

Spoken introductions use this list together with the standing section of
[design-review-video-checklist.md](../design-review-video-checklist.md).
The owner's bar is stable loudness, no noise bed, no torn or interrupted word,
no gibberish, breaths and pauses at clause boundaries, fillers scattered
rather than clustered, and one performance of the whole script.

Measurements below are the check. A reviewer who cannot run the command does
not pass the item.

## Loudness and level

- Measure the whole file with `ffmpeg` `ebur128=peak=true`. Integrated loudness
  is -16 LUFS ±1, which is the spoken-word target already on the voice-over
  craft card. Apple Podcasts publishes -16 LKFS ±1 and a true peak of -1 dBTP
  ([Apple podcast audio requirements](https://podcasters.apple.com/support/893-audio-requirements)).
  EBU R 128 measures the programme in its entirety and caps true peak at
  -1 dBTP ([R 128-2023](https://tech.ebu.ch/docs/r/r128v5_0.pdf)).
- True peak stays at or below -1 dBTP. Sample peak stays at or below 0.89,
  about -1 dBFS.
- For this voice, the private review listen is -20 LUFS ±2. The owner heard
  -15.8 LUFS as shouting. Do not master that listen back to -16. Apple's -16
  LKFS figure stays the podcast-publish figure, not this review gate.
- One gain for the whole file. AES TD1008 treats loudness normalization as
  uniform gain, not a fader ride
  ([Production Advice on TD1008](https://productionadvice.co.uk/td1008/)).
  The opening sentence is not louder than the rest. Short-term speech
  loudness stays within 3 LU of the integrated loudness. That 3 LU window is
  the gate for the defect where one sentence was mastered hotter. It is not
  an EBU number.
- Do not compress phrase by phrase to hide an uneven read. Regenerate it.

## Noise and tears

- A pause of at least 200 ms is at or below -70 dBFS RMS. Do not zero a dip
  shorter than 200 ms. A short dip can be a quiet consonant.
- Clicks, digital tears, and a noise bed under the voice fail the take.
  Regenerate. Do not denoise inside a word.

## One performance

- The file is one synthesis of the whole script. Do not splice a louder take
  into it. A one-second window whose samples match a reference clip is a splice.
- A silence inside a word, or a word cut in half, fails.

## Pronunciation and words

- An automatic transcript catches gibberish, an extra clause, and an
  interrupted word. Content words match the script.
- A proper noun is judged against the owner's recording, not against
  transcript spelling. The reference transcript matches the reference audio.

## Cadence, breath, and fillers

- A breath or a pause sits at a clause boundary. Filled pauses in studied
  speech belong at boundaries, not in the middle of a word
  ([Watanabe, 2017](https://www.jstage.jst.go.jp/article/onseikenkyu/21/3/21_24/_pdf)).
- A filler is not clustered: no two fillers inside eight seconds, and no
  sentence carries more than one. Eight seconds is this project's scatter
  rule, about twenty words at a conversational pace.
- A scripted filler is checked on a short slice around its boundary, with
  previous-text conditioning off. A full-file transcript can drop a real
  "Um". On 2026-11-12 the full file had no filler token, and a 1.2 second
  slice of the same file heard "Um" at 43.16 s. Absence on the full pass is
  not absence in the audio.
- Speaking rate is words divided by the whole duration, pauses included.
  The gate is 140 to 175 words a minute. Tauroza and Allison (Applied
  Linguistics, 1990) place an average lecture at 125 to 160 and call a
  lecture faster than normal above 185. The take the owner liked measured
  168. The take he called simply too fast measured 210, with the words
  themselves at about 302 once pauses were removed.
- Articulation rate is words divided by the time whose 10 ms frames sit
  above -40 dBFS. The gate is at or below 280. Do not pass a file whose
  pauses hide a rushed read.
- Do not time-stretch the file. `atempo`, rubberband, and sox tempo are
  failures even when the resulting rate falls inside the gate. The owner
  heard a whole-file slowdown as inhuman. Pace comes from the reference
  recording, not from a later stretch. There is no speech-rate control on
  `generate_voice_clone`.

## Script

Write for the ear. One main claim per sentence, long enough to sound spoken
and short enough for one breath
([Voice Overs guide](https://www.voiceovers.com/guides/how-to-write-a-script-for-voice-over)).
Time the draft. A conversational read is about 150 words a minute
([VoiceJungle](https://www.voicejungle.com/blog/how-to-write-a-voiceover-script/)).
A sixty to ninety second intro lands near 150 to 190 words. Two minutes is
the hard stop.

Use contractions, active voice, and the first person. Write numbers as words.
Put a phonetic note beside a hard name. Do not read that note aloud.
Vary the sentence length so the read is not a metronome
([VoiceBros](https://voicebros.com/en/blog/voice-over-script-writing)).
Facts in the script have a source. Directions stay off the spoken page.

Fillers, when the owner asked for them, sit at a boundary and stay scattered.
They are not piled into one sentence.

## Not used

- Broadcast -23 LUFS, and the AES speech-distribution -18 LUFS, are not this
  target. The craft card already uses -16 LUFS for spoken stereo.
- A ten-word sentence cap fights a spoken paragraph that still fits one breath.
- A rule that forbids opening with the person's name does not apply when the
  owner asked for the name first.
- A listening-panel MOS score is not available in this review. The transcript
  and the measurements above are the check.
