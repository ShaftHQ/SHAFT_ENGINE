# L81 NO-SELF-CLOCK

Caption / motion-graphics gate never takes onset from the cue under test.
All onsets come from audio-only analysis committed before comparison
(`asr_path`). No-onset → FAIL (not 0). Ban `mid_speech_voiced` (and kin) when
`phrase_onset == cue_start`. Mutant: shift one mid-speech cue +500 ms → FAIL.
Module: `motion_graphics_gate.py`.
