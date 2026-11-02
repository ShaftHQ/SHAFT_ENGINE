# L93 VOICE-ASR

TTS / cloned-voice gates must include ASR intelligibility, not speaker match
alone:

- English: WER ≤ 15% vs the script
- Arabic: CER ≤ 15% vs the script
- Tokenizer preflight: unknown-token rate ≤ 5% before train or generate
- `language_id` check on the ASR transcript
- Checkpoint pick = speaker score AND ASR pass

Enforced via `design_qc.py tts` / voice gate helpers (defaults and CER path).
