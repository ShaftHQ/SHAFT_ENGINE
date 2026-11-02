# L96 VOICE-PROPER-NOUNS

ASR intelligibility gates must catch proper-noun mangling (example: EN WER 0.30
on a personal name / brand token while speaker match passed).

Script proper nouns get a respell/lexicon check and a per-noun ASR match.
Failure of any listed proper noun is a gate FAIL even when overall WER passes.
