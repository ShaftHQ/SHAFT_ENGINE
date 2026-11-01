# VERIFY-DROPPED (2026.11.01 inputs already shipped)

- Carl L1–L4 / voice speaker+ASR+unk+proper-nouns → L93/L96 (`design_qc.py tts`)
- Carl L8 cap-2 → `design_fix_loop.py` (L72)
- Carl L9 / Cleo stop-all / E1 peer cancel → L90 + E1 `evaluate_stop_scope`
- Cleo dropped-item verify → L86 `verify_dropped.py`
- Cleo no-probe-live → L75 `no_probe_live.py`
- C4 dropped-item board owner → L86 (`board owner` marker in `verify_dropped.py`)
- L95 installer path → L100 + pr-gate `packs/**`
