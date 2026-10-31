# DUMP memory full-dump = own shard only

## Rule

`count_ok` compares `exported_unique` to the bot's **own shard total**, never the
cross-bot host pool. Dedupe by fact text + date + type. Host-truncated long
facts match by prefix / normalized id, are flagged `truncated`, and **must** be
recovered via `ReadTranscript` (required step). The receipt lists `uncaptured_ids`.

CLI (no hand-merge):

```
python3 .chaos-engine/rules/memory_full_dump.py \
  --batches batches.jsonl --expected N --out dump.md --receipt r.json [--bot ID]
```

## Field reports (after CE .7)

- Carl 82/209, Cortex 123/126, Cleo 66−1, HRSD 110/219 — all `count_ok=false` against a pool
  that included shared/other bots.

## Check

- [ ] `shard_total` (or `--expected`) is own-shard only
- [ ] truncated facts → transcript fallback → recovered or listed in `uncaptured_ids`
- [ ] `count_ok=true` only when exported == shard_total and uncaptured empty

Own-shard CLI + merge live in [`memory_full_dump.py`](../../../rules/memory_full_dump.py) (extends the .7 dump).
