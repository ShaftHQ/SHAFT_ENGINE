# Deja token A/B table

Counts are UTF-8 bytes of each run's stdout and stderr, divided by four, recorded with `session_token_usage.py record` and read back with `summarize`. Not a vendor invoice. `defaultOn` changes only when both median and mean drop.
Regenerate with [deja_ab_measure.py](../../deja_ab_measure.py).
Rows: [deja-token-ab-rows.json](deja-token-ab-rows.json).

| task | control median | control mean | deja median | deja mean |
| --- | --- | --- | --- | --- |
| deja-store | 495.0 | 495.0 | 589.0 | 589.0 |
| overlay-pre-push | 261.0 | 261.0 | 359.0 | 359.0 |
| parent-rog-shell | 1.0 | 1.0 | 98.0 | 98.0 |
| all runs | 261.0 | 252.3 | 359.0 | 348.7 |

defaultOn stays false
