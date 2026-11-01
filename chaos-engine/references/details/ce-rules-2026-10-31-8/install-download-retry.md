# INST per-file download retry + local-clone fallback

## Rule

Raw GitHub per-file downloads retry transient SSL / network blips with
exponential backoff (`ssl.SSLEOFError`, `UNEXPECTED_EOF`, GnuTLS markers).
After the bounded retry budget is exhausted, fall back to a local clone when
`CHAOS_ENGINE_LOCAL_CLONE` or a known box checkout is available.
`tool.py maintain` retries `git fetch` the same way (Cortex GnuTLS).

## Field reports

- Coach: SSL `UNEXPECTED_EOF` after ~590 raw files mid-install.
- Cortex: GnuTLS handshake / non-proper TLS termination on fetch.

## Check

- [ ] `retry_delay` returns backoff for `ssl.SSLEOFError` (proven-red: old path returned None)
- [ ] `download_source_with_local_fallback` copies from local clone after network failure
- [ ] `maintain_sync` retries transient fetch failures

Modules: [`bootstrap.py`](../../../bootstrap.py), [`tool.py`](../../../tool.py).
