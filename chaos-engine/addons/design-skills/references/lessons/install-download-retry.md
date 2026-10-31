# Install download retry + local clone

Per-file raw downloads: exponential backoff on SSL UNEXPECTED_EOF / GnuTLS.
Fall back to `CHAOS_ENGINE_LOCAL_CLONE` when retries exhaust. Maintain fetch
retries the same markers. See CE 2026.10.31.8 INST card.
