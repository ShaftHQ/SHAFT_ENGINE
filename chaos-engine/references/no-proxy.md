# No Headroom-style proxy

Permanent. Source of truth for Codex, Claude, Grok, Gemini, Copilot, and Grok Bot.

Do not install, pin, wrap, vendor, or doctor-require a traffic proxy or host-invoke wrap of the Headroom kind. Headroom left in #5693. Do not bring it back. Do not treat wrap/proxy as install health. No `HEADROOM_*` env. No `headroom_policy` module. No Gemini proxy. No equivalent on the host invoke path.

A local model server is allowed only as an optional loopback probe. It must not wrap the host and must not rewrite host configs. Never `ft launch`. Never enforce a proxy as doctor-healthy.

Checked-in memory: `.memory/memory/constraints/never-use-or-enforce-a-headroom-style-proxy.md`.

Doctor uniqueness lives in [`mcp_policy.py`](../mcp_policy.py). Overlay hash lives in [`overlay_match.py`](../overlay_match.py).
