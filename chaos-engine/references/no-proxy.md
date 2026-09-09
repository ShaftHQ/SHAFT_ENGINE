# No proxy

Permanent. Source of truth for Codex, Claude, Grok, Gemini, Copilot, and Grok Bot.

Never install a traffic proxy. Do not install, pin, wrap, vendor, or doctor-require a traffic proxy or host-invoke wrap. Do not treat wrap/proxy as install health. No equivalent on the host invoke path. The ban lives in project memory constraints.

A local model server is allowed only as an optional loopback probe. It must not wrap the host and must not rewrite host configs. Never `ft launch`. Never enforce a proxy as doctor-healthy.

Doctor uniqueness lives in [`mcp_policy.py`](../mcp_policy.py). Overlay hash lives in [`overlay_match.py`](../overlay_match.py).
