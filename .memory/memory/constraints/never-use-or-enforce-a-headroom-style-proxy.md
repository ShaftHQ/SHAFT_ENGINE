Permanent harness constraint (2026-09-09): never install, pin, wrap, vendor, or doctor-require a traffic proxy or host-invoke wrap of the Headroom kind.

Headroom was removed in #5693. Do not bring it back. Do not treat wrap/proxy as install health. Do not add HEADROOM_* env, a headroom_policy module, a Gemini proxy, or any equivalent that sits on the host invoke path.

A local model server (FreeToken loopback probe, OmniRoute) is not a proxy of this kind only when it stays optional, never wraps the host, and never rewrites host configs. Never `ft launch`. Never enforce such a proxy as doctor-healthy.

If a future change needs a proxy, stop. The answer is no, not a new flag.
