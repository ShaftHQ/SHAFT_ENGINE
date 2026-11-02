Permanent harness constraint: never use a traffic proxy like Headroom, never install, pin, wrap, vendor, doctor-require, or include it in the installer ever again.

Do not treat wrap/proxy as install health. Do not add HEADROOM_* env, a headroom_policy module, a Gemini proxy, or any equivalent that sits on the host invoke path.

A local model server (FreeToken loopback probe, OmniRoute) is not a proxy of this kind only when it stays optional, never wraps the host, and never rewrites host configs. Never `ft launch`. Never enforce such a proxy as doctor-healthy.

If a future change needs a proxy, stop. The answer is no, not a new flag.
