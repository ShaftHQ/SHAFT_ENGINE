# FreeToken

Optional **local MoE / local-weights** companion from
[FlashML-org/FreeToken](https://github.com/FlashML-org/FreeToken). It is not a
workflow owner; select the canonical workflow in
[execution workflows](../../../references/execution-workflows.md) first. Follow
the [loopback runtime contract](loopback-runtime-contract.md); peer ports are in
the [transport-order table](../../../references/execution-workflows.md#transport-is-orthogonal).

| Fact | Value |
| --- | --- |
| Loopback | `http://127.0.0.1:1919` (OpenAI `/v1`, Anthropic `/v1/messages`) |
| CLI | `ft` |
| Probe | [`probe.py`](../../freetoken/scripts/probe.py): `python3 .chaos-engine/skills/freetoken/scripts/probe.py [attest\|models --json]` |
| Ignored ambient variable | `FREETOKEN_BASE_URL` |
| Guide | repo-only `chaos-engine/guides/freetoken.md` |

**Standalone from OmniRoute.** Neither requires the other. Missing FreeToken is
normal: use OmniRoute (if READY), a qualified native implementer, `SOLO`, or
another local OpenAI-compat runtime.

**Peer to Colibri.** FreeToken is the default local coding MoE path for agency
loops; [Colibri](colibri.md) (`:8000`) serves frontier multitier MoE when the
operator already serves it.

Extra rail: never run `ft serve` or `ft launch` (launch rewrites host agent
configs and can clear cloud API keys).
