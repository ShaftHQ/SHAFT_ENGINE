# Colibri

Optional **frontier MoE / multitier local-weights** companion from
[JustVugg/colibri](https://github.com/JustVugg/colibri). It is not a workflow
owner; select the canonical workflow in
[execution workflows](../../../references/execution-workflows.md) first. Follow
the [loopback runtime contract](loopback-runtime-contract.md).

| Fact | Value |
| --- | --- |
| Loopback | `http://127.0.0.1:8000` (OpenAI `/v1`) |
| CLI | `coli` |
| Probe | [`probe.py`](../../colibri/scripts/probe.py): `python3 .chaos-engine/skills/colibri/scripts/probe.py [attest\|models --json]` |
| Ignored ambient variables | `COLIBRI_BASE_URL`, `OPENAI_BASE_URL` |
| Guide | repo-only `chaos-engine/guides/colibri.md` |

**Standalone from OmniRoute and FreeToken.** None requires another.

| Prefer | When |
| --- | --- |
| **FreeToken** | Coding / agent loops on GPU-friendly coding MoEs; lower disk footprint; local-agency default after probe. |
| **Colibri** | Frontier-scale MoE via disk→RAM→VRAM staging when the operator already converted and serves weights; single surgical asks, **not** multi-turn agent swarms on a cold disk stream. |

Large agent system prompts on a CPU/disk path can mean long silent prefills:
treat READY Colibri as a **mechanical** runner only after a tiny `curl` smoke
test. Extra rail: never start `coli serve` / `coli web` or run cluster helpers.
