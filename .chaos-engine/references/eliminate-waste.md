# Eliminate waste

Permanent. Learned 2026-09-09. Token optimization is the objective. The method is Kanban: eliminate waste.

Waste is any token, retry, duplicate tool, proxy hop, or retry-the-same-search that does not change the next decision.

## Do

- One store, one bounded retrieve, then act.
- One owned path. No second MCP for the same job. CLI over MCP when both exist.
- Drop work that does not change the delivery. Do not re-implement a removed path.
- Keep the original issue intent and details. Ignore only the part that is waste (a traffic proxy). Do not rewrite the rest into a new program.
- Prefer a smaller diff that removes a hop over a larger diff that adds a wrapper.

## Do not

- Never install a traffic proxy. See [no-proxy](no-proxy.md).
- Do not enforce a proxy as install health.
- Do not duplicate Caveman bodies into host guidance.
- Do not spend a second research pass when the first receipt already names the next edit.

Checked-in memory: `.memory/memory/constraints/token-optimization-is-eliminate-waste.md`.
