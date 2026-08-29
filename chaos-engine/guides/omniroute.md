# OmniRoute: a selective free-model add-on

OmniRoute is an optional local gateway that can expose several model providers
through an OpenAI-compatible endpoint. Use it only for bounded, non-sensitive
delegated work after each selected route passes the tests in this guide. Keep
the parent agent, architecture decisions, reviews, terminal work, and all
confidential code on the existing primary provider.

This is an integration guide, not part of ChaosEngine's installer or runtime.
ChaosEngine does not install OmniRoute, create provider accounts, retain
provider credentials, start a service, or choose a provider on your behalf.

> **Research snapshot — 2026-08-29.** Commands and provider/model identifiers
> below were reviewed against OmniRoute `v3.8.50`. Its own
> [free-tier catalogue](https://github.com/diegosouzapw/OmniRoute/blob/release/v3.8.50/docs/reference/FREE_TIERS.md)
> labels free allowances as estimates and says they change frequently. Before
> onboarding, recheck the provider's current official pricing, terms, privacy,
> model availability, regional eligibility, and card/phone/KYC requirements.
> “Free” never means unlimited, private, reliable, or suitable for production.

## Safety boundary

Use a unique provider key for each service. Never paste an upstream API key,
OmniRoute endpoint key, account password, cookie, OAuth callback, or recovery
code into a repository, issue, prompt, shell history, TOML file, log, or chat.
If a password has been shared in any chat or ticket, rotate it before use.

Store each upstream key in the operating-system secret store or inject it into
the current shell only. The examples deliberately reference environment
variable *names*, never key values:

```bash
export GROQ_API_KEY='obtain-this-from-the-provider-secret-store'
omniroute providers add groq --credential-env GROQ_API_KEY --yes \
  --default-model openai/gpt-oss-120b
unset GROQ_API_KEY
```

`--credential-env` is supported by OmniRoute 3.8.50 and prevents the credential
from appearing in the command line. It still becomes provider configuration;
protect OmniRoute's local data directory and use storage encryption if the
version's doctor reports it is available. Do not put a literal key in a systemd
unit, shell profile, or `~/.codex/config.toml`.

This guide excludes provider routes classified as `tos: avoid`, Kiro proxying,
OpenCode/session replay, web-cookie integrations, AI Horde for tool-using
agents, card-required services, phone/KYC-only services, and any CAPTCHA,
anti-bot, quota-evasion, multi-account, or credential-sharing workaround.
Those exclusions apply even when a route appears in OmniRoute's discovery UI.

## Install and run on demand

OmniRoute `3.8.50` supports Node.js `>=22.22.2 <23` or `>=24 <27`. Confirm the
active runtime first:

```bash
node --version
npm --version
```

Install the reviewed release for the current login account, including optional
packages required by the distribution:

```bash
npm install --global --include=optional omniroute@3.8.50
command -v omniroute
omniroute --version
omniroute doctor --json
```

Do not substitute `latest` for the reviewed version until a new release has
been reviewed. For a later intentional update, first read its release notes and
repeat all route tests, then run:

```bash
npm install --global --include=optional omniroute@<reviewed-version>
omniroute --version
omniroute doctor --json
```

Start it only when needed. `HOST=127.0.0.1` keeps the gateway on loopback; do
not bind it to a LAN address or expose it through a tunnel.

```bash
HOST=127.0.0.1 omniroute serve --port 20128 --no-open
```

In a separate terminal, verify the service and only then open the local
dashboard at `http://127.0.0.1:20128` if you need the visual provider picker:

```bash
curl --fail --silent http://127.0.0.1:20128/health
omniroute providers available --search groq
omniroute providers list
```

Create one dedicated OmniRoute endpoint key using **Dashboard → API Keys**.
Copy it once into a secret manager, then make it available only to processes
that use this gateway:

```bash
export OMNIROUTE_API_KEY='read-this-from-your-secret-manager'
curl --fail --silent http://127.0.0.1:20128/v1/models \
  -H "Authorization: Bearer $OMNIROUTE_API_KEY"
```

The endpoint key authorizes access to the local gateway; it is not an upstream
provider key. Stop an on-demand server with `Ctrl-C`. No autostart is enabled
by this guide.

### Optional disabled systemd user unit

Use this only after on-demand operation and route tests work. First resolve the
actual executable and preserve it through Node/NVM updates:

```bash
command -v omniroute
```

Create `~/.config/systemd/user/omniroute.service` with the exact absolute path
printed above substituted for `<absolute-path-to-omniroute>`:

```ini
[Unit]
Description=OmniRoute local gateway
After=network-online.target

[Service]
Type=simple
Environment=HOST=127.0.0.1
ExecStart=<absolute-path-to-omniroute> serve --port 20128 --no-open
Restart=on-failure
RestartSec=5

[Install]
WantedBy=default.target
```

Keep upstream credentials out of this file. After every Node/NVM update,
re-resolve `command -v omniroute` and update `ExecStart` before restarting.
The unit remains disabled until explicitly enabled:

```bash
systemctl --user daemon-reload
systemctl --user start omniroute
systemctl --user status omniroute --no-pager
curl --fail --silent http://127.0.0.1:20128/health
systemctl --user stop omniroute
```

To opt in to autostart later, run `systemctl --user enable omniroute`; verify
again that the listener is loopback-only with `ss -ltnp | rg ':20128'`.

## Curated top 10: manual account onboarding

This is a ranked shortlist, not a claim that all ten are equal. “Best model” is
the reviewed OmniRoute identifier to test first, not a quality guarantee. A
provider must be admitted to a combo only after it passes the direct Responses
and tool-use tests below. Provider signup remains manual: accept terms, complete
email verification, and stop if the service requests a card, phone number,
KYC, or CAPTCHA that you do not wish to provide.

| Rank and status | Provider and official signup | First coding model / OmniRoute ID | Free-use snapshot and account steps | Caveats |
| --- | --- | --- | --- | --- |
| 1 — recommended | [Groq Console](https://console.groq.com/) | `openai/gpt-oss-120b` / `groq` | Create account, verify email, create API key, save as `GROQ_API_KEY`. Groq publishes free-plan limits; its current page lists 1,000 requests/day and 200K tokens/day for this model family. | Shared organization limits; low minute limits can bind first. Personal/internal use only; do not expose the gateway to others. |
| 2 — recommended for non-sensitive work | [Pollinations](https://enter.pollinations.ai/) | `qwen-coder` / `pollinations` | No account or upstream key for reviewed keyless models. In Dashboard, add **Pollinations** and choose `qwen-coder`; CLI command below uses `--no-credential`. | No published allowance, no SLA, model set can change. Treat prompts as non-sensitive and test tool calling. |
| 3 — recommended for non-sensitive work after testing | [UncloseAI](https://uncloseai.com/) | `qwen3.6:27b` / `uncloseai` | The reviewed route is keyless: add **UncloseAI**, select `qwen3.6:27b`, and test. | No published allowance or durable privacy commitment in reviewed material; service availability and tool compatibility require live proof. |
| 4 — conditional | [Mistral Console](https://console.mistral.ai/) | `codestral-latest` / `mistral` | Create account, verify email, create an API key, save as `MISTRAL_API_KEY`. OmniRoute's audited catalogue records a 1B-token/month shared pool, but confirm the live console before relying on it. | Signup/eligibility and quota can change; confirm no paid overage or card requirement before enabling. |
| 5 — conditional | [SambaNova Cloud](https://cloud.sambanova.ai/) | `DeepSeek-V3.2` / `sambanova` | Create account, verify email, create key, save as `SAMBANOVA_API_KEY`. OmniRoute catalog estimates a shared 6M-token/month recurring pool. | Review privacy/terms and regional access; published onboarding details can change. |
| 6 — trial-only | [Cerebras Cloud](https://cloud.cerebras.ai/) | `gpt-oss-120b` / `cerebras` | Create account, verify email, create key, save as `CEREBRAS_API_KEY`. OmniRoute catalog records a no-card 1M-token/day free trial (about 30M/month). | Treat as finite trial capacity: confirm expiry, account gate, and model access in the current console. |
| 7 — conditional | [OpenRouter Keys](https://openrouter.ai/keys) | an explicit current `:free` model / `openrouter` | Create account, create key, save as `OPENROUTER_API_KEY`, then select one specific free model from the live catalogue. Its free pool is request-limited; never use `auto` as a no-cost guarantee. | Multi-provider routing changes privacy/data handling. A paid top-up changes quota and is outside this guide. |
| 8 — conditional | [Hugging Face tokens](https://huggingface.co/settings/tokens) | `deepseek-ai/DeepSeek-V3` / `huggingface` | Create account, verify email, accept model terms when prompted, create a fine-grained inference token, save as `HF_TOKEN`. OmniRoute catalog estimates a small shared monthly pool (about 200K tokens). | Third-party inference routing and model licenses apply; not enough capacity for routine coding delegation. |
| 9 — conditional | [Cloudflare dashboard](https://dash.cloudflare.com/) | `@cf/qwen/qwen2.5-coder-32b-instruct` / `cloudflare-ai` | Create account, create a Workers AI API token with least privilege, save as `CLOUDFLARE_API_TOKEN`, and record account ID only outside the repository. Cloudflare publishes 10,000 Neurons/day; the limit resets at 00:00 UTC. | Practical coding volume depends on model Neuron price. Some frontier models require Workers Paid or prepaid credit. |
| 10 — prototyping-only | [NVIDIA Build](https://build.nvidia.com/) | `mistralai/devstral-2-123b-instruct-2512` / `nvidia` | Create account, accept terms, generate NIM key, save as `NVIDIA_API_KEY`, then verify the chosen model is still available. | NVIDIA does not publish a stable numeric hosted allowance here; use only for development/evaluation, never production or a cost guarantee. |

### Add and test each provider

Start the local gateway and export only the key required for the provider you
are onboarding. The following commands use 3.8.50's provider IDs and avoid
literal credentials. For providers with a dashboard-only field (for example a
Cloudflare account ID), use **Providers → Add Provider** and enter the value
there; never place it in repository configuration.

```bash
# Keyless routes: connection succeeds without an upstream credential.
omniroute providers add pollinations --no-credential --yes --default-model qwen-coder
omniroute providers add uncloseai --no-credential --yes --default-model qwen3.6:27b

# API-key routes: export one value only for this command, then unset it.
omniroute providers add groq --credential-env GROQ_API_KEY --yes \
  --default-model openai/gpt-oss-120b
omniroute providers add mistral --credential-env MISTRAL_API_KEY --yes \
  --default-model codestral-latest
omniroute providers add sambanova --credential-env SAMBANOVA_API_KEY --yes \
  --default-model DeepSeek-V3.2
omniroute providers add cerebras --credential-env CEREBRAS_API_KEY --yes \
  --default-model gpt-oss-120b
omniroute providers add openrouter --credential-env OPENROUTER_API_KEY --yes \
  --default-model '<verified-free-model-id>'
omniroute providers add huggingface --credential-env HF_TOKEN --yes \
  --default-model deepseek-ai/DeepSeek-V3
omniroute providers add cloudflare-ai --credential-env CLOUDFLARE_API_TOKEN --yes \
  --default-model @cf/qwen/qwen2.5-coder-32b-instruct
omniroute providers add nvidia --credential-env NVIDIA_API_KEY --yes \
  --default-model mistralai/devstral-2-123b-instruct-2512

omniroute providers list
omniroute providers test groq
omniroute providers validate
```

`providers test` can intentionally skip a no-auth connection because no API key
probe exists. That is not proof of working inference. Test every exact target
through the local endpoint, substituting the model shown by `/v1/models`:

```bash
curl --fail-with-body http://127.0.0.1:20128/v1/responses \
  -H "Authorization: Bearer $OMNIROUTE_API_KEY" \
  -H 'Content-Type: application/json' \
  -d '{"model":"groq/openai/gpt-oss-120b","input":"Return only: route-ok"}'
```

Then test a harmless function call through the same exact model. Admit it only
if OmniRoute logs show the intended provider/model and the response includes a
valid call to `report_route`:

```bash
curl --fail-with-body http://127.0.0.1:20128/v1/responses \
  -H "Authorization: Bearer $OMNIROUTE_API_KEY" \
  -H 'Content-Type: application/json' \
  -d '{
    "model":"groq/openai/gpt-oss-120b",
    "input":"Call report_route with status route-ok. Do not answer in text.",
    "tools":[{
      "type":"function",
      "name":"report_route",
      "description":"Returns a harmless route-check result.",
      "parameters":{"type":"object","properties":{"status":{"type":"string"}},"required":["status"],"additionalProperties":false}
    }]
  }'
```

If Responses or tool use fails, leave that target out of the coding combo; a
chat-completions success alone is insufficient.

## Strict, fail-closed free coding combo

Do not use `auto`, `auto/coding`, “best available,” model aliases, or broad
fallbacks as a zero-cost promise. They may select paid or unreviewed routes.
Create a named priority combo only from individual targets that you have just
tested and whose current terms state that quota exhaustion fails rather than
bills. Get each connection ID from `omniroute providers list` or the provider
detail page, then replace every placeholder below with your recorded proven
provider/model/connection tuple:

```bash
omniroute combo create chaosengine-free-coding --strategy priority \
  --models '[
    {"providerId":"groq","model":"openai/gpt-oss-120b","connectionId":"<groq-connection-id>"},
    {"providerId":"pollinations","model":"qwen-coder","connectionId":"<pollinations-connection-id>"},
    {"providerId":"uncloseai","model":"qwen3.6:27b","connectionId":"<uncloseai-connection-id>"}
  ]'
omniroute combo list
omniroute combo switch chaosengine-free-coding
```

The combo must contain only exact provider/model/connection tuples. In the
dashboard's combo editor, remove every implicit fallback and require the
request to fail when all listed targets fail, rate-limit, or exhaust quota.
Run a direct request against `chaosengine-free-coding`, then deliberately test
an unapproved model, a nonexistent target, and an exhausted/disabled target.
Each must return an error, not reroute to a paid or broad automatic target.

## Codex configuration: selective delegation by default

Back up the Codex configuration before editing it. Refuse a collision with an
existing `model_providers.omniroute` definition rather than overwriting it:

```bash
mkdir -p ~/.codex/backups
cp ~/.codex/config.toml ~/.codex/backups/config.toml.before-omniroute.$(date +%Y%m%d%H%M%S)
```

Add this provider block to `~/.codex/config.toml`; preserve the existing
top-level `model_provider` so the parent session remains on its normal primary
provider:

```toml
[model_providers.omniroute]
base_url = "http://127.0.0.1:20128/v1"
env_key = "OMNIROUTE_API_KEY"
requires_openai_auth = false
wire_api = "responses"
```

Set `OMNIROUTE_API_KEY` only in the launch environment or a secret manager
integration. It must not be committed to Codex project configuration.

Create three *personal* agent definitions under `~/.codex/agents/`. Each agent
uses `model_provider = "omniroute"` and
`model = "chaosengine-free-coding"`, then contains only a thin role locator:

```toml
# ~/.codex/agents/omniroute_explorer.toml
name = "omniroute_explorer"
model_provider = "omniroute"
model = "chaosengine-free-coding"
developer_instructions = "Load applicable AGENTS.md, the canonical ChaosEngine skill, and the explorer role before work. Stay read-only."
```

Create corresponding `omniroute_worker.toml` and `omniroute_tester.toml` with
the named worker/tester role in the instruction. Do not move architecture or
terminal reviewer work to this provider. Invoke these named subagents only for
low-risk work after the exact combo is proven.

An optional all-free profile can use the same provider and exact combo for a
low-risk, explicitly launched session. It is not the default profile and does
not replace the parent provider. Smoke-test it without making it persistent,
then discard it if it fails Responses or tool-use tests.

## Acceptance checks

Perform these checks after each provider change and before delegating a real
task:

```bash
omniroute --version
omniroute doctor --json
curl --fail --silent http://127.0.0.1:20128/health
curl --fail --silent http://127.0.0.1:20128/v1/models \
  -H "Authorization: Bearer $OMNIROUTE_API_KEY"
omniroute providers validate
omniroute combo list
```

Record the following facts in a private, secret-free inventory. “No account
created” is a valid status; do not fabricate account completion.

| Provider | Signup site | Email used | Auth type | Account status | Free allowance | Tested model | Human checkpoint | Verified date |
| --- | --- | --- | --- | --- | --- | --- | --- | --- |
| Example only | URL | private reference, not the address | API key / none | pending / active | current documented limit | exact ID | terms, privacy, billing checked | YYYY-MM-DD |

## Troubleshooting and rollback

- **`doctor` reports an unsupported Node runtime:** select a supported Node
  release, reinstall the same reviewed OmniRoute version, and rerun all checks.
- **Health fails:** run `omniroute serve --port 20128 --no-open` in the
  foreground, inspect sanitized output, and check `ss -ltnp | rg ':20128'`.
  Never change the listener to a public interface as a workaround.
- **A provider returns 401/403/429:** confirm the account, key scope, current
  quota, and provider terms. Do not create another account or bypass controls.
- **A model is missing or tool calls fail:** refresh the provider's current
  catalogue, retest the exact ID, and remove it from the combo until it works.
- **Unexpected model or charge:** stop the gateway, disable the connection,
  revoke the endpoint and upstream keys, review OmniRoute logs for the exact
  route, and notify the provider if its billing page shows a charge.

To remove the integration, stop and disable the optional unit, remove the
personal agents and provider block, restore the saved Codex backup, revoke
endpoint/upstream keys, then uninstall the user-global package:

```bash
systemctl --user disable --now omniroute 2>/dev/null || true
rm -f ~/.config/systemd/user/omniroute.service
systemctl --user daemon-reload
npm uninstall --global omniroute
```

Use `omniroute providers remove <connection>` and `omniroute combo delete
chaosengine-free-coding --yes` before uninstalling when the gateway is running.
Do not delete a data directory until credentials are revoked and a backup is
unnecessary; OmniRoute's own backup/export and data-location commands should be
used first for the reviewed release.

## Sources

- [OmniRoute v3.8.50 free-tier reference](https://github.com/diegosouzapw/OmniRoute/blob/release/v3.8.50/docs/reference/FREE_TIERS.md)
- [OmniRoute v3.8.50 quick start](https://github.com/diegosouzapw/OmniRoute/blob/release/v3.8.50/docs/getting-started/QUICK-START.md)
- [Groq free-plan rate limits](https://console.groq.com/docs/rate-limits)
- [Cloudflare Workers AI pricing and free allowance](https://developers.cloudflare.com/workers-ai/platform/pricing/)
- [NVIDIA NIM Run Anywhere terms](https://docs.api.nvidia.com/nim/re/docs/run-anywhere)
