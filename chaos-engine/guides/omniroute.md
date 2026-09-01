# OmniRoute: enable ChaosEngine to use the local gateway

OmniRoute is an optional local gateway. ChaosEngine uses it through the
provider-neutral [OmniRoot skill](../skills/omniroot/SKILL.md) after the
canonical [execution workflow](../references/execution-workflows.md) is
selected. The goal is to make a running local OmniRoute easy to use, not to
fence operators behind restricted keys or attestation hashes.

OmniRoot probes only `http://127.0.0.1:20128/`. Missing OmniRoute is normal and
leaves native delegation and `SOLO` valid. Missing
`~/.config/chaos-engine/omniroot.json` is also normal: the runner uses
`chaosengine-omniroute` or `omniroute` from PATH.

This is an operator integration guide, not a second workflow definition.
ChaosEngine does not install OmniRoute, create provider accounts, retain
provider credentials, start a service, or choose a provider on your behalf.

> **Research snapshot — 2026-08-30.** Commands and provider/model identifiers
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
omniroute doctor --no-liveness
```

Do not substitute `latest` for the reviewed version until a new release has
been reviewed. For a later intentional update, first read its release notes and
repeat all route tests, then run:

```bash
npm install --global --include=optional omniroute@<reviewed-version>
omniroute --version
omniroute doctor --no-liveness
```

If an installed OmniRoute binary is present and
`curl -sf --max-time 2 http://127.0.0.1:20128/api/health` fails, start loopback
only. Never install OmniRoute from a task. `OMNIROUTE_SERVER_HOST=127.0.0.1`
keeps the gateway on loopback; do not bind it to a LAN address or expose it
through a tunnel.
`HOST` is not OmniRoute's cross-platform bind setting.

```bash
OMNIROUTE_SERVER_HOST=127.0.0.1 omniroute serve --port 20128 --no-open
```

In a separate terminal, verify the service and only then open the local
dashboard at `http://127.0.0.1:20128` if you need the visual provider picker:

```bash
curl --fail --silent http://127.0.0.1:20128/api/health
omniroute health --json
omniroute providers available --search groq
omniroute providers list
```

OmniRoute 3.8.50 deliberately gives anonymous `/api/health` callers only
`status` and `timestamp`. OmniRoot does not accept that as a build sentinel.
When that exact fixed-loopback response occurs, OmniRoot verifies the local
`omniroute` and Node executables, invokes CLI health with a temporary scrubbed
working directory and fixed `127.0.0.1` target, and retains only its non-empty
build/version. OmniRoot never reads, passes, prints, or stores endpoint keys or
CLI token material; the verified CLI resolves its own local machine-token proof.
It never records routes or provider data. If local CLI build evidence is
unavailable, qualification stays `UNHEALTHY`; repair the local installation or
use native fallback.

Create an OmniRoute endpoint key using **Dashboard → API Keys**. A full-access
key is fine. Copy it once into a secret manager, then export it only to
processes that use this gateway:

```bash
export OMNIROUTE_API_KEY='read-this-from-your-secret-manager'
curl --fail --silent http://127.0.0.1:20128/v1/models \
  -H "Authorization: Bearer $OMNIROUTE_API_KEY"
python3 chaos-engine/skills/omniroot/scripts/runner.py probe
```

`probe` is `READY` when the API answers and at least one catalog model has
remaining tokens. Then use it. It does not require a restricted key or a
private attestation file.

The endpoint key authorizes access to the local gateway; it is not an upstream
provider key. Stop an on-demand server with `Ctrl-C`. No autostart is enabled
by this guide.

### Optional disabled systemd user unit

Use this only after on-demand operation and route tests work. Generate a unit
from current absolute Node and OmniRoute paths; this makes it work under NVM
without hard-coding another user's paths. Regenerate it after every Node/NVM
or OmniRoute update:

```bash
mkdir -p "$HOME/.config/systemd/user"
readonly omniroute_bin="$(command -v omniroute)"
readonly node_bin="$(command -v node)"
readonly node_dir="$(dirname "$node_bin")"
[[ -x "$omniroute_bin" && -x "$node_bin" ]] || {
  printf '%s\n' 'Resolve supported Node and OmniRoute before creating this unit.' >&2
  exit 1
}
{
  printf '%s\n' '[Unit]' 'Description=OmniRoute local gateway' 'After=network-online.target'
  printf '%s\n' '' '[Service]' 'Type=simple'
  printf '%s\n' 'Environment="OMNIROUTE_SERVER_HOST=127.0.0.1"'
  printf 'Environment="PATH=%s:/usr/local/bin:/usr/bin:/bin"\n' "$node_dir"
  printf 'ExecStart="%s" serve --port 20128 --no-open\n' "$omniroute_bin"
  printf '%s\n' 'Restart=on-failure' 'RestartSec=5' '' '[Install]' 'WantedBy=default.target'
} > "$HOME/.config/systemd/user/omniroute.service"
systemd-analyze --user verify "$HOME/.config/systemd/user/omniroute.service"
```

This writes both a loopback-only `OMNIROUTE_SERVER_HOST` and an absolute
`ExecStart`; `PATH` contains the active NVM Node directory. Keep upstream
credentials out of this file. The unit remains disabled until explicitly
enabled:

```bash
systemctl --user daemon-reload
systemctl --user start omniroute
systemctl --user status omniroute --no-pager
curl --fail --silent http://127.0.0.1:20128/api/health
systemctl --user stop omniroute
```

To opt in to autostart later, run `systemctl --user enable omniroute`; verify
again that the listener is loopback-only with `ss -ltnp | rg ':20128'`.

## Curated top 10: manual account onboarding

This is a ranked shortlist, not a claim that all ten are equal. “Best model” is
the reviewed OmniRoute identifier to test first, not a quality guarantee. A
provider must be admitted to the selected route only after it passes the direct Responses
and tool-use tests below. Provider signup remains manual: accept terms, complete
email verification, and stop if the service requests a card, phone number,
KYC, or CAPTCHA that you do not wish to provide.

There is no safe keyless bootstrap on the reviewed machine. When no provider
key exists, the minimum onboarding checkpoint is a manually created Groq
account and a new `GROQ_API_KEY`; do not configure a coding subagent until that
exact target passes both tests. A catalog entry or a successful `providers add`
command is never proof of availability, free entitlement, tool support, or
privacy.

| Rank and status | Provider and official signup | First coding model / OmniRoute ID | Free-use snapshot and account steps | Caveats |
| --- | --- | --- | --- | --- |
| 1 — recommended | [Groq Console](https://console.groq.com/) | `openai/gpt-oss-120b` / `groq` | Create account, verify email, create API key, save as `GROQ_API_KEY`. Groq publishes free-plan limits; its current page lists 1,000 requests/day and 200K tokens/day for this model family. | Shared organization limits; low minute limits can bind first. Personal/internal use only; do not expose the gateway to others. |
| 2 — verified default | [Google AI Studio](https://aistudio.google.com/) | `gemini-3.1-flash-lite` / `gemini` | Create account, verify email, create API key, save as `GEMINI_API_KEY`. Before adding it, manually confirm **AI Studio Plan = Free** and that no Cloud Billing account is attached. Current limits are visible only in AI Studio. | Verified on the reviewed machine with a restricted local endpoint key and a real Codex shell tool call. Google free-tier handling can include training and human review: public, non-sensitive prompts only. `gemini-2.5-flash` exhausted its free request quota, `gemini-2.5-flash-lite` returned 404 for a new user, and `gemini-3.7-flash` returned high-demand HTTP 503; none is admitted. |
| 3 — conditional | [Mistral Console](https://console.mistral.ai/) | `codestral-latest` / `mistral` | Create account, verify email, create an API key, save as `MISTRAL_API_KEY`. OmniRoute's audited catalogue records a 1B-token/month shared pool, but confirm the live console before relying on it. | Signup/eligibility and quota can change; confirm no paid overage or card requirement before enabling. |
| 4 — conditional | [SambaNova Cloud](https://cloud.sambanova.ai/) | `DeepSeek-V3.2` / `sambanova` | Create account, verify email, create key, save as `SAMBANOVA_API_KEY`. OmniRoute catalog estimates a shared 6M-token/month recurring pool. | Review privacy/terms and regional access; published onboarding details can change. |
| 5 — conditional, trial-only | [Cohere Dashboard](https://dashboard.cohere.com/api-keys) | `command-a-03-2025` / `cohere` | Create an account, verify email, copy the automatically created trial key or create one, then save it as `COHERE_API_KEY`. Cohere's official docs describe a free trial key with 1,000 API calls/month and 20 Chat requests/minute. | OmniRoute 3.8.50 lists Cohere as an API-key provider with a no-card free trial; live signup requirements can change, so stop if it asks for a card, phone, KYC, or CAPTCHA. Trial capacity is for evaluation, not routine delegation. Test exact Responses and tool use before admission. |
| 6 — conditional, prototype-only | [NVIDIA API Catalog](https://build.nvidia.com/) | `mistralai/devstral-2-123b-instruct-2512` / `nvidia` | Join NVIDIA Developer, accept API Catalog terms, create an API key, save as `NVIDIA_API_KEY`, then confirm live quota and model access. NVIDIA offers hosted NIM endpoints free for prototyping, not production. | NVIDIA does not publish a stable numeric allowance for every model; rate-limit and model availability vary. Test Responses and tools before admission. |
| 7 — conditional, light free use | [Ollama](https://ollama.com/) | `gpt-oss:120b` / `ollama-cloud` | Create account, create an API key, save as `OLLAMA_API_KEY`. Free includes cloud-model access with light usage; session limits reset every five hours and weekly limits every seven days. | Free capacity is not a fixed token grant. Confirm the model is available to Free before use; no paid usage/overage is enabled by this guide. |
| 8 — conditional | [OpenRouter Keys](https://openrouter.ai/keys) | an explicit current `:free` model / `openrouter` | Create account, create key, save as `OPENROUTER_API_KEY`, then select one specific free model from the live catalogue. Its free pool is request-limited; never use `auto` as a no-cost guarantee. | Multi-provider routing changes privacy/data handling. A paid top-up changes quota and is outside this guide. |
| 9 — conditional | [Hugging Face tokens](https://huggingface.co/settings/tokens) | `deepseek-ai/DeepSeek-V3` / `huggingface` | Create account, verify email, accept model terms when prompted, create a fine-grained inference token, save as `HF_TOKEN`. OmniRoute catalog estimates a small shared monthly pool (about 200K tokens). | Third-party inference routing and model licenses apply; not enough capacity for routine coding delegation. |
| 10 — conditional | [Cloudflare dashboard](https://dash.cloudflare.com/) | `@cf/qwen/qwen2.5-coder-32b-instruct` / `cloudflare-ai` | Create account, create a Workers AI API token with least privilege, save as `CLOUDFLARE_API_TOKEN`, and record account ID only outside the repository. Cloudflare publishes 10,000 Neurons/day; the limit resets at 00:00 UTC. | Practical coding volume depends on model Neuron price. Some frontier models require Workers Paid or prepaid credit. |

### Excluded near miss: Cerebras

Cerebras is intentionally not in this shortlist. Its current official rate-limit
page says the Free Trial gives new accounts $5 in credits **only after adding a
verified payment method**, expires after 30 days, and has no permanently free
tier. The same page lists `gpt-oss-120b` at 5 RPM and 30K TPM. That conflicts
with this guide's no-card and recurring-free requirements, even though
OmniRoute supports Cerebras. Do not add it as a substitute or workaround.

### Add and test each provider

Start the local gateway and export only the key required for the provider you
are onboarding. The following commands use 3.8.50's provider IDs and avoid
literal credentials. For providers with a dashboard-only field (for example a
Cloudflare account ID), use **Providers → Add Provider** and enter the value
there; never place it in repository configuration.

```bash
# API-key routes: export one value only for this command, then unset it.
omniroute providers add groq --credential-env GROQ_API_KEY --yes \
  --default-model openai/gpt-oss-120b
omniroute providers add gemini --credential-env GEMINI_API_KEY --yes \
  --default-model gemini-3.1-flash-lite
omniroute providers add mistral --credential-env MISTRAL_API_KEY --yes \
  --default-model codestral-latest
omniroute providers add sambanova --credential-env SAMBANOVA_API_KEY --yes \
  --default-model DeepSeek-V3.2
omniroute providers add cohere --credential-env COHERE_API_KEY --yes \
  --default-model command-a-03-2025
omniroute providers add nvidia --credential-env NVIDIA_API_KEY --yes \
  --default-model mistralai/devstral-2-123b-instruct-2512
omniroute providers add ollama-cloud --credential-env OLLAMA_API_KEY --yes \
  --default-model gpt-oss:120b
omniroute providers add openrouter --credential-env OPENROUTER_API_KEY --yes \
  --default-model '<verified-free-model-id>'
omniroute providers add huggingface --credential-env HF_TOKEN --yes \
  --default-model deepseek-ai/DeepSeek-V3
omniroute providers add cloudflare-ai --credential-env CLOUDFLARE_API_TOKEN --yes \
  --default-model @cf/qwen/qwen2.5-coder-32b-instruct

omniroute providers list
omniroute providers test groq
omniroute providers validate
```

Do not add Pollinations or UncloseAI as an initial route. Their reviewed
keyless claims did not survive live validation: Pollinations registration failed
and its documented `qwen-coder` target returned 404; UncloseAI's `lorbus`
accepted chat but returned HTTP 500 for tool use. Keep both out of the ranked
recommendations and never use fingerprint/session-pool features to make either
route work.

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

If Responses or tool use fails, leave that target out of the selected route; a
chat-completions success alone is insufficient.

## ChaosEngine catalog: prefer free, allow what the key can call

Do not treat `auto` as a cost guarantee. Before every dispatch, query the live
catalog with no cache files:

```bash
omniroute --output json models
omniroute --output json usage quota
omniroute --output json models glm
python3 chaos-engine/skills/omniroot/scripts/runner.py candidates --capability default
```

Rank remaining `default` models first for implementation, then most-intelligent,
then mechanical. Do not pin `chaosengine-omniroute`'s Codex profile model
(Gemini Flash-Lite). `omniroute --output json models` may list friendly names
(`GLM 4.5`); dispatch the native id (`glm-4.5`) with
`omniroute run --model --provider` and Codex `-c model='<provider>/<id>'`.
The launcher sets `model_provider=omniroute` but does not replace Codex's
default model name. On HTTP 429 or HTTP 400 live-catalog miss, skip that
identity and pick the next remaining candidate. Native host models are last
resort when the catalog is empty.

## Codex configuration: optional separate session

Back up the Codex configuration before editing it. Refuse a collision with an
existing `model_providers.omniroute` definition rather than overwriting it:

```bash
mkdir -p ~/.codex/backups
cp ~/.codex/config.toml ~/.codex/backups/config.toml.before-omniroute.$(date +%Y%m%d%H%M%S)
```

Add this provider block to `~/.codex/config.toml`; preserve the existing
top-level `model_provider` so normal Codex sessions remain on their primary
provider:

```toml
[model_providers.omniroute]
base_url = "http://127.0.0.1:20128/v1"
env_key = "OMNIROUTE_API_KEY"
requires_openai_auth = false
wire_api = "responses"
```

Set `OMNIROUTE_API_KEY` only in a mode-600 launch environment file or an
operating-system secret manager. It must not be committed to Codex project
configuration. Create an opt-in free-session profile at
`~/.codex/omniroute-free.config.toml`:

```toml
# OMNIROUTE_API_KEY is loaded by the launcher, not stored here.
# Do not pin a model here. OmniRoot fetches the live catalog and passes
# --model / --provider on each launch.
model_provider = "omniroute"
model_reasoning_effort = "low"
web_search = "disabled"

# Required today: OmniRoute's Gemini translation rejects host App/MCP schemas.
[apps._default]
enabled = false
```

Create a user-owned launcher that reads its secret file and starts a separate
Codex process. Substitute your actual home-relative locations; do not copy a
different user's absolute path or credentials. The launcher needs mode 700 and
the environment file needs mode 600:

```bash
mkdir -p "$HOME/.config/omniroute" "$HOME/.local/bin"
chmod 700 "$HOME/.config/omniroute"
```

Write the following contents to `$HOME/.local/bin/chaosengine-omniroute`; do
not execute this block in the current shell:

```bash
#!/usr/bin/env bash
set -euo pipefail
readonly env_file="$HOME/.config/omniroute/chaosengine-free-coding.env"
readonly profile_file="$HOME/.codex/omniroute-free.config.toml"
[[ -r "$env_file" && -r "$profile_file" ]] || {
  printf '%s\n' 'OmniRoute free profile is not configured for this user.' >&2
  exit 2
}
set -a
# shellcheck disable=SC1090
. "$env_file"
set +a
exec codex --profile omniroute-free --disable apps "$@"
```

```bash
chmod 700 "$HOME/.local/bin/chaosengine-omniroute"
```

The secret file contains only a quoted `OMNIROUTE_API_KEY` assignment for the
local endpoint key (full access is fine), for example
`OMNIROUTE_API_KEY='retrieve-from-your-secret-manager'`. Prefer retrieving the
value from an operating-system secret manager when creating that file (for
example, `secret-tool lookup service omniroute key chaosengine-free-coding` on
Linux) instead of typing it. Do not place an upstream provider key there. Start a
bounded session with:

```bash
chmod 600 "$HOME/.config/omniroute/chaosengine-free-coding.env"
```

```bash
chaosengine-omniroute exec --ephemeral -C "$PWD" -s read-only \
  '<bounded task>'
```

This launches a separate Codex process. It is the verified path for a free
delegate-style task today. That child still loads the repository's `AGENTS.md`,
the canonical ChaosEngine skill, and the role named in its task instructions.
Keep its scope read-only and non-sensitive.

For bounded automatic delegation, canonical orchestration must probe the fixed
loopback endpoint before native fallback, with no endpoint prompt. Missing
operator config is normal. When that probe is `READY`, OmniRoot dispatches
through `chaosengine-omniroute` or `omniroute run`. Rank live catalog
free/remaining-first, then any other model the local endpoint can call.
OmniRoot never reads, writes, or persists route, model, or provider IDs. When
no callable model remains, the launcher may exit `78`; OmniRoot records
`RUNTIME_EXHAUSTED`, then the canonical workflow may use a native implementer.

### Current subagent limitation

Built-in `spawn_agent` cannot select OmniRoute. Do not claim that it can select
this free OmniRoute route.
The built-in all-free subagent path returns
`multi_agent_v1_spawn_agent` unsupported. A ChatGPT-authenticated parent also
rejects a cross-provider Gemini child. Named personal files such as
`omniroute_explorer.toml`, `omniroute_worker.toml`, and
`omniroute_tester.toml` are **not installed**. They remain inactive/quarantined
future wiring; do not create or recommend them as a working delegation
mechanism. Re-evaluate only after both host limitations are resolved and the
full acceptance sequence below passes.

Do not move architecture, terminal review, or confidential work to this
separate free session. Apps must remain disabled in its dedicated profile until
OmniRoute's Gemini adapter accepts the host App/MCP tool schemas.

## Acceptance checks

After the gateway is up and an endpoint key exists:

```bash
omniroute --version
omniroute doctor --no-liveness
omniroute health --json
curl --fail --silent http://127.0.0.1:20128/api/health
curl --fail --silent http://127.0.0.1:20128/v1/models \
  -H "Authorization: Bearer $OMNIROUTE_API_KEY"
python3 chaos-engine/skills/omniroot/scripts/runner.py probe
python3 chaos-engine/skills/omniroot/scripts/runner.py candidates --capability default
```

`probe` must print `READY`. Missing operator config is normal. Then prove a
child actually ran (parent-model text is not proof):

```bash
readonly acceptance_task='Before answering, load the applicable AGENTS.md, canonical ChaosEngine core, Caveman, Ponytail, selected SHAFT profile, and selected role. Run pwd. Return loaded file paths, role name, exact pwd command, its output, and git status --short before and after. Do not edit files or use network tools.'
chaosengine-omniroute exec --ephemeral -C "$PWD" -s read-only "$acceptance_task"
```

1. Live catalog returns at least one callable model. Prefer free/remaining
   first; other models the key can call are allowed.
2. `chaosengine-omniroute exec` or `omniroute run` returns the child's output.
3. Built-in `spawn_agent` is not an OmniRoute acceptance test. Do not treat a
   parent-model reply as proof that the local route ran.

Record the following facts in a private, secret-free inventory. “No account
created” is a valid status; do not fabricate account completion.

| Provider | Signup site | Email used | Auth type | Account status | Free allowance | Tested model | Human checkpoint | Verified date |
| --- | --- | --- | --- | --- | --- | --- | --- | --- |
| Example only | URL | private reference, not the address | API key / none | pending / active | current documented limit | exact ID | terms, privacy, billing checked | YYYY-MM-DD |

## Troubleshooting and rollback

### Secret-free OmniRoot qualification reasons

`python3 chaos-engine/skills/omniroot/scripts/runner.py probe` reports a
bounded secret-free state. Missing operator config is normal. Use the table
only when probe is not `READY`.

| State or `reasonCode` | Operator action |
| --- | --- |
| `ABSENT` / `UNHEALTHY` | Start loopback: `OMNIROUTE_SERVER_HOST=127.0.0.1 omniroute serve --port 20128 --no-open`. |
| `UNAUTHENTICATED` / `ENDPOINT_CREDENTIAL_MISSING` | Export `OMNIROUTE_API_KEY` or use the launcher env file. Full access is fine. |
| `LAUNCHER_UNQUALIFIED` | Put `chaosengine-omniroute` or `omniroute` on PATH as an owner-owned executable. |
| `RUNTIME_EXHAUSTED` | Wait for quota, or use another model the key can call. Native host models only if OmniRoute cannot run. |

- **`doctor` reports an unsupported Node runtime:** select a supported Node
  release, reinstall the same reviewed OmniRoute version, and rerun all checks.
- **Health fails:** run `omniroute serve --port 20128 --no-open` in the
  foreground, inspect sanitized output, and check `ss -ltnp | rg ':20128'`.
  Never change the listener to a public interface as a workaround.
- **A provider returns 401/403/429:** confirm the account, key scope, current
  quota, and provider terms. Prefer the next remaining free catalog model;
  other callable models are allowed. Do not create another account or bypass
  controls.
- **A model is missing or tool calls fail:** refresh the provider's current
  catalogue, retest the exact ID, and remove it from the selected route until
  it works.
- **Unexpected model or charge:** stop the gateway, disable the connection,
  revoke the endpoint and upstream keys, review OmniRoute logs for the exact
  route, and notify the provider if its billing page shows a charge.

To remove the integration, stop and disable the optional unit, remove the
free-session profile and launcher, remove any experimental personal agents and
provider block, restore the saved Codex backup, revoke endpoint/upstream keys,
then uninstall the user-global package:

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
- [Google AI Studio](https://aistudio.google.com/), [Gemini 3.1 Flash-Lite](https://ai.google.dev/gemini-api/docs/models/gemini-3.1-flash-lite), and [Gemini API pricing](https://ai.google.dev/gemini-api/docs/pricing)
- [Cohere trial-key rate limits](https://docs.cohere.com/v2/docs/rate-limits) and [trial-key onboarding](https://docs.cohere.com/v2/docs/going-live)
- [Cerebras Free Trial rate limits and billing requirement](https://inference-docs.cerebras.ai/support/rate-limits)
- [NVIDIA NIM hosted-endpoint terms](https://docs.api.nvidia.com/nim/docs/run-anywhere) and [API Catalog](https://build.nvidia.com/)
- [Ollama pricing](https://ollama.com/pricing) and [Ollama Cloud API](https://docs.ollama.com/cloud)
- [Cloudflare Workers AI pricing and free allowance](https://developers.cloudflare.com/workers-ai/platform/pricing/)
