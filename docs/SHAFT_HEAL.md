# SHAFT Heal

SHAFT Heal is an optional, action-scoped WebDriver locator recovery provider.
It is disabled by default and is delivered by
`io.github.shafthq:shaft-heal`. The required `shaft-engine` artifact owns the
integration SPI and continues to throw the original locator failure when the
provider is absent, rejects every candidate, or encounters an internal error.

## Add and enable the module

Import `shaft-bom`, then add the optional module beside `shaft-engine`:

```xml
<dependency>
    <groupId>io.github.shafthq</groupId>
    <artifactId>shaft-heal</artifactId>
</dependency>
```

Enable deterministic recovery explicitly:

```properties
healing.strategy=shaft-heal
```

The equivalent current-thread override is:

```java
SHAFT.Properties.healing.set()
        .strategy("shaft-heal")
        .minimumConfidence(0.75)
        .ambiguityMargin(0.10);
```

SHAFT first attempts the original locator. Recovery runs only after a web
locator-not-found result, and the engine executes the action only after the
provider returns exactly one validated element.

## Strategy and Healenium coexistence

| Configuration | Effective behavior |
| --- | --- |
| `healing.strategy=disabled` | No SHAFT Heal. Legacy `heal-enabled=true` still enables Healenium for compatibility. |
| `healing.strategy=healenium` | Healenium only. |
| `healing.strategy=shaft-heal` | SHAFT Heal only, even when `heal-enabled=true` remains in an older property file. |
| `healing.strategy=composite` | Healenium driver wrapping plus SHAFT Heal fallback. |

Use `healing.strategy=composite` only after validating both systems against the
same application. It is an explicit opt-in because both providers can add
latency and maintain separate histories.

## Deterministic decision policy

The provider stores a successful element fingerprint and later discovers a
bounded candidate set from stable semantic evidence:

- accessible name and associated label
- configured test IDs
- stable ID and name
- role, type, placeholder, title, alt, and autocomplete
- visible text
- a checksum of the structural DOM path

Each evidence category is scored separately. A candidate is used only when it
is unique, remains in the requested frame or shadow context, satisfies the
action preconditions, meets `healing.minimumConfidence`, and leads the next
eligible candidate by at least `healing.ambiguityMargin`. Ties and low
confidence preserve the original failure.

## History and reports

History is local, versioned, checksummed, bounded, retention-limited, and
written atomically:

```properties
healing.history.enabled=true
healing.history.path=target/shaft-heal/history.json
healing.history.maxEntries=500
healing.history.retentionDays=30
```

Attempt reports are attached to Allure and written below
`target/shaft-heal/reports`. Reports include the failed locator, normalized
failure category, ranked candidates, per-category scores, selected candidate,
decision reason, provider metadata, and post-action outcome. Runtime code can
inspect the current thread's latest report with `ShaftHeal.lastReport()`.

Successful original resolutions seed history. A recovered fingerprint replaces
the old record only after the engine reports that the intended action
succeeded.

## Privacy and optional providers

SHAFT Heal uses a whitelist-only evidence model. It does not collect input
values, cookies, authorization headers, full page source, or the full DOM.
Password fields omit text, labels, accessible names, placeholders, and titles.
URLs exclude query strings and fragments, and common token or credential
patterns are redacted.

Visual comparison is local and disabled by default:

```properties
healing.visual.enabled=false
```

When enabled, add `shaft-visual`. Its `HealingVisualProvider` compares the
stored element screenshot with candidate screenshots. Visual scores remain
separate and cannot bypass the deterministic confidence threshold.

AI reranking is also disabled by default:

```properties
healing.ai.enabled=false
```

AI uses the provider-neutral controls from `shaft-pilot-core`. Add `shaft-ai`
only for direct OpenAI, Anthropic, Gemini, or Ollama calls, and configure the
Pilot approval, evidence, budget, timeout, and provider properties separately.
Only minimized candidate records are eligible for transfer. Provider output may
rerank supplied candidate IDs but cannot invent an element or override
deterministic acceptance.

## Source changes and limits

SHAFT Heal never edits test source during runtime. The
`healing.sourcePatch.enabled=false` property is a consent gate reserved for a
separate reviewed SHAFT Doctor proposal workflow; this module always reports
`sourcePatchProposed=false`.

The current provider supports web element actions in the active browsing
context. Native mobile recovery is excluded. Frame identity is retained, and
shadow-content recovery is supported while the configured shadow host remains
resolvable. A changed frame locator or shadow-host locator preserves the
original failure.
