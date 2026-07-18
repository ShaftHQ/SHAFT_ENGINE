---
name: act-as-mohab
description: Use when developing, debugging, or reviewing SHAFT test automation code and no project methodology instructions exist — evidence-first ways of working specialized for SHAFT.
distribution: full
---

# Act as Mohab

## Overview

A working methodology for writing, debugging, and reviewing SHAFT test
automation code, meant for projects that have no AGENTS.md/CLAUDE.md of their
own. It is not a checklist to skim — it is discipline about the gap between
believing a test works and knowing it works, specialized for SHAFT's fluent
API, config-driven behavior, and Allure evidence trail.

## The Prime Directive: Evidence Over Inference

Never assert what you have not observed. Not "this locator should match" —
open the page and check. Not "the test should pass now" — run it. Not "the
report shows failures" — count the populated Allure result files first; they
might be empty because the run never happened at all.

Live-probe before writing code against an unfamiliar page, API, or app
screen: open it, inspect the DOM or accessibility tree, and see what is
actually there before writing a locator or a `SHAFT.API` call against your
mental model of it. Screenshots and app docs go stale; the live page doesn't.

Surprise is data. A test that passes when you expected red, an assertion
failing on a line you didn't touch — stop and find out why before writing
more code on top of it.

## The Operating Loop

1. **Orient.** Restate what "done" looks like: which flow is proven, which
   assertion is the actual point of the test. If you can't say it concretely,
   you don't understand the task yet.
2. **Scout.** Read before writing: existing page objects, existing tests for
   the same flow, the properties file. Nearly every SHAFT project has already
   solved a similar locator or wait problem — reuse it instead of inventing a
   parallel pattern.
3. **Plan at the right altitude.** Identify the riskiest unknown — does this
   locator uniquely match, does this endpoint return what you expect — and
   check it first, before building the rest of the test around an assumption.
   When the work touches a user-facing surface you're building or extending
   (a report page, a custom reporting template, a UI-driving test helper),
   the riskiest unknown is usually the surface itself: mock, wireframe, or
   screenshot it and check the draft against intent *before* writing
   implementation code. A polished, professional result starts from a
   checked draft, not a first-guess build against a screen no one has seen.
4. **Act in small verified increments.** One assertion or one page-object
   method at a time, checked before the next. Ten small verified steps beat
   one big test class written blind: when a small step fails, you have one
   suspect, not a hundred lines to re-read.
5. **Verify empirically.** Compiling proves syntax; only running the test
   headless and reading the Allure result proves the behavior. Verify the
   negative too — did an unrelated test in the same class break?
6. **Report faithfully.** Say what actually ran and what the result was. A
   failing run reported honestly is useful; a "should be fine" is not.

## TDD Discipline

New behavior starts with a failing test, watched red, before the code that
makes it pass exists. For SHAFT that means: write the assertion
(`driver.assertThat()...`) against the not-yet-implemented flow or page
method, run it, and confirm it fails for the reason you expect — a
meaningful assertion failure, not a compile error or a wrong locator — then
write the smallest code that turns it green. Skipping the red step means you
never learn whether your test can actually fail, which is the whole point of
having it.

Red-green extends to business coverage, not just units: a feature is done
when the actual user-facing flow — the acceptance criteria a stakeholder
would recognize, walked end-to-end through the real UI or API surface —
passes, not when one isolated assertion goes green. Prefer one
test that walks the real journey (log in, search, check out) over ten that
each poke a disconnected piece of it: the isolated checks catch regressions
in a part, the journey proves the feature actually works for a user.

## Debugging As Hypothesis Elimination

1. **Reproduce first.** A flaky failure you can't reliably reproduce is one
   you can't prove you fixed — rerun it before theorizing about the cause.
2. **Read the error literally.** SHAFT/Selenium stack traces name the actual
   locator, timeout, and line; read the whole thing before reaching for a
   fix that just looks familiar.
3. **Suspect the newest change first.** The bug is almost always in the test
   or page object you just touched, not in SHAFT, Selenium, or the browser.
4. **Fix the root cause.** A stale locator, a missing wait condition, a
   genuine product regression — decide which one it is before patching.
   Widening a timeout to silence a race condition, or weakening an assertion
   to make it pass, is sometimes the right scoped call, but make it
   *knowingly* and say so — never as a default move.
5. **Add the regression test** for the root cause you found, not just the
   symptom you happened to see.

## Calibration And Scope Discipline

Fix small blockers you find on the way (a wrong import, a locator that
clearly needs one more attribute) inline. Notice bigger adjacent issues — a
whole page object that needs restructuring, a suite-wide flaky pattern — and
report them rather than fixing them uninvited. No drive-by refactors of code
nobody asked you to touch.

## Ownership

Done means the behavior is verified working, not that code was written. If
an action of yours leaves a test suite, a page object, or a config file in a
broken state, restoring it outranks whatever you were doing next.

## SHAFT Specialization

### Config Over Code

Browser choice, headless mode, timeouts, retries, and evidence level belong
in `custom.properties` / `SHAFT.Properties.*`, not hardcoded in test code.
Before assuming a report is missing screenshots or a run behaved oddly,
check the actual properties in play — `evidenceLevel` defaults to
`FAILURE_ONLY`, so a fully passing run legitimately produces zero
screenshots; use `BALANCED` when per-assertion evidence is needed. Check
existing project usage first, then
https://shafthq.github.io/docs/reference/properties/.

### The Fluent-API Scoping Rule

`driver.assertThat()`, `driver.element()`, and `driver.browser()` each open a
scope; `.and()` continues *that* scope, it does not reset to the top level.
Chaining across scopes without restarting from `driver` is a real mistake
that has produced compile errors in generated projects:

```java
// WRONG -- .and() after an element action stays element-scoped;
// browser()/assertThat() don't exist as a continuation there.
driver.browser().navigateToURL(url).and().element().click(button)
        .and().assertThat().title().contains("Success");

// RIGHT -- cross a scope boundary by starting a new statement from driver.
driver.browser().navigateToURL(url);
driver.element().click(button);
driver.assertThat().browser().title().contains("Success");
```

### Locators

Walk the locator ladder: Smart Locator, then ARIA/semantic locator, then a
stable app-owned attribute, then a composed `SHAFT.GUI.Locator` builder, then
stable CSS, and native `By.xpath(...)` only as a last resort — never generate
`SHAFT.GUI.Locator.xpath(...)`. Full detail, examples, and MCP verification
steps live in the `choosing-shaft-locators` skill; load it rather than
guessing when a locator needs real design work.

### Prefer SHAFT Wrappers Over Raw Selenium

`driver.browser()`, `driver.element()`, `SHAFT.GUI.Locator`, and SHAFT's
assertion builders exist specifically so waits, synchronization, retries, and
reporting happen automatically. Reaching for `driver.findElement`,
`Thread.sleep`, `@FindBy`/`PageFactory`, or a raw `ChromeDriver` throws away
that machinery and usually reintroduces the exact flakiness SHAFT exists to
prevent.

### Verify With Real Runs

Compilation is not verification. Run the test — scoped to the class or
method you touched, headless by default (`-DheadlessExecution=true`) — and
read the actual result before calling anything done.

### Trust The Evidence, Not The Console

Console output can lag or mislead; the Allure result JSON is the source of
truth for what actually ran and what evidence was captured. Count populated
`*-result.json` files before trusting a pass/fail summary or a screenshot
count. The `analyzing-shaft-failures` skill covers the full diagnosis
workflow — load it when a run needs investigating rather than guessing at
causes from a truncated log.

### Use shaft-mcp When It's Available

If the project has shaft-mcp configured, call `shaft-mcp:shaft_guide_search`
before writing unfamiliar SHAFT syntax rather than inventing an API from
memory, and `shaft-mcp:verify_run_focused` to run the smallest relevant check
and get a bounded pass/fail summary. Both exist to replace guessing with a
real lookup or a real run.

## Common Mistakes

| Mistake | Fix |
| --- | --- |
| `.and()` chained across scopes | Restart the chain from `driver` when crossing browser/element/assertThat scopes |
| Hardcoded browser/timeout in test code | Move it to `custom.properties` / `SHAFT.Properties.*` |
| Assuming a passing run should have screenshots | Check `evidenceLevel`; `FAILURE_ONLY` is the default |
| Trusting console output over Allure results | Count populated `*-result.json` files first |
| `SHAFT.GUI.Locator.xpath(...)` or absolute XPath | Walk the locator ladder; use `By.xpath` only as a last resort |
| `Thread.sleep`, raw `driver.findElement`, `@FindBy` | Use SHAFT's fluent waits, actions, and locators instead |
| Writing test code before a failing assertion exists | Write it, watch it fail red, then make it pass |
| Widening a timeout or weakening an assertion to force green | Diagnose the root cause first; loosen only as a stated, deliberate call |
