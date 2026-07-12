# Selenium WebDriver BiDi

## Model
BiDi is a WebSocket protocol alongside classic WebDriver HTTP. Selenium 4.4x
exposes it via `Augmenter`/`HasBiDi`. Key domains: `script` (preload scripts,
evaluate/callFunction), `browsingContext` (tree, navigation, screenshots),
`network` (intercept/continue), `log`. Everything is per-browsing-context;
"the page" is never one context — every iframe, popup, and worker-adjacent
frame is its own context with its own lifecycle.

## Rules that prevent real SHAFT bugs
- `script.addPreloadScript` runs in EVERY browsing context, including
  same-origin subframes. Any injected UI or persisted state must be gated
  `window === window.top`; same-origin subframes SHARE the top frame's
  sessionStorage, so a subframe restoring "persisted UI state" creates
  phantom events (SHAFT recorder incident, PR #3432: phantom Navigate rows).
- Injection order races with subframe load timing are real and CI-speed
  dependent. Deterministic repro trick: delay a fixture iframe's response
  (`?frameDelay=900`) to force the slow-CI initialization order locally.
- Channel messages (`script.message` from `channel` callbacks) arrive on the
  WebSocket dispatch thread — never block it, queue and process elsewhere.
- BiDi session lifetime is tied to the driver; a recording/capture session
  must own a driver whose lifetime outlives the initiating request. Never
  start a BiDi-dependent recording inside an ephemeral process/turn that
  exits right after (SHAFT incident: recordings died seconds after start
  because capture ran inside a one-shot agent child, PR #3431).
- Not all SHAFT driver backends guarantee BiDi (remote grids, some mobile
  paths) — feature-detect, degrade gracefully, never assume.

## Network capture
`network.addIntercept` + `continueRequest/continueResponse` for mutation;
passive capture via `network.responseCompleted` events. Response bodies need
`network.getData`/data collectors (size-capped) — capture headers/timings
eagerly, bodies lazily. SHAFT's `BrowserNetworkInterceptor` (DevTools) and
`HttpContractRecorder` already normalize contracts — compose, don't rebuild.

## Debugging
- Enable BiDi wire logging first (`-Dwebdriver.remote.verbose` /
  Selenium's `BiDi` logger) — most "BiDi bugs" are ordering, visible in the log.
- Screenshot via `browsingContext.captureScreenshot` works on non-focused
  contexts; classic screenshot only on the active top context.
