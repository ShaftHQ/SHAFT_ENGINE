# CLI cache

Compact cache rule: name, when to use, and essential arguments only. Do not embed a JSON schema. Load this file only when the shaft skills entrypoint selects this skill.

## Command forms

Do not run `--help` for ordinary discovery.

- `shaft-cli call <TOOL> [key=value ...] [--args '<json>'] [--json] [--stdio-ok]`
- `shaft-cli session start`
- `shaft-cli session status`
- `shaft-cli session stop`
- `shaft-cli tools --cached [--json]`
- `shaft-cli codegen [--session <path>]`
- `shaft-cli setup catalog|profiles [--json]`
- `shaft-cli setup doctor|status|verify --profile REPORTING [--json]`
- `shaft-cli setup plan --profile REPORTING --mode MANAGED --output <absolute-plan.json>`
- `shaft-cli setup install|apply|update --plan <absolute-plan.json> --approve <sha256:digest>`
- `shaft-cli setup start|stop --profile <profile>`
- `shaft-cli setup logs --profile REPORTING`
- `shaft-cli browser navigate|screenshot|dom|url`
- `shaft-cli element click|type|hover`
- `shaft-cli capture start|stop|status|code|step-delete|step-reorder`
- `shaft-cli guide search`
- `shaft-cli doctor analyze|suggest|local-ai-status`

Browser, element, capture, and driver commands need `shaft-cli session start` unless the call is a deliberately disposable one-shot.

## Tools

Pass required names as `key=value`. A trailing `?` is optional.

| Tool | When | Args |
| --- | --- | --- |
| `autobot_local_agent_clients` | lists local SHAFT Autobot CLI clients that do not require SHAFT cloud API keys | - |
| `autobot_local_agent_run` | routes Ask, Plan, or approved Agent requests to Codex, Claude Code, or Copilot CLI | allowSourceMutation, client, command, environment, mode, prompt, timeoutSeconds, workingDirectory |
| `autobot_provider_chat` | routes Ask or Plan requests to configured SHAFT cloud providers without source mutation | allowSourceMutation, mode, model, prompt, provider, timeoutSeconds, workingDirectory |
| `autobot_provider_models` | discovers model IDs from the configured SHAFT provider without exposing responses, headers, or credentials | model, provider |
| `autobot_provider_status` | reports the configured SHAFT cloud provider, model, API-key presence (never the value), and structured-output support for the IntelliJ readiness view | model, provider |
| `browser_accessibility_audit` | runs a non-failing axe-core WCAG audit of the current page and returns the violations it finds | wcagTags |
| `browser_aria_snapshot` | captures an accessible-name-tree aria snapshot (YAML) of the whole page, or of one element when a locator is supplied; dispatches to the active engine | locatorStrategy, locatorValue |
| `browser_delete_cookies` | deletes a cookie by name, or every cookie when name is omitted; dispatches to the active engine; absorbs browser_delete_cookie/browser_delete_all_cookies | name? |
| `browser_get_current_url` | gets current URL; dispatches to the active engine | - |
| `browser_get_page_dom` | returns bounded current-page DOM for locator inspection before element_*; dispatches to the active engine | maxCharacters |
| `browser_get_title` | gets current page title; dispatches to the active engine | - |
| `browser_navigate` | opens a URL in the active browser; newWindow=true opens that URL in a new window instead of the current one | newWindow?, targetUrl |
| `browser_navigate_back` | navigates back to the previous page; dispatches to the active engine | - |
| `browser_navigate_forward` | navigates forward to the next page; dispatches to the active engine | - |
| `browser_network_requests` | lists the active browser's network transactions without bodies; optional id returns one redacted detail; DevTools trace only, not Playwright | id?, limit, urlFilter |
| `browser_open_intent` | opens a URL and returns bounded DOM plus capture-ranked locator candidates for the user intent; dispatches to the active engine | maxCharacters, maxElements, targetUrl, userIntent |
| `browser_refresh` | refreshes the current page; dispatches to the active engine | - |
| `browser_route` | mocks one browser request by method and URL glob and returns the given status, body, and headers; browser_unroute clears every route | method, responseBody, responseHeaders, responseStatus, url, urlGlob |
| `browser_set_window_size` | sets the browser window size; mode is custom with width and height, maximize, or fullscreen | height, mode?, width |
| `browser_storage_state_load` | loads cookies, localStorage, and sessionStorage from JSON into the active browser; open the target origin first | filePath |
| `browser_storage_state_save` | saves the active browser session's cookies, localStorage, and sessionStorage to a JSON file; dispatches to the active engine | filePath |
| `browser_take_screenshot` | takes a PNG screenshot of the current browser viewport; dispatches to the active engine (web, mobile, or Playwright) | includeBase64, outputPath |
| `browser_unroute` | clears every mock route on the active browser session; routeId is reference-only because a single rule cannot be removed | routeId |
| `capture_api_generate` | generates, compiles, and returns copy-paste SHAFT.API code blocks from recorded API transactions | className, excludedTransactionIds, openApiSpecPath, outputDirectory, overwrite, packageName, pinnedJsonPaths, replay, sessionPath, style, validationDepth |
| `capture_api_response_leaves` | classifies a session's response-body leaves as stable, volatile, or sensitive without generating test source | excludedTransactionIds, sessionPath |
| `capture_api_start` | starts API network capture; uses the mobile MITM proxy when a mobile engine or mobilePlatform is set, otherwise WEB capture | browser, headless, mobileDeviceLabel?, mobilePlatform?, networkOptions, outputPath?, targetUrl |
| `capture_api_status` | returns API capture status, transaction count, and recent endpoints for the active WEB or mobile MITM session | - |
| `capture_api_stop` | stops API capture for the active WEB session or a running mobile MITM session, using the same single-session lock as capture_stop | discard |
| `capture_api_transactions` | lists captured API transactions without bodies or sensitive headers; mobile reads the MITM proxy, and WEB can filter asset noise | excludePattern, includeAssets |
| `capture_backend_comparison` | compares WebDriver and Playwright Capture code-block outputs without editing source | className, driverVariableName, outputDirectory, overwrite, packageName, sessionPath |
| `capture_checkpoint` | records a USER_MARKER, ASSERTION, PAGE_TRANSITION, RECOVERY, FLOW_START, or FLOW_END checkpoint | description, kind |
| `capture_code_blocks` | generates a Java class snippet and insertion guidance from a saved recording; backend web, playwright, or mobile defaults to the active engine | backend?, className?, driverVariableName?, outputDirectory?, overwrite?, packageName?, sessionPath? |
| `capture_codegen_features` | returns Playwright codegen features and how SHAFT Capture/MCP maps each feature | - |
| `capture_evidence_pack` | returns a local manifest of Capture source, report, review UI, screenshots, and checks | reportPath, reviewPath, screenshotPaths, sourcePath |
| `capture_generate_replay` | generates, compiles, and optionally replays SHAFT code from a saved recording; backend web, playwright, or mobile defaults to the active engine | allowLocalAi?, allowRemoteAi?, backend?, className?, driverVariableName?, outputDirectory?, overwrite?, packageName?, replay?, sessionPath?, useAi? |
| `capture_pick_locator` | ranks caller-supplied locator candidates for a picked element and returns a copy-paste SHAFT.GUI.Locator snippet | candidates |
| `capture_record_at_target_code_blocks` | generates Capture snippets for an existing Java source anchor; backend web, playwright, or mobile defaults to the active engine | backend?, className?, driverVariableName?, insertAfter, outputDirectory?, overwrite?, packageName?, sessionPath?, targetSourcePath |
| `capture_set_mode` | reads or toggles the active recorder's live authoring mode: record (default) or inspect | mode |
| `capture_start` | starts a recording on the active engine; with no session it launches desktop web CDP capture and element_* drive that browser; codegenOptions is WEB-only | browser?, codegenOptions?, headless?, outputPath?, sessionGoal?, targetUrl? |
| `capture_status` | returns recorder status for the active web, Playwright, or mobile engine; the union's engine field names which status is set | - |
| `capture_step_delete` | deletes a Playwright or mobile recorded step by stepId; a WEB CDP recording has no step editor | stepId |
| `capture_step_reorder` | moves a Playwright or mobile recorded step up or down by stepId; a WEB CDP recording has no step editor | direction, stepId |
| `capture_stop` | stops the active engine's recording; pass the WEB outputPath to capture_generate_replay to prove the generated code | discard |
| `capture_target_candidates` | suggests existing Java Page Object or test targets for Capture record-at-target insertion | maxResults, repositoryPath |
| `design_analyze` | runs the requirements playbook on a story or workspace file and returns GAP IDs; writes nothing and does not draft Gherkin | acceptedGapIds, filePath, sourceUrl, text |
| `design_coverage` | maps AC IDs to Gherkin @AC-* scenario tags; uncovered AC blocks Ready unless waived with a reason | acceptedGapIds, filePath, gherkin, sourceUrl, text, waived |
| `design_examples` | proposes a Scenario Outline and editable Examples rows from an analyzed pack; never writes files | acceptedGapIds, droppedExampleIds, filePath, sourceUrl, text |
| `design_gap_map` | classifies accepted Gherkin steps as mapped to SHAFT fluent API, new-helper, needs-recording, or ambiguous; never writes production Java | acceptedGapIds, filePath, gherkin, sourceUrl, text |
| `design_gherkin_draft` | drafts declarative Gherkin from an analyzed story pack for human review; never writes files or Java | acceptedGapIds, filePath, sourceUrl, text |
| `design_handoff` | serializes a Ready Design pack for Automation prefill; sourceUrl is used only when the user supplied it, and it writes no Java | accept, acceptedGapIds, acceptedGherkinSnapshot, coverageWaived, filePath, gherkin, lintWaived, sourceUrl, text |
| `design_ingest` | turns pasted story text or a workspace file into a Design pack with actor, outcome, and numbered AC IDs; writes nothing | filePath, sourceUrl, text |
| `design_lexicon` | suggests or accepts project-local business phrases; never indexes SHAFT locator steps | action, phrase, query |
| `design_lint` | lints a Gherkin draft for missing Then, click/xpath smells, duplicates, and invented SHAFT APIs; fail closed on errors | acceptedGapIds, filePath, gherkin, sourceUrl, text, waived |
| `design_readiness` | evaluates Draft / Needs questions / Ready for a Design pack; handoffAllowed only when Ready after analysis, lint, coverage, and explicit accept | accept, acceptedGapIds, acceptedGherkinSnapshot, coverageWaived, filePath, gherkin, lintWaived, sourceUrl, text |
| `doctor_analyze_failed_allure` | analyzes failed Allure evidence into actions and copy-paste fixes; empty paths use the newest workspace evidence; backend web or playwright | allowLocalAi, allowRemoteAi, allowedSourcePaths, allureResultPaths, backend?, driverVariableName, historicalBundlePaths, includePageSnapshots, includeScreenshots, minimumAllureResults, outputDirectory, repositoryRoot, useAi |
| `doctor_analyze_trace` | analyzes a persisted SHAFT trace and returns deterministic Doctor remediation guidance | backend, tracePath |
| `doctor_cause_label` | confirms or suggests a persisted Doctor defect label for a signature; the store is local, and evidence paths are redacted | action, causeCategory?, evidencePath?, signature |
| `doctor_managed_local_ai_status` | returns read-only managed local AI configuration, eligibility, cache health, runtime and model diagnostics | - |
| `doctor_propose_advisory_locator` | maps a low-trust SHAFT Heal report to one reviewable advisory locator comment proposal without editing, replacing, or publishing | healingReportPath, outputDirectory, repositoryRoot, sourcePatchConsent, sourcePath |
| `doctor_propose_healed_locator` | maps a verified SHAFT Heal report to one reviewable locator patch without editing source or publishing | healingReportPath, outputDirectory, repositoryRoot, sourcePatchConsent, sourcePath |
| `doctor_suggest_fix` | returns copy-paste remediation blocks from an existing Doctor report; backend web or playwright selects the snippet engine | allowLocalAi, allowRemoteAi, allowedSourcePaths, backend?, driverVariableName, jsonReportPath, repositoryRoot, useAi |
| `driver_initialize` | launches a web, Playwright, or mobile session; engine is web, playwright, mobile_native, or mobile_web, and mobileOptions carries device setup | engine?, mobileOptions?, targetBrowser? |
| `driver_quit` | closes whichever engine session (web, mobile, or Playwright) is currently active | - |
| `element_clear` | clears text from an element; dispatches to the active engine and records the step when a mobile or Playwright recording is active | locatorStrategy, locatorValue |
| `element_click` | clicks an element; mode is single (default), double, or long, and records the step on mobile or Playwright | locatorStrategy, locatorValue, mode? |
| `element_count` | counts the elements a locator matches on the live page; dispatches to the active engine | locatorStrategy, locatorValue |
| `element_drag_and_drop` | drags an element onto a target locator, or by offsetX and offsetY when no target is given; records the step on Playwright | offsetX?, offsetY?, sourceLocatorStrategy, sourceLocatorValue, targetLocatorStrategy?, targetLocatorValue? |
| `element_highlight` | briefly outlines the elements a locator matches on the live page and returns the match count; dispatches to the active engine | locatorStrategy, locatorValue |
| `element_hover` | hovers over an element; dispatches to the active engine and records the step when a Playwright recording is active | locatorStrategy, locatorValue |
| `element_is_displayed` | checks if an element is displayed; dispatches to the active engine | locatorStrategy, locatorValue |
| `element_is_enabled` | checks if an element is enabled; dispatches to the active engine | locatorStrategy, locatorValue |
| `element_is_selected` | checks if an element is selected; dispatches to the active engine | locatorStrategy, locatorValue |
| `element_type` | types text into an element; append adds text and clear=false keeps existing text; records the step on mobile or Playwright | append?, clear?, locatorStrategy, locatorValue, text |
| `element_upload_file` | drops file to an element to upload; dispatches to the active engine and records the step when a Playwright recording is active | filePath, locatorStrategy, locatorValue |
| `generate_test_report` | generates a test report for the current session | - |
| `healer_run_failed_test` | reruns a failing SHAFT test, reads fresh Allure evidence, and returns review-only fixes; backend web or playwright | allowLocalAi, allowRemoteAi, allowedSourcePaths, backend?, driverVariableName, includePageSnapshots, includeScreenshots, maxAttempts, networkValidationApproved, outputDirectory, repositoryRoot, testCommand, useConfiguredAi |
| `mobile_activate_app` | activates an installed app by Android package or iOS bundle id | appId |
| `mobile_background_app` | sends the active mobile app to the background | seconds |
| `mobile_get_accessibility_tree` | gets the current Appium native accessibility XML tree or mobile web source | maxCharacters |
| `mobile_get_contexts` | gets Appium contexts plus current native XML or web DOM source | maxCharacters |
| `mobile_hide_keyboard` | hides the native mobile keyboard | - |
| `mobile_inspector_record_start` | prepares and starts a wrapped Appium Inspector recording session in one call, absorbing mobile_inspector_record_prepare | androidAbi, androidApiLevel, androidCores, androidDeviceProfile, androidImageTag, androidRamMb, app, appActivity, appPackage, bundleId, deviceName, includeSensitiveValues, openInspector, outputPath, platformName, platformVersion, provisionAndroidEmulator, selectedAndroidAvdName, udid |
| `mobile_inspector_record_status` | returns Appium Inspector recording status; optional action pause, resume, checkpoint, stop, or discard runs that control first | action?, checkpointName? |
| `mobile_inspector_record_stop` | stops a wrapped Appium Inspector recording and returns generated replay code | discard |
| `mobile_keyboard_key` | sends a native keyboard action such as DONE, SEARCH, GO, NEXT, or SEND | key |
| `mobile_rotate` | rotates the mobile device to PORTRAIT or LANDSCAPE | orientation |
| `mobile_swipe` | swipes on the mobile screen by text, a locator, a locator offset, or start and end coordinates | direction?, endX?, endY?, locatorStrategy?, locatorValue?, movement?, offsetX?, offsetY?, startX?, startY?, text? |
| `mobile_switch_context` | switches Appium context, for example NATIVE_APP or WEBVIEW_* | contextName |
| `mobile_tap_coordinates` | fallback-only: taps viewport coordinates only after locator-based element_click cannot be used | x, y |
| `mobile_toolchain_status` | checks local Appium, Inspector, adb, emulator, and SDK tooling status with repair diagnostics | platformName |
| `report_clusters` | clusters failed Allure results by Doctor historical-signature keys into error → impacted tests; deterministic, no cloud ML; empty results are empty-state | allureResultsPath?, doctorReportPath? |
| `report_flake` | builds separate intra-run and cross-launch flake tables; unknown history stays explicit, and CI commit metadata is optional | allureResultsPath?, doctorReportPath?, historyPath?, limitPerHistoryId?, transitionThreshold? |
| `report_heal` | dashboards Heal decisions with a persist-on-pass review gate; RECOVERED plus pass is reviewable, and it never writes locators | proposalsPath?, reportsPath? |
| `report_history` | ingests Allure history.jsonl and optional Doctor JSON for cross-run history; missing history is an empty state, not a failure | allureResultsPath?, doctorReportPath?, historyPath?, limitPerHistoryId? |
| `report_merge_shards` | merges N per-shard Allure/trace/doctor-intelligence blobs into one Allure result set plus a flaky-clustering speedboard HTML | outputDirectory, shardBlobPaths |
| `report_mute` | mutes a local flake with a required reason until N local passes; the store is gitignored .shaft/local-mutes.json unless a path is set | action?, muteStorePath?, passed?, reason?, recoverAfterPasses?, testId?, writeSurefireExcludes? |
| `report_open` | opens the newest Allure HTML report, or names generate_test_report when none exists; never rewrites the HTML | allureResultsPath?, openInBrowser?, reportPath? |
| `report_smart_tags` | tags Allure history as New, Always-failing, Flaky, Regressed, or Fixed; a first failure is New, and thin history is not Flaky | allureResultsPath?, doctorReportPath?, flakyTransitionThreshold?, historyPath?, limitPerHistoryId? |
| `report_summary` | builds engineer and stakeholder report summaries from Allure, flake, and heal counts, and never includes secrets | allureResultsPath?, doctorReportPath?, healProposalsPath?, healReportsPath?, historyPath?, reportPath? |
| `setup_catalog` | returns the deterministic SHAFT setup profiles, targets, and capabilities | - |
| `setup_doctor` | diagnoses a setup profile without installing, downloading, or starting services | request |
| `setup_install` | installs one reviewed setup plan after exact digest and license approval | acceptedLicenses?, approvedDigest, planJson, request |
| `setup_logs` | reads bounded logs for a SHAFT-owned setup service without mutating the host; profile-only calls remain supported as legacy capability queries | profile?, request? |
| `setup_plan` | creates an exact reviewable setup plan and approval digest without mutating the host | request |
| `setup_start` | starts one managed service from an exact reviewed setup plan; profile-only calls remain supported as legacy capability queries | acceptedLicenses?, approvedDigest?, planJson?, profile?, request? |
| `setup_status` | returns actual read-only readiness for a registered setup profile | request |
| `setup_stop` | stops one SHAFT-owned service using an exact reviewed setup plan; profile-only calls remain supported as legacy capability queries | acceptedLicenses?, approvedDigest?, planJson?, profile?, request? |
| `setup_verify` | verifies exact setup readiness without mutating the host | request |
| `shaft_coding_partner_diff` | previews a unified diff that inserts reviewed SHAFT code at a Java method or text anchor; never writes files | codeBlocks, insertionAnchor, repositoryPath, targetSourcePath |
| `shaft_coding_partner_plan` | plans a repository-aware SHAFT coding-partner workflow from source, selection, page objects, capture evidence, and verification | artifactPaths, backend, currentSourcePath, intent, maxResults, repositoryPath, selectedText |
| `shaft_guide_search` | searches the live official SHAFT user guide before writing SHAFT tests, page objects, locators, API, GUI, CLI, mobile, troubleshooting, or best-practice code | maxResults, query |
| `shaft_project_create` | creates a new SHAFT Maven project from the same examples and rules used by the guide generator | artifactId, groupId, includeDependabot, includeGithubActions, optionalModules, outputDirectory, overwrite, platform, runner, shaftVersion, version |
| `shaft_project_init_agents` | scaffolds SHAFT skill bridges and host instructions for one or all coding-agent loops; overwrite=true replaces only SHAFT-owned files | loop, overwrite, targetDirectory |
| `shaft_project_upgrade` | runs the existing SHAFT modular project upgrader script against the current Java project | allowAiRepair, approve, compileCommand, compileTimeout, dryRun, projectRoot, shaftVersion, skipBaselineCompile, upgradeType |
| `test_automation_scenarios` | returns SHAFT MCP scenarios, actions, patterns, guardrails, and done criteria for API, GUI, mobile, capture, Doctor, and CI | area, intent, maxResults |
| `test_code_guardrails_check` | checks SHAFT test code for sleeps, brittle locators, raw driver calls, headed setup, direct properties, and obvious secrets | code, language |
| `test_plan_explore` | crawls the active SHAFT browser from a URL and writes one deterministic Markdown test plan per discovered flow under specs/ | goal, maxDepth, maxPages, targetUrl |
| `trace_latest` | returns recent persisted SHAFT trace indexes from target/shaft-traces | maxResults |
| `trace_open_viewer` | resolves (extracting from the trace ZIP if needed) the offline SHAFT Trace Report HTML viewer for a persisted trace | tracePath |
| `trace_read` | returns redacted SHAFT trace JSON from a trace path with explicit output bounds | maxCharacters, tracePath |
| `trace_summarize` | returns a deterministic summary of a persisted SHAFT trace without AI | tracePath |
| `verify_run_focused` | runs guarded Maven compile, test-compile, test, verify, or package in the workspace and returns a bounded pass or fail summary | command, networkValidationApproved, repositoryRoot |
