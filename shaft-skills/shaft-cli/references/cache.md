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
| `autobot_provider_models` | discovers model IDs from the configured SHAFT provider without exposing responses, headers, o... | model, provider |
| `autobot_provider_status` | reports the configured SHAFT cloud provider, model, API-key presence (never the value), and s... | model, provider |
| `browser_accessibility_audit` | runs a non-asserting axe-core WCAG accessibility audit on the current page and returns the vi... | wcagTags |
| `browser_aria_snapshot` | captures an accessible-name-tree aria snapshot (YAML) of the whole page, or of one element wh... | locatorStrategy, locatorValue |
| `browser_delete_cookies` | deletes a cookie by name, or every cookie when name is omitted; dispatches to the active engi... | name? |
| `browser_get_current_url` | gets current URL; dispatches to the active engine | - |
| `browser_get_page_dom` | returns bounded current-page DOM for locator inspection before element_*; dispatches to the a... | maxCharacters |
| `browser_get_title` | gets current page title; dispatches to the active engine | - |
| `browser_navigate` | opens a URL in the active engine's browser session; optional newWindow (default false) opens... | newWindow?, targetUrl |
| `browser_navigate_back` | navigates back to the previous page; dispatches to the active engine | - |
| `browser_navigate_forward` | navigates forward to the next page; dispatches to the active engine | - |
| `browser_network_requests` | lists the active browser session's observed network transactions (method, URL, status, mimeTy... | id?, limit, urlFilter |
| `browser_open_intent` | opens a URL and returns bounded DOM plus capture-ranked locator candidates for the user inten... | maxCharacters, maxElements, targetUrl, userIntent |
| `browser_refresh` | refreshes the current page; dispatches to the active engine | - |
| `browser_route` | registers a mock network route on the active browser session, matching by optional HTTP metho... | method, responseBody, responseHeaders, responseStatus, url, urlGlob |
| `browser_set_window_size` | sets the browser window to a specific size; optional mode selects custom (default, uses width... | height, mode?, width |
| `browser_storage_state_load` | loads cookies, localStorage, and sessionStorage from a JSON file into the active browser sess... | filePath |
| `browser_storage_state_save` | saves the active browser session's cookies, localStorage, and sessionStorage to a JSON file;... | filePath |
| `browser_take_screenshot` | takes a PNG screenshot of the current browser viewport; dispatches to the active engine (web,... | includeBase64, outputPath |
| `browser_unroute` | clears registered browser mock routes for the active session; routeId is accepted for referen... | routeId |
| `capture_api_generate` | generates, compiles, and returns copy-paste SHAFT.API code blocks from recorded API transactions | className, excludedTransactionIds, openApiSpecPath, outputDirectory, overwrite, packageName, pinnedJsonPaths, replay, sessionPath, style, validationDepth |
| `capture_api_response_leaves` | returns classified response-body leaves (stable/volatile/sensitive) per transaction for a rec... | excludedTransactionIds, sessionPath |
| `capture_api_start` | starts SHAFT Capture with API network recording enabled; dispatches to the mobile loopback MI... | browser, headless, mobileDeviceLabel?, mobilePlatform?, networkOptions, outputPath?, targetUrl |
| `capture_api_status` | returns SHAFT Capture session status including network transaction count and recent endpoints... | - |
| `capture_api_stop` | stops SHAFT API Capture with the same single-session lock guarantee as capture_stop; dispatch... | discard |
| `capture_api_transactions` | returns captured network transactions without bodies or sensitive headers; supports filtering... | excludePattern, includeAssets |
| `capture_backend_comparison` | compares WebDriver and Playwright Capture code-block outputs without editing source | className, driverVariableName, outputDirectory, overwrite, packageName, sessionPath |
| `capture_checkpoint` | records a USER_MARKER, ASSERTION, PAGE_TRANSITION, RECOVERY, FLOW_START, or FLOW_END checkpoint | description, kind |
| `capture_code_blocks` | generates a Java full-class snippet plus agent guidance for repo-aware insertion from a persi... | backend?, className?, driverVariableName?, outputDirectory?, overwrite?, packageName?, sessionPath? |
| `capture_codegen_features` | returns Playwright codegen features and how SHAFT Capture/MCP maps each feature | - |
| `capture_evidence_pack` | returns a local manifest of Capture source, report, review UI, screenshots, and checks | reportPath, reviewPath, screenshotPaths, sourcePath |
| `capture_generate_replay` | generates, compiles, optionally replays, and returns copy-paste SHAFT code blocks from a pers... | allowLocalAi?, allowRemoteAi?, backend?, className?, driverVariableName?, outputDirectory?, overwrite?, packageName?, replay?, sessionPath?, useAi? |
| `capture_pick_locator` | ranks caller-supplied locator candidates for a picked element and returns a copy-paste SHAFT.... | candidates |
| `capture_record_at_target_code_blocks` | generates focused Capture snippets for insertion at an existing Java source anchor; optional... | backend?, className?, driverVariableName?, insertAfter, outputDirectory?, overwrite?, packageName?, sessionPath?, targetSourcePath |
| `capture_set_mode` | reads or toggles the active recorder's live authoring mode: record (default) or inspect | mode |
| `capture_start` | starts a recording, dispatching to the active engine: WEB (or no active session) always launc... | browser?, codegenOptions?, headless?, outputPath?, sessionGoal?, targetUrl? |
| `capture_status` | returns recorder status for the active engine: WEB (or no active session) returns SHAFT Captu... | - |
| `capture_step_delete` | deletes a recorded step by its stable stepId from the active Playwright or mobile recording;... | stepId |
| `capture_step_reorder` | moves a recorded step up or down by its stable stepId (direction: up/down) within the active... | direction, stepId |
| `capture_stop` | stops the active recording for the active engine; after WEB reaches COMPLETED, show outputPat... | discard |
| `capture_target_candidates` | suggests existing Java Page Object or test targets for Capture record-at-target insertion | maxResults, repositoryPath |
| `design_analyze` | runs the requirements-analysis playbook on a story or workspace file and returns a gap regist... | acceptedGapIds, filePath, sourceUrl, text |
| `design_coverage` | maps AC IDs to Gherkin @AC-* scenario tags; uncovered AC blocks Ready unless waived with a re... | acceptedGapIds, filePath, gherkin, sourceUrl, text, waived |
| `design_examples` | proposes a Scenario Outline and editable Examples rows from an analyzed pack; never writes files | acceptedGapIds, droppedExampleIds, filePath, sourceUrl, text |
| `design_gap_map` | classifies accepted Gherkin steps as mapped to SHAFT fluent API, new-helper, needs-recording,... | acceptedGapIds, filePath, gherkin, sourceUrl, text |
| `design_gherkin_draft` | drafts declarative Gherkin from an analyzed story pack for human review; never writes files o... | acceptedGapIds, filePath, sourceUrl, text |
| `design_handoff` | serializes a Ready Design pack for Automation prefill (scenarios, AC traces, examples, gap ma... | accept, acceptedGapIds, acceptedGherkinSnapshot, coverageWaived, filePath, gherkin, lintWaived, sourceUrl, text |
| `design_ingest` | turns pasted user-story or acceptance-criteria text, or a workspace file, into a structured D... | filePath, sourceUrl, text |
| `design_lexicon` | suggests or accepts project-local business phrases; never indexes SHAFT locator steps | action, phrase, query |
| `design_lint` | lints a Gherkin draft for missing Then, click/xpath smells, duplicates, and invented SHAFT AP... | acceptedGapIds, filePath, gherkin, sourceUrl, text, waived |
| `design_readiness` | evaluates Draft / Needs questions / Ready for a Design pack; handoffAllowed only when Ready a... | accept, acceptedGapIds, acceptedGherkinSnapshot, coverageWaived, filePath, gherkin, lintWaived, sourceUrl, text |
| `doctor_analyze_failed_allure` | analyzes failed Allure results and returns deterministic actions plus copy-paste code blocks;... | allowLocalAi, allowRemoteAi, allowedSourcePaths, allureResultPaths, backend?, driverVariableName, historicalBundlePaths, includePageSnapshots, includeScreenshots, minimumAllureResults, outputDirectory, repositoryRoot, useAi |
| `doctor_analyze_trace` | analyzes a persisted SHAFT trace and returns deterministic Doctor remediation guidance | backend, tracePath |
| `doctor_cause_label` | confirms or suggests persisted Doctor defect labels keyed by signature (PRODUCT/TEST/ENVIRONM... | action, causeCategory?, evidencePath?, signature |
| `doctor_managed_local_ai_status` | returns read-only managed local AI configuration, eligibility, cache health, runtime and mode... | - |
| `doctor_propose_advisory_locator` | maps a low-trust SHAFT Heal report to one reviewable advisory locator comment proposal withou... | healingReportPath, outputDirectory, repositoryRoot, sourcePatchConsent, sourcePath |
| `doctor_propose_healed_locator` | maps a verified SHAFT Heal report to one reviewable locator patch without editing source or p... | healingReportPath, outputDirectory, repositoryRoot, sourcePatchConsent, sourcePath |
| `doctor_suggest_fix` | returns copy-paste remediation code blocks from an existing SHAFT Doctor report; optional bac... | allowLocalAi, allowRemoteAi, allowedSourcePaths, backend?, driverVariableName, jsonReportPath, repositoryRoot, useAi |
| `driver_initialize` | launches browser, Playwright, or a mobile session; optional engine selects web (default) / pl... | engine?, mobileOptions?, targetBrowser? |
| `driver_quit` | closes whichever engine session (web, mobile, or Playwright) is currently active | - |
| `element_clear` | clears text from an element; dispatches to the active engine and records the step when a mobi... | locatorStrategy, locatorValue |
| `element_click` | clicks an element; optional mode selects single (default) / double / long; dispatches to the... | locatorStrategy, locatorValue, mode? |
| `element_count` | counts the elements a locator matches on the live page; dispatches to the active engine | locatorStrategy, locatorValue |
| `element_drag_and_drop` | drags and drops an element from source to a target locator, or by offsetX/offsetY when no tar... | offsetX?, offsetY?, sourceLocatorStrategy, sourceLocatorValue, targetLocatorStrategy?, targetLocatorValue? |
| `element_highlight` | briefly outlines the elements a locator matches on the live page and returns the match count;... | locatorStrategy, locatorValue |
| `element_hover` | hovers over an element; dispatches to the active engine and records the step when a Playwrigh... | locatorStrategy, locatorValue |
| `element_is_displayed` | checks if an element is displayed; dispatches to the active engine | locatorStrategy, locatorValue |
| `element_is_enabled` | checks if an element is enabled; dispatches to the active engine | locatorStrategy, locatorValue |
| `element_is_selected` | checks if an element is selected; dispatches to the active engine | locatorStrategy, locatorValue |
| `element_type` | types text into an element; optional append (default false) appends instead of clearing first... | append?, clear?, locatorStrategy, locatorValue, text |
| `element_upload_file` | drops file to an element to upload; dispatches to the active engine and records the step when... | filePath, locatorStrategy, locatorValue |
| `generate_test_report` | generates a test report for the current session | - |
| `healer_run_failed_test` | reruns a failing SHAFT/Selenium test, analyzes fresh Allure evidence, and returns review-only... | allowLocalAi, allowRemoteAi, allowedSourcePaths, backend?, driverVariableName, includePageSnapshots, includeScreenshots, maxAttempts, networkValidationApproved, outputDirectory, repositoryRoot, testCommand, useConfiguredAi |
| `mobile_activate_app` | activates an installed app by Android package or iOS bundle id | appId |
| `mobile_background_app` | sends the active mobile app to the background | seconds |
| `mobile_get_accessibility_tree` | gets the current Appium native accessibility XML tree or mobile web source | maxCharacters |
| `mobile_get_contexts` | gets Appium contexts plus current native XML or web DOM source | maxCharacters |
| `mobile_hide_keyboard` | hides the native mobile keyboard | - |
| `mobile_inspector_record_start` | prepares and starts a wrapped Appium Inspector recording session in one call, absorbing mobil... | androidAbi, androidApiLevel, androidCores, androidDeviceProfile, androidImageTag, androidRamMb, app, appActivity, appPackage, bundleId, deviceName, includeSensitiveValues, openInspector, outputPath, platformName, platformVersion, provisionAndroidEmulator, selectedAndroidAvdName, udid |
| `mobile_inspector_record_status` | returns the wrapped Appium Inspector recording status; optional action (pause/resume/checkpoi... | action?, checkpointName? |
| `mobile_inspector_record_stop` | stops a wrapped Appium Inspector recording and returns generated replay code | discard |
| `mobile_keyboard_key` | sends a native keyboard action such as DONE, SEARCH, GO, NEXT, or SEND | key |
| `mobile_rotate` | rotates the mobile device to PORTRAIT or LANDSCAPE | orientation |
| `mobile_swipe` | swipes on the mobile screen; dispatches on whichever optional params are supplied -- text (sw... | direction?, endX?, endY?, locatorStrategy?, locatorValue?, movement?, offsetX?, offsetY?, startX?, startY?, text? |
| `mobile_switch_context` | switches Appium context, for example NATIVE_APP or WEBVIEW_* | contextName |
| `mobile_tap_coordinates` | fallback-only: taps viewport coordinates only after locator-based element_click cannot be used | x, y |
| `mobile_toolchain_status` | checks local Appium, Inspector, adb, emulator, and SDK tooling status with repair diagnostics | platformName |
| `report_clusters` | clusters failed Allure results by Doctor historical-signature keys into error → impacted test... | allureResultsPath?, doctorReportPath? |
| `report_flake` | builds a dual flake table with separate retry-hidden (intra-run) and cross-launch transition... | allureResultsPath?, doctorReportPath?, historyPath?, limitPerHistoryId?, transitionThreshold? |
| `report_heal` | dashboards SHAFT Heal insights by HealingDecision status with persist-on-pass review gate; RE... | proposalsPath?, reportsPath? |
| `report_history` | ingests Allure history.jsonl plus optional Doctor JSON for cross-run history in the Reporting... | allureResultsPath?, doctorReportPath?, historyPath?, limitPerHistoryId? |
| `report_merge_shards` | merges N per-shard Allure/trace/doctor-intelligence blobs into one Allure result set plus a f... | outputDirectory, shardBlobPaths |
| `report_mute` | local flake mute/quarantine with required reason and recover-after-N-local-passes; never writ... | action?, muteStorePath?, passed?, reason?, recoverAfterPasses?, testId?, writeSurefireExcludes? |
| `report_open` | opens the newest Allure HTML report when present; empty-state returns generate_test_report CT... | allureResultsPath?, openInBrowser?, reportPath? |
| `report_smart_tags` | computes New / Always-failing / Flaky / Regressed / Fixed smart tags from Allure history; fir... | allureResultsPath?, doctorReportPath?, flakyTransitionThreshold?, historyPath?, limitPerHistoryId? |
| `report_summary` | builds engineer and stakeholder Reporting summaries with reconciled Allure counts, flake and... | allureResultsPath?, doctorReportPath?, healProposalsPath?, healReportsPath?, historyPath?, reportPath? |
| `setup_catalog` | returns the deterministic SHAFT setup profiles, targets, and capabilities | - |
| `setup_doctor` | diagnoses a setup profile without installing, downloading, or starting services | request |
| `setup_install` | installs one reviewed setup plan after exact digest and license approval | acceptedLicenses?, approvedDigest, planJson, request |
| `setup_logs` | reads bounded logs for a SHAFT-owned setup service without mutating the host; profile-only ca... | profile?, request? |
| `setup_plan` | creates an exact reviewable setup plan and approval digest without mutating the host | request |
| `setup_start` | starts one managed service from an exact reviewed setup plan; profile-only calls remain suppo... | acceptedLicenses?, approvedDigest?, planJson?, profile?, request? |
| `setup_status` | returns actual read-only readiness for a registered setup profile | request |
| `setup_stop` | stops one SHAFT-owned service using an exact reviewed setup plan; profile-only calls remain s... | acceptedLicenses?, approvedDigest?, planJson?, profile?, request? |
| `setup_verify` | verifies exact setup readiness without mutating the host | request |
| `shaft_coding_partner_diff` | produces a preview-only unified diff that inserts reviewed SHAFT code blocks into an existing... | codeBlocks, insertionAnchor, repositoryPath, targetSourcePath |
| `shaft_coding_partner_plan` | plans a repository-aware SHAFT IntelliJ coding-partner workflow using current source, selecte... | artifactPaths, backend, currentSourcePath, intent, maxResults, repositoryPath, selectedText |
| `shaft_guide_search` | searches the live official SHAFT user guide before writing SHAFT tests, page objects, locator... | maxResults, query |
| `shaft_project_create` | creates a new SHAFT Maven project from the same examples and rules used by the guide generator | artifactId, groupId, includeDependabot, includeGithubActions, optionalModules, outputDirectory, overwrite, platform, runner, shaftVersion, version |
| `shaft_project_init_agents` | scaffolds SHAFT skill bridges and managed host-instruction blocks into an existing test repo... | loop, overwrite, targetDirectory |
| `shaft_project_upgrade` | runs the existing SHAFT modular project upgrader script against the current Java project | allowAiRepair, approve, compileCommand, compileTimeout, dryRun, projectRoot, shaftVersion, skipBaselineCompile, upgradeType |
| `test_automation_scenarios` | returns SHAFT MCP usage scenarios, agent actions, design patterns, guardrails, and completion... | area, intent, maxResults |
| `test_code_guardrails_check` | checks generated SHAFT test code for lexical anti-patterns such as sleeps, brittle locators,... | code, language |
| `test_plan_explore` | crawls the active SHAFT browser session breadth-first from a URL, discovering forms, primary... | goal, maxDepth, maxPages, targetUrl |
| `trace_latest` | returns recent persisted SHAFT trace indexes from target/shaft-traces | maxResults |
| `trace_open_viewer` | resolves (extracting from the trace ZIP if needed) the offline SHAFT Trace Report HTML viewer... | tracePath |
| `trace_read` | returns redacted SHAFT trace JSON from a trace path with explicit output bounds | maxCharacters, tracePath |
| `trace_summarize` | returns a deterministic summary of a persisted SHAFT trace without AI | tracePath |
| `verify_run_focused` | runs a guarded, tokenized Maven verification command (compile, test-compile, test, verify, pa... | command, networkValidationApproved, repositoryRoot |
