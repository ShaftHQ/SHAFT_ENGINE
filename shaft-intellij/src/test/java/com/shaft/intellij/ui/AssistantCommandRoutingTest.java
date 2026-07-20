package com.shaft.intellij.ui;

import org.junit.jupiter.api.Test;

import java.util.List;
import java.util.Locale;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Covers {@link AssistantCommand} routing: slash commands, natural-language intent matching,
 * command hints/tooltips/help text, and the SHAFT-project gating checks -- split out of
 * {@code AssistantCommandTest} (which now focuses on prompt construction and the local-agent-runner
 * command/effort/model wiring) so each concern has its own focused test class, matching the
 * convention already used for the {@code AssistantLocalAgentRunner*Test} split.
 */
class AssistantCommandRoutingTest {
    @Test
    void appAndPluginRuntimePromptsStayLocalWithHandoffMessage() {
        AssistantCommand.Invocation invocation = AssistantCommand.fromPrompt(
                "Open this in Copilot",
                AssistantCommand.Selection.local("COPILOT", "IDE_PLUGIN"),
                "ASK",
                "C:/work/project",
                "",
                false);

        assertTrue(invocation.isLocal());
        assertTrue(invocation.localResponse().contains("GitHub Copilot plugin"));
    }

    @Test
    void recordingIntentsRouteToPluginRecorderNotLocalAgent() {
        // A recording started by a one-shot local agent turn dies with that turn's MCP child
        // process (issue #3429), so every natural recording phrasing — including the Assistant's
        // own suggestion prefills — must resolve to a plugin-run recorder tool instead of the agent.
        AssistantCommand.Invocation webPrefill =
                command("Record my browser actions on https://duckduckgo.com");
        AssistantCommand.Invocation mobilePrefill =
                command("Record my mobile actions on the Android emulator");

        assertAll(
                () -> assertEquals("capture_start", webPrefill.toolName()),
                () -> assertEquals("https://duckduckgo.com",
                        webPrefill.arguments().get("targetUrl").getAsString()),
                () -> assertEquals("capture_start",
                        command("record a web flow on https://example.com").toolName()),
                () -> assertEquals("capture_start",
                        command("Record my actions on https://example.com/login").toolName()),
                () -> assertEquals("capture_start", mobilePrefill.toolName()),
                () -> assertEquals("capture_start",
                        command("record my app on the emulator").toolName()),
                // Non-recording "record"-adjacent asks still go to the agent untouched.
                () -> assertEquals("autobot_local_agent_run",
                        command("record a video of the sprint demo").toolName()));
    }

    @Test
    void recordApiCommandStartsApiCaptureFromSlashAndNaturalLanguage() {
        // Issue #3726: "Record API" must be a discoverable assistant command that starts an
        // API-testing recording session (capture_api_start, with network request/response body
        // capture enabled) instead of falling through to the plain UI recorder (capture_start).
        AssistantCommand.Invocation slash = command("/record-api https://example.com/api");
        AssistantCommand.Invocation naturalPrefill = command("Record API traffic on https://example.com");
        AssistantCommand.Invocation naturalPhrase = command("record api traffic for the checkout flow");

        assertAll(
                () -> assertEquals("capture_api_start", slash.toolName()),
                () -> assertEquals("https://example.com/api", slash.arguments().get("targetUrl").getAsString()),
                () -> assertEquals("Chrome", slash.arguments().get("browser").getAsString()),
                () -> assertTrue(slash.arguments().get("networkOptions").getAsJsonObject()
                        .get("enabled").getAsBoolean()),
                () -> assertTrue(slash.arguments().get("networkOptions").getAsJsonObject()
                        .get("captureRequestBodies").getAsBoolean()),
                () -> assertTrue(slash.arguments().get("networkOptions").getAsJsonObject()
                        .get("captureResponseBodies").getAsBoolean()),
                // RecordApiWebAction.recordApiPrompt's exact prefill text -- sent into the chat panel
                // when Advanced UI is off (issue #3552's assistant-fallback route) -- must resolve to
                // the same MCP tool the dedicated API Recording tab drives.
                () -> assertEquals("capture_api_start", naturalPrefill.toolName()),
                () -> assertEquals("https://example.com",
                        naturalPrefill.arguments().get("targetUrl").getAsString()),
                () -> assertEquals("capture_api_start", naturalPhrase.toolName()));
    }

    @Test
    void recordApiNaturalLanguageDoesNotStealTheNoBrowserMobileApiRecordingPrompt() {
        // RecordApiMobileAction's "without a browser" prefill drives a different MCP tool
        // (capture_api_start with mobile routing) -- it now has its own deterministic
        // route (issue #3738); capture_api_start (this command) must still not steal it.
        AssistantCommand.Invocation mobileApiPrefill =
                command("Record API traffic without a browser on Android");

        assertEquals("capture_api_start", mobileApiPrefill.toolName());
    }

    @Test
    void noBrowserApiRecordingPrefillRoutesToMobileApiRecordStart() {
        // Issue #3738: RecordApiMobileAction's no-browser proxy prefill ("Record API traffic
        // without a browser on <platform>") was being swallowed by isMobileRecordingStartIntent's
        // broad "android"/"mobile" keyword list and routed to mobile_record_start (the UI-driven
        // mobile recorder) instead of the intended mobile_api_record_start proxy tool. It must
        // resolve deterministically to mobile_api_record_start with the same argument shape
        // RecordApiMobileAction's Advanced-UI path builds (platform, blank deviceLabel/outputPath).
        AssistantCommand.Invocation androidPrefill =
                command("Record API traffic without a browser on Android");
        AssistantCommand.Invocation iosPrefill =
                command("Record API traffic without a browser on iOS");
        // Plain mobile-recording phrasing (no "without a browser"/API wording) must keep starting
        // the UI-driven mobile recorder -- no regression on the existing route.
        AssistantCommand.Invocation plainMobileRecording =
                command("Record my mobile actions on the Android emulator");

        assertAll(
                () -> assertEquals("capture_api_start", androidPrefill.toolName()),
                () -> assertEquals("Android", androidPrefill.arguments().get("mobilePlatform").getAsString()),
                () -> assertEquals("", androidPrefill.arguments().get("mobileDeviceLabel").getAsString()),
                () -> assertEquals("", androidPrefill.arguments().get("outputPath").getAsString()),
                () -> assertEquals("capture_api_start", iosPrefill.toolName()),
                () -> assertEquals("iOS", iosPrefill.arguments().get("mobilePlatform").getAsString()),
                () -> assertEquals("", iosPrefill.arguments().get("mobileDeviceLabel").getAsString()),
                () -> assertEquals("", iosPrefill.arguments().get("outputPath").getAsString()),
                () -> assertEquals("capture_start", plainMobileRecording.toolName()));
    }

    @Test
    void mobileRecordingOutweighsBrowserRecordingWithoutCrossPredicateExclusion() {
        // NATURAL_INTENTS used to resolve this by having isBrowserRecordingIntent explicitly call
        // isMobileRecordingStartIntent and bail out (the issue #3429 patch). That cross-predicate
        // exclusion is gone now: directIntent scores every matching intent and mobile-recording's
        // weight (WEIGHT_MOBILE_RECORDING) is set above browser-recording's (WEIGHT_BROWSER_RECORDING),
        // so the same phrasing still starts the mobile recorder.
        AssistantCommand.Invocation mobileWordPrefill =
                command("Record my mobile actions on the Android emulator");
        // Genuinely ambiguous chosen example: dropping "mobile" still leaves "my actions on" (a
        // capture_start keyword) AND "android"/"emulator" (mobile_record_start keywords), so both
        // isBrowserRecordingIntent and isMobileRecordingStartIntent match this exact phrase under
        // their raw keyword lists -- only the weight difference decides the winner.
        AssistantCommand.Invocation ambiguousPhrase =
                command("record my actions on the android emulator");

        assertAll(
                () -> assertEquals("capture_start", mobileWordPrefill.toolName()),
                () -> assertEquals("capture_start", ambiguousPhrase.toolName()));
    }

    @Test
    void naturalIntentTiesResolveByDeclarationOrder() {
        // browser-control and mobile-control share WEIGHT_TOPIC_CONTROL: both are equally broad
        // "topic + action-word" recognizers, so a phrase satisfying both predicates' raw conditions
        // must resolve by declaration order (browser-control is declared first in NATURAL_INTENTS),
        // exactly like the old first-match linear scan. "open mobile inspector http://..." mentions
        // an action word + URL (browser-control) and "mobile" + "inspector" (mobile-control).
        for (int attempt = 0; attempt < 5; attempt++) {
            AssistantCommand.Invocation invocation = command("open mobile inspector http://example.com");

            assertAll("attempt " + attempt,
                    () -> assertTrue(invocation.isSequence()),
                    () -> assertEquals("driver_initialize", invocation.toolCalls().get(0).toolName()),
                    () -> assertEquals("browser_open_intent", invocation.toolCalls().get(1).toolName()));
        }
    }

    @Test
    void slashCommandsMapToCuratedTools() {
        assertEquals("shaft_guide_search", command("/guide locators").toolName());
        assertEquals("locators", command("/guide locators").arguments().get("query").getAsString());
        assertEquals("shaft_guide_search", command("/docs locators").toolName());
        assertEquals("test_automation_scenarios", command("/scenarios checkout").toolName());
        assertEquals("checkout", command("/scenarios checkout").arguments().get("intent").getAsString());
        assertEquals("capture_start", command("/record-web https://example.com").toolName());
        assertEquals("https://example.com", command("/record-web https://example.com")
                .arguments().get("targetUrl").getAsString());
        assertEquals("capture_start", command("/record").toolName());
        assertEquals("Chrome", command("/record").arguments().get("browser").getAsString());
        assertTrue(command("/record playwright").isSequence());
        assertEquals(List.of("driver_initialize", "capture_start"),
                command("/record playwright").toolCalls().stream()
                        .map(AssistantCommand.ToolCall::toolName).toList());
        assertTrue(command("/inspect").isLocal());
        assertTrue(command("/locator").isLocal());
        assertTrue(command("/inspect https://example.com sign in").isSequence());
        assertEquals(List.of("driver_initialize", "browser_open_intent"),
                command("/inspect https://example.com sign in").toolCalls().stream()
                        .map(AssistantCommand.ToolCall::toolName).toList());
        assertEquals("https://example.com", command("/inspect https://example.com sign in")
                .toolCalls().get(1).arguments().get("targetUrl").getAsString());
        assertEquals("sign in", command("/inspect https://example.com sign in")
                .toolCalls().get(1).arguments().get("userIntent").getAsString());
        assertEquals("test_code_guardrails_check", command("/guardrails driver.element().click(locator);").toolName());
        assertEquals("java", command("/guardrails code").arguments().get("language").getAsString());
        assertEquals("autobot_local_agent_clients", command("/assistant").toolName());
        assertEquals("autobot_local_agent_clients", command("/agent").toolName());
        assertEquals("autobot_local_agent_clients", command("/ask").toolName());
        assertEquals("autobot_local_agent_clients", command("/plan").toolName());
        assertEquals("autobot_local_agent_clients", command("/clients").toolName());
        assertEquals("test_automation_scenarios", command("/generatetest login").toolName());
        // Explicit codegen against a Capture recording re-executes it (generate + compile +
        // headless replay), per issue #3409; Playwright/mobile recordings replay through their
        // schema-native replay tools so their returned code blocks are proven live too.
        assertEquals("capture_generate_replay", command("/generatetest recordings/capture-session.json").toolName());
        assertTrue(command("/generatetest recordings/capture-session.json").arguments().get("replay").getAsBoolean());
        assertFalse(command("/generatetest recordings/capture-session.json").arguments().get("useAi").getAsBoolean());
        assertTrue(command("/generatetest recordings/playwright-session.json").isSequence());
        assertEquals(List.of("driver_initialize", "capture_generate_replay"),
                command("/generatetest recordings/playwright-session.json").toolCalls().stream()
                        .map(AssistantCommand.ToolCall::toolName).toList());
        assertEquals("recordings/playwright-session.json",
                command("/generatetest recordings/playwright-session.json")
                        .toolCalls().get(1).arguments().get("sessionPath").getAsString());
        assertEquals("capture_generate_replay", command("/codegen recordings/capture-session.json").toolName());
        assertEquals("capture_generate_replay", command("/codegen mobile recordings/mobile-session.json").toolName());
        assertEquals("recordings/mobile-session.json",
                command("/codegen mobile recordings/mobile-session.json")
                        .arguments().get("sessionPath").getAsString());
        assertEquals("mobile_inspector_record_start",
                command("/record-mobile inspector Android recordings/inspector.json").toolName());
    }

    @Test
    void slashUpgradeShowsCopyableAutomatedUpgradeScript() {
        AssistantCommand.Invocation upgrade = command("/upgrade .");
        String response = upgrade.localResponse();

        assertAll(
                () -> assertTrue(upgrade.isLocal()),
                // Outside Agent mode the command explains exactly how to authorize the agent-run
                // upgrade instead of silently pasting a recipe (issue #3426 B6)...
                () -> assertTrue(response.contains("Allow source edits"), response),
                () -> assertTrue(response.contains("**Agent**"), response),
                // ...while still offering the manual command for users who prefer to run it.
                () -> assertTrue(response.contains("```shell\npython -c \"import runpy,sys,urllib.request as u;")),
                () -> assertTrue(response.contains("u.urlretrieve('https://raw.githubusercontent.com/ShaftHQ/SHAFT_ENGINE/main/shaft-upgrader/upgrade_to_modular_shaft.py',p)")),
                () -> assertTrue(response.contains("sys.argv=[p,'--project','.'];runpy.run_path(p,run_name='__main__')")),
                () -> assertTrue(response.contains("\n```")));
    }

    @Test
    void slashUpgradeInAgentModeWithSourceEditsRunsTheUpgradeThroughTheLocalAgent() {
        AssistantCommand.Invocation invocation = AssistantCommand.fromPrompt(
                "/upgrade .",
                AssistantCommand.Selection.local("CODEX", "CLI"),
                "AGENT",
                "C:/work/project",
                "",
                true);
        String prompt = invocation.arguments().get("prompt").getAsString();

        assertAll(
                () -> assertEquals("autobot_local_agent_run", invocation.toolName()),
                () -> assertTrue(invocation.arguments().get("allowSourceMutation").getAsBoolean()),
                // The agent performs the upgrade itself, non-interactively, and must narrate.
                () -> assertTrue(prompt.contains("Perform the upgrade yourself"), prompt),
                () -> assertTrue(prompt.contains("shaft_project_upgrade"), prompt),
                () -> assertTrue(prompt.contains("'--yes'"), prompt),
                () -> assertTrue(prompt.contains("mvn -B -q test-compile"), prompt),
                () -> assertTrue(prompt.contains("Never reply with a bare confirmation like \"Done\""), prompt));
    }

    @Test
    void slashUpgradeOnCloudRouteKeepsTheManualCommand() {
        AssistantCommand.Invocation invocation = AssistantCommand.fromPrompt(
                "/upgrade .",
                AssistantCommand.Selection.cloud("gemini", "gemini-2.5-flash"),
                "AGENT",
                ".",
                "",
                true);

        assertAll(
                () -> assertTrue(invocation.isLocal()),
                () -> assertTrue(invocation.localResponse().contains("upgrade_to_modular_shaft.py")));
    }

    @Test
    void naturalUpgradePhraseStillReachesAgentPerformedUpgradePath() {
        // isProjectUpgradeIntent used to also match "upgrade shaft project"/"upgrade this shaft
        // project", but both branches were dead: isNaturalUpgradeIntent (checked in fromPrompt
        // before directIntent -- and this predicate -- is ever consulted) already intercepts this
        // phrasing and routes it to the agent-performed upgrade (issue #3426 B6). Removing the dead
        // branches from the predicate must not change that routing.
        AssistantCommand.Invocation invocation = AssistantCommand.fromPrompt(
                "upgrade shaft project .",
                AssistantCommand.Selection.local("CODEX", "CLI"),
                "AGENT",
                "C:/work/project",
                "",
                true);
        String prompt = invocation.arguments().get("prompt").getAsString();

        assertAll(
                () -> assertFalse(invocation.isLocal()),
                () -> assertEquals("autobot_local_agent_run", invocation.toolName()),
                () -> assertTrue(invocation.arguments().get("allowSourceMutation").getAsBoolean()),
                () -> assertTrue(prompt.contains("Perform the upgrade yourself"), prompt),
                () -> assertTrue(prompt.contains("shaft_project_upgrade"), prompt));
    }

    @Test
    void commandRegistryExposesCoreByDefaultAndExpertCommandsInExpertMode() {
        List<String> coreHints = AssistantCommand.commandHints(false).stream()
                .map(AssistantCommand.CommandHint::canonical).toList();
        List<String> allHints = AssistantCommand.commandHints(true).stream()
                .map(AssistantCommand.CommandHint::canonical).toList();
        String coreTooltip = AssistantCommand.commandTooltip(false);
        String expertTooltip = AssistantCommand.commandTooltip(true);

        assertAll(
                // default composer shows only the six core entry points
                () -> assertEquals(List.of("/record", "/record-mobile", "/record-api", "/codegen", "/doctor",
                        "/upgrade"), coreHints),
                // Expert mode reveals the rest, including the two new commands
                () -> assertTrue(allHints.containsAll(List.of("/record", "/record-mobile", "/record-api",
                        "/codegen", "/doctor", "/upgrade", "/partner", "/guide", "/guardrails", "/browser",
                        "/mobile", "/project", "/verify", "/skills"))),
                () -> assertEquals(AssistantCommand.commandHints().size(), allHints.size()),
                () -> assertTrue(coreTooltip.contains("/record-web")),
                () -> assertTrue(coreTooltip.contains("/record-api")),
                () -> assertFalse(coreTooltip.contains("/partner")),
                () -> assertFalse(coreTooltip.contains("/verify")),
                () -> assertFalse(coreTooltip.contains("/skills")),
                () -> assertTrue(expertTooltip.contains("/partner")),
                () -> assertTrue(expertTooltip.contains("/verify")),
                () -> assertTrue(expertTooltip.contains("/skills")),
                () -> assertFalse(expertTooltip.contains("/commands")),
                () -> assertFalse(expertTooltip.contains("/assistant")),
                () -> assertFalse(expertTooltip.contains("/mobile-record")));
    }

    @Test
    void commandHelpShowsCoreCommandsAndPointsToExpertMode() {
        AssistantCommand.Invocation help = command("/commands");
        String response = help.localResponse();

        assertAll(
                () -> assertTrue(help.isLocal()),
                () -> assertTrue(response.contains("**/codegen**")),
                () -> assertTrue(response.contains("**/record**")),
                () -> assertTrue(response.contains("**/record-mobile**")),
                () -> assertTrue(response.contains("**/record-api**")),
                () -> assertTrue(response.contains("**/doctor**")),
                () -> assertTrue(response.contains("**/upgrade**")),
                // expert commands are not shown as sections in the default help
                () -> assertFalse(response.contains("**/guide**")),
                () -> assertFalse(response.contains("**/partner**")),
                // but the Expert-mode footer names them
                () -> assertTrue(response.contains("Expert mode")),
                () -> assertTrue(response.contains("/verify")),
                () -> assertTrue(response.contains("/skills")),
                () -> assertTrue(response.contains("```text\n/codegen recordings/intellij-capture.json\n```")),
                () -> assertTrue(response.contains("```text\n/record https://example.com\n```")),
                () -> assertTrue(response.contains("```text\n/upgrade .\n```")),
                () -> assertFalse(response.contains("/commands -")),
                () -> assertFalse(response.contains("/assistant -")));
    }

    @Test
    void verifyCommandRoutesToGuardedFocusedVerification() {
        assertEquals("verify_run_focused", command("/verify mvn -q test-compile").toolName());
        assertEquals("verify_run_focused", command("/verify").toolName());
        assertEquals("verify_run_focused", command("/check").toolName());
    }

    @Test
    void skillsCommandListsAuthoringSkills() {
        AssistantCommand.Invocation invocation = command("/skills");
        assertAll(
                () -> assertTrue(invocation.isLocal()),
                () -> assertTrue(invocation.localResponse().contains("$writing-shaft-tests")),
                () -> assertTrue(invocation.localResponse().contains("$choosing-shaft-locators")),
                () -> assertTrue(invocation.localResponse().contains("$verifying-and-applying-shaft-changes")));
    }

    @Test
    void setupGateOnlyAppliesToDirectMcpFeatureInvocations() {
        AssistantCommand.Invocation broadLocal = AssistantCommand.fromPrompt(
                "Plan how to use mobile recording in this project",
                AssistantCommand.Selection.local("CODEX", "CLI"),
                "ASK",
                ".",
                "",
                false);
        AssistantCommand.Invocation broadCloud = AssistantCommand.fromPrompt(
                "Plan how to use mobile recording in this project",
                AssistantCommand.Selection.cloud("github", "openai/gpt-4.1"),
                "PLAN",
                ".",
                "",
                false);
        AssistantCommand.Invocation explicitFeature = command("start mobile recording");
        AssistantCommand.Invocation slashFeature = command("/guide locators");
        AssistantCommand.Invocation localHelp = command("/help");

        assertAll(
                () -> assertEquals("autobot_local_agent_run", broadLocal.toolName()),
                () -> assertFalse(broadLocal.requiresMcpConfiguration()),
                () -> assertEquals("autobot_provider_chat", broadCloud.toolName()),
                () -> assertFalse(broadCloud.requiresMcpConfiguration()),
                () -> assertTrue(explicitFeature.requiresMcpConfiguration()),
                () -> assertTrue(slashFeature.requiresMcpConfiguration()),
                () -> assertFalse(localHelp.requiresMcpConfiguration()));
    }

    @Test
    void browserCommandsCreateOrderedToolSequencesWhenSessionSetupIsRequired() {
        AssistantCommand.Invocation webdriver = command("/browser open https://example.com sign in");
        AssistantCommand.Invocation inspectAlias = command("/locator https://example.com sign in");
        AssistantCommand.Invocation natural = command("open https://example.com and inspect the sign in link");
        AssistantCommand.Invocation playwright = command("/web playwright open https://example.com");

        assertAll(
                () -> assertTrue(webdriver.isSequence()),
                () -> assertEquals(List.of("driver_initialize", "browser_open_intent"),
                        webdriver.toolCalls().stream().map(AssistantCommand.ToolCall::toolName).toList()),
                () -> assertEquals("CHROME", webdriver.toolCalls().get(0).arguments().get("targetBrowser").getAsString()),
                () -> assertEquals("https://example.com",
                        webdriver.toolCalls().get(1).arguments().get("targetUrl").getAsString()),
                () -> assertEquals("sign in", webdriver.toolCalls().get(1).arguments().get("userIntent").getAsString()),
                () -> assertEquals(List.of("driver_initialize", "browser_open_intent"),
                        inspectAlias.toolCalls().stream().map(AssistantCommand.ToolCall::toolName).toList()),
                () -> assertEquals(List.of("driver_initialize", "browser_open_intent"),
                        natural.toolCalls().stream().map(AssistantCommand.ToolCall::toolName).toList()),
                () -> assertTrue(playwright.isSequence()),
                () -> assertEquals(List.of("driver_initialize", "browser_navigate"),
                        playwright.toolCalls().stream().map(AssistantCommand.ToolCall::toolName).toList()),
                () -> assertEquals("CHROME", playwright.toolCalls().get(0).arguments().get("targetBrowser").getAsString()),
                () -> assertEquals("https://example.com",
                        playwright.toolCalls().get(1).arguments().get("targetUrl").getAsString()));
    }

    @Test
    void browserCommandsExposeCommonDirectControls() {
        AssistantCommand.Invocation screenshot = command("/browser screenshot target/shaft-browser/home.png");

        assertAll(
                () -> assertEquals("browser_take_screenshot", screenshot.toolName()),
                () -> assertEquals("target/shaft-browser/home.png",
                        screenshot.arguments().get("outputPath").getAsString()),
                () -> assertFalse(screenshot.arguments().get("includeBase64").getAsBoolean()),
                () -> assertEquals("browser_get_page_dom", command("/browser dom").toolName()),
                () -> assertEquals("browser_get_title", command("/browser title").toolName()),
                () -> assertEquals("browser_refresh", command("/browser refresh").toolName()),
                () -> assertEquals("browser_navigate_back", command("/browser back").toolName()),
                () -> assertEquals("browser_navigate_forward", command("/browser forward").toolName()),
                () -> assertEquals("browser_set_window_size", command("/browser maximize").toolName()),
                () -> assertEquals(0, command("/browser maximize").arguments().get("width").getAsInt()),
                () -> assertEquals(0, command("/browser maximize").arguments().get("height").getAsInt()),
                () -> assertEquals("MAXIMIZE", command("/browser maximize").arguments().get("mode").getAsString()),
                () -> assertEquals("browser_set_window_size", command("/browser fullscreen").toolName()),
                () -> assertEquals(0, command("/browser fullscreen").arguments().get("width").getAsInt()),
                () -> assertEquals(0, command("/browser fullscreen").arguments().get("height").getAsInt()),
                () -> assertEquals("FULLSCREEN", command("/browser fullscreen").arguments().get("mode").getAsString()),
                () -> assertEquals("driver_quit", command("/browser quit").toolName()));
    }

    @Test
    void mobileCommandsMapToControlRecordingCodegenAndInspectorTools() {
        AssistantCommand.Invocation nativeSession = command("/mobile native Android Pixel_6");
        AssistantCommand.Invocation webSession = command("/mobile web https://example.com");
        AssistantCommand.Invocation inspector = command("/mobile-record inspector Android recordings/inspector.json");
        AssistantCommand.Invocation unknown = command("/mobile calibrate");

        assertAll(
                () -> assertEquals("mobile_toolchain_status", command("/mobile doctor Android").toolName()),
                () -> assertEquals("Android",
                        command("/mobile status Android").arguments().get("platformName").getAsString()),
                () -> assertEquals("driver_initialize", nativeSession.toolName()),
                () -> assertEquals("MOBILE_NATIVE", nativeSession.arguments().get("engine").getAsString()),
                () -> assertEquals("Android", nativeSession.arguments().get("mobileOptions").getAsJsonObject().get("platformName").getAsString()),
                () -> assertEquals("Pixel_6", nativeSession.arguments().get("mobileOptions").getAsJsonObject().get("deviceName").getAsString()),
                () -> assertEquals("driver_initialize", webSession.toolName()),
                () -> assertEquals("MOBILE_WEB", webSession.arguments().get("engine").getAsString()),
                () -> assertEquals("https://example.com", webSession.arguments().get("mobileOptions").getAsJsonObject().get("targetUrl").getAsString()),
                () -> assertEquals("CHROME", webSession.arguments().get("mobileOptions").getAsJsonObject().get("browser").getAsString()),
                () -> assertEquals("mobile_get_accessibility_tree", command("/mobile tree").toolName()),
                () -> assertEquals("browser_take_screenshot", command("/mobile screenshot target/mobile.png").toolName()),
                () -> assertFalse(command("/mobile screenshot target/mobile.png").arguments().get("includeBase64").getAsBoolean()),
                () -> assertEquals("mobile_get_contexts", command("/mobile contexts").toolName()),
                () -> assertEquals("mobile_switch_context", command("/mobile switch WEBVIEW_chrome").toolName()),
                () -> assertEquals("WEBVIEW_chrome", command("/mobile switch WEBVIEW_chrome").arguments().get("contextName").getAsString()),
                () -> assertEquals("mobile_switch_context", command("/mobile context NATIVE_APP").toolName()),
                () -> assertEquals("capture_start", command("/mobile-record start recordings/mobile.json").toolName()),
                () -> assertEquals("recordings/mobile.json",
                        command("/mobile-record start recordings/mobile.json").arguments().get("outputPath").getAsString()),
                () -> assertEquals("capture_stop", command("/app-record stop").toolName()),
                () -> assertEquals("mobile_inspector_record_start", inspector.toolName()),
                () -> assertEquals("mobile_inspector_record_start", command("/inspector-record Android recordings/inspector.json").toolName()),
                () -> assertEquals("Android", inspector.arguments().get("platformName").getAsString()),
                () -> assertEquals("recordings/inspector.json", inspector.arguments().get("outputPath").getAsString()),
                () -> assertEquals("capture_code_blocks",
                        command("/mobile-codegen recordings/mobile.json").toolName()),
                () -> assertEquals("capture_generate_replay",
                        command("/mobile-replay recordings/mobile.json").toolName()),
                () -> assertEquals("driver_quit", command("/mobile quit").toolName()),
                () -> assertTrue(unknown.isLocal()),
                () -> assertTrue(unknown.localResponse().contains("Unknown mobile command")));
    }

    @Test
    void browserCommandsExposeSessionActionTools() {
        AssistantCommand.Invocation size = command("/browser size 1920x1080");
        AssistantCommand.Invocation resizeDefault = command("/browser resize");
        AssistantCommand.Invocation deleteCookie = command("/browser delete cookie sessionId");
        AssistantCommand.Invocation network = command("/browser network requests example.com");
        AssistantCommand.Invocation saveSession = command("/browser save session recordings/state.json");
        AssistantCommand.Invocation loadSessionDefault = command("/browser load session");

        assertAll(
                () -> assertEquals("browser_set_window_size", size.toolName()),
                () -> assertEquals(1920, size.arguments().get("width").getAsInt()),
                () -> assertEquals(1080, size.arguments().get("height").getAsInt()),
                () -> assertEquals(1280, resizeDefault.arguments().get("width").getAsInt()),
                () -> assertEquals(800, resizeDefault.arguments().get("height").getAsInt()),
                () -> assertEquals("browser_delete_cookies", command("/browser clear cookies").toolName()),
                () -> assertEquals("browser_delete_cookies", command("/browser delete all cookies").toolName()),
                () -> assertEquals("browser_delete_cookies", deleteCookie.toolName()),
                () -> assertEquals("sessionId", deleteCookie.arguments().get("name").getAsString()),
                () -> assertTrue(command("/browser delete cookie").isLocal()),
                () -> assertEquals("browser_aria_snapshot", command("/browser aria").toolName()),
                () -> assertEquals("", command("/browser aria").arguments().get("locatorValue").getAsString()),
                () -> assertFalse(command("/browser aria").arguments().has("locatorStrategy")),
                () -> assertEquals("browser_accessibility_audit", command("/browser audit").toolName()),
                () -> assertEquals(0, command("/browser audit").arguments().getAsJsonArray("wcagTags").size()),
                () -> assertEquals(2, command("/browser audit wcag2a wcag21aa").arguments()
                        .getAsJsonArray("wcagTags").size()),
                () -> assertEquals("browser_network_requests", network.toolName()),
                () -> assertEquals("example.com", network.arguments().get("urlFilter").getAsString()),
                () -> assertEquals(50, network.arguments().get("limit").getAsInt()),
                () -> assertEquals("browser_storage_state_save", saveSession.toolName()),
                () -> assertEquals("recordings/state.json", saveSession.arguments().get("filePath").getAsString()),
                () -> assertEquals("browser_storage_state_load",
                        command("/browser load session recordings/state.json").toolName()),
                () -> assertEquals("recordings/state.json", command("/browser load session recordings/state.json")
                        .arguments().get("filePath").getAsString()),
                () -> assertTrue(loadSessionDefault.isLocal()));
    }

    @Test
    void browserSessionActionsAreReachableFromNaturalLanguageWhenTheyNameTheBrowser() {
        assertAll(
                () -> assertEquals("browser_set_window_size",
                        command("resize the browser window to 1920x1080").toolName()),
                () -> assertEquals(1920, command("resize the browser window to 1920x1080")
                        .arguments().get("width").getAsInt()),
                () -> assertEquals("browser_delete_cookies",
                        command("clear cookies on the browser").toolName()),
                () -> assertEquals("browser_delete_cookies",
                        command("delete cookie sessionId from the browser").toolName()),
                () -> assertEquals("sessionId", command("delete cookie sessionId from the browser")
                        .arguments().get("name").getAsString()),
                () -> assertEquals("browser_aria_snapshot",
                        command("aria snapshot of the current browser page").toolName()),
                () -> assertEquals("browser_accessibility_audit",
                        command("run an accessibility audit on the browser").toolName()),
                () -> assertEquals("browser_network_requests",
                        command("list network requests from the browser").toolName()),
                // "clear cookies" with no mention of "browser" is deliberately left ambiguous (could
                // be about a different app or a generic question) and falls through to the agent.
                () -> assertEquals("autobot_local_agent_run", command("clear cookies").toolName()));
    }

    @Test
    void playwrightBrowserCommandsExposeNavigationAndWindowTools() {
        assertAll(
                () -> assertEquals("browser_refresh", command("/browser playwright refresh").toolName()),
                () -> assertEquals("browser_navigate_back", command("/browser playwright back").toolName()),
                () -> assertEquals("browser_navigate_forward", command("/browser playwright forward").toolName()),
                () -> assertEquals("browser_set_window_size", command("/browser playwright size 1024x768").toolName()),
                () -> assertEquals(1024, command("/browser playwright size 1024x768").arguments().get("width").getAsInt()),
                () -> assertEquals(768, command("/browser playwright size 1024x768").arguments().get("height").getAsInt()),
                () -> assertEquals("browser_navigate", command("/browser playwright newtab https://example.com").toolName()),
                () -> assertTrue(command("/browser playwright newtab https://example.com").arguments().get("newWindow").getAsBoolean()),
                () -> assertEquals("https://example.com", command("/browser playwright newtab https://example.com").arguments().get("targetUrl").getAsString()),
                () -> assertEquals("browser_navigate", command("/browser playwright newwindow").toolName()),
                () -> assertTrue(command("/browser playwright newwindow").arguments().get("newWindow").getAsBoolean()));
    }

    @Test
    void mobileCommandsExposeTouchAndAppLifecycleTools() {
        assertAll(
                () -> assertEquals("mobile_rotate", command("/mobile rotate landscape").toolName()),
                () -> assertEquals("LANDSCAPE", command("/mobile rotate landscape").arguments().get("orientation").getAsString()),
                () -> assertEquals("PORTRAIT", command("/mobile rotate").arguments().get("orientation").getAsString()),
                () -> assertEquals("mobile_hide_keyboard", command("/mobile hide keyboard").toolName()),
                () -> assertEquals("mobile_background_app", command("/mobile background 10").toolName()),
                () -> assertEquals(10, command("/mobile background 10").arguments().get("seconds").getAsInt()),
                () -> assertEquals(5, command("/mobile background").arguments().get("seconds").getAsInt()),
                () -> assertEquals("mobile_activate_app", command("/mobile activate com.example.app").toolName()),
                () -> assertEquals("com.example.app",
                        command("/mobile activate com.example.app").arguments().get("appId").getAsString()),
                () -> assertTrue(command("/mobile activate").isLocal()),
                () -> assertEquals("mobile_rotate", command("rotate the mobile device to landscape").toolName()),
                () -> assertEquals("LANDSCAPE",
                        command("rotate the mobile device to landscape").arguments().get("orientation").getAsString()),
                () -> assertEquals("mobile_hide_keyboard", command("on my mobile, hide keyboard").toolName()),
                () -> assertEquals("mobile_background_app",
                        command("on my mobile, background the app").toolName()),
                () -> assertEquals("mobile_activate_app",
                        command("activate app com.example.app on mobile").toolName()),
                () -> assertEquals("com.example.app", command("activate app com.example.app on mobile")
                        .arguments().get("appId").getAsString()));
    }

    @Test
    void captureStatusIsReachableFromNaturalLanguage() {
        assertAll(
                () -> assertEquals("capture_status", command("recording status").toolName()),
                () -> assertEquals("capture_status", command("capture status").toolName()),
                () -> assertEquals("capture_status", command("is a recording active").toolName()));
    }

    @Test
    void doctorCommandsMapToAllureTraceAndFixRecommendationTools() {
        AssistantCommand.Invocation analyze = command("/doctor target/allure-results", "C:/work/project");
        AssistantCommand.Invocation playwrightAnalyze = command("/doctor playwright target/allure-results");
        AssistantCommand.Invocation suggest = command("/doctor fix target/shaft-doctor/doctor-report.json");
        AssistantCommand.Invocation trace = command("/doctor trace target/shaft-traces");
        AssistantCommand.Invocation triageAlias = command("/triage target/custom-results", "C:/work/project");
        AssistantCommand.Invocation fixAlias = command("/fixTestFailure target/custom-results", "C:/work/project");

        assertAll(
                () -> assertEquals("doctor_analyze_failed_allure", analyze.toolName()),
                () -> assertEquals("target/allure-results",
                        analyze.arguments().getAsJsonArray("allureResultPaths").get(0).getAsString()),
                () -> assertEquals("C:/work/project", analyze.arguments().get("repositoryRoot").getAsString()),
                () -> assertEquals("doctor_analyze_failed_allure", playwrightAnalyze.toolName()),
                () -> assertEquals("playwright", playwrightAnalyze.arguments().get("backend").getAsString()),
                () -> assertEquals("doctor_suggest_fix", suggest.toolName()),
                () -> assertEquals("target/shaft-doctor/doctor-report.json",
                        suggest.arguments().get("jsonReportPath").getAsString()),
                () -> assertEquals("doctor_analyze_trace", trace.toolName()),
                () -> assertEquals("target/shaft-traces", trace.arguments().get("tracePath").getAsString()),
                () -> assertEquals("webdriver", trace.arguments().get("backend").getAsString()),
                () -> assertEquals("doctor_analyze_failed_allure", command("/allure target/allure-results").toolName()),
                () -> assertEquals("target/custom-results",
                        triageAlias.arguments().getAsJsonArray("allureResultPaths").get(0).getAsString()),
                () -> assertEquals("target/custom-results",
                        fixAlias.arguments().getAsJsonArray("allureResultPaths").get(0).getAsString()));
    }

    @Test
    void rawMcpCommandAcceptsExplicitToolAndJsonArguments() {
        AssistantCommand.Invocation raw = command("/mcp browser_take_screenshot {\"outputPath\":\"target/x.png\",\"includeBase64\":false}");

        assertAll(
                () -> assertEquals("browser_get_title", command("/tool browser_get_title {}").toolName()),
                () -> assertEquals("browser_take_screenshot", raw.toolName()),
                () -> assertEquals("target/x.png", raw.arguments().get("outputPath").getAsString()),
                () -> assertTrue(command("/mcp browser_take_screenshot {bad json}").isLocal()));
    }

    @Test
    void partnerCommandRoutesRepositoryIntentToCodingPartnerPlan() {
        AssistantCommand.Invocation slash = command(
                "/partner Log in with valid credentials then verify account menu",
                "C:/work/project");
        AssistantCommand.Invocation natural = command(
                "plan coding partner work for checkout happy path",
                "C:/work/project");

        assertAll(
                () -> assertEquals("shaft_coding_partner_plan", slash.toolName()),
                () -> assertEquals("C:/work/project", slash.arguments().get("repositoryPath").getAsString()),
                () -> assertEquals("Log in with valid credentials then verify account menu",
                        slash.arguments().get("intent").getAsString()),
                () -> assertEquals("WebDriver", slash.arguments().get("backend").getAsString()),
                () -> assertEquals("", slash.arguments().get("currentSourcePath").getAsString()),
                () -> assertEquals("", slash.arguments().get("selectedText").getAsString()),
                () -> assertEquals(0, slash.arguments().getAsJsonArray("artifactPaths").size()),
                () -> assertEquals(10, slash.arguments().get("maxResults").getAsInt()),
                () -> assertEquals("shaft_coding_partner_plan", natural.toolName()),
                () -> assertEquals("checkout happy path", natural.arguments().get("intent").getAsString()));
    }

    @Test
    void partnerCommandIncludesOpenFileContextWhenAvailable() {
        AssistantCommand.OpenFileContext openFile = new AssistantCommand.OpenFileContext(
                "src/test/java/pages/LoginPage.java",
                "class LoginPage { void login() {} }",
                "driver.findElement(By.id(\"email\")).sendKeys(username);");
        AssistantCommand.Invocation slash = AssistantCommand.fromPrompt(
                "/partner Convert this login step to SHAFT",
                AssistantCommand.Selection.local("CODEX", "CLI"),
                "AGENT",
                "C:/work/project",
                "",
                true,
                openFile);
        AssistantCommand.Invocation natural = AssistantCommand.fromPrompt(
                "plan coding partner work for mobile login",
                AssistantCommand.Selection.local("CODEX", "CLI"),
                "AGENT",
                "C:/work/project",
                "",
                true,
                openFile);

        assertAll(
                () -> assertEquals("src/test/java/pages/LoginPage.java",
                        slash.arguments().get("currentSourcePath").getAsString()),
                () -> assertEquals("driver.findElement(By.id(\"email\")).sendKeys(username);",
                        slash.arguments().get("selectedText").getAsString()),
                () -> assertEquals("Mobile", natural.arguments().get("backend").getAsString()),
                () -> assertEquals("src/test/java/pages/LoginPage.java",
                        natural.arguments().get("currentSourcePath").getAsString()));
    }

    @Test
    void naturalIntentRoutesObviousMcpFeatureRequests() {
        AssistantCommand.Invocation projectUpgrade = command("preview shaft upgrade .", "C:/work/project");
        AssistantCommand.Invocation projectCreate = command("create SHAFT project demo-web");

        assertAll(
                () -> assertEquals("shaft_guide_search", command("search SHAFT docs locators").toolName()),
                () -> assertEquals("locators", command("search SHAFT docs locators").arguments().get("query").getAsString()),
                () -> assertEquals("test_automation_scenarios", command("find automation scenarios checkout").toolName()),
                () -> assertEquals("checkout", command("find automation scenarios checkout").arguments().get("intent").getAsString()),
                () -> assertEquals("test_code_guardrails_check",
                        command("check generated Java code driver.element().click(locator);").toolName()),
                () -> assertEquals("driver.element().click(locator);",
                        command("check generated Java code driver.element().click(locator);").arguments().get("code").getAsString()),
                () -> assertTrue(projectCreate.isLocal()),
                () -> assertTrue(projectCreate.localResponse().contains("writes files")),
                () -> assertTrue(projectCreate.localResponse().contains("demo-web")),
                () -> assertEquals("shaft_project_upgrade", projectUpgrade.toolName()),
                () -> assertTrue(projectUpgrade.arguments().get("dryRun").getAsBoolean()),
                () -> assertFalse(projectUpgrade.arguments().get("approve").getAsBoolean()),
                () -> assertEquals(".", projectUpgrade.arguments().get("projectRoot").getAsString()),
                () -> assertEquals("capture_start", command("start mobile recording").toolName()),
                () -> assertEquals("capture_start", command("start a browser recording").toolName()),
                () -> assertEquals("doctor_analyze_failed_allure",
                        command("run doctor on target/allure-results").toolName()),
                () -> assertEquals("capture_code_blocks",
                        command("generate mobile code from recordings/mobile.json").toolName()),
                () -> assertEquals("mobile_toolchain_status",
                        command("check my Android mobile toolchain").toolName()),
                () -> assertEquals("Android",
                        command("check my Android mobile toolchain").arguments().get("platformName").getAsString()),
                () -> assertEquals("mobile_get_accessibility_tree",
                        command("inspect the current mobile screen").toolName()),
                () -> assertEquals(8000,
                        command("inspect the current mobile screen").arguments().get("maxCharacters").getAsInt()),
                () -> assertEquals("mobile_get_contexts",
                        command("show mobile contexts").toolName()),
                () -> assertEquals("browser_take_screenshot",
                        command("take a mobile screenshot target/shaft-mobile/home.png").toolName()),
                () -> assertFalse(command("take a mobile screenshot target/shaft-mobile/home.png")
                        .arguments().get("includeBase64").getAsBoolean()),
                () -> assertTrue(command("what commands can I use for mobile recording?").isLocal()),
                () -> assertTrue(command("what commands can I use for mobile recording?")
                        .localResponse().contains("/record-mobile")));
    }

    @Test
    void naturalTriagePhrasingsRouteToAutoDiscoveringDoctor() {
        // No path in the text -> empty allureResultPaths -> the server auto-discovers the newest
        // allure-results directory or single-file AllureReport.html in the workspace.
        for (String phrase : List.of(
                "diagnose my last run",
                "diagnose the latest run",
                "analyze the latest report",
                "why did my test fail?",
                "why did my tests fail")) {
            AssistantCommand.Invocation triage = command(phrase, "C:/work/project");
            assertAll(phrase,
                    () -> assertEquals("doctor_analyze_failed_allure", triage.toolName()),
                    () -> assertEquals(0, triage.arguments().get("allureResultPaths").getAsJsonArray().size()),
                    () -> assertEquals("C:/work/project", triage.arguments().get("repositoryRoot").getAsString()));
        }
    }

    @Test
    void requiresShaftProjectGatesOnlyMutatingOrShaftReportingSpecificTools() {
        AssistantCommand.Invocation projectUpgrade = command("preview shaft upgrade .", "C:/work/project");
        AssistantCommand.Invocation verify = command("/verify mvn -q test-compile");
        AssistantCommand.Invocation doctor = command("run doctor on target/allure-results");
        AssistantCommand.Invocation guideSearch = command("search SHAFT docs locators");
        AssistantCommand.Invocation codeGuardrails = command("check generated Java code driver.element().click(locator);");
        AssistantCommand.Invocation localAgentRun = command("Plan a resilient login test");
        AssistantCommand.Invocation local = command("/help");

        assertAll(
                () -> assertTrue(AssistantCommand.requiresShaftProject(projectUpgrade),
                        "shaft_project_upgrade mutates the project and must be gated"),
                () -> assertTrue(AssistantCommand.requiresShaftProject(verify),
                        "verify_run_focused runs a real Maven build and must be gated"),
                () -> assertTrue(AssistantCommand.requiresShaftProject(doctor),
                        "doctor_analyze_failed_allure only makes sense against SHAFT Allure evidence"),
                () -> assertFalse(AssistantCommand.requiresShaftProject(guideSearch),
                        "shaft_guide_search is read-only and useful while adopting SHAFT"),
                () -> assertFalse(AssistantCommand.requiresShaftProject(codeGuardrails),
                        "test_code_guardrails_check is read-only"),
                () -> assertFalse(AssistantCommand.requiresShaftProject(localAgentRun),
                        "autobot_local_agent_run is not SHAFT-project-specific"),
                () -> assertFalse(AssistantCommand.requiresShaftProject(local),
                        "local invocations never dispatch a tool"),
                () -> assertFalse(AssistantCommand.requiresShaftProject(null)));
    }

    @Test
    void shaftProjectRequiredNudgeIsALocalOnboardingMessage() {
        AssistantCommand.Invocation nudge = AssistantCommand.shaftProjectRequiredNudge("verify_run_focused");

        assertAll(
                () -> assertTrue(nudge.isLocal()),
                () -> assertTrue(nudge.localResponse().contains("verify_run_focused")),
                () -> assertTrue(nudge.localResponse().contains("doesn't look like a SHAFT project")),
                () -> assertTrue(nudge.localResponse().contains("Create SHAFT Project")));
    }

    @Test
    void naturalRecordingCommandsMapToSafeRecorderWorkflow() {
        AssistantCommand.Invocation simpleStart = command("start recording");
        AssistantCommand.Invocation articleStart = command("start a recording");
        AssistantCommand.Invocation start = command("start recording https://duckduckgo.com/");
        AssistantCommand.Invocation stop = command("stop");
        AssistantCommand.Invocation review = command("review recording recordings/demo.json");
        AssistantCommand.Invocation defaultReview = command("review recording");

        assertAll(
                () -> assertEquals("", simpleStart.arguments().get("targetUrl").getAsString(),
                        "no target parsed from bare 'start recording' should stay blank so capture_start "
                                + "resolves it to an empty browser downstream, not a hardcoded default site"),
                () -> assertEquals("", articleStart.arguments().get("targetUrl").getAsString()),
                () -> assertEquals("capture_start", start.toolName()),
                () -> assertEquals("https://duckduckgo.com/",
                        start.arguments().get("targetUrl").getAsString()),
                () -> assertTrue(start.arguments().get("outputPath").getAsString()
                        .startsWith(AssistantCommand.DEFAULT_CAPTURE_RECORDING_PATH_PREFIX)),
                () -> assertTrue(start.arguments().get("outputPath").getAsString().endsWith(".json")),
                () -> assertFalse(start.arguments().get("headless").getAsBoolean()),
                () -> assertEquals("capture_stop", stop.toolName()),
                () -> assertFalse(stop.arguments().get("discard").getAsBoolean()),
                () -> assertEquals("capture_code_blocks", review.toolName()),
                () -> assertEquals("recordings/demo.json", review.arguments().get("sessionPath").getAsString()),
                () -> assertEquals(AssistantCommand.DEFAULT_CAPTURE_REVIEW_DIRECTORY,
                        review.arguments().get("outputDirectory").getAsString()),
                () -> assertEquals(AssistantCommand.DEFAULT_CAPTURE_RECORDING_PATH,
                        defaultReview.arguments().get("sessionPath").getAsString()));
    }

    @Test
    void recordScenarioPhrasesRouteDeterministicallyToBlankCaptureStart() {
        // Issue #3673: common recording phrases ("record a scenario", "start recording", ...)
        // must resolve deterministically to capture_start with no target rather than falling
        // through to the LLM agent. No target parsed means an empty/blank-page browser opens.
        AssistantCommand.Invocation recordAScenario = command("record a scenario");
        AssistantCommand.Invocation recordANewScenario = command("record a new scenario");
        AssistantCommand.Invocation recordScenario = command("record scenario");
        AssistantCommand.Invocation recordNewScenario = command("record new scenario");
        // Regression guard: the narrow new phrases must not disturb the existing mobile-recording
        // routing for phrasing that mentions "record" alongside mobile keywords.
        AssistantCommand.Invocation mobileRecording = command("record my actions on the Android emulator");

        assertAll(
                () -> assertEquals("capture_start", recordAScenario.toolName()),
                () -> assertEquals("", recordAScenario.arguments().get("targetUrl").getAsString()),
                () -> assertEquals("capture_start", recordANewScenario.toolName()),
                () -> assertEquals("", recordANewScenario.arguments().get("targetUrl").getAsString()),
                () -> assertEquals("capture_start", recordScenario.toolName()),
                () -> assertEquals("", recordScenario.arguments().get("targetUrl").getAsString()),
                () -> assertEquals("capture_start", recordNewScenario.toolName()),
                () -> assertEquals("", recordNewScenario.arguments().get("targetUrl").getAsString()),
                () -> assertEquals("capture_start", mobileRecording.toolName(),
                        "the narrow record-a-scenario predicate must not collide with mobile recording intent"));
    }

    @Test
    void proseScenarioDescriptionRoutesToAgentWithFullOrchestrationGuidanceInsteadOfBareRecording() {
        // Issue #3692 item 3: the onboarding "Record a sample flow" quick action sends this prose
        // scenario description as a chat prompt (see ShaftAssistantPanel's empty-state chip), not a
        // bare "start recording"/"record a scenario" trigger. Before this fix, isBrowserRecordingIntent
        // matched "web flow" in the description and deterministically short-circuited it into a bare
        // capture_start with no target, silently dropping "add one assertion" and "generate a
        // reviewed test." It must instead reach the agent with the full record -> act -> stop ->
        // codegen -> self-heal guidance.
        AssistantCommand.Invocation invocation = command(
                "Record a sample web flow on a practice page, add one assertion, and generate a reviewed test.");

        assertAll(
                () -> assertEquals("autobot_local_agent_run", invocation.toolName()),
                () -> assertFalse(invocation.arguments().has("targetUrl"),
                        "must not deterministically dispatch a bare capture_start"));

        String prompt = invocation.arguments().get("prompt").getAsString();
        assertAll(
                () -> assertTrue(prompt.contains("This is a code-generation request. Before returning Java:"), prompt),
                () -> assertTrue(prompt.contains("start a fresh session with capture_start"), prompt),
                () -> assertTrue(prompt.contains("capture_generate_replay"), prompt),
                () -> assertTrue(prompt.contains("healer_run_failed_test"), prompt),
                () -> assertTrue(prompt.contains("Page Object Model"), prompt),
                () -> assertTrue(prompt.contains(
                        "Record a sample web flow on a practice page, add one assertion, and generate a reviewed test."),
                        prompt));
    }

    @Test
    void bareRecordingTriggersStillRouteDeterministicallyDespiteScenarioDescriptionCarveOut() {
        // Regression guard for the isScenarioDescriptionIntent carve-out added alongside the test
        // above: it must not swallow the narrow, exact-match deterministic triggers (issue #3673)
        // that other tests already pin down.
        AssistantCommand.Invocation startRecording = command("start recording");
        AssistantCommand.Invocation recordAScenario = command("record a scenario");

        assertAll(
                () -> assertEquals("capture_start", startRecording.toolName()),
                () -> assertEquals("capture_start", recordAScenario.toolName()));
    }

    @Test
    void naturalRecorderCommandsCanDiscardAnActiveCaptureSession() {
        AssistantCommand.Invocation discard = command("discard recording");
        AssistantCommand.Invocation discardAlias = command("discard capture session");

        assertAll(
                () -> assertEquals("capture_stop", discard.toolName()),
                () -> assertEquals("capture_stop", discardAlias.toolName()),
                () -> assertTrue(discard.arguments().get("discard").getAsBoolean()),
                () -> assertTrue(discardAlias.arguments().get("discard").getAsBoolean()));
    }

    @Test
    void reRecordStopsAndRestartsCaptureInASequence() {
        AssistantCommand.Invocation reRecord = command("re-record");
        AssistantCommand.Invocation reRecordWithUrl = command("re-record https://duckduckgo.com/");

        assertAll(
                () -> assertTrue(reRecord.isSequence()),
                () -> assertEquals("capture_stop", reRecord.toolCalls().get(0).toolName()),
                () -> assertEquals("capture_start", reRecord.toolCalls().get(1).toolName()),
                () -> assertTrue(reRecord.toolCalls().get(0).arguments().get("discard").getAsBoolean()),
                () -> assertEquals("capture_stop", reRecordWithUrl.toolCalls().get(0).toolName()),
                () -> assertEquals("capture_start", reRecordWithUrl.toolCalls().get(1).toolName()),
                () -> assertTrue(reRecordWithUrl.toolCalls().get(0).arguments().get("discard").getAsBoolean()),
                () -> assertEquals("https://duckduckgo.com/",
                        reRecordWithUrl.toolCalls().get(1).arguments().get("targetUrl").getAsString()),
                () -> assertTrue(reRecordWithUrl.toolCalls().get(1).arguments().get("outputPath")
                        .getAsString().startsWith(AssistantCommand.DEFAULT_CAPTURE_RECORDING_PATH_PREFIX)),
                () -> assertEquals("chrome", reRecordWithUrl.toolCalls().get(1).arguments().get("browser").getAsString().toLowerCase(Locale.ROOT)),
                () -> assertFalse(reRecordWithUrl.toolCalls().get(1).arguments().get("headless").getAsBoolean()));
    }

    @Test
    void captureReviewUsesIgnoredReviewDirectoryAndApprovalCreatesAgentRequest() {
        assertTrue(AssistantCommand.isCaptureApproval("okay"));
        assertTrue(AssistantCommand.isCaptureApproval("generate"));
        assertTrue(AssistantCommand.isCaptureApproval("apply it"));
        assertFalse(AssistantCommand.isCaptureApproval("yes"));
        assertFalse(AssistantCommand.isCaptureApproval("go ahead"));
        assertFalse(AssistantCommand.isCaptureApproval("looks good"));
        assertFalse(AssistantCommand.isCaptureApproval("create files"));
        assertFalse(AssistantCommand.isCaptureApproval("generate a login test"));

        var review = AssistantCommand.captureCodeReview("recordings/demo.json");
        assertAll(
                () -> assertEquals("recordings/demo.json", review.get("sessionPath").getAsString()),
                () -> assertEquals(AssistantCommand.DEFAULT_CAPTURE_REVIEW_DIRECTORY,
                        review.get("outputDirectory").getAsString()),
                () -> assertTrue(review.get("overwrite").getAsBoolean()));

        AssistantCommand.Invocation apply = AssistantCommand.approvedCaptureIntegration(
                AssistantCommand.Selection.local("CODEX", "CLI"),
                "C:/work/project",
                "",
                "```java\nclass Generated {}\n```",
                "{\"codeBlocks\":[]}");

        assertAll(
                () -> assertEquals("autobot_local_agent_run", apply.toolName()),
                () -> assertEquals("AGENT", apply.arguments().get("mode").getAsString()),
                () -> assertTrue(apply.arguments().get("allowSourceMutation").getAsBoolean()),
                () -> assertTrue(apply.arguments().get("prompt").getAsString().contains("Page Object Model")),
                () -> assertTrue(apply.arguments().get("prompt").getAsString().contains("Do not start a new recording")),
                () -> assertTrue(apply.arguments().get("prompt").getAsString()
                        .contains("Preserve the recorded browser journey")),
                () -> assertTrue(apply.arguments().get("prompt").getAsString()
                        .contains("open the first result")),
                () -> assertTrue(apply.arguments().get("prompt").getAsString()
                        .contains("assert the final page title")));
    }

    @Test
    void triageAndFixTestFailureRouteToDoctorWithWorkingDirectory() {
        String workingDirectory = "C:/work/project";
        AssistantCommand.Invocation triage = command("/triage", workingDirectory);
        AssistantCommand.Invocation fix = command("/fixTestFailure", workingDirectory);

        assertEquals("doctor_analyze_failed_allure", triage.toolName());
        assertEquals("doctor_analyze_failed_allure", fix.toolName());
        // Empty allureResultPaths tells the MCP server to auto-discover the most recent results.
        assertTrue(triage.arguments().get("allureResultPaths").getAsJsonArray().isEmpty());
        assertEquals(workingDirectory, triage.arguments().get("repositoryRoot").getAsString());
        assertEquals(workingDirectory, fix.arguments().get("repositoryRoot").getAsString());
    }

    @Test
    void helpAndUnknownCommandsStayLocal() {
        assertTrue(command("/help").isLocal());
        assertTrue(command("/help").localResponse().contains("/record"));
        assertTrue(command("/missing").isLocal());
        assertTrue(command("   ").isLocal());
    }

    @Test
    void cloudPromptsIncludeShaftOptionsHintSoClarifyingQuestionsRenderChips() {
        // Issue #3690: the shaft-options fence instruction (added to SHAFT_MCP_USAGE_HINT by #3674)
        // must also reach cloud-provider chat prompts (autobot_provider_chat), not just local-agent
        // ones, so a clarifying question asked by a cloud model still renders selectable chips.
        AssistantCommand.Invocation nonCodegen = AssistantCommand.fromPrompt(
                "Which login flow should I test?",
                AssistantCommand.Selection.cloud("openai", "gpt-5"),
                "ASK",
                ".",
                "",
                false);
        AssistantCommand.Invocation codegen = AssistantCommand.fromPrompt(
                "generate test for the login page",
                AssistantCommand.Selection.cloud("openai", "gpt-5"),
                "ASK",
                ".",
                "",
                false);

        String nonCodegenPrompt = nonCodegen.arguments().get("prompt").getAsString();
        String codegenPrompt = codegen.arguments().get("prompt").getAsString();

        assertAll(
                () -> assertTrue(nonCodegenPrompt.contains("tagged shaft-options"), nonCodegenPrompt),
                () -> assertTrue(nonCodegenPrompt.contains("Which login flow should I test?"), nonCodegenPrompt),
                () -> assertTrue(codegenPrompt.contains("tagged shaft-options"), codegenPrompt));
    }

    private static AssistantCommand.Invocation command(String prompt) {
        return command(prompt, ".");
    }

    private static AssistantCommand.Invocation command(String prompt, String workingDirectory) {
        return AssistantCommand.fromPrompt(prompt, "CODEX", "ASK", workingDirectory, "", false);
    }
}
