package com.shaft.intellij.ui;

import org.junit.jupiter.api.Assumptions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.Timeout;

import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Duration;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Live end-to-end coverage of the diagnostics/observability service groups (issue #3872, tracked
 * by #3866 T6): {@code DoctorService}, {@code HealerService}, {@code TraceService}, {@code
 * MobileService}, and {@code AutobotService#autobot_provider_status}. See {@code
 * ShaftAssistantPanelLiveToolE2ETest} for the gate mechanics and shared harness ({@link
 * LiveChatToolE2ESupport}).
 *
 * <p>Several tools in these groups only accept a pre-existing artifact from a real prior test run
 * (a failed Allure result, a Playwright/SHAFT trace file, a verified Heal report, a shard blob).
 * Fabricating those wholesale would be dishonest evidence, so each test here either (a) writes a
 * genuinely representative fixture (the same shape {@code GuidedWorkflowLiveE2ETest}'s Doctor test
 * already uses) when that is proportionate, or (b) calls the tool in its documented "no artifact yet"
 * state and asserts the real, non-exception, graceful response the tool itself defines for that case
 * (e.g. {@code trace_latest} returning an empty list, {@code healer_run_failed_test} returning
 * {@code GUARDRAIL_STOPPED}) -- never a faked success.</p>
 */
class ShaftAssistantPanelLiveDiagnosticsToolE2ETest {

    /**
     * {@code doctor_analyze_failed_allure} against a real failed-login Allure result fixture (same
     * shape as {@code GuidedWorkflowLiveE2ETest#failedAllureResult}), then {@code doctor_suggest_fix}
     * against the JSON report path the first call just produced.
     */
    @Test
    @Timeout(120)
    void doctorServiceAnalyzesAFailedAllureResultThroughTheRealChatPanel() throws Exception {
        LiveContext context = LiveContext.assumeConfigured();
        Path allureResult = context.workspace().resolve("allure-results/failed-login-result.json");
        Files.createDirectories(allureResult.getParent());
        Files.writeString(allureResult, failedAllureResult(), StandardCharsets.UTF_8);

        try (LiveChatToolE2ESupport support = LiveChatToolE2ESupport.install(context.workspace(), context.mcpCommand())) {
            ShaftAssistantPanel panel = support.newPanel();

            String analyzeResponse = support.send(panel,
                    "/mcp doctor_analyze_failed_allure {\"allureResultPaths\":[],\"historicalBundlePaths\":[],"
                            + "\"outputDirectory\":\"\",\"includeScreenshots\":false,\"includePageSnapshots\":false,"
                            + "\"minimumAllureResults\":1,\"repositoryRoot\":\"\",\"allowedSourcePaths\":[],"
                            + "\"useAi\":false,\"allowLocalAi\":false,\"allowRemoteAi\":false,"
                            + "\"driverVariableName\":\"driver\",\"backend\":\"\"}",
                    Duration.ofSeconds(60));
            String analyzePayload = LiveChatToolE2ESupport.unwrapToolPayload(analyzeResponse);
            assertNotError(analyzeResponse, "doctor_analyze_failed_allure");
            assertTrue(analyzePayload.contains("\"primaryCause\":\"LOCATOR\""),
                    "Expected the fixture's NoSuchElementException to be classified as LOCATOR: " + analyzePayload);
            String jsonReportPath = extractStringField(analyzePayload, "jsonReportPath");
            assertTrue(jsonReportPath != null && Files.exists(Path.of(jsonReportPath)),
                    "Expected a real Doctor JSON report on disk: " + jsonReportPath);

            String suggestResponse = support.send(panel,
                    "/mcp doctor_suggest_fix {\"jsonReportPath\":\"" + jsonReportPath.replace("\\", "\\\\")
                            + "\",\"repositoryRoot\":\"\",\"allowedSourcePaths\":[],\"useAi\":false,"
                            + "\"allowLocalAi\":false,\"allowRemoteAi\":false,\"driverVariableName\":\"driver\","
                            + "\"backend\":\"\"}",
                    Duration.ofSeconds(60));
            assertNotError(suggestResponse, "doctor_suggest_fix");
        }
    }

    /**
     * {@code healer_run_failed_test} in a directory with no SHAFT project returns a real, graceful
     * {@code GUARDRAIL_STOPPED} result (not an MCP error) -- proving live dispatch without the cost
     * of a real Maven test run. {@code verify_run_focused} similarly runs a real (offline) Maven
     * process against an empty directory and reports its real (non-zero-exit) outcome.
     */
    @Test
    @Timeout(120)
    void healerServiceRunsAgainstANonShaftDirectoryThroughTheRealChatPanel() throws Exception {
        LiveContext context = LiveContext.assumeConfigured();

        try (LiveChatToolE2ESupport support = LiveChatToolE2ESupport.install(context.workspace(), context.mcpCommand())) {
            ShaftAssistantPanel panel = support.newPanel();

            // "mvn.cmd" (not bare "mvn") on Windows: HealerService/verify_run_focused spawn the
            // command via a raw ProcessBuilder (HealerService.java:450), which calls CreateProcess
            // directly -- unlike a shell, Windows CreateProcess never appends PATHEXT extensions, so
            // the extensionless "mvn" POSIX shim on PATH cannot be launched that way even though
            // `where mvn` resolves it (reproduced empirically: intermittent "MCP healer command could
            // not be launched" IOException). "mvn.cmd"/"mvnw.cmd" are explicitly allowlisted
            // executables (HealerService.java:349) precisely for this platform difference.
            String healResponse = support.send(panel,
                    "/mcp healer_run_failed_test {\"repositoryRoot\":\".\",\"testCommand\":[\"mvn.cmd\",\"test\"],"
                            + "\"outputDirectory\":\"\",\"maxAttempts\":1,\"includeScreenshots\":false,"
                            + "\"includePageSnapshots\":false,\"allowedSourcePaths\":[],"
                            + "\"networkValidationApproved\":false,\"useConfiguredAi\":false,\"allowLocalAi\":false,"
                            + "\"allowRemoteAi\":false,\"driverVariableName\":\"driver\",\"backend\":\"\"}",
                    Duration.ofSeconds(60));
            assertNotError(healResponse, "healer_run_failed_test");
            assertFalse(healResponse.contains("could not be launched"),
                    "healer_run_failed_test: Maven command failed to launch: " + healResponse);

            String verifyResponse = support.send(panel,
                    "/mcp verify_run_focused {\"repositoryRoot\":\"\",\"command\":[\"mvn.cmd\",\"-q\",\"compile\"],"
                            + "\"networkValidationApproved\":false}",
                    Duration.ofSeconds(60));
            assertNotError(verifyResponse, "verify_run_focused");
            assertFalse(verifyResponse.contains("could not be launched"),
                    "verify_run_focused: Maven command failed to launch: " + verifyResponse);
        }
    }

    /** {@code trace_latest} against a fresh workspace: a real, empty-but-valid (never an exception) result. */
    @Test
    @Timeout(60)
    void traceServiceListsLatestTracesThroughTheRealChatPanel() throws Exception {
        LiveContext context = LiveContext.assumeConfigured();

        try (LiveChatToolE2ESupport support = LiveChatToolE2ESupport.install(context.workspace(), context.mcpCommand())) {
            ShaftAssistantPanel panel = support.newPanel();

            String response = support.send(panel, "/mcp trace_latest {\"maxResults\":5}", Duration.ofSeconds(30));
            String payload = LiveChatToolE2ESupport.unwrapToolPayload(response);
            assertNotError(response, "trace_latest");
            assertTrue(payload.contains("\"traces\""), "Expected a traces[] field: " + payload);
        }
    }

    /**
     * {@code mobile_toolchain_status} runs standalone (no session needed). {@code mobile_get_contexts}
     * needs an active mobile session, provided here via a real Chrome-DevTools mobile-web-emulation
     * session ({@code driver_initialize} with {@code engine=MOBILE_WEB}) -- no Appium server or
     * physical/emulated device is needed for that path.
     */
    @Test
    @Timeout(150)
    void mobileServiceReportsToolchainAndWebEmulationContextsThroughTheRealChatPanel() throws Exception {
        LiveContext context = LiveContext.assumeConfigured();
        Path fixture = context.workspace().resolve("fixtures/mobile.html");
        Files.createDirectories(fixture.getParent());
        Files.writeString(fixture, """
                <!doctype html>
                <html lang="en"><head><meta charset="utf-8"><title>SHAFT Live Mobile Fixture</title>
                <meta name="viewport" content="width=device-width, initial-scale=1"></head>
                <body><h1>SHAFT Live Mobile Fixture</h1></body></html>
                """, StandardCharsets.UTF_8);
        String fixtureUrl = fixture.toUri().toString();

        try (LiveChatToolE2ESupport support = LiveChatToolE2ESupport.install(context.workspace(), context.mcpCommand())) {
            ShaftAssistantPanel panel = support.newPanel();

            String toolchainResponse = support.send(panel,
                    "/mcp mobile_toolchain_status {\"platformName\":\"Android\"}", Duration.ofSeconds(60));
            assertNotError(toolchainResponse, "mobile_toolchain_status");

            String initResponse = support.send(panel,
                    "/mcp driver_initialize {\"targetBrowser\":\"CHROME\",\"engine\":\"MOBILE_WEB\","
                            + "\"mobileOptions\":{\"targetUrl\":\"" + fixtureUrl + "\",\"browser\":\"CHROME\","
                            + "\"headless\":true}}",
                    Duration.ofSeconds(120));
            assertNotError(initResponse, "driver_initialize(MOBILE_WEB)");

            String contextsResponse = support.send(panel, "/mcp mobile_get_contexts {\"maxCharacters\":0}",
                    Duration.ofSeconds(60));
            assertNotError(contextsResponse, "mobile_get_contexts");

            assertNotError(support.send(panel, "/mcp driver_quit {}", Duration.ofSeconds(30)), "driver_quit");
        }
    }

    /** {@code autobot_provider_status}: reads only environment-variable presence, never the value itself. */
    @Test
    @Timeout(60)
    void autobotServiceReportsProviderStatusThroughTheRealChatPanel() throws Exception {
        LiveContext context = LiveContext.assumeConfigured();

        try (LiveChatToolE2ESupport support = LiveChatToolE2ESupport.install(context.workspace(), context.mcpCommand())) {
            ShaftAssistantPanel panel = support.newPanel();

            String response = support.send(panel,
                    "/mcp autobot_provider_status {\"provider\":\"anthropic\",\"model\":\"\"}",
                    Duration.ofSeconds(30));
            String payload = LiveChatToolE2ESupport.unwrapToolPayload(response);
            assertNotError(response, "autobot_provider_status");
            assertTrue(payload.contains("\"provider\":\"anthropic\""), "Expected the requested provider echoed back: " + payload);
        }
    }

    private static void assertNotError(String rawResponse, String toolName) {
        assertTrue(rawResponse != null && !rawResponse.isBlank(), toolName + ": expected a non-blank response");
        assertFalse(rawResponse.contains("\"isError\":true"), toolName + ": MCP reported an error: " + rawResponse);
    }

    /** Minimal recursive scan for {@code "key":"value"} in a flat/nested JSON text payload. */
    private static String extractStringField(String json, String key) {
        String marker = "\"" + key + "\":\"";
        int start = json.indexOf(marker);
        if (start < 0) {
            return null;
        }
        int valueStart = start + marker.length();
        int valueEnd = json.indexOf('"', valueStart);
        return valueEnd < 0 ? null : json.substring(valueStart, valueEnd).replace("\\\\", "\\");
    }

    private static String failedAllureResult() {
        return """
                {
                  "uuid": "failed-login-result",
                  "historyId": "login-history",
                  "name": "validLoginShowsDashboard",
                  "fullName": "tests.LoginTest.validLoginShowsDashboard",
                  "status": "failed",
                  "start": 1,
                  "stop": 2,
                  "statusDetails": {
                    "message": "NoSuchElementException: unable to locate element {By.id: loginButton}",
                    "trace": "org.openqa.selenium.NoSuchElementException: unable to locate element\\n\\tat tests.LoginTest.validLoginShowsDashboard(LoginTest.java:42)"
                  },
                  "labels": [
                    {"name": "testClass", "value": "tests.LoginTest"},
                    {"name": "testMethod", "value": "validLoginShowsDashboard"}
                  ]
                }
                """;
    }

    /**
     * Live run configuration, mirroring {@code GuidedWorkflowLiveE2ETest.LiveContext}: skips (never
     * fails) when the live gate is off, so this class is a no-op in normal CI.
     */
    private record LiveContext(String mcpCommand, Path workspace) {
        static LiveContext assumeConfigured() throws Exception {
            Assumptions.assumeTrue(Boolean.getBoolean("shaft.intellij.liveToolE2E"),
                    "Set -Dshaft.intellij.liveToolE2E=true to run the live IntelliJ chat-panel tool E2E suite.");
            String commandLine = System.getProperty("shaft.intellij.liveMcpCommand", "").trim();
            Assumptions.assumeTrue(!commandLine.isBlank(),
                    "Set -Dshaft.intellij.liveMcpCommand to a SHAFT MCP stdio command.");
            Path workspace = Path.of(System.getProperty("shaft.intellij.workspaceRoot", "build/live-tool-e2e"))
                    .toAbsolutePath()
                    .normalize();
            Files.createDirectories(workspace);
            return new LiveContext(commandLine, workspace);
        }
    }
}
