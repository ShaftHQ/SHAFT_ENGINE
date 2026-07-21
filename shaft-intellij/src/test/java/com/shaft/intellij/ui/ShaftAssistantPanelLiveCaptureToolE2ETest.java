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
 * Live end-to-end coverage of {@code CaptureService} (issue #3872, tracked by #3866 T6): the WEB
 * recording chain ({@code capture_start} -> {@code capture_status} -> {@code capture_stop}) and the
 * API-network recording chain ({@code capture_api_start} -> {@code capture_api_status} -> {@code
 * capture_api_stop}), driven through {@link ShaftAssistantPanel#send} exactly like a real user typing
 * into the chat composer and clicking Send. See {@code ShaftAssistantPanelLiveToolE2ETest} for the
 * gate mechanics and harness this class shares ({@link LiveChatToolE2ESupport}).
 *
 * <p>{@code capture_start} on the WEB engine launches its own privacy-safe SHAFT-managed browser via
 * CDP capture -- it does NOT need a prior {@code driver_initialize} call (per its own tool
 * description), so this class never touches EngineService/BrowserService/ElementService directly.</p>
 */
class ShaftAssistantPanelLiveCaptureToolE2ETest {

    /**
     * {@code capture_start} (targeting a local fixture) -> {@code capture_status} (asserts an ACTIVE
     * WEB session) -> {@code capture_stop} (discarding the recording, so no generated-code follow-up
     * is needed for this smoke coverage).
     */
    @Test
    @Timeout(180)
    void captureServiceRunsAWebRecordingChainThroughTheRealChatPanel() throws Exception {
        LiveContext context = LiveContext.assumeConfigured();
        Path fixture = context.workspace().resolve("fixtures/capture-web.html");
        Files.createDirectories(fixture.getParent());
        Files.writeString(fixture, webFixture(), StandardCharsets.UTF_8);
        String fixtureUrl = fixture.toUri().toString();

        try (LiveChatToolE2ESupport support = LiveChatToolE2ESupport.install(context.workspace(), context.mcpCommand())) {
            ShaftAssistantPanel panel = support.newPanel();

            String startResponse = support.send(panel,
                    "/mcp capture_start {\"targetUrl\":\"" + fixtureUrl + "\",\"browser\":\"Chrome\","
                            + "\"headless\":true,\"sessionGoal\":\"live e2e smoke\"}",
                    Duration.ofSeconds(120));
            String startPayload = LiveChatToolE2ESupport.unwrapToolPayload(startResponse);
            assertNotError(startResponse, "capture_start");
            assertTrue(startPayload.contains("\"WEB\""), "Expected the WEB engine discriminator: " + startPayload);

            String statusResponse = support.send(panel, "/mcp capture_status {}", Duration.ofSeconds(60));
            String statusPayload = LiveChatToolE2ESupport.unwrapToolPayload(statusResponse);
            assertNotError(statusResponse, "capture_status");
            assertTrue(statusPayload.contains("ACTIVE"),
                    "Expected an ACTIVE recording session: " + statusPayload);

            String stopResponse = support.send(panel, "/mcp capture_stop {\"discard\":true}", Duration.ofSeconds(60));
            assertNotError(stopResponse, "capture_stop");
        }
    }

    /**
     * {@code capture_api_start} (WEB engine, network capture enabled) -> {@code capture_api_status}
     * -> {@code capture_api_stop}, proving the API-recording half of {@code CaptureService} that
     * {@link #captureServiceRunsAWebRecordingChainThroughTheRealChatPanel} does not exercise.
     */
    @Test
    @Timeout(180)
    void captureServiceRunsAnApiRecordingChainThroughTheRealChatPanel() throws Exception {
        LiveContext context = LiveContext.assumeConfigured();
        Path fixture = context.workspace().resolve("fixtures/capture-api.html");
        Files.createDirectories(fixture.getParent());
        Files.writeString(fixture, webFixture(), StandardCharsets.UTF_8);
        String fixtureUrl = fixture.toUri().toString();

        try (LiveChatToolE2ESupport support = LiveChatToolE2ESupport.install(context.workspace(), context.mcpCommand())) {
            ShaftAssistantPanel panel = support.newPanel();

            String startResponse = support.send(panel,
                    "/mcp capture_api_start {\"targetUrl\":\"" + fixtureUrl + "\",\"browser\":\"Chrome\","
                            + "\"headless\":true,\"networkOptions\":{\"enabled\":true,\"captureRequestBodies\":false,"
                            + "\"captureResponseBodies\":false}}",
                    Duration.ofSeconds(120));
            assertNotError(startResponse, "capture_api_start");

            String statusResponse = support.send(panel, "/mcp capture_api_status {}", Duration.ofSeconds(60));
            assertNotError(statusResponse, "capture_api_status");

            String stopResponse = support.send(panel, "/mcp capture_api_stop {\"discard\":true}", Duration.ofSeconds(60));
            assertNotError(stopResponse, "capture_api_stop");
        }
    }

    private static void assertNotError(String rawResponse, String toolName) {
        assertTrue(rawResponse != null && !rawResponse.isBlank(), toolName + ": expected a non-blank response");
        assertFalse(rawResponse.contains("\"isError\":true"), toolName + ": MCP reported an error: " + rawResponse);
    }

    private static String webFixture() {
        return """
                <!doctype html>
                <html lang="en"><head><meta charset="utf-8"><title>SHAFT Live Capture Fixture</title></head>
                <body>
                <h1>SHAFT Live Capture Fixture</h1>
                <input id="username" name="username" type="text" placeholder="Username">
                <button id="go" type="button" onclick="document.title='SHAFT Live Capture Fixture - done'">Go</button>
                </body></html>
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
