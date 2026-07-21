package com.shaft.intellij.ui;

import com.google.gson.JsonParser;
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
 * Live end-to-end verification that {@link ShaftAssistantPanel#send} -- the real IntelliJ
 * Assistant chat entry point -- can actually reach a real SHAFT MCP server (issue #3872, tracked
 * by #3866 T6). Every request is typed into the real composer and dispatched by clicking the real
 * "Send" button (accessible name {@code "Send assistant prompt"}), exactly as the sibling {@code
 * GuidedWorkflowLiveE2ETest} drives {@code GuidedWorkflowPanel} -- never a direct call into {@code
 * ShaftMcpInvocationService} bypassing the panel.
 *
 * <p>Gated by {@code -Dshaft.intellij.liveToolE2E=true} (absent/false: every test in this class is
 * skipped, so normal CI stays fast and green) plus the same {@code -Dshaft.intellij.liveMcpCommand}
 * / {@code -Dshaft.intellij.workspaceRoot} properties the sibling live suites already use.</p>
 *
 * <p>Prompts use the {@code /mcp <toolName> [json]} slash command (issue #3870/#3866 T4, {@code
 * AssistantCommand#rawMcp}): it is the one deterministic router that reaches every one of the
 * 89 surviving tools directly, so a representative call per service group needs no guessing at
 * natural-language phrasing -- {@code AssistantCommand#fromPrompt} parses it into an {@code
 * Invocation.tool(toolName, parsedJson)} with zero ambiguity, verified separately and exhaustively
 * by {@code AssistantCommandRoutingTest}/{@code ShaftAssistantPromptRoutingTest}. This class exists
 * to prove the OTHER half: that the resulting invocation actually reaches a live MCP server through
 * the panel's own dispatch path and comes back with a real, non-error response.</p>
 */
class ShaftAssistantPanelLiveToolE2ETest {

    /**
     * Proof-of-concept for the whole harness (issue #3872 T6): {@code autobot_local_agent_clients}
     * takes no arguments, needs no network access, and no external CLI/credentials -- the simplest
     * possible tool call to prove {@link LiveChatToolE2ESupport} actually reaches a live MCP server
     * through {@link ShaftAssistantPanel#send}.
     */
    @Test
    @Timeout(120)
    void autobotServiceListsLocalAgentClientsThroughTheRealChatPanel() throws Exception {
        LiveContext context = LiveContext.assumeConfigured();

        try (LiveChatToolE2ESupport support = LiveChatToolE2ESupport.install(context.workspace(), context.mcpCommand())) {
            ShaftAssistantPanel panel = support.newPanel();

            String rawResponse = support.send(panel, "/mcp autobot_local_agent_clients", Duration.ofSeconds(90));
            String payload = LiveChatToolE2ESupport.unwrapToolPayload(rawResponse);

            assertTrue(payload != null && !payload.isBlank(),
                    "Expected a non-blank raw MCP response. Transcript:\n" + panel.transcriptMarkdown());
            var parsed = JsonParser.parseString(payload);
            assertTrue(parsed.isJsonArray(), "autobot_local_agent_clients should return a JSON array: " + payload);
            assertTrue(!parsed.getAsJsonArray().isEmpty(),
                    "autobot_local_agent_clients should list at least the built-in CLI clients: " + payload);
            assertTrue(payload.contains("CODEX") && payload.contains("CLAUDE_CODE") && payload.contains("COPILOT_CLI"),
                    "Expected the three built-in local agent clients: " + payload);
        }
    }

    /**
     * Covers EngineService (issue #3872 T6), ElementService, and BrowserService in one realistic
     * headless web session: {@code driver_initialize} starts a real (headless, per {@code
     * -DheadlessExecution=true} on the spawned MCP server) Chrome session, {@code browser_navigate}
     * opens a local fixture, {@code element_type}/{@code element_click} drive a real form,
     * {@code browser_get_title} reads back the result, and {@code driver_quit} tears the session down
     * -- all six calls share the one spawned MCP server process/driver session this fixture keeps
     * alive for the whole test.
     */
    @Test
    @Timeout(180)
    void engineElementAndBrowserServicesDriveARealHeadlessSessionThroughTheRealChatPanel() throws Exception {
        LiveContext context = LiveContext.assumeConfigured();
        Path fixture = context.workspace().resolve("fixtures/engine-element-browser.html");
        Files.createDirectories(fixture.getParent());
        Files.writeString(fixture, webFixture(), StandardCharsets.UTF_8);
        String fixtureUrl = fixture.toUri().toString();

        try (LiveChatToolE2ESupport support = LiveChatToolE2ESupport.install(context.workspace(), context.mcpCommand())) {
            ShaftAssistantPanel panel = support.newPanel();

            String initResponse = support.send(panel, "/mcp driver_initialize {\"targetBrowser\":\"CHROME\"}",
                    Duration.ofSeconds(120));
            assertNotError(initResponse, "driver_initialize");

            String navigateResponse = support.send(panel,
                    "/mcp browser_navigate {\"targetUrl\":\"" + fixtureUrl + "\"}", Duration.ofSeconds(60));
            assertNotError(navigateResponse, "browser_navigate");

            String typeResponse = support.send(panel,
                    "/mcp element_type {\"locatorStrategy\":\"ID\",\"locatorValue\":\"username\","
                            + "\"text\":\"shaft-live-e2e\"}",
                    Duration.ofSeconds(60));
            assertNotError(typeResponse, "element_type");

            String clickResponse = support.send(panel,
                    "/mcp element_click {\"locatorStrategy\":\"ID\",\"locatorValue\":\"go\"}", Duration.ofSeconds(60));
            assertNotError(clickResponse, "element_click");

            String titleResponse = support.send(panel, "/mcp browser_get_title {}", Duration.ofSeconds(60));
            String title = LiveChatToolE2ESupport.unwrapToolPayload(titleResponse);
            // Proves element_click actually landed on the real page (not merely a non-error MCP
            // response): the click handler mutates document.title, only observable here.
            assertTrue(title != null && title.contains("done"),
                    "Expected the click-mutated page title back from a live browser session: " + title);

            String quitResponse = support.send(panel, "/mcp driver_quit {}", Duration.ofSeconds(60));
            assertNotError(quitResponse, "driver_quit");
        }
    }

    private static void assertNotError(String rawResponse, String toolName) {
        assertTrue(rawResponse != null && !rawResponse.isBlank(), toolName + ": expected a non-blank response");
        assertFalse(rawResponse.contains("\"isError\":true"), toolName + ": MCP reported an error: " + rawResponse);
    }

    private static String webFixture() {
        return """
                <!doctype html>
                <html lang="en"><head><meta charset="utf-8"><title>SHAFT Live E2E Fixture</title></head>
                <body>
                <h1>SHAFT Live E2E Fixture</h1>
                <input id="username" name="username" type="text" placeholder="Username">
                <button id="go" type="button" onclick="document.title='SHAFT Live E2E Fixture - done'">Go</button>
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
