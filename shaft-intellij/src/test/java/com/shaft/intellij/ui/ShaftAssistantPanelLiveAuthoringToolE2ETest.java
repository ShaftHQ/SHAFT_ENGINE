package com.shaft.intellij.ui;

import org.junit.jupiter.api.Assumptions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.Timeout;

import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Duration;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Live end-to-end coverage of the authoring/guidance service groups (issue #3872, tracked by #3866
 * T6): {@code GuideService}, {@code ShaftProjectService}, {@code TestAutomationService}, {@code
 * PlannerService}, and {@code CodingPartnerService}. See {@code ShaftAssistantPanelLiveToolE2ETest}
 * for the gate mechanics and shared harness ({@link LiveChatToolE2ESupport}).
 */
class ShaftAssistantPanelLiveAuthoringToolE2ETest {

    /**
     * {@code shaft_guide_search} is the one tool in this class that reaches an external host
     * (the live shafthq.github.io docs index) -- honestly reported rather than excluded outright,
     * since the sandboxed CI runners this suite targets do have outbound HTTPS.
     */
    @Test
    @Timeout(90)
    void guideServiceSearchesTheLiveUserGuideThroughTheRealChatPanel() throws Exception {
        LiveContext context = LiveContext.assumeConfigured();

        try (LiveChatToolE2ESupport support = LiveChatToolE2ESupport.install(context.workspace(), context.mcpCommand())) {
            ShaftAssistantPanel panel = support.newPanel();

            String response = support.send(panel,
                    "/mcp shaft_guide_search {\"query\":\"page object model\",\"maxResults\":3}",
                    Duration.ofSeconds(60));
            String payload = LiveChatToolE2ESupport.unwrapToolPayload(response);

            assertNotError(response, "shaft_guide_search");
            assertTrue(payload.contains("\"matches\""), "Expected a matches[] field: " + payload);
        }
    }

    /**
     * {@code shaft_project_create} (explicit {@code shaftVersion} avoids the Maven Central lookup so
     * this stays network-free) followed by {@code shaft_project_init_agents} against the freshly
     * created project directory.
     */
    @Test
    @Timeout(120)
    void shaftProjectServiceCreatesAndInitializesAgentsThroughTheRealChatPanel() throws Exception {
        LiveContext context = LiveContext.assumeConfigured();

        try (LiveChatToolE2ESupport support = LiveChatToolE2ESupport.install(context.workspace(), context.mcpCommand())) {
            ShaftAssistantPanel panel = support.newPanel();

            String createResponse = support.send(panel,
                    "/mcp shaft_project_create {\"outputDirectory\":\"live-e2e-demo-project\",\"runner\":\"TestNG\","
                            + "\"platform\":\"web\",\"groupId\":\"\",\"artifactId\":\"\",\"version\":\"\","
                            + "\"shaftVersion\":\"1.0.0\",\"optionalModules\":[],\"includeGithubActions\":false,"
                            + "\"includeDependabot\":false,\"overwrite\":true}",
                    Duration.ofSeconds(90));
            String createPayload = LiveChatToolE2ESupport.unwrapToolPayload(createResponse);
            assertNotError(createResponse, "shaft_project_create");
            assertTrue(createPayload.contains("pomPath"), "Expected a generated pomPath: " + createPayload);
            assertTrue(Files.exists(context.workspace().resolve("live-e2e-demo-project/pom.xml")),
                    "Expected a real generated pom.xml on disk");

            String initAgentsResponse = support.send(panel,
                    "/mcp shaft_project_init_agents {\"loop\":\"claude\","
                            + "\"targetDirectory\":\"live-e2e-demo-project\",\"overwrite\":true}",
                    Duration.ofSeconds(60));
            assertNotError(initAgentsResponse, "shaft_project_init_agents");

            // dryRun=true: previews the upgrade plan against the just-created project without ever
            // approving a real modification (approve=false is the tool's own required companion to
            // dryRun=false, so this is the only combination that runs read-only).
            String upgradeResponse = support.send(panel,
                    "/mcp shaft_project_upgrade {\"projectRoot\":\"live-e2e-demo-project\","
                            + "\"upgradeType\":\"basic\",\"dryRun\":true,\"approve\":false,\"shaftVersion\":\"\","
                            + "\"compileCommand\":\"\",\"compileTimeout\":0,\"skipBaselineCompile\":true,"
                            + "\"allowAiRepair\":false}",
                    Duration.ofSeconds(90));
            assertNotError(upgradeResponse, "shaft_project_upgrade");
        }
    }

    /** {@code test_automation_scenarios} and {@code test_code_guardrails_check}: pure, side-effect-free catalogs. */
    @Test
    @Timeout(60)
    void testAutomationServiceLooksUpScenariosAndChecksGuardrailsThroughTheRealChatPanel() throws Exception {
        LiveContext context = LiveContext.assumeConfigured();

        try (LiveChatToolE2ESupport support = LiveChatToolE2ESupport.install(context.workspace(), context.mcpCommand())) {
            ShaftAssistantPanel panel = support.newPanel();

            String scenariosResponse = support.send(panel,
                    "/mcp test_automation_scenarios {\"area\":\"web\",\"intent\":\"login\",\"maxResults\":5}",
                    Duration.ofSeconds(30));
            String scenariosPayload = LiveChatToolE2ESupport.unwrapToolPayload(scenariosResponse);
            assertNotError(scenariosResponse, "test_automation_scenarios");
            assertTrue(scenariosPayload.contains("\"scenarios\""), "Expected scenarios[]: " + scenariosPayload);

            String guardrailsResponse = support.send(panel,
                    "/mcp test_code_guardrails_check {\"language\":\"java\","
                            + "\"code\":\"driver.element().click(SHAFT.GUI.Locator.id(\\\"login\\\"));\"}",
                    Duration.ofSeconds(30));
            assertNotError(guardrailsResponse, "test_code_guardrails_check");
        }
    }

    /**
     * {@code test_plan_explore} requires an active driver session (per {@code EngineService.getDriver()})
     * AND rejects any {@code targetUrl} that is not absolute with a real scheme+host -- a {@code
     * file://} fixture URL's empty host fails that check, so (like {@code
     * guideServiceSearchesTheLiveUserGuideThroughTheRealChatPanel}) this test accepts a real,
     * stable, single-page public URL instead of fabricating a local one; {@code maxPages=1} keeps
     * the crawl to that one page.
     */
    @Test
    @Timeout(150)
    void plannerServiceExploresARealHeadlessSessionThroughTheRealChatPanel() throws Exception {
        LiveContext context = LiveContext.assumeConfigured();

        try (LiveChatToolE2ESupport support = LiveChatToolE2ESupport.install(context.workspace(), context.mcpCommand())) {
            ShaftAssistantPanel panel = support.newPanel();

            assertNotError(support.send(panel, "/mcp driver_initialize {\"targetBrowser\":\"CHROME\"}",
                    Duration.ofSeconds(120)), "driver_initialize");

            String exploreResponse = support.send(panel,
                    "/mcp test_plan_explore {\"targetUrl\":\"https://example.com/\",\"goal\":\"smoke\","
                            + "\"maxDepth\":1,\"maxPages\":1}",
                    Duration.ofSeconds(60));
            String explorePayload = LiveChatToolE2ESupport.unwrapToolPayload(exploreResponse);
            assertNotError(exploreResponse, "test_plan_explore");
            assertTrue(explorePayload.contains("pagesVisited"), "Expected pagesVisited: " + explorePayload);

            assertNotError(support.send(panel, "/mcp driver_quit {}", Duration.ofSeconds(30)), "driver_quit");
        }
    }

    /**
     * {@code shaft_coding_partner_plan} and {@code shaft_coding_partner_diff}, both read-only/preview
     * tools scoped to the workspace root itself (always exists, no prior project creation needed).
     */
    @Test
    @Timeout(90)
    void codingPartnerServicePlansAndDiffsThroughTheRealChatPanel() throws Exception {
        LiveContext context = LiveContext.assumeConfigured();

        try (LiveChatToolE2ESupport support = LiveChatToolE2ESupport.install(context.workspace(), context.mcpCommand())) {
            ShaftAssistantPanel panel = support.newPanel();

            String planResponse = support.send(panel,
                    "/mcp shaft_coding_partner_plan {\"repositoryPath\":\".\",\"intent\":\"add a login test\","
                            + "\"backend\":\"\",\"currentSourcePath\":\"\",\"selectedText\":\"\","
                            + "\"artifactPaths\":[],\"maxResults\":5}",
                    Duration.ofSeconds(60));
            String planPayload = LiveChatToolE2ESupport.unwrapToolPayload(planResponse);
            assertNotError(planResponse, "shaft_coding_partner_plan");
            assertTrue(planPayload.contains("workingSetSummary") || planPayload.contains("steps"),
                    "Expected a coding-partner plan payload: " + planPayload);

            String diffResponse = support.send(panel,
                    "/mcp shaft_coding_partner_diff {\"repositoryPath\":\".\","
                            + "\"targetSourcePath\":\"src/test/java/tests/LiveE2EDemoTest.java\","
                            + "\"codeBlocks\":[\"System.out.println(\\\"hi\\\");\"],\"insertionAnchor\":\"\"}",
                    Duration.ofSeconds(60));
            assertNotError(diffResponse, "shaft_coding_partner_diff");
        }
    }

    private static void assertNotError(String rawResponse, String toolName) {
        assertTrue(rawResponse != null && !rawResponse.isBlank(), toolName + ": expected a non-blank response");
        assertFalse(rawResponse.contains("\"isError\":true"), toolName + ": MCP reported an error: " + rawResponse);
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
