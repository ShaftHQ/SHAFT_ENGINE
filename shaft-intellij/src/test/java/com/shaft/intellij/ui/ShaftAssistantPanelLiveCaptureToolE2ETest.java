package com.shaft.intellij.ui;

import com.google.gson.JsonObject;
import org.junit.jupiter.api.Assumptions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.Timeout;

import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Duration;
import java.util.List;
import java.util.concurrent.TimeUnit;

import static org.junit.jupiter.api.Assertions.assertEquals;
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

    @Test
    @Timeout(900)
    void codegenScenarioRecordsAndReturnsToModularSourceGeneration() throws Exception {
        Assumptions.assumeTrue(Boolean.getBoolean("shaft.intellij.liveAgentE2E"),
                "Set -Dshaft.intellij.liveAgentE2E=true to run the real local-agent integration flow.");
        Assumptions.assumeTrue(Boolean.getBoolean("shaft.intellij.liveUnrestrictedAgentE2E"),
                "Set -Dshaft.intellij.liveUnrestrictedAgentE2E=true to authorize the no-sandbox recovery flow.");
        LiveContext context = LiveContext.assumeConfigured();
        Path projectRoot = context.workspace().resolve("prompt-record-codegen");
        resetDirectory(projectRoot);
        Files.createDirectories(projectRoot);
        Path fixture = projectRoot.resolve("fixtures/scenario.html");
        Files.createDirectories(fixture.getParent());
        Files.writeString(fixture, webFixture(), StandardCharsets.UTF_8);
        String fixtureUrl = fixture.toUri().toString();
        String shaftVersion = System.getProperty("shaft.intellij.liveShaftVersion", "").trim();
        assertTrue(shaftVersion.matches("[0-9A-Za-z.-]+"), "A concrete SHAFT version is required: " + shaftVersion);

        try (LiveChatToolE2ESupport support = LiveChatToolE2ESupport.install(projectRoot, context.mcpCommand())) {
            ShaftAssistantPanel panel = support.newPanel();
            assertNotError(support.send(panel,
                    "/mcp shaft_project_create {\"outputDirectory\":\".\",\"runner\":\"TestNG\","
                            + "\"platform\":\"web\",\"groupId\":\"com.example\",\"artifactId\":\"recorded-flow\","
                            + "\"version\":\"1.0.0\",\"shaftVersion\":\"" + shaftVersion
                            + "\",\"optionalModules\":[],"
                            + "\"includeGithubActions\":false,\"includeDependabot\":false,\"overwrite\":true}",
                    Duration.ofSeconds(90)), "shaft_project_create");
            assertTrue(Files.isRegularFile(projectRoot.resolve("pom.xml")),
                    "shaft_project_create must produce a Maven build descriptor");
            String originalPom = Files.readString(projectRoot.resolve("pom.xml"), StandardCharsets.UTF_8);
            assertNotError(support.send(panel,
                    "/mcp shaft_project_init_agents {\"loop\":\"codex\",\"targetDirectory\":\".\","
                            + "\"overwrite\":true}", Duration.ofSeconds(60)), "shaft_project_init_agents");

            String scenario = "navigate to " + fixtureUrl
                    + ", enter mohab in the username field, click Go, and assert the final title";
            String startResponse = support.send(panel, "/codegen " + scenario, Duration.ofSeconds(120));
            assertNotError(startResponse, "capture_start");
            assertTrue(LiveChatToolE2ESupport.unwrapToolPayload(startResponse).contains("\"ACTIVE\""),
                    startResponse);

            assertNotError(support.send(panel,
                    "/mcp element_type {\"locatorStrategy\":\"ID\",\"locatorValue\":\"username\","
                            + "\"text\":\"mohab\",\"append\":false,\"clear\":true}",
                    Duration.ofSeconds(60)), "element_type");
            assertNotError(support.send(panel,
                    "/mcp element_click {\"locatorStrategy\":\"ID\",\"locatorValue\":\"go\","
                            + "\"mode\":\"SINGLE\"}", Duration.ofSeconds(60)), "element_click");

            String reviewResponse = support.send(panel, "stop recording", Duration.ofSeconds(120));
            assertNotError(reviewResponse, "capture_code_blocks");
            assertTrue(panel.transcriptMarkdown().contains("Awaiting approval")
                            || panel.transcriptMarkdown().contains("Review before writing files"),
                    panel.transcriptMarkdown());

            String integrationResponse = support.send(panel, "approve", Duration.ofSeconds(480));
            assertFalse(integrationResponse.isBlank(), "approved local agent integration must return a result");

            List<Path> javaFiles;
            try (var files = Files.walk(projectRoot.resolve("src/test/java"))) {
                javaFiles = files.filter(path -> path.toString().endsWith(".java")).toList();
                assertTrue(javaFiles.stream().anyMatch(path -> path.getFileName().toString().contains("Page")),
                        "Expected a generated page object: " + javaFiles + "\nAgent result:\n" + integrationResponse
                                + "\nTranscript:\n" + panel.transcriptMarkdown());
                assertTrue(javaFiles.stream().anyMatch(path -> path.getFileName().toString().endsWith("Test.java")),
                        "Expected a generated test class: " + javaFiles + "\nAgent result:\n" + integrationResponse
                                + "\nTranscript:\n" + panel.transcriptMarkdown());
            }
            assertEquals(originalPom, Files.readString(projectRoot.resolve("pom.xml"), StandardCharsets.UTF_8),
                    "The local agent must preserve the generated Maven build descriptor byte-for-byte");
            assertShaftGeneratedCodeQuality(support, panel, projectRoot, javaFiles);
            assertGeneratedProjectCompiles(projectRoot);
        }
    }

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

    private static void resetDirectory(Path directory) throws Exception {
        if (!Files.exists(directory)) {
            return;
        }
        try (var paths = Files.walk(directory)) {
            for (Path path : paths.sorted((left, right) -> right.compareTo(left)).toList()) {
                Files.delete(path);
            }
        }
    }

    private static void assertGeneratedProjectCompiles(Path projectRoot) throws Exception {
        String maven = System.getProperty("os.name", "").toLowerCase().contains("win") ? "mvn.cmd" : "mvn";
        Process process = new ProcessBuilder(maven, "--quiet", "-DskipTests",
                "-Dallure.automaticallyOpen=false", "test-compile")
                .directory(projectRoot.toFile())
                .redirectErrorStream(true)
                .start();
        boolean finished = process.waitFor(180, TimeUnit.SECONDS);
        if (!finished) {
            process.destroyForcibly();
        }
        String output = new String(process.getInputStream().readAllBytes(), StandardCharsets.UTF_8);
        assertTrue(finished, "Generated project test-compile timed out:\n" + output);
        assertTrue(process.exitValue() == 0, "Generated project test-compile failed:\n" + output);
    }

    private static void assertShaftGeneratedCodeQuality(
            LiveChatToolE2ESupport support, ShaftAssistantPanel panel, Path projectRoot, List<Path> javaFiles)
            throws Exception {
        Path pagePath = javaFiles.stream()
                .filter(path -> path.getFileName().toString().contains("Page"))
                .findFirst().orElseThrow();
        Path testPath = javaFiles.stream()
                .filter(path -> path.getFileName().toString().endsWith("Test.java"))
                .findFirst().orElseThrow();
        String pageSource = Files.readString(pagePath, StandardCharsets.UTF_8);
        String testSource = Files.readString(testPath, StandardCharsets.UTF_8);
        String combinedSource = pageSource + "\n" + testSource;

        assertTrue(pageSource.contains("SHAFT.GUI.Locator"), "Page locators must use SHAFT.GUI.Locator:\n" + pageSource);
        assertTrue(pageSource.contains("hasId(\"username\")") && pageSource.contains("hasId(\"go\")"),
                "Recorded controls must use stable ID locators:\n" + pageSource);
        assertTrue(pageSource.contains(".element().type(") && pageSource.contains(".element().click("),
                "Recorded actions must use SHAFT element APIs:\n" + pageSource);
        assertTrue(testSource.contains("assertThat().browser().title()")
                        || testSource.contains("browser().assertThat().title()"),
                "Final validation must use the SHAFT browser assertion builder:\n" + testSource);
        assertFalse(combinedSource.contains("driver.findElement") || combinedSource.contains("Thread.sleep")
                        || combinedSource.contains("WebDriverWait") || combinedSource.contains("/html/"),
                "Generated code contains a native Selenium or brittle-locator anti-pattern:\n" + combinedSource);
        String data = Files.readString(projectRoot.resolve(
                "src/test/resources/testDataFiles/recorded-flow-test.json"), StandardCharsets.UTF_8);
        assertTrue(data.contains("SHAFT Live Capture Fixture - done"),
                "Final-title oracle must be externalized in test data:\n" + data);

        JsonObject arguments = new JsonObject();
        arguments.addProperty("language", "java");
        arguments.addProperty("code", combinedSource);
        String response = support.send(panel, "/mcp test_code_guardrails_check " + arguments,
                Duration.ofSeconds(60));
        assertNotError(response, "test_code_guardrails_check");
        String payload = LiveChatToolE2ESupport.unwrapToolPayload(response);
        assertTrue(payload.contains("\"passed\": true") || payload.contains("\"passed\":true"), payload);
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
