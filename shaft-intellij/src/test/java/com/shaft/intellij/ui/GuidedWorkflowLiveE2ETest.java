package com.shaft.intellij.ui;

import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import com.shaft.intellij.mcp.LiveMcpTestHarness;
import com.shaft.intellij.mcp.ShaftCommandLine;
import com.shaft.intellij.settings.ShaftSettingsState;
import org.junit.jupiter.api.Assumptions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.Timeout;

import javax.accessibility.AccessibleContext;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.text.JTextComponent;
import java.awt.Component;
import java.awt.Container;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Duration;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;

/**
 * Live end-to-end verification of the three guided plugin workflows against a real SHAFT MCP
 * server process, entered exactly the way the IntelliJ plugin enters them: every session tool
 * request is produced by clicking {@link GuidedWorkflowPanel} controls and the resulting
 * invocation is sent through the plugin's own stdio client and project scoping.
 *
 * <p>Run with:
 * {@code gradle test --tests com.shaft.intellij.ui.GuidedWorkflowLiveE2ETest
 * -Dshaft.intellij.liveWorkflows=true
 * -Dshaft.intellij.liveMcpCommand="<java> @<shaft-mcp.args>"
 * -Dshaft.intellij.workspaceRoot=<scratch workspace>}</p>
 */
class GuidedWorkflowLiveE2ETest {
    private static final Duration SESSION_START_TIMEOUT = Duration.ofSeconds(240);
    private static final Duration TOOL_TIMEOUT = Duration.ofSeconds(120);
    private static final String SECRET_CANARY = "sup3r-secret-canary";

    @Test
    @Timeout(600)
    void webRecorderCapturesSelfDrivingLoginAndGeneratesShaftCode() throws Exception {
        LiveContext context = LiveContext.assumeConfigured();
        context.deleteWorkspacePaths("recordings/web-live.json", "src", "target/shaft-capture");
        Path fixture = context.writeFixture("fixtures/login.html", webFixture());
        PanelDriver panel = new PanelDriver();
        panel.setField("Target URL", fixture.toUri().toString());
        panel.setField("Session path", "recordings/web-live.json");
        panel.setHeadless(true);

        try (LiveMcp mcp = context.connect()) {
            JsonObject started = mcp.invoke(panel.click("Start recording"), SESSION_START_TIMEOUT);
            assertEquals("ACTIVE", webStatus(started).get("state").getAsString(), started.toString());

            JsonObject status = awaitEvents(mcp, 4, Duration.ofSeconds(90));
            assertTrue(status.get("eventCount").getAsInt() >= 4, status.toString());

            JsonObject stopped = mcp.invoke(panel.click("Stop recording"), TOOL_TIMEOUT);
            assertEquals("COMPLETED", webStatus(stopped).get("state").getAsString(), stopped.toString());
            assertTrue(context.workspaceFile("recordings/web-live.json"), "capture JSON missing");

            JsonObject generated = mcp.invoke(panel.click("Review code"), TOOL_TIMEOUT);
            String code = generated.toString();
            assertTrue(generated.get("successful").getAsBoolean(), code);
            assertTrue(code.contains("GeneratedShaftTest"), code);
            assertTrue(code.contains("driver.element()"), code);
            assertTrue(code.contains("typeSecure"),
                    "Recorded password entry should generate a secure typing action: " + code);

            Path source = context.workspacePath(
                    "src/test/java/tests/generated/GeneratedShaftTest.java");
            Path data = context.workspacePath(
                    "src/test/resources/testDataFiles/generated-shaft-test.json");
            assertTrue(Files.exists(source), "Generated source missing: " + source);
            assertTrue(Files.exists(data), "Generated test data missing: " + data);
            String sourceText = Files.readString(source, StandardCharsets.UTF_8);
            String dataText = Files.readString(data, StandardCharsets.UTF_8);
            assertTrue(dataText.contains("shaft.user"),
                    "Externalized test data should hold the recorded username: " + dataText);
            assertFalse(sourceText.contains(SECRET_CANARY) || dataText.contains(SECRET_CANARY)
                            || code.contains(SECRET_CANARY),
                    "Generated artifacts must not leak the recorded password value");
        }
    }

    @Test
    @Timeout(600)
    void mobileRecorderRecordsEmulatedActionsAndGeneratesShaftCode() throws Exception {
        LiveContext context = LiveContext.assumeConfigured();
        context.deleteWorkspacePaths("recordings/mobile-live.json");
        Path fixture = context.writeFixture("fixtures/mobile.html", mobileFixture());
        PanelDriver panel = new PanelDriver();
        panel.setField("Target URL", fixture.toUri().toString());
        panel.setField("Session path", "recordings/mobile-live.json");
        panel.setHeadless(true);
        panel.selectBackend(GuidedWorkflowPanel.BACKEND_MOBILE);

        try (LiveMcp mcp = context.connect()) {
            // driver_initialize's mobile dispatch (design doc amendment A9) returns no structured
            // status -- the former mobile_initialize_web_emulation session summary is absorbed but not
            // surfaced back through the union response family, so success here is proven by invoke()
            // itself (it fails the test on isError) and by the recording/actions that follow.
            mcp.invoke(panel.useTemplate("Start mobile emulation session for recording"), SESSION_START_TIMEOUT);

            JsonObject recording = mcp.invoke(panel.click("Start recording"), TOOL_TIMEOUT);
            assertTrue(mobileStatus(recording).get("active").getAsBoolean(), recording.toString());

            mcp.invoke(mobileAction("element_type", "ID", "search",
                    Map.of("text", "SHAFT Engine")), TOOL_TIMEOUT);
            mcp.invoke(mobileAction("element_click", "ID", "go", Map.of()), TOOL_TIMEOUT);

            JsonObject stopped = mcp.invoke(panel.click("Stop recording"), TOOL_TIMEOUT);
            assertFalse(mobileStatus(stopped).get("active").getAsBoolean(), stopped.toString());
            assertTrue(mobileStatus(stopped).get("actionCount").getAsInt() >= 2, stopped.toString());
            assertTrue(context.workspaceFile("recordings/mobile-live.json"), "mobile recording missing");

            JsonObject generated = mcp.invoke(panel.click("Review code"), TOOL_TIMEOUT);
            String code = generated.toString();
            assertTrue(code.contains("driver.element()"), code);
            assertTrue(code.toLowerCase(Locale.ROOT).contains("tap"), code);

            mcp.invoke(new Invocation("driver_quit", new JsonObject()), TOOL_TIMEOUT);
        }
    }

    @Test
    @Timeout(300)
    void doctorAnalyzesSimulatedFailedAllureResultsDeterministically() throws Exception {
        LiveContext context = LiveContext.assumeConfigured();
        context.deleteWorkspacePaths("allure-results", "target/shaft-doctor");
        context.writeFixture("allure-results/failed-login-result.json", failedAllureResult());
        PanelDriver panel = new PanelDriver();

        try (LiveMcp mcp = context.connect()) {
            JsonObject report = mcp.invoke(
                    panel.useTemplate("Analyze failed Allure results"), TOOL_TIMEOUT);
            assertEquals("LOCATOR", report.get("primaryCause").getAsString(), report.toString());
            assertNotNull(report.get("jsonReportPath"), report.toString());
            assertNotNull(report.get("markdownReportPath"), report.toString());
            assertTrue(report.get("codeBlocks").isJsonArray(), report.toString());
            String jsonReportPath = report.get("jsonReportPath").getAsString();
            assertTrue(Files.exists(Path.of(jsonReportPath)), "Doctor JSON report missing: " + jsonReportPath);
        }
    }

    private static JsonObject awaitEvents(LiveMcp mcp, int minimumEvents, Duration limit) throws Exception {
        long deadline = System.nanoTime() + limit.toNanos();
        JsonObject status = new JsonObject();
        while (System.nanoTime() < deadline) {
            status = webStatus(mcp.invoke(new Invocation("capture_status", new JsonObject()), TOOL_TIMEOUT));
            if (status.has("eventCount") && status.get("eventCount").getAsInt() >= minimumEvents) {
                return status;
            }
            Thread.sleep(2000);
        }
        return status;
    }

    /**
     * Unwraps {@code capture_start}/{@code capture_status}/{@code capture_stop}'s
     * {@code McpCaptureUnionStatus} response down to its WEB-engine {@code webStatus} section
     * (design doc amendment A3, landed by #3881): the union's {@code state}/{@code eventCount}
     * fields moved off the top level into this nested object.
     */
    private static JsonObject webStatus(JsonObject union) {
        JsonObject webStatus = union.getAsJsonObject("webStatus");
        assertNotNull(webStatus, "Expected a populated webStatus section: " + union);
        return webStatus;
    }

    /**
     * Unwraps the same union response's {@code mobileStatus} section, populated instead of
     * {@code webStatus} when the active engine is MOBILE_NATIVE/MOBILE_WEB.
     */
    private static JsonObject mobileStatus(JsonObject union) {
        JsonObject mobileStatus = union.getAsJsonObject("mobileStatus");
        assertNotNull(mobileStatus, "Expected a populated mobileStatus section: " + union);
        return mobileStatus;
    }

    /**
     * Builds a locator-scoped invocation for the unified {@code element_type}/{@code element_click}
     * tools. With the MOBILE_WEB session started above active, these dispatch through
     * {@code MobileService.dispatchType}/{@code dispatchClick} and record the step exactly like the
     * {@code mobile_type}/{@code mobile_tap} tools removed by design doc amendment A1 (#3881) did.
     */
    private static Invocation mobileAction(
            String toolName, String strategy, String value, Map<String, String> extra) {
        JsonObject arguments = new JsonObject();
        arguments.addProperty("locatorStrategy", strategy);
        arguments.addProperty("locatorValue", value);
        extra.forEach(arguments::addProperty);
        return new Invocation(toolName, arguments);
    }

    private static String webFixture() {
        return """
                <!doctype html>
                <html lang="en"><head><meta charset="utf-8"><title>SHAFT Live Login Fixture</title></head>
                <body>
                <h1>Login</h1>
                <form id="form">
                  <label for="username">Username</label>
                  <input id="username" name="username" type="text" placeholder="Username">
                  <label for="password">Password</label>
                  <input id="password" name="password" type="password" placeholder="Password">
                  <button id="login" type="button">Log in</button>
                </form>
                <div id="status">signed-out</div>
                <script>
                  // Mirrors real user typing: input events while typing, then the committed
                  // change event a browser fires when focus leaves the dirty field.
                  function drive(el, value) {
                    el.focus();
                    el.value = value;
                    el.dispatchEvent(new Event("input", {bubbles: true}));
                    el.dispatchEvent(new Event("change", {bubbles: true}));
                  }
                  document.getElementById("login").addEventListener("click", function () {
                    document.getElementById("status").textContent = "signed-in";
                  });
                  setTimeout(function () {
                    drive(document.getElementById("username"), "shaft.user");
                    drive(document.getElementById("password"), "%s");
                    setTimeout(function () { document.getElementById("login").click(); }, 500);
                  }, 2500);
                </script>
                </body></html>
                """.formatted(SECRET_CANARY);
    }

    private static String mobileFixture() {
        return """
                <!doctype html>
                <html lang="en"><head><meta charset="utf-8"><title>SHAFT Live Mobile Fixture</title>
                <meta name="viewport" content="width=device-width, initial-scale=1"></head>
                <body>
                <h1>Search</h1>
                <input id="search" type="text" placeholder="Search">
                <button id="go" type="button">Go</button>
                <div id="results" hidden>results</div>
                <script>
                  document.getElementById("go").addEventListener("click", function () {
                    document.getElementById("results").hidden = false;
                  });
                </script>
                </body></html>
                """;
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
     * Headless {@link GuidedWorkflowPanel} driver: clicking its real controls is the plugin-side
     * entry point for every tool request this test sends.
     */
    private static final class PanelDriver implements GuidedWorkflowPanel.ToolPrefill {
        private final GuidedWorkflowPanel panel = new GuidedWorkflowPanel(null, this);
        private final List<Invocation> invocations = new ArrayList<>();

        @Override
        public void prefill(String toolName, JsonObject arguments) {
            invocations.add(new Invocation(toolName, arguments));
        }

        void setField(String accessibleName, String value) {
            JTextComponent field = find(panel, accessibleName, JTextComponent.class);
            assertNotNull(field, "Missing field: " + accessibleName);
            field.setText(value);
        }

        void setHeadless(boolean headless) {
            JCheckBox checkbox = find(panel, "Headless browser", JCheckBox.class);
            assertNotNull(checkbox, "Missing headless checkbox");
            checkbox.setSelected(headless);
        }

        void selectBackend(String backendLabel) {
            JComboBox<?> backend = find(panel, "Guided workflow backend", JComboBox.class);
            assertNotNull(backend, "Missing backend selector");
            backend.setSelectedItem(backendLabel);
            assertEquals(backendLabel, backend.getSelectedItem());
        }

        Invocation click(String buttonAccessibleName) {
            JButton button = findButton(panel, buttonAccessibleName);
            assertNotNull(button, "Missing button: " + buttonAccessibleName);
            button.doClick();
            assertFalse(invocations.isEmpty(), "Button produced no invocation: " + buttonAccessibleName);
            return invocations.remove(invocations.size() - 1);
        }

        Invocation useTemplate(String templateLabel) {
            JComboBox<?> templates = find(panel, "Workflow template", JComboBox.class);
            assertNotNull(templates, "Missing template selector");
            for (int index = 0; index < templates.getItemCount(); index++) {
                if (templateLabel.equals(String.valueOf(templates.getItemAt(index)))) {
                    templates.setSelectedIndex(index);
                    return click("Use template");
                }
            }
            throw new AssertionError("Missing template: " + templateLabel);
        }

        private static JButton findButton(Component component, String accessibleName) {
            return find(component, accessibleName, JButton.class);
        }

        private static <T extends Component> T find(Component component, String accessibleName, Class<T> type) {
            if (type.isInstance(component) && component instanceof JComponent jComponent) {
                AccessibleContext context = jComponent.getAccessibleContext();
                if (context != null && accessibleName.equals(context.getAccessibleName())) {
                    return type.cast(component);
                }
            }
            if (component instanceof Container container) {
                for (Component child : container.getComponents()) {
                    T found = find(child, accessibleName, type);
                    if (found != null) {
                        return found;
                    }
                }
            }
            return null;
        }
    }

    private record Invocation(String toolName, JsonObject arguments) {
    }

    /**
     * Live run configuration resolved from the same system properties as the other live plugin
     * tests, skipping the test (never failing) when the run is not explicitly enabled.
     */
    private record LiveContext(List<String> command, Path workspace, Map<String, String> environment) {
        static LiveContext assumeConfigured() throws Exception {
            Assumptions.assumeTrue(Boolean.getBoolean("shaft.intellij.liveWorkflows"),
                    "Set -Dshaft.intellij.liveWorkflows=true to run live guided workflow tests.");
            String commandLine = System.getProperty("shaft.intellij.liveMcpCommand", "").trim();
            Assumptions.assumeTrue(!commandLine.isBlank(),
                    "Set -Dshaft.intellij.liveMcpCommand to a SHAFT MCP stdio command.");
            Path workspace = Path.of(System.getProperty("shaft.intellij.workspaceRoot", "build/live-workflows"))
                    .toAbsolutePath()
                    .normalize();
            Files.createDirectories(workspace);
            ShaftSettingsState.Settings settings = new ShaftSettingsState.Settings();
            settings.passProviderApiKeysToMcp = false;
            Map<String, String> environment = LiveMcpTestHarness.scopedEnvironment(settings, workspace);
            List<String> command = LiveMcpTestHarness.scopedCommand(ShaftCommandLine.parse(commandLine), workspace);
            return new LiveContext(command, workspace, environment);
        }

        LiveMcp connect() throws Exception {
            return new LiveMcp(command, workspace, environment);
        }

        Path writeFixture(String relativePath, String content) throws IOException {
            Path path = workspace.resolve(relativePath);
            Files.createDirectories(path.getParent());
            Files.writeString(path, content, StandardCharsets.UTF_8);
            return path;
        }

        /** Removes leftovers from earlier runs so each live workflow starts clean. */
        void deleteWorkspacePaths(String... relativePaths) throws IOException {
            for (String relativePath : relativePaths) {
                Path path = workspace.resolve(relativePath);
                if (!Files.exists(path)) {
                    continue;
                }
                try (var stream = Files.walk(path)) {
                    for (Path entry : stream.sorted(java.util.Comparator.reverseOrder()).toList()) {
                        Files.deleteIfExists(entry);
                    }
                }
            }
        }

        boolean workspaceFile(String relativePath) {
            return Files.exists(workspace.resolve(relativePath));
        }

        Path workspacePath(String relativePath) {
            return workspace.resolve(relativePath);
        }

    }

    /**
     * One long-lived SHAFT MCP server process reached through the plugin's own stdio client, so
     * session state (capture recordings, live drivers) survives across tool calls exactly as it
     * does inside the IDE.
     */
    private static final class LiveMcp implements AutoCloseable {
        private final LiveMcpTestHarness harness;

        LiveMcp(List<String> command, Path workspace, Map<String, String> environment) throws Exception {
            harness = new LiveMcpTestHarness(command, workspace, environment);
        }

        /**
         * Calls a tool and unwraps the MCP content envelope into the tool's JSON payload, failing
         * the test when the server reports {@code isError}.
         */
        JsonObject invoke(Invocation invocation, Duration timeout) throws Exception {
            JsonElement result = harness.callTool(invocation.toolName(), invocation.arguments(), timeout);
            if (result != null && result.isJsonObject()) {
                JsonObject object = result.getAsJsonObject();
                if (object.has("isError") && object.get("isError").getAsBoolean()) {
                    fail(invocation.toolName() + " failed: " + result);
                }
            }
            String text = mcpText(result);
            try {
                JsonElement payload = JsonParser.parseString(text);
                return payload.isJsonObject() ? payload.getAsJsonObject() : wrap(payload);
            } catch (com.google.gson.JsonSyntaxException exception) {
                return wrap(new com.google.gson.JsonPrimitive(text));
            }
        }

        private static JsonObject wrap(JsonElement value) {
            JsonObject object = new JsonObject();
            object.add("value", value);
            return object;
        }

        private static String mcpText(JsonElement result) {
            if (result != null && result.isJsonObject()) {
                JsonObject object = result.getAsJsonObject();
                if (object.has("content") && object.get("content").isJsonArray()
                        && !object.getAsJsonArray("content").isEmpty()) {
                    JsonObject item = object.getAsJsonArray("content").get(0).getAsJsonObject();
                    if (item.has("text")) {
                        return item.get("text").getAsString();
                    }
                }
            }
            return result == null ? "{}" : result.toString();
        }

        @Override
        public void close() {
            harness.close();
        }
    }
}
