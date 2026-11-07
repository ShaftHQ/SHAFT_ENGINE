package com.shaft.intellij.ui;

import com.google.gson.JsonArray;
import com.google.gson.JsonNull;
import com.google.gson.JsonObject;
import com.intellij.openapi.project.Project;
import com.intellij.ui.components.JBList;
import com.shaft.intellij.mcp.McpInvocationError;
import com.shaft.intellij.mcp.ShaftMcpInvocation;
import com.shaft.intellij.mcp.ShaftMcpInvocationService;
import com.shaft.intellij.mcp.ShaftMcpToolResult;

import org.junit.jupiter.api.Test;

import javax.swing.JButton;
import javax.swing.JComboBox;
import java.awt.Component;
import java.io.IOException;
import java.lang.reflect.Proxy;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CompletableFuture;
import java.util.stream.Stream;

import static com.shaft.intellij.ui.GuidedWorkflowPanelTest.expandAdvanced;
import static com.shaft.intellij.ui.GuidedWorkflowPanelTest.findButton;
import static com.shaft.intellij.ui.GuidedWorkflowPanelTest.findByAccessibleName;
import static com.shaft.intellij.ui.GuidedWorkflowPanelTest.select;
import static com.shaft.intellij.ui.GuidedWorkflowPanelTest.setText;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * MCP-connected Guided Workflow branches. The project stub returns a mock invocation service so
 * Start, Stop, Clear, sample, mobile, API, checkpoints, and Create test class run their real
 * callbacks without a live MCP process or browser.
 */
class GuidedWorkflowPanelConnectedProjectTest {
    @Test
    void teamPolicyCopiesOutputDirectoryAndReportsMalformedJson() throws IOException {
        Path outputRoot = Files.createTempDirectory("shaft-guided-policy-output");
        Path badRoot = Files.createTempDirectory("shaft-guided-policy-bad");
        try {
            Files.createDirectories(outputRoot.resolve(".shaft"));
            Files.writeString(outputRoot.resolve(".shaft/recorder-policy.json"),
                    "{\"headless\": false, \"outputDirectory\": \"target/team-recordings\"}");
            GuidedWorkflowPanel applied = new GuidedWorkflowPanel(project(outputRoot, null), (tool, args) -> {
            });
            try {
                expandAdvanced(applied);
                assertEquals("target/team-recordings/intellij-capture.json", text(applied, "Session path"));
                assertStatus(applied, "Team recorder policy applied");
            } finally {
                applied.dispose();
            }

            Files.createDirectories(badRoot.resolve(".shaft"));
            Files.writeString(badRoot.resolve(".shaft/recorder-policy.json"), "{");
            GuidedWorkflowPanel broken = new GuidedWorkflowPanel(project(badRoot, null), (tool, args) -> {
            });
            try {
                assertStatus(broken, "Could not read .shaft/recorder-policy.json");
            } finally {
                broken.dispose();
            }
        } finally {
            deleteTree(outputRoot);
            deleteTree(badRoot);
        }
    }

    @Test
    void connectedWebRecordingReportsStartStopClearAndStorageFailures() throws Exception {
        try (ConnectedPanel connected = new ConnectedPanel()) {
            GuidedWorkflowPanel panel = connected.panel;
            expandAdvanced(panel);

            connected.enqueue(ShaftMcpToolResult.success("{}"));
            click(panel, "Start recording");
            assertStatus(panel, "Waiting for the recording");
            assertEquals("capture_start", connected.lastTool());
            assertTrue(panel.pollingActive);
            panel.pollingActive = false;

            connected.enqueue(toolFailure("browser failed"));
            click(panel, "Start recording");
            assertStatus(panel, "Recording failed to start: browser failed");

            connected.enqueue(new IllegalStateException("mcp down"));
            click(panel, "Start recording");
            assertStatus(panel, "mcp down");

            connected.enqueueNull();
            click(panel, "Start recording");
            assertStatus(panel, "no result");

            click(panel, "Pause recording");
            assertStatus(panel, "Recording paused");
            assertTrue(panel.recordingPausedForTests());
            click(panel, "Pause recording");
            assertStatus(panel, "Recording resumed");
            assertFalse(panel.recordingPausedForTests());

            connected.enqueue(ShaftMcpToolResult.success("{}"));
            click(panel, "Stop recording");
            assertStatus(panel, "Recording stopped");
            assertEquals("capture_stop", connected.lastTool());
            assertFalse(connected.arguments.get("capture_stop").get("discard").getAsBoolean());

            connected.enqueue(toolFailure("stop failed"));
            click(panel, "Stop recording");
            assertStatus(panel, "Recording failed to stop");

            click(panel, "Pause recording");
            connected.enqueue(ShaftMcpToolResult.success("{}"));
            click(panel, "Clear recording");
            assertStatus(panel, "Recording cleared");
            assertFalse(panel.recordingPausedForTests());
            assertTrue(connected.arguments.get("capture_stop").get("discard").getAsBoolean());

            connected.enqueue(toolFailure("clear failed"));
            click(panel, "Clear recording");
            assertStatus(panel, "Recording failed to clear");

            setText(panel, "Load storage state", "missing-auth.json");
            click(panel, "Start recording");
            assertStatus(panel, "fail closed");

            Files.writeString(connected.base.resolve("auth-state.json"), "{}");
            setText(panel, "Load storage state", "auth-state.json");
            connected.enqueue(ShaftMcpToolResult.success("{}"));
            click(panel, "Start recording");
            JsonObject codegen = connected.arguments.get("capture_start").getAsJsonObject("codegenOptions");
            assertEquals("auth-state.json", codegen.get("loadStoragePath").getAsString());
            assertStatus(panel, "Waiting for the recording");
        }
    }

    @Test
    void samplePlaywrightMobileAndApiRecordingUseTheConnectedService() throws Exception {
        try (ConnectedPanel sample = new ConnectedPanel()) {
            sample.enqueue(toolFailure("sample failed"));
            click(sample.panel, "Try SHAFT on a sample page");
            assertStatus(sample.panel, "Sample recording failed to start: sample failed");
            assertEquals("recordings/sample-tour.json", text(sample.panel, "Session path"));

            sample.enqueue(ShaftMcpToolResult.success("{}"));
            click(sample.panel, "Try SHAFT on a sample page");
            assertStatus(sample.panel, "Waiting for the recording");
            assertTrue(text(sample.panel, "Target URL").startsWith("file:"));
            assertTrue(sample.panel.pollingActive);
        }

        try (ConnectedPanel playwright = new ConnectedPanel()) {
            expandAdvanced(playwright.panel);
            select(backend(playwright.panel), "Playwright");
            setText(playwright.panel, "Load storage state", "missing-auth.json");
            playwright.enqueue(ShaftMcpToolResult.success("{}"));
            click(playwright.panel, "Start recording");
            assertEquals("capture_start", playwright.lastTool());
            assertEquals("recordings/intellij-capture.json",
                    playwright.arguments.get("capture_start").get("outputPath").getAsString());
            assertStatus(playwright.panel, "Waiting for the recording");
            playwright.panel.pollingActive = false;

            playwright.enqueue(toolFailure("playwright failed"));
            click(playwright.panel, "Start recording");
            assertStatus(playwright.panel, "Recording failed to start: playwright failed");
        }

        try (ConnectedPanel mobile = new ConnectedPanel()) {
            expandAdvanced(mobile.panel);
            select(backend(mobile.panel), "Mobile (web emulation)");
            mobile.enqueue(toolFailure("session failed"));
            click(mobile.panel, "Start recording");
            assertStatus(mobile.panel, "Mobile emulation session failed: session failed");

            mobile.enqueue(ShaftMcpToolResult.success("{}"), toolFailure("recorder failed"));
            click(mobile.panel, "Start recording");
            assertStatus(mobile.panel, "Mobile recorder failed to start: recorder failed");

            mobile.enqueue(ShaftMcpToolResult.success("{}"), ShaftMcpToolResult.success("{}"));
            click(mobile.panel, "Start recording");
            assertStatus(mobile.panel, "Waiting for the recording");
            assertEquals(List.of("driver_initialize", "driver_initialize", "capture_start",
                    "driver_initialize", "capture_start"), mobile.tools);
            assertTrue(mobile.panel.pollingActive);
        }

        try (ConnectedPanel api = new ConnectedPanel()) {
            expandAdvanced(api.panel);
            select(backend(api.panel), "API");
            api.enqueue(toolFailure("api failed"));
            click(api.panel, "Start recording");
            assertStatus(api.panel, "API recording failed to start: api failed");

            JsonObject webStatus = new JsonObject();
            webStatus.addProperty("outputPath", "recordings/from-server.json");
            JsonObject started = new JsonObject();
            started.add("webStatus", webStatus);
            api.enqueue(ShaftMcpToolResult.success(started.toString()));
            click(api.panel, "Start recording");
            assertEquals("capture_api_start", api.lastTool());
            assertStatus(api.panel, "Waiting for the recording");
            api.panel.pollingActive = false;

            api.enqueue(fullClass("SUCCESS", "package demo.api;\n\npublic class ApiFlowTest {}\n"));
            click(api.panel, "Create test class");
            assertEquals("capture_api_generate", api.lastTool());
            assertEquals("recordings/from-server.json",
                    api.arguments.get("capture_api_generate").get("sessionPath").getAsString());
            assertTrue(api.arguments.get("capture_api_generate").get("replay").getAsBoolean());
            Path created = api.base.resolve("src/test/java/demo/api/ApiFlowTest.java");
            assertEquals("package demo.api;\n\npublic class ApiFlowTest {}\n", Files.readString(created));
            assertStatus(api.panel, "Created ApiFlowTest.java");

            api.enqueue(ShaftMcpToolResult.success("{}"));
            click(api.panel, "Stop recording");
            assertEquals("capture_api_stop", api.lastTool());
            assertFalse(api.arguments.get("capture_api_stop").get("discard").getAsBoolean());
            assertStatus(api.panel, "Recording stopped");

            api.enqueue(ShaftMcpToolResult.success("{}"));
            click(api.panel, "Clear recording");
            assertTrue(api.arguments.get("capture_api_stop").get("discard").getAsBoolean());
            assertStatus(api.panel, "Recording cleared");
        }
    }

    @Test
    void createTestClassWritesOnlyReplayProvenSources() throws Exception {
        try (ConnectedPanel connected = new ConnectedPanel()) {
            GuidedWorkflowPanel panel = connected.panel;
            JButton copyDraft = findButton(panel, "Copy unconfirmed draft");
            assertNotNull(copyDraft);
            copyDraft.setEnabled(true);
            copyDraft.doClick();
            assertStatus(panel, "No UNCONFIRMED draft to copy");

            connected.enqueue(fullClass("SUCCESS", ""));
            click(panel, "Create test class");
            assertStatus(panel, "no full-class code block");

            connected.enqueue(fullClass("SUCCESS", "class "));
            click(panel, "Create test class");
            assertStatus(panel, "Could not determine the generated class name");

            connected.enqueue(fullClass("UNCONFIRMED", "public class UnconfirmedDraftTest {}\n"));
            click(panel, "Create test class");
            assertStatus(panel, "UNCONFIRMED");
            click(panel, "Copy unconfirmed draft");
            String copyStatus = panel.recorderStatusLabel().getText();
            assertTrue(copyStatus.contains("Could not copy") || copyStatus.contains("Copied UNCONFIRMED"),
                    copyStatus);

            JsonArray blocks = new JsonArray();
            blocks.add(JsonNull.INSTANCE);
            blocks.add(codeBlock(null, "   ", null));
            blocks.add(codeBlock(null, "public class IgnoredFallback {}", null));
            blocks.add(codeBlock("capture-full-class", null, "public class ContentKeyTest {}"));
            connected.enqueue(fullClass("SUCCESS", blocks));
            click(panel, "Create test class");
            Path created = connected.base.resolve("src/test/java/tests/generated/ContentKeyTest.java");
            String written = Files.readString(created);
            assertTrue(written.startsWith("package tests.generated;\n\npublic class ContentKeyTest {}"), written);
            assertStatus(panel, "Created ContentKeyTest.java");

            connected.enqueue(fullClass("SUCCESS", blocks));
            click(panel, "Create test class");
            assertStatus(panel, "Already exists");
            assertEquals(written, Files.readString(created));
        }

        try (ConnectedPanel blocked = new ConnectedPanel()) {
            Path javaRoot = blocked.base.resolve("src/test/java");
            Files.createDirectories(javaRoot.getParent());
            Files.writeString(javaRoot, "not-a-directory");
            blocked.enqueue(fullClass("SUCCESS", "public class BlockedWriteTest {}"));
            click(blocked.panel, "Create test class");
            assertStatus(blocked.panel, "Could not write test class");
        }
    }

    @Test
    void checkpointsAndStepActionsReportConnectedResults() throws Exception {
        try (ConnectedPanel connected = new ConnectedPanel()) {
            GuidedWorkflowPanel panel = connected.panel;
            panel.applyReadyPackPrefill("https://example.test/shop", "buy a book");
            assertStatus(panel, "Ready pack loaded. Review");
            panel.applyReadyPackPrefill("https://example.test/shop", "buy a book", " \n | :  | ac:visible cart");
            assertStatus(panel, "1 suggested");

            JButton accept = findButton(panel, "Accept suggested checkpoint");
            assertNotNull(accept);
            accept.setEnabled(true);
            accept.doClick();
            assertStatus(panel, "Select a suggested Then oracle first");

            JBList<?> oracles = findByAccessibleName(panel,
                    "Suggested checkpoints from Ready-pack Then oracles", JBList.class);
            assertNotNull(oracles);
            oracles.setSelectedIndex(0);
            assertTrue(panel.acceptOracleButton().isEnabled());

            connected.enqueue(toolFailure("checkpoint failed"));
            click(panel, "Accept suggested checkpoint");
            assertStatus(panel, "Checkpoint failed: checkpoint failed");

            JsonObject step = new JsonObject();
            step.addProperty("stepId", "s-1");
            step.addProperty("sequence", 1);
            step.addProperty("action", "click");
            step.addProperty("locatorStrategy", "id");
            step.addProperty("locatorValue", "go");
            step.addProperty("risky", false);
            JsonArray steps = new JsonArray();
            steps.add(step);
            JsonObject status = new JsonObject();
            status.add("steps", steps);
            connected.enqueue(ShaftMcpToolResult.success(status.toString()));
            click(panel, "Accept suggested checkpoint");
            assertStatus(panel, "Recorded SHAFT ASSERTION checkpoint: visible cart");
            JBList<?> recorded = findByAccessibleName(panel, "Recorded steps", JBList.class);
            assertNotNull(recorded);
            assertEquals(1, recorded.getModel().getSize());

            panel.recordAssertionCheckpoint("  ");
            assertStatus(panel, "Checkpoint description is required");

            setText(panel, "Intent", " ");
            connected.enqueue(ShaftMcpToolResult.success("{"));
            click(panel, "Add checkpoint");
            assertStatus(panel, "Recorded SHAFT ASSERTION checkpoint: Assert the current page state");

            connected.enqueue(ShaftMcpToolResult.success(""));
            panel.recordAssertionCheckpoint("cart badge");
            assertStatus(panel, "Recorded SHAFT ASSERTION checkpoint: cart badge");

            expandAdvanced(panel);
            select(backend(panel), "Playwright");
            panel.renderStepsFromStatus(status);
            recorded.setSelectedIndex(0);
            connected.enqueue(toolFailure("delete failed"));
            click(panel, "Delete");
            assertEquals("capture_step_delete", connected.lastTool());
            assertStatus(panel, "Step action failed: delete failed");
            assertFalse(findButton(panel, "Delete").isEnabled());

            recorded.clearSelection();
            recorded.setSelectedIndex(0);
            connected.enqueue(ShaftMcpToolResult.success("{}"));
            click(panel, "Move Up");
            assertEquals("capture_step_reorder", connected.lastTool());
            assertEquals("up", connected.arguments.get("capture_step_reorder").get("direction").getAsString());
            assertEquals("s-1", connected.arguments.get("capture_step_reorder").get("stepId").getAsString());
        }
    }

    @Test
    void statusPollCoversFailureFinishIncompleteAndIdle() {
        GuidedWorkflowPanel panel = new GuidedWorkflowPanel(null, (tool, args) -> {
        });
        try {
            panel.applyStatusPoll(unionState("ACTIVE"), null);
            assertEquals("Recorder idle.", panel.recorderStatusLabel().getText());

            panel.pollingActive = true;
            panel.applyStatusPoll(null, new IllegalStateException("poll broke"));
            assertStatus(panel, "Recorder status unavailable: poll broke");
            assertTrue(panel.pollingActive);

            panel.applyStatusPoll(unionState("ACTIVE"), null);
            assertStatus(panel, "Recording");
            panel.applyStatusPoll(unionState("INCOMPLETE"), null);
            assertStatus(panel, "unexpected");
            assertStatus(panel, "INCOMPLETE");
            assertFalse(panel.pollingActive);
        } finally {
            panel.dispose();
        }

        GuidedWorkflowPanel failed = new GuidedWorkflowPanel(null, (tool, args) -> {
        });
        try {
            failed.pollingActive = true;
            failed.applyStatusPoll(unionState("ACTIVE"), null);
            failed.applyStatusPoll(unionState("FAILED"), null);
            assertStatus(failed, "FAILED");
            assertFalse(failed.pollingActive);
        } finally {
            failed.dispose();
        }

        GuidedWorkflowPanel finished = new GuidedWorkflowPanel(null, (tool, args) -> {
        });
        try {
            finished.pollingActive = true;
            finished.applyStatusPoll(unionState("ACTIVE"), null);
            finished.applyStatusPoll(unionState("COMPLETED"), null);
            assertStatus(finished, "Recording finished");
            assertFalse(finished.pollingActive);
        } finally {
            finished.dispose();
        }

        GuidedWorkflowPanel idle = new GuidedWorkflowPanel(null, (tool, args) -> {
        });
        try {
            idle.pollingActive = true;
            ShaftMcpToolResult idleResult = unionState("IDLE");
            for (int poll = 0; poll < 149; poll++) {
                idle.applyStatusPoll(idleResult, null);
            }
            assertTrue(idle.pollingActive);
            idle.applyStatusPoll(idleResult, null);
            assertFalse(idle.pollingActive);
            assertStatus(idle, "No active recording detected");
        } finally {
            idle.dispose();
        }
    }

    @Test
    void statusPollAlarmInvokesTheStatusTool() throws Exception {
        try (ConnectedPanel connected = new ConnectedPanel()) {
            connected.enqueue(ShaftMcpToolResult.success("{}"));
            click(connected.panel, "Start recording");
            long deadline = System.nanoTime() + 5_000_000_000L;
            while (System.nanoTime() < deadline && !connected.saw("capture_status")) {
                Thread.sleep(50);
            }
            assertTrue(connected.saw("capture_status"), connected.tools.toString());
        }
    }

    private static ShaftMcpToolResult toolFailure(String output) {
        return ShaftMcpToolResult.failure(output, McpInvocationError.TOOL_ERROR, "Retry");
    }

    private static ShaftMcpToolResult unionState(String state) {
        JsonObject webStatus = new JsonObject();
        webStatus.addProperty("state", state);
        webStatus.addProperty("eventCount", 2);
        JsonObject union = new JsonObject();
        union.add("webStatus", webStatus);
        return ShaftMcpToolResult.success(union.toString());
    }

    private static ShaftMcpToolResult fullClass(String status, String code) {
        JsonArray blocks = new JsonArray();
        if (!code.isEmpty()) {
            blocks.add(codeBlock("capture-full-class", code, null));
        }
        return fullClass(status, blocks);
    }

    private static ShaftMcpToolResult fullClass(String status, JsonArray blocks) {
        JsonObject report = new JsonObject();
        report.addProperty("status", status);
        JsonObject raw = new JsonObject();
        raw.add("report", report);
        raw.add("codeBlocks", blocks);
        return ShaftMcpToolResult.success(raw.toString());
    }

    private static JsonObject codeBlock(String id, String code, String content) {
        JsonObject block = new JsonObject();
        block.addProperty("kind", "FULL_CLASS");
        if (id != null) {
            block.addProperty("id", id);
        }
        if (code != null) {
            block.addProperty("code", code);
        }
        if (content != null) {
            block.addProperty("content", content);
        }
        return block;
    }

    private static void assertStatus(GuidedWorkflowPanel panel, String fragment) {
        String text = panel.recorderStatusLabel().getText();
        assertTrue(text.contains(fragment), text);
    }

    private static JComboBox<?> backend(GuidedWorkflowPanel panel) {
        JComboBox<?> combo = findByAccessibleName(panel, "Guided workflow backend", JComboBox.class);
        assertNotNull(combo);
        return combo;
    }

    private static final class ConnectedPanel implements AutoCloseable {
        private static final Object NULL_RESULT = new Object();

        final Path base;
        final GuidedWorkflowPanel panel;
        final List<String> tools = new ArrayList<>();
        final Map<String, JsonObject> arguments = new HashMap<>();
        private final ArrayDeque<Object> script = new ArrayDeque<>();

        ConnectedPanel() throws IOException {
            base = Files.createTempDirectory("shaft-guided-connected");
            ShaftMcpInvocationService service = mock(ShaftMcpInvocationService.class);
            when(service.startTool(anyString(), any(JsonObject.class))).thenAnswer(invocation -> {
                String toolName = invocation.getArgument(0);
                JsonObject args = ((JsonObject) invocation.getArgument(1)).deepCopy();
                Object outcome;
                synchronized (this) {
                    tools.add(toolName);
                    arguments.put(toolName, args);
                    outcome = script.isEmpty() ? ShaftMcpToolResult.success("{}") : script.removeFirst();
                }
                return new ShaftMcpInvocation(futureFor(outcome), () -> {
                });
            });
            panel = new GuidedWorkflowPanel(project(base, service), (toolName, args) -> {
                throw new AssertionError("prefill fallback used for " + toolName);
            });
        }

        void enqueue(Object... outcomes) {
            synchronized (this) {
                for (Object outcome : outcomes) {
                    script.addLast(outcome);
                }
            }
        }

        void enqueueNull() {
            enqueue(NULL_RESULT);
        }

        synchronized String lastTool() {
            assertFalse(tools.isEmpty(), "no MCP tool was invoked");
            return tools.get(tools.size() - 1);
        }

        synchronized boolean saw(String toolName) {
            return tools.contains(toolName);
        }

        private static CompletableFuture<ShaftMcpToolResult> futureFor(Object outcome) {
            CompletableFuture<ShaftMcpToolResult> future = new CompletableFuture<>();
            if (outcome instanceof Throwable error) {
                future.completeExceptionally(error);
            } else if (outcome == NULL_RESULT) {
                future.complete(null);
            } else {
                future.complete((ShaftMcpToolResult) outcome);
            }
            return future;
        }

        @Override
        public void close() throws IOException {
            panel.dispose();
            deleteTree(base);
        }
    }

    private static Project project(Path base, ShaftMcpInvocationService service) {
        return (Project) Proxy.newProxyInstance(Project.class.getClassLoader(), new Class<?>[]{Project.class},
                (proxy, method, arguments) -> {
                    switch (method.getName()) {
                        case "equals":
                            return proxy == (arguments == null || arguments.length == 0 ? null : arguments[0]);
                        case "hashCode":
                            return System.identityHashCode(proxy);
                        case "toString":
                            return "guided-workflow-connected-project";
                        case "getBasePath":
                            return base.toString();
                        case "getName":
                            return "guided-workflow-connected-project";
                        case "isDisposed":
                        case "isDefault":
                            return false;
                        case "isOpen":
                            return true;
                        case "getService":
                            if (arguments != null && arguments.length > 0
                                    && arguments[0] == ShaftMcpInvocationService.class) {
                                return service;
                            }
                            return null;
                        default:
                            return defaultValue(method.getReturnType());
                    }
                });
    }

    private static Object defaultValue(Class<?> returnType) {
        if (!returnType.isPrimitive()) {
            return null;
        }
        if (returnType == boolean.class) {
            return false;
        }
        return 0;
    }

    private static void deleteTree(Path root) throws IOException {
        if (!Files.exists(root)) {
            return;
        }
        try (Stream<Path> walk = Files.walk(root)) {
            for (Path path : walk.sorted(Comparator.reverseOrder()).toList()) {
                Files.deleteIfExists(path);
            }
        }
    }

    private static void click(Component parent, String accessibleName) {
        JButton button = GuidedWorkflowPanelTest.findButton(parent, accessibleName);
        assertNotNull(button, accessibleName);
        button.doClick();
    }

    private static String text(Component parent, String accessibleName) {
        javax.swing.text.JTextComponent field = GuidedWorkflowPanelTest.findByAccessibleName(
                parent, accessibleName, javax.swing.text.JTextComponent.class);
        assertNotNull(field, accessibleName);
        return field.getText();
    }
}
