package com.shaft.intellij.ui;

import com.google.gson.JsonObject;
import com.google.gson.JsonArray;
import com.google.gson.JsonParser;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.ui.components.JBTextArea;
import com.intellij.openapi.project.Project;
import com.shaft.intellij.mcp.ShaftMcpInstallResult;
import com.shaft.intellij.mcp.ShaftMcpToolResult;
import com.shaft.intellij.settings.ShaftSettingsState;
import org.junit.jupiter.api.Test;

import javax.accessibility.AccessibleContext;
import javax.swing.AbstractButton;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.Icon;
import javax.swing.JLabel;
import javax.swing.JProgressBar;
import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.Graphics2D;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.image.BufferedImage;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.lang.reflect.Proxy;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.CancellationException;
import java.util.concurrent.atomic.AtomicBoolean;
import javax.swing.text.JTextComponent;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class ShaftPanelSetupTest {
    @Test
    void assistantExplainsMissingMcpConfiguration() {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, blankMcpSettings());

        assertAll(
                () -> assertTrue(containsText(panel, "Configure SHAFT MCP")),
                () -> assertNotNull(findByAccessibleName(panel, "Open SHAFT settings", JButton.class)));
    }

    @Test
    void assistantMcpSetupGateIgnoresBroadLocalCliPrompts() {
        AssistantCommand.Invocation broadLocal = AssistantCommand.fromPrompt(
                "Plan a browser recording workflow",
                AssistantCommand.Selection.local("CODEX", "CLI"),
                "ASK",
                ".",
                "",
                false);
        AssistantCommand.Invocation broadCloud = AssistantCommand.fromPrompt(
                "Plan a browser recording workflow",
                AssistantCommand.Selection.cloud("github", "openai/gpt-4.1"),
                "PLAN",
                ".",
                "",
                false);
        AssistantCommand.Invocation mcpFeature = AssistantCommand.fromPrompt(
                "/guide locators",
                "CODEX",
                "ASK",
                ".",
                "",
                false);

        assertAll(
                () -> assertFalse(ShaftAssistantPanel.requiresMcpSetup(broadLocal, false)),
                () -> assertFalse(ShaftAssistantPanel.requiresMcpSetup(broadCloud, false)),
                () -> assertTrue(ShaftAssistantPanel.requiresMcpSetup(mcpFeature, false)),
                () -> assertFalse(ShaftAssistantPanel.requiresMcpSetup(mcpFeature, true)));
    }

    @Test
    void assistantRoutesRecordingCommandsForWebDriverAndPlaywright() throws Exception {
        AssistantCommand.Invocation webStart = AssistantCommand.fromPrompt(
                "/record https://example.com",
                "CODEX",
                "ASK",
                ".",
                "",
                false);
        AssistantCommand.Invocation playwrightStart = AssistantCommand.fromPrompt(
                "/record playwright",
                "CODEX",
                "ASK",
                ".",
                "",
                false);
        AssistantCommand.Invocation explicitPlaywrightStop = AssistantCommand.fromPrompt(
                "stop playwright recording",
                "CODEX",
                "ASK",
                ".",
                "",
                false);

        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, blankMcpSettings());
        Object playwrightBackend = recordingBackend("PLAYWRIGHT");
        setField(panel, "activeRecordingBackend", playwrightBackend);
        AssistantCommand.Invocation naturalStop = AssistantCommand.fromPrompt(
                "stop recording",
                "CODEX",
                "ASK",
                ".",
                "",
                false);
        Method route = ShaftAssistantPanel.class.getDeclaredMethod(
                "routeNaturalStopToActiveRecorder", String.class, AssistantCommand.Invocation.class);
        route.setAccessible(true);
        AssistantCommand.Invocation routedStop = (AssistantCommand.Invocation) route.invoke(
                panel, "stop recording", naturalStop);

        assertAll(
                () -> assertEquals("capture_start", webStart.toolName()),
                () -> assertEquals("https://example.com", webStart.arguments().get("targetUrl").getAsString()),
                () -> assertEquals("playwright_record_start", playwrightStart.toolName()),
                () -> assertEquals("playwright_record_stop", explicitPlaywrightStop.toolName()),
                () -> assertEquals("playwright_record_stop", routedStop.toolName()));
    }

    @Test
    void assistantBuildsPlaywrightReviewFromActiveRecordingPath() throws Exception {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, blankMcpSettings());
        Object playwrightBackend = recordingBackend("PLAYWRIGHT");
        setField(panel, "activeRecordingBackend", playwrightBackend);
        setField(panel, "activePlaywrightRecordingPath", "recordings/custom-playwright.json");

        Method review = ShaftAssistantPanel.class.getDeclaredMethod(
                "recordingCodeReviewInvocation", Class.forName("com.shaft.intellij.ui.ShaftAssistantPanel$RecordingBackend"));
        review.setAccessible(true);
        AssistantCommand.Invocation invocation = (AssistantCommand.Invocation) review.invoke(panel, playwrightBackend);

        assertAll(
                () -> assertEquals("playwright_recording_code_blocks", invocation.toolName()),
                () -> assertEquals("recordings/custom-playwright.json",
                        invocation.arguments().get("recordingPath").getAsString()));
    }

    @Test
    void toolsExplainMissingMcpConfiguration() {
        ShaftFeaturePanel panel = new ShaftFeaturePanel(null, blankMcpSettings());

        assertAll(
                () -> assertTrue(containsText(panel, "Configure SHAFT MCP")),
                () -> assertNotNull(findByAccessibleName(panel, "Open SHAFT settings", JButton.class)));
    }

    @Test
    void editableAndOutputTextAreasWrapLongContent() {
        List<JBTextArea> textAreas = new ArrayList<>();
        collectTextAreas(new ShaftAssistantPanel(null, blankMcpSettings()), textAreas);
        collectTextAreas(new ShaftFeaturePanel(null, blankMcpSettings()), textAreas);

        assertFalse(textAreas.isEmpty());
        assertAll(textAreas.stream()
                .map(textArea -> () -> assertTrue(textArea.getLineWrap())));
        assertAll(textAreas.stream()
                .map(textArea -> () -> assertTrue(textArea.getWrapStyleWord())));
    }

    @Test
    void toolsPreserveNonObjectMcpOutputForReview() throws Exception {
        ShaftFeaturePanel panel = new ShaftFeaturePanel(null, blankMcpSettings());

        showToolResult(panel, ShaftMcpToolResult.success("[\"a\",\"b\"]"));
        assertEquals("[\n  \"a\",\n  \"b\"\n]", outputText(panel));

        showToolResult(panel, ShaftMcpToolResult.success("plain text result"));
        assertEquals("plain text result", outputText(panel));
    }

    @Test
    void toolWindowShowsFirstRunSetupUntilMcpConnectionIsComplete() {
        ShaftToolWindowPanel toolWindow = new ShaftToolWindowPanel(fakeProject(), blankMcpSettings());

        assertNull(toolWindowWorkflowSelector(toolWindow));
        assertTrue(containsText(toolWindow, "Project"));
        assertTrue(containsText(toolWindow, "MCP"));
        assertTrue(containsText(toolWindow, "Runtime"));
        assertTrue(containsText(toolWindow, "Assist"));
        assertTrue(containsText(toolWindow, "Project: Configured"));
        assertTrue(containsText(toolWindow, "MCP: Not configured"));
        assertTrue(containsText(toolWindow, "Runtime: Configured"));
        assertTrue(containsText(toolWindow, "Assist: Not configured"));
        assertNotNull(findByAccessibleName(toolWindow, "Project setup section", JLabel.class));
        assertNotNull(findByAccessibleName(toolWindow, "MCP setup section", JLabel.class));
        assertNotNull(findByAccessibleName(toolWindow, "Runtime setup section", JLabel.class));
        assertNotNull(findByAccessibleName(toolWindow, "Assist setup section", JLabel.class));
        assertNotNull(findByAccessibleName(toolWindow, "Project setup status", JLabel.class));
        assertNotNull(findByAccessibleName(toolWindow, "SHAFT MCP install status", JLabel.class));
        assertNotNull(findByAccessibleName(toolWindow, "Assistant runtime setup status", JLabel.class));
        assertNotNull(findByAccessibleName(toolWindow, "Assistant connection setup status", JLabel.class));
        assertNotNull(findByAccessibleName(toolWindow, "SHAFT MCP setup next step", JLabel.class));
        assertNotNull(findByAccessibleName(toolWindow, "Install or update SHAFT MCP", JButton.class));
        assertTrue(containsText(toolWindow, "Test connection"));
    }

    @Test
    void setupPanelFormatsInstallerOutputForHumans() {
        String formatted = ShaftMcpSetupPanel.formatInstallOutput("""
                ===================
                SHAFT MCP installer
                ===================
                  ____  __  __
                 / __ \\/ / / /
                MCP installer
                27.6%
                ########
                Client target: intellij-plugin
                Resolving io.github.shafthq:shaft-mcp:LATEST...
                {"client":"intellij-plugin","server":"shaft-mcp","version":"1.0.0","command":"C:\\\\Java\\\\bin\\\\java.exe","args":["@C:\\\\shaft-mcp.args"]}
                """);

        assertAll(
                () -> assertTrue(formatted.startsWith("SHAFT MCP installation")),
                () -> assertTrue(formatted.contains("Summary")),
                () -> assertTrue(formatted.contains("- Client: intellij-plugin")),
                () -> assertTrue(formatted.contains("- Version: 1.0.0")),
                () -> assertTrue(formatted.contains("Installation log")),
                () -> assertTrue(formatted.contains("- Client target: intellij-plugin")),
                () -> assertFalse(formatted.contains("===================")),
                () -> assertFalse(formatted.contains("MCP installer")),
                () -> assertFalse(formatted.contains("27.6%")),
                () -> assertFalse(formatted.contains("########")));
    }

    @Test
    void setupPanelShowsClearGreenInstallSuccessNextStep() throws Exception {
        ShaftMcpSetupPanel panel = new ShaftMcpSetupPanel(fakeProject(), blankMcpSettings(), () -> {
        });
        ShaftMcpInstallResult result = new ShaftMcpInstallResult(true, "\"java\" \"@target/shaft-mcp.args\"", """
                Client target: codex
                {"client":"codex","server":"shaft-mcp","version":"1.0.0","command":"java","args":["@target/shaft-mcp.args"]}
                """);

        showInstallResult(panel, result);

        assertAll(
                () -> assertTrue(containsText(panel, "SUCCESS: Installation completed successfully.")),
                () -> assertTrue(containsText(panel, "Test connection and start chatting")),
                () -> assertTrue(containsText(panel, "MCP: Configured")),
                () -> assertTrue(containsText(panel, "Assist: Not configured")),
                () -> assertFalse(findButton(panel, "Install / Update SHAFT MCP").isEnabled()),
                () -> assertTrue(findButton(panel, "Test connection and start chatting").isEnabled()));
    }

    @Test
    void setupPanelReenablesInstallOnlyWhenAssistantSelectionChanges() throws Exception {
        ShaftMcpSetupPanel panel = new ShaftMcpSetupPanel(fakeProject(), blankMcpSettings(), () -> {
        });
        ShaftMcpInstallResult result = new ShaftMcpInstallResult(true, "\"java\" \"@target/shaft-mcp.args\"", """
                {"client":"codex","server":"shaft-mcp","version":"1.0.0","command":"java","args":["@target/shaft-mcp.args"]}
                """);

        showInstallResult(panel, result);
        JButton install = findButton(panel, "Install / Update SHAFT MCP");
        JButton test = findButton(panel, "Test connection and start chatting");
        JComboBox<?> family = findByAccessibleName(panel, "Assistant family", JComboBox.class);
        JComboBox<?> runtime = findByAccessibleName(panel, "Assistant runtime", JComboBox.class);

        assertAll(
                () -> assertNotNull(install),
                () -> assertNotNull(test),
                () -> assertNotNull(family),
                () -> assertNotNull(runtime),
                () -> assertFalse(install.isEnabled()),
                () -> assertTrue(test.isEnabled()));

        family.setSelectedItem("CLAUDE");
        assertAll(
                () -> assertTrue(install.isEnabled()),
                () -> assertFalse(test.isEnabled()),
                () -> assertTrue(containsText(panel, "MCP: Not configured")));

        family.setSelectedItem("CODEX");
        assertAll(
                () -> assertFalse(install.isEnabled()),
                () -> assertTrue(test.isEnabled()),
                () -> assertTrue(containsText(panel, "MCP: Configured")));

        runtime.setSelectedItem("DESKTOP_APP");
        assertAll(
                () -> assertTrue(install.isEnabled()),
                () -> assertFalse(test.isEnabled()),
                () -> assertTrue(containsText(panel, "MCP: Not configured")));
    }

    @Test
    void setupPanelShowsClearConnectionSuccess() throws Exception {
        AtomicBoolean connected = new AtomicBoolean();
        ShaftSettingsState.Settings settings = connectedMcpSettings();
        ShaftMcpSetupPanel panel = new ShaftMcpSetupPanel(fakeProject(), settings, () -> connected.set(true), readyProbe());

        showTestResult(panel, ShaftMcpToolResult.success("Probe OK"));

        assertAll(
                () -> assertTrue(containsText(panel, "Probe OK")),
                () -> assertTrue(containsText(panel, "SUCCESS: Connected to SHAFT MCP.")),
                () -> assertTrue(containsText(panel, "SUCCESS: Codex CLI executable is available on PATH.")),
                () -> assertTrue(containsText(panel, "Connected")),
                () -> assertTrue(connected.get()),
                () -> assertTrue(settings.mcpSetupComplete),
                () -> assertTrue(settings.agentGuidanceOptimizationPromptPending));
    }

    @Test
    void setupPanelBlocksConnectionWhenSelectedAgentIsNotReady() throws Exception {
        AtomicBoolean connected = new AtomicBoolean();
        ShaftSettingsState.Settings settings = connectedMcpSettings();
        ShaftMcpSetupPanel panel = new ShaftMcpSetupPanel(fakeProject(), settings, () -> connected.set(true),
                (client, runtime) -> ShaftMcpToolResult.failure("Codex CLI executable is not available on PATH."));

        showTestResult(panel, ShaftMcpToolResult.success("Probe OK"));

        assertAll(
                () -> assertTrue(containsText(panel, "Assist: Error")),
                () -> assertTrue(containsText(panel, "Agent readiness failed: Codex CLI executable is not available on PATH.")),
                () -> assertFalse(connected.get()),
                () -> assertFalse(settings.mcpSetupComplete),
                () -> assertFalse(settings.agentGuidanceOptimizationPromptPending));
    }

    @Test
    void assistantConsumesPendingGuidanceOptimizationPromptOnce() {
        ShaftSettingsState.Settings settings = connectedMcpSettings();
        settings.agentGuidanceOptimizationPromptPending = true;

        ShaftAssistantPanel first = new ShaftAssistantPanel(null, settings);
        ShaftAssistantPanel second = new ShaftAssistantPanel(null, settings);

        assertAll(
                () -> assertTrue(containsText(first, "Audit and optimize")),
                () -> assertTrue(containsText(first, "shaft_guide_search")),
                () -> assertTrue(containsText(first, "test_automation_scenarios")),
                () -> assertTrue(containsText(first, "test_code_guardrails_check")),
                () -> assertFalse(settings.agentGuidanceOptimizationPromptPending),
                () -> assertFalse(containsText(second, "Audit and optimize")));
    }

    @Test
    void guidanceOptimizationPromptUsesSelectedAgentSurfaces() {
        ShaftSettingsState.Settings codex = connectedMcpSettings();
        ShaftSettingsState.Settings claude = connectedMcpSettings();
        claude.assistantFamily = "CLAUDE";
        ShaftSettingsState.Settings copilot = connectedMcpSettings();
        copilot.assistantFamily = "COPILOT";

        assertAll(
                () -> assertTrue(ShaftAssistantPanel.agentGuidanceOptimizationPrompt(codex)
                        .contains("AGENTS.md, .codex/config.toml, .agents/skills/**, .memory/**")),
                () -> assertTrue(ShaftAssistantPanel.agentGuidanceOptimizationPrompt(claude)
                        .contains("CLAUDE.md, AGENTS.md, .agents/skills/**, .memory/**")),
                () -> assertTrue(ShaftAssistantPanel.agentGuidanceOptimizationPrompt(copilot)
                        .contains(".github/copilot-instructions.md, AGENTS.md, .github/instructions/**, .github/skills/**, .memory/**")));
    }

    @Test
    void setupPanelShowsInlineErrorStatesAndKeepsRetryEnabled() throws Exception {
        ShaftMcpSetupPanel installPanel = new ShaftMcpSetupPanel(fakeProject(), blankMcpSettings(), () -> {
        });
        showInstallError(installPanel, new IllegalStateException("installer unavailable"));

        assertAll(
                () -> assertTrue(containsText(installPanel, "MCP: Error")),
                () -> assertTrue(containsText(installPanel, "installer unavailable")),
                () -> assertTrue(findButton(installPanel, "Install / Update SHAFT MCP").isEnabled()),
                () -> assertFalse(findButton(installPanel, "Test connection and start chatting").isEnabled()));

        ShaftMcpSetupPanel testPanel = new ShaftMcpSetupPanel(fakeProject(), connectedMcpSettings(), () -> {
        });
        showTestResult(testPanel, ShaftMcpToolResult.failure("Probe failed"));

        assertAll(
                () -> assertTrue(containsText(testPanel, "Assist: Error")),
                () -> assertTrue(containsText(testPanel, "Probe failed")),
                () -> assertTrue(findButton(testPanel, "Test connection and start chatting").isEnabled()));
    }

    @Test
    void toolWindowHidesAdvancedWorkflowsByDefault() {
        ShaftToolWindowPanel toolWindow = new ShaftToolWindowPanel(fakeProject(), connectedMcpSettings());

        JComboBox<ShaftToolWindowPanel.WorkflowView> selector = toolWindowWorkflowSelector(toolWindow);
        assertAll(
                () -> assertNull(selector),
                () -> assertFalse(containsText(toolWindow, "Workflow")),
                () -> assertTrue(containsText(toolWindow, "Configure")));
    }

    @Test
    void toolWindowShowsReadableWorkflowSelectorLabelsWhenAdvancedUiIsEnabled() {
        ShaftToolWindowPanel toolWindow = new ShaftToolWindowPanel(fakeProject(), advancedConnectedMcpSettings());

        JComboBox<ShaftToolWindowPanel.WorkflowView> selector = toolWindowWorkflowSelector(toolWindow);
        assertNotNull(selector);
        List<String> labels = new ArrayList<>();
        for (int index = 0; index < selector.getItemCount(); index++) {
            labels.add(selector.getItemAt(index).label());
        }

        assertEquals(List.of("Assistant", "Guided", "Recorder", "Inspector", "Triage", "Evidence",
                "Projects", "Advanced"), labels);
    }

    @Test
    void workflowSelectorKeepsEnoughHeightForVisibleTopLabels() {
        ShaftToolWindowPanel toolWindow = new ShaftToolWindowPanel(fakeProject(), advancedConnectedMcpSettings());
        JComboBox<ShaftToolWindowPanel.WorkflowView> selector = toolWindowWorkflowSelector(toolWindow);

        assertNotNull(selector);
        assertTrue(selector.getPreferredSize().height >= 30);
    }

    @Test
    void prefillToolSelectsMatchingWorkflowAndCategory() {
        ShaftToolWindowPanel toolWindow = new ShaftToolWindowPanel(fakeProject(), advancedConnectedMcpSettings());
        JComboBox<ShaftToolWindowPanel.WorkflowView> selector = toolWindowWorkflowSelector(toolWindow);
        assertNotNull(selector);
        JsonObject arguments = JsonParser.parseString("{}").getAsJsonObject();

        toolWindow.prefillTool("capture_start", arguments);
        assertEquals("Recorder", selectedWorkflow(toolWindow));
        assertEquals("Recorder", selectedCategory(toolWindow));

        toolWindow.prefillTool("mobile_get_accessibility_tree", arguments);
        assertEquals("Inspector", selectedWorkflow(toolWindow));
        assertEquals("Inspector", selectedCategory(toolWindow));

        toolWindow.prefillTool("doctor_analyze_trace", arguments);
        assertEquals("Evidence", selectedWorkflow(toolWindow));
        assertEquals("Evidence", selectedCategory(toolWindow));

        toolWindow.prefillTool("shaft_project_create", arguments);
        assertEquals("Projects", selectedWorkflow(toolWindow));
        assertEquals("Projects", selectedCategory(toolWindow));
    }

    @Test
    void assistantDisplaysMarkdownInsteadOfNestedMcpJsonEnvelope() throws Exception {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, blankMcpSettings());
        String javaCode = """
                public class LoginTest {
                    @Test void logsIn() {
                    }
                }
                """.stripIndent().trim();
        String output = mcpText(mcpText(javaCode));

        showAssistantResult(panel, ShaftMcpToolResult.success(output));

        String markdown = transcriptMarkdown(panel);
        assertAll(
                () -> assertTrue(markdown.contains("```java")),
                () -> assertTrue(markdown.contains("public class LoginTest")),
                () -> assertFalse(markdown.contains("\\\"content\\\"")),
                () -> assertNotNull(findButton(panel, "Copy raw")));
    }

    @Test
    void assistantDisplaysKnownClientListAsMarkdown() throws Exception {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, blankMcpSettings());

        showAssistantResult(panel, "autobot_local_agent_clients", ShaftMcpToolResult.success(mcpText("""
                [
                  {"id":"CODEX","displayName":"Codex CLI","executableName":"codex","requiresCloudApiKey":false}
                ]
                """)));

        String markdown = transcriptMarkdown(panel);
        assertAll(
                () -> assertTrue(markdown.contains("| Client | Command | SHAFT API key |")),
                () -> assertTrue(markdown.contains("| Codex CLI | `codex` | Not required |")),
                () -> assertFalse(markdown.contains("\\\"displayName\\\"")));
    }

    @Test
    void assistantCaptureCodegenResultWaitsForApprovalBeforeWritingFiles() throws Exception {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, blankMcpSettings());
        setField(panel, "captureReviewGenerationRunning", true);

        showAssistantResult(panel, "capture_code_blocks", ShaftMcpToolResult.success(mcpText("""
                {
                  "successful": true,
                  "codeBlocks": [
                    {"language":"java","code":"public class RecordedFlowTest {}"}
                  ]
                }
                """)));

        String markdown = transcriptMarkdown(panel);
        JComponent reviewPanel = findByAccessibleName(panel, "Capture review approval", JComponent.class);
        assertAll(
                () -> assertTrue(markdown.contains("public class RecordedFlowTest")),
                () -> assertTrue(markdown.contains("Review before writing files")),
                () -> assertTrue(markdown.contains("`approve`, `okay`, or `generate`")),
                () -> assertNotNull(reviewPanel),
                () -> assertTrue(reviewPanel.isVisible()),
                () -> assertTrue(containsText(reviewPanel, "Capture review ready")),
                () -> assertNotNull(findByAccessibleName(panel, "Approve Capture review", JButton.class)),
                () -> assertNotNull(findByAccessibleName(panel, "Copy Capture review", JButton.class)),
                () -> assertNotNull(findByAccessibleName(panel, "Dismiss Capture review", JButton.class)));
    }

    @Test
    void dismissedCaptureReviewClearsApprovalPanelOnly() throws Exception {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, blankMcpSettings());
        setField(panel, "captureReviewGenerationRunning", true);

        showAssistantResult(panel, "capture_code_blocks", ShaftMcpToolResult.success(mcpText("""
                {
                  "successful": true,
                  "codeBlocks": [
                    {"language":"java","code":"public class RecordedFlowTest {}"}
                  ]
                }
                """)));

        click(panel, "Dismiss");

        JComponent reviewPanel = findByAccessibleName(panel, "Capture review approval", JComponent.class);
        assertAll(
                () -> assertNotNull(reviewPanel),
                () -> assertFalse(reviewPanel.isVisible()),
                () -> assertNull(getField(panel, "pendingCaptureReview")),
                () -> assertTrue(transcriptMarkdown(panel).contains("RecordedFlowTest")));
    }

    @Test
    void captureStartDiagnosticShowsExitedRecorderStatus() throws Exception {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, connectedMcpSettings());
        String startOutput = mcpText("""
                {
                  "state": "ACTIVE",
                  "outputPath": "recordings/intellij-capture-1.json",
                  "processId": 111
                }
                """);
        String statusOutput = mcpText("""
                {
                  "state": "NOT_RUNNING",
                  "outputPath": "",
                  "processId": 222,
                  "warnings": ["The recorder process is no longer reachable."]
                }
                """);

        showCaptureStartDiagnostic(panel, "recordings/intellij-capture-1.json", startOutput,
                ShaftMcpToolResult.success(statusOutput));

        String markdown = transcriptMarkdown(panel);
        assertAll(
                () -> assertTrue(markdown.contains("Capture diagnostic")),
                () -> assertTrue(markdown.contains("State: `NOT_RUNNING`")),
                () -> assertTrue(markdown.contains("Recorder process: `111`")),
                () -> assertTrue(markdown.contains("Output: `recordings/intellij-capture-1.json`")),
                () -> assertTrue(markdown.contains("The recorder process is no longer reachable.")));
    }

    @Test
    void cancelledCaptureCodegenReviewDoesNotArmLaterApprovalFlow() throws Exception {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, blankMcpSettings());
        setField(panel, "captureReviewGenerationRunning", true);

        showAssistantResult(panel, "capture_code_blocks", null, new CancellationException("cancelled"));
        showAssistantResult(panel, "capture_code_blocks", ShaftMcpToolResult.success(mcpText("""
                {
                  "successful": true,
                  "codeBlocks": [
                    {"language":"java","code":"public class LaterGeneratedTest {}"}
                  ]
                }
                """)));

        String markdown = transcriptMarkdown(panel);
        assertAll(
                () -> assertTrue(markdown.contains("capture_code_blocks cancelled")),
                () -> assertTrue(markdown.contains("LaterGeneratedTest")),
                () -> assertFalse(markdown.contains("Review before writing files")),
                () -> assertNull(getField(panel, "pendingCaptureReview")),
                () -> assertFalse((Boolean) getField(panel, "captureReviewGenerationRunning")));
    }

    @Test
    void assistantKeepsShaftWrapperForCuratedMcpToolResponses() throws Exception {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, connectedMcpSettings());

        showAssistantResult(panel, "shaft_guide_search", ShaftMcpToolResult.success(mcpText("Use Page Object locators.")));

        String markdown = transcriptMarkdown(panel);
        assertAll(
                () -> assertTrue(markdown.contains("**SHAFT Assistant (shaft_guide_search OK)**")),
                () -> assertTrue(markdown.contains("Use Page Object locators.")));
    }

    @Test
    void assistantDisplaysDirectLocalAgentMarkdownWithoutWrapperMetadata() throws Exception {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, connectedMcpSettings());

        showAgentResult(panel, ShaftMcpToolResult.success("""
                ## Done

                Opened DuckDuckGo and searched for SHAFT Engine.
                """.stripIndent().trim()));

        String markdown = transcriptMarkdown(panel);
        assertAll(
                () -> assertTrue(markdown.contains("## Done")),
                () -> assertTrue(markdown.contains("Opened DuckDuckGo")),
                () -> assertFalse(markdown.contains("SHAFT Assistant")),
                () -> assertFalse(markdown.contains("Status:")),
                () -> assertFalse(markdown.contains("autobot_local_agent_run")));
    }

    @Test
    void assistantLocalAgentStaleCompletionClearsThinkingState() throws Exception {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, connectedMcpSettings());
        panel.setRunning(true, "Thinking...");
        setField(panel, "activeLocalAgentStreamToken", -1);

        showAgentResult(panel, 42, ShaftMcpToolResult.success("Created Page Object files."));

        JLabel status = (JLabel) getField(panel, "status");
        JProgressBar progress = (JProgressBar) getField(panel, "progress");
        assertAll(
                () -> assertEquals("ready", status.getText()),
                () -> assertFalse(progress.isVisible()),
                () -> assertTrue(transcriptMarkdown(panel).contains("Created Page Object files.")),
                () -> assertFalse(transcriptMarkdown(panel).contains("Running local assistant")));
    }

    @Test
    void assistantLocalAgentStaleCompletionDoesNotInterruptActiveStream() throws Exception {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, connectedMcpSettings());
        panel.setRunning(true, "Thinking...");
        setField(panel, "activeLocalAgentStreamToken", 7);

        showAgentResult(panel, 42, ShaftMcpToolResult.success("Stale output"));

        JLabel status = (JLabel) getField(panel, "status");
        JProgressBar progress = (JProgressBar) getField(panel, "progress");
        assertAll(
                () -> assertEquals("Thinking...", status.getText()),
                () -> assertTrue(progress.isVisible()),
                () -> assertFalse(transcriptMarkdown(panel).contains("Stale output")));
    }

    @Test
    void assistantRejectsNativeSeleniumLocalAgentCodeAndNamesShaftPracticeTools() throws Exception {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, connectedMcpSettings());

        showAgentResult(panel, ShaftMcpToolResult.success("""
                ```java
                public class BadGeneratedTest {
                    @Test
                    void login() {
                        driver.get("https://example.com");
                        driver.findElement(By.id("login")).click();
                    }
                }
                ```
                """.stripIndent().trim()));

        String markdown = transcriptMarkdown(panel);
        String exported = assistantExport(panel);

        assertAll(
                () -> assertTrue(markdown.contains("**Generated code rejected**")),
                () -> assertTrue(markdown.contains("Ask the agent to regenerate")),
                () -> assertTrue(markdown.contains("`shaft_guide_search`")),
                () -> assertTrue(markdown.contains("`test_automation_scenarios`")),
                () -> assertTrue(markdown.contains("`test_code_guardrails_check`")),
                () -> assertFalse(markdown.contains("driver.get")),
                () -> assertFalse(markdown.contains("driver.findElement")),
                () -> assertFalse(exported.contains("driver.get")),
                () -> assertFalse(exported.contains("driver.findElement")),
                () -> assertFalse(exported.contains("autobot_local_agent_run")));
    }

    @Test
    void assistantRejectsNativeSeleniumMcpCodeBlocksBeforeApproval() throws Exception {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, connectedMcpSettings());
        setField(panel, "captureReviewGenerationRunning", true);

        showAssistantResult(panel, "capture_code_blocks", ShaftMcpToolResult.success(mcpText("""
                {
                  "codeBlocks": [
                    {"language":"java","code":"driver.get(\\"https://example.com\\");\\ndriver.findElement(By.id(\\"login\\")).click();"}
                  ]
                }
                """)));

        String markdown = transcriptMarkdown(panel);
        String exported = assistantExport(panel);

        assertAll(
                () -> assertTrue(markdown.contains("capture_code_blocks rejected")),
                () -> assertTrue(markdown.contains("**Generated code rejected**")),
                () -> assertTrue(markdown.contains("`shaft_guide_search`")),
                () -> assertFalse(markdown.contains("Review before writing files")),
                () -> assertFalse(markdown.contains("driver.get")),
                () -> assertFalse(markdown.contains("driver.findElement")),
                () -> assertNull(getField(panel, "pendingCaptureReview")),
                () -> assertFalse((Boolean) getField(panel, "captureReviewGenerationRunning")),
                () -> assertFalse(exported.contains("driver.get")),
                () -> assertFalse(exported.contains("driver.findElement")));
    }

    @Test
    void assistantRejectsNativeSeleniumSequenceCodeBlocksBeforeEvidenceExport() throws Exception {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, connectedMcpSettings());
        setField(panel, "sequenceMarkdown", new StringBuilder());
        setField(panel, "sequenceRawOutput", new StringBuilder());

        showSequenceResult(
                panel,
                new AssistantCommand.ToolCall("capture_code_blocks", new JsonObject()),
                ShaftMcpToolResult.success(mcpText("""
                        {
                          "codeBlocks": [
                            {"language":"java","code":"driver.get(\\"https://example.com\\");\\ndriver.findElement(By.id(\\"login\\")).click();"}
                          ]
                        }
                        """)));

        String markdown = transcriptMarkdown(panel);
        String exported = assistantExport(panel);

        assertAll(
                () -> assertTrue(markdown.contains("SHAFT Assistant sequence rejected")),
                () -> assertTrue(markdown.contains("capture_code_blocks rejected")),
                () -> assertTrue(markdown.contains("**Generated code rejected**")),
                () -> assertFalse(markdown.contains("driver.get")),
                () -> assertFalse(markdown.contains("driver.findElement")),
                () -> assertFalse(exported.contains("driver.get")),
                () -> assertFalse(exported.contains("driver.findElement")),
                () -> assertFalse(exported.contains("Tool evidence")));
    }

    @Test
    void assistantPersistsActiveChatAndCanStartNewContext() throws Exception {
        ShaftAssistantChatState chatState = new ShaftAssistantChatState();
        ShaftSettingsState.Settings settings = blankMcpSettings();
        ShaftAssistantPanel firstPanel = new ShaftAssistantPanel(null, settings, chatState);

        showAssistantResult(firstPanel, ShaftMcpToolResult.success(mcpText("First answer")));

        ShaftAssistantPanel reopenedPanel = new ShaftAssistantPanel(null, settings, chatState);
        assertTrue(transcriptMarkdown(reopenedPanel).contains("First answer"));

        click(reopenedPanel, "New chat");
        assertFalse(transcriptMarkdown(reopenedPanel).contains("First answer"));

        showAssistantResult(reopenedPanel, ShaftMcpToolResult.success(mcpText("Second answer")));
        assertAll(
                () -> assertTrue(transcriptMarkdown(reopenedPanel).contains("Second answer")),
                () -> assertEquals(2, chatState.sessions().size()));
    }

    @Test
    void persistedSetupOpensAssistantWithPreviousChatsInDropdown() {
        ShaftSettingsState.Settings settings = connectedMcpSettings();
        ShaftAssistantChatState chatState = new ShaftAssistantChatState();
        Project project = fakeProject(chatState);
        chatState.append("user", "start recording", "{}");
        chatState.append("assistant", "Capture browser opened.", "{}");
        chatState.newSession();
        chatState.append("user", "generate reviewed code", "{}");

        ShaftToolWindowPanel toolWindow = new ShaftToolWindowPanel(project, settings);
        JComboBox<?> chats = findByAccessibleName(toolWindow, "Assistant chat", JComboBox.class);

        assertAll(
                () -> assertNull(setupPanel(toolWindow)),
                () -> assertNull(toolWindowWorkflowSelector(toolWindow)),
                () -> assertTrue(transcriptMarkdown(toolWindow).contains("generate reviewed code")),
                () -> assertNotNull(chats),
                () -> assertEquals(2, chats.getItemCount()),
                () -> assertTrue(comboContains(chats, "start recording")),
                () -> assertTrue(comboContains(chats, "generate reviewed code")));
    }

    @Test
    void userMessagesDoNotRenderSpeakerLabelsOrUseThemAsChatTitles() {
        ShaftAssistantChatState chatState = new ShaftAssistantChatState();
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, blankMcpSettings(), chatState);

        assistantPrompt(panel).setText("start recording");
        clickAccessible(panel, "Send assistant prompt");

        String transcript = transcriptMarkdown(panel);
        assertAll(
                () -> assertTrue(transcript.contains("start recording")),
                () -> assertFalse(transcript.contains(String.valueOf(new char[]{'Y', 'o', 'u'}))),
                () -> assertEquals("start recording", chatState.activeSession().title));

        String speaker = String.valueOf(new char[]{'Y', 'o', 'u'});
        String localUser = System.getProperty("user.name", "");
        ShaftAssistantChatState legacyState = new ShaftAssistantChatState();
        legacyState.append("user", "**" + speaker + " (Agent via Codex CLI)**\n\n"
                + localUser + " open duckduckgo", "{}");

        assertAll(
                () -> assertTrue(legacyState.activeMarkdown().contains("open duckduckgo")),
                () -> assertFalse(legacyState.activeMarkdown().contains("Agent via")),
                () -> assertFalse(legacyState.activeSession().title.contains(speaker)),
                () -> assertFalse(localUser.length() > 1
                        && legacyState.activeSession().title.contains(localUser)));
    }

    @Test
    void assistantDoesNotPersistRawResponsePayloads() throws Exception {
        ShaftAssistantChatState chatState = new ShaftAssistantChatState();

        chatState.append("assistant", "Rendered response", "{\"secret\":\"raw payload\"}");

        assertAll(
                () -> assertEquals("Rendered response",
                        chatState.getState().sessions.get(0).messages.get(0).markdown),
                () -> assertThrows(NoSuchFieldException.class,
                        () -> ShaftAssistantChatState.Message.class.getDeclaredField("raw")));
    }

    @Test
    void assistantNewChatClearsRerunAndCopyState() {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, blankMcpSettings());

        assistantPrompt(panel).setText("/help");
        clickAccessible(panel, "Send assistant prompt");
        assertAll(
                () -> assertTrue(findButton(panel, "Rerun").isEnabled()),
                () -> assertTrue(findButton(panel, "Copy response").isEnabled()));

        click(panel, "New chat");

        assertAll(
                () -> assertFalse(findButton(panel, "Rerun").isEnabled()),
                () -> assertFalse(findButton(panel, "Copy response").isEnabled()),
                () -> assertFalse(transcriptMarkdown(panel).contains("Slash commands:")));
    }

    @Test
    void assistantAndToolsControlsExposeAccessibleMetadata() {
        assertAccessibleControls(new ShaftAssistantPanel(null, blankMcpSettings()));
        assertAccessibleControls(new ShaftFeaturePanel(null, blankMcpSettings()));
        assertAccessibleControls(new ShaftMcpSetupPanel(fakeProject(), blankMcpSettings(), () -> {
        }));
        assertAccessibleControls(new GuidedWorkflowPanel(null, (tool, arguments) -> {
        }));
        assertAccessibleControls(new EvidenceTriagePanel(null, (tool, arguments) -> {
        }));
    }

    @Test
    void assistantComposerUsesCommandAutocompleteAndModernThinkingIndicator() {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, blankMcpSettings());

        JComboBox<?> commandAutocomplete = findByAccessibleName(panel, "Assistant command autocomplete", JComboBox.class);
        JProgressBar spinner = findByAccessibleName(panel, "Assistant thinking spinner", JProgressBar.class);
        JButton sendButton = findByAccessibleName(panel, "Send assistant prompt", JButton.class);

        assertAll(
                () -> assertNotNull(commandAutocomplete),
                () -> assertTrue(commandAutocomplete.isEditable()),
                () -> assertTrue(comboContains(commandAutocomplete, "/commands")),
                () -> assertTrue(comboContains(commandAutocomplete, "/browser")),
                () -> assertFalse(containsText(panel, "Add context (#), extensions (@), commands (/commands)")),
                () -> assertNull(findByAccessibleName(panel, "SHAFT command hints", JButton.class)),
                () -> assertNotNull(spinner),
                () -> assertTrue(spinner.isIndeterminate()),
                () -> assertFalse(spinner.isVisible()),
                () -> assertNotNull(sendButton),
                () -> assertEquals("", sendButton.getText()),
                () -> assertNotNull(sendButton.getIcon()),
                () -> assertTrue(sendButton.getIcon().getIconWidth() > 0),
                () -> assertTrue(sendButton.getIcon().getIconHeight() > 0));
    }

    @Test
    void assistantCommandAutocompleteInsertsCommandIntoPrompt() {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, blankMcpSettings());
        JComboBox<?> commandAutocomplete = findByAccessibleName(panel, "Assistant command autocomplete", JComboBox.class);
        JTextComponent prompt = assistantPrompt(panel);

        prompt.setText("please ");
        prompt.setCaretPosition(prompt.getDocument().getLength());
        commandAutocomplete.setSelectedItem("/browser");

        assertEquals("please /browser ", prompt.getText());
    }

    @Test
    void assistantListedControlsAreIconOnlyAndSymmetric() {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, blankMcpSettings());
        List<String> controls = List.of(
                "Start a new Assistant chat",
                "Copy last assistant response",
                "Copy last raw assistant response",
                "Copy assistant transcript",
                "Clear assistant transcript",
                "Rerun last assistant prompt",
                "Cancel assistant request");

        assertAll(controls.stream()
                .map(accessibleName -> () -> assertIconOnlySymmetric(
                        findByAccessibleName(panel, accessibleName, JButton.class))));
    }

    @Test
    void toolWindowButtonsAreIconOnlyAndSymmetric() {
        List<Component> panels = List.of(
                new ShaftAssistantPanel(null, blankMcpSettings()),
                new ShaftFeaturePanel(null, blankMcpSettings()),
                new ShaftMcpSetupPanel(fakeProject(), blankMcpSettings(), () -> {
                }),
                new GuidedWorkflowPanel(null, (tool, arguments) -> {
                }),
                new EvidenceTriagePanel(null, (tool, arguments) -> {
                }));

        assertAll(panels.stream()
                .flatMap(panel -> collectButtons(panel).stream())
                .map(button -> () -> assertIconOnlySymmetric(button)));
    }

    @Test
    void iconButtonsUseClickableIconOnlySurfaces() {
        JButton active = new JButton("Run");
        JButton inactive = new JButton("Cancel");
        ShaftIconButtons.apply(active, ShaftIcons.SEND);
        ShaftIconButtons.apply(inactive, ShaftIcons.CANCEL);

        inactive.setEnabled(false);

        assertAll(
                () -> assertNotNull(inactive.getDisabledIcon()),
                () -> assertEquals(inactive.getIcon().getIconWidth(), inactive.getDisabledIcon().getIconWidth()),
                () -> assertTrue(active.isContentAreaFilled()),
                () -> assertTrue(active.isBorderPainted()),
                () -> assertTrue(active.isFocusPainted()),
                () -> assertTrue(active.isRolloverEnabled()),
                () -> assertTrue(inactive.isContentAreaFilled()),
                () -> assertTrue(inactive.isBorderPainted()));
    }

    @Test
    void assistantSendButtonTurnsIntoProgressAndHoverCancelWhileRunning() {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, blankMcpSettings());
        JButton sendButton = findByAccessibleName(panel, "Send assistant prompt", JButton.class);
        assertNotNull(sendButton);
        Icon readyIcon = sendButton.getIcon();

        panel.setRunning(true, "Thinking...");
        Icon runningIcon = sendButton.getIcon();

        assertAll(
                () -> assertTrue(sendButton.isEnabled()),
                () -> assertNotEquals(readyIcon, runningIcon),
                () -> assertEquals("Assistant request running", sendButton.getToolTipText()));

        notifyShaftMouseListener(sendButton, MouseEvent.MOUSE_ENTERED);
        Icon hoverIcon = sendButton.getIcon();
        assertAll(
                () -> assertNotEquals(runningIcon, hoverIcon),
                () -> assertEquals("Cancel assistant request", sendButton.getToolTipText()));

        notifyShaftMouseListener(sendButton, MouseEvent.MOUSE_EXITED);
        assertAll(
                () -> assertEquals(runningIcon, sendButton.getIcon()),
                () -> assertEquals("Assistant request running", sendButton.getToolTipText()));

        panel.setRunning(false, "ready");
        assertAll(
                () -> assertEquals(readyIcon, sendButton.getIcon()),
                () -> assertEquals("Send assistant prompt", sendButton.getToolTipText()));
    }

    @Test
    void actionButtonsUseModernSvgIcons() {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, blankMcpSettings());
        ShaftFeaturePanel featurePanel = new ShaftFeaturePanel(null, blankMcpSettings());
        ShaftMcpSetupPanel setupPanel = new ShaftMcpSetupPanel(fakeProject(), blankMcpSettings(), () -> {
        });
        GuidedWorkflowPanel guidedPanel = new GuidedWorkflowPanel(null, (tool, arguments) -> {
        });
        EvidenceTriagePanel triagePanel = new EvidenceTriagePanel(null, (tool, arguments) -> {
        });

        assertAll(
                () -> assertNull(findButton(panel, "Test connection and start chatting")),
                () -> assertIcon(findButton(panel, "New chat")),
                () -> assertIcon(findButton(panel, "Copy response")),
                () -> assertIcon(findButton(panel, "Copy raw")),
                () -> assertIcon(findButton(panel, "Copy all")),
                () -> assertIcon(findButton(panel, "Clear")),
                () -> assertIcon(findButton(panel, "Rerun")),
                () -> assertIcon(findButton(panel, "Cancel")),
                () -> assertIcon(findByAccessibleName(panel, "Send assistant prompt", JButton.class)),
                () -> assertIcon(findButton(featurePanel, "Run")),
                () -> assertIcon(findButton(featurePanel, "Cancel")),
                () -> assertIcon(findButton(featurePanel, "Restore defaults")),
                () -> assertIcon(findButton(featurePanel, "Copy output")),
                () -> assertIcon(findButton(featurePanel, "Refresh tools")),
                () -> assertIcon(findButton(setupPanel, "Install / Update SHAFT MCP")),
                () -> assertIcon(findButton(setupPanel, "Test connection and start chatting")),
                () -> assertIcon(findButton(guidedPanel, "Start recording")),
                () -> assertIcon(findButton(guidedPanel, "Stop recording")),
                () -> assertIcon(findButton(guidedPanel, "Generate code")),
                () -> assertIcon(findButton(guidedPanel, "Inspect locator")),
                () -> assertIcon(findButton(guidedPanel, "Guardrail check")),
                () -> assertIcon(findButton(triagePanel, "Analyze Allure")),
                () -> assertIcon(findButton(triagePanel, "Analyze Trace")),
                () -> assertIcon(findButton(triagePanel, "Suggest Fix")),
                () -> assertIcon(findButton(triagePanel, "Run Healer")),
                () -> assertIcon(findButton(triagePanel, "Propose Locator")));
    }

    @Test
    void assistantPanelShowsConfigureButtonWhenSetupCallbackIsConfigured() {
        ShaftAssistantChatState chatState = new ShaftAssistantChatState();
        AtomicBoolean openedSetup = new AtomicBoolean();
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, connectedMcpSettings(), chatState,
                () -> openedSetup.set(true));

        JButton configure = findButton(panel, "Configure");
        JComboBox<?> family = findByAccessibleName(panel, "Assistant family", JComboBox.class);
        JComboBox<?> runtime = findByAccessibleName(panel, "Assistant runtime", JComboBox.class);

        assertAll(
                () -> assertNotNull(configure),
                () -> assertNotNull(family),
                () -> assertNotNull(runtime),
                () -> assertTrue(configure.isVisible()),
                () -> assertFalse(family.isVisible()),
                () -> assertFalse(runtime.isVisible()),
                () -> assertTrue(containsText(panel, "Agent: Local / Codex / CLI")),
                () -> assertNull(findButton(panel, "Test connection and start chatting")),
                () -> assertFalse(openedSetup.get()));

        configure.doClick();
        assertTrue(openedSetup.get());
    }

    @Test
    void assistantTranscriptInitialHintUsesCanonicalCommandsEntry() {
        AssistantTranscriptView transcript = new AssistantTranscriptView();
        String html = transcript.html();

        assertAll(
                () -> assertTrue(html.contains("/commands")),
                () -> assertFalse(html.contains("/help")));
    }

    @Test
    void assistantTranscriptRendersRoleBasedMessageBubbles() {
        AssistantTranscriptView transcript = new AssistantTranscriptView();
        transcript.append("user", "Hello assistant\n\n```java\nclass UserPrompt {}\n```");
        transcript.append("assistant", "Hi user");

        String html = transcript.html();
        assertAll(
                () -> assertTrue(html.contains("class=\"shaft-chat-row user ")),
                () -> assertTrue(html.contains("class=\"shaft-chat-row assistant ")),
                () -> assertTrue(html.contains("class=\"shaft-chat-bubble user ")),
                () -> assertTrue(html.contains("class=\"shaft-chat-bubble assistant ")),
                () -> assertTrue(html.contains("shaft-chat-row-user")),
                () -> assertTrue(html.contains("shaft-chat-bubble-user")),
                () -> assertTrue(html.contains("background-color:")),
                () -> assertTrue(html.contains("border-radius")),
                () -> assertTrue(html.contains("class=\"shaft-chat-hint\"")),
                () -> assertTrue(html.contains("class=\"shaft-code-copy\"")),
                () -> assertTrue(html.contains("data-copy-code=\"class UserPrompt")),
                () -> assertTrue(html.contains("aria-label=\"Copy code\"")),
                () -> assertTrue(html.contains("<svg width=\"16\" height=\"16\"")),
                () -> assertTrue(html.contains("class=\"shaft-code-copy-icon\"")),
                () -> assertTrue(html.contains("&#x2398;")),
                () -> assertTrue(html.contains("class=\"shaft-code-highlighted language-java\""), html),
                () -> assertTrue(html.contains("<span style=\""), html),
                () -> assertFalse(html.contains(">Copy code<")),
                () -> assertTrue(html.contains("Hello assistant")),
                () -> assertTrue(html.contains("Hi user")),
                () -> assertTrue(html.indexOf("Hi user") < html.indexOf("Type a question")),
                () -> assertFalse(html.contains("cellpadding=\"8\"")),
                () -> assertFalse(html.contains("border=\"1\"")),
                () -> assertFalse(html.contains("<table")));
    }

    @Test
    void assistantTranscriptUsesReadActionForIntellijRendering() {
        assertTrue(AssistantTranscriptView.renderReadActionForTest(
                () -> ApplicationManager.getApplication() == null
                        || ApplicationManager.getApplication().isReadAccessAllowed()));
    }

    @Test
    void assistantTranscriptKeepsLatestMessageInView() {
        AssistantTranscriptView transcript = new AssistantTranscriptView();
        transcript.append("user", "Show the sent prompt");

        assertTrue(transcript.html().contains("window.scrollTo(0, document.body.scrollHeight)"));
    }

    @Test
    void assistantTranscriptBubblesDoNotPaintRectangularOutlines() {
        AssistantTranscriptView transcript = new AssistantTranscriptView();
        transcript.append("assistant", "Assistant response");

        String html = transcript.html();
        String assistantBubbleCss = html.substring(
                html.indexOf(".shaft-chat-bubble.assistant"),
                html.indexOf(".shaft-chat-hint"));
        assertFalse(assistantBubbleCss.contains("border:"));
    }

    @Test
    void assistantTranscriptCodeCopyControlDoesNotPaintRectangularOutline() {
        AssistantTranscriptView transcript = new AssistantTranscriptView();
        transcript.append("assistant", """
                ```java
                class AssistantResponse {}
                ```
                """.stripIndent().trim());

        String html = transcript.html();
        String copyControlCss = html.substring(
                html.indexOf(".shaft-code-copy {"),
                html.indexOf(".shaft-code-copy svg"));
        assertAll(
                () -> assertFalse(copyControlCss.contains("border:")),
                () -> assertFalse(copyControlCss.contains("background:")),
                () -> assertFalse(html.contains(";border:1px solid ")));
    }

    @Test
    void assistantTranscriptUsesStandardMarkdownForHeadersRulesAndCodeBlocks() {
        AssistantTranscriptView transcript = new AssistantTranscriptView();
        transcript.append("user", """
                ## User payload

                ---

                ```json
                {"browser":"chrome"}
                ```
                """.stripIndent().trim());
        transcript.append("assistant", """
                ## Agent response

                ```java
                public class LoginTest {
                    void signsIn() {}
                }
                ```

                ```xml
                <suite name="smoke"/>
                ```
                """.stripIndent().trim());

        String html = transcript.html();

        assertAll(
                () -> assertTrue(html.contains("<h2")),
                () -> assertTrue(html.contains("<hr")),
                () -> assertTrue(html.contains("language-json")),
                () -> assertTrue(html.contains("language-java")),
                () -> assertTrue(html.contains("language-xml")),
                () -> assertEquals(3, countOccurrences(html, "class=\"shaft-code-copy\"")),
                () -> assertTrue(html.contains("data-copy-code=\"public class LoginTest")),
                () -> assertTrue(html.contains("href=\"shaft-copy-code:public+class+LoginTest")),
                () -> assertTrue(html.contains("aria-label=\"Copy code\"")),
                () -> assertTrue(html.contains("<svg width=\"16\" height=\"16\"")),
                () -> assertTrue(html.contains("class=\"shaft-code-copy-icon\"")),
                () -> assertTrue(html.contains("&#x2398;")),
                () -> assertTrue(html.contains("class=\"shaft-code-highlighted language-java\""), html),
                () -> assertTrue(html.contains("<span style=\""), html),
                () -> assertFalse(html.contains(">Copy code<")));
    }

    @Test
    void assistantTranscriptCanReplaceLiveAgentOutputWithFinalMessage() {
        AssistantTranscriptView transcript = new AssistantTranscriptView();
        transcript.append("assistant", "Running Codex CLI...");
        transcript.replaceLast("assistant", "Validated DuckDuckGo title.");

        assertAll(
                () -> assertEquals("Validated DuckDuckGo title.", transcript.markdown()),
                () -> assertFalse(transcript.html().contains("Running Codex CLI")),
                () -> assertTrue(transcript.html().contains("Validated DuckDuckGo title.")));
    }

    @Test
    void assistantTranscriptRestoresMessagesFromChatState() {
        ShaftAssistantChatState state = new ShaftAssistantChatState();
        state.append("user", "User prompt", "{}");
        state.append("assistant", "Assistant reply", "{}");

        AssistantTranscriptView transcript = new AssistantTranscriptView();
        transcript.setMessages(state.activeMessages());

        assertEquals("User prompt\n\nAssistant reply", transcript.markdown());
        assertNotNull(transcript.html());
    }

    @Test
    void assistantAgentModeWarnsOnlyWhenSourceMutationIsNotApproved() {
        ShaftAssistantPanel blockedPanel = new ShaftAssistantPanel(null, blankMcpSettings());
        JComboBox<?> blockedMode = findByAccessibleName(blockedPanel, "Assistant mode", JComboBox.class);
        assertNotNull(blockedMode);
        blockedMode.setSelectedItem("AGENT");
        assistantPrompt(blockedPanel).setText("edit this test");
        clickAccessible(blockedPanel, "Send assistant prompt");
        assertTrue(transcriptMarkdown(blockedPanel).contains("tick **Allow source edits**"));

        ShaftAssistantPanel approvedPanel = new ShaftAssistantPanel(null, blankMcpSettings());
        JComboBox<?> approvedMode = findByAccessibleName(approvedPanel, "Assistant mode", JComboBox.class);
        JCheckBox allowEdits = findByAccessibleName(approvedPanel,
                "Approve source mutation for Agent mode", JCheckBox.class);
        assertAll(
                () -> assertNotNull(approvedMode),
                () -> assertNotNull(allowEdits));
        approvedMode.setSelectedItem("AGENT");
        allowEdits.setSelected(true);
        assistantPrompt(approvedPanel).setText("edit this test");
        clickAccessible(approvedPanel, "Send assistant prompt");
        assertFalse(transcriptMarkdown(approvedPanel).contains("tick **Allow source edits**"));
    }

    @Test
    void assistantAgentModeDoesNotWarnForBrowserOnlyTasksWhenSourceEditsNotApproved() {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, blankMcpSettings());
        JComboBox<?> blockedMode = findByAccessibleName(panel, "Assistant mode", JComboBox.class);
        assertNotNull(blockedMode);
        blockedMode.setSelectedItem("AGENT");
        assistantPrompt(panel).setText("open https://example.com and update me on the login form");
        clickAccessible(panel, "Send assistant prompt");

        assertFalse(transcriptMarkdown(panel).contains("tick **Allow source edits**"));
    }

    @Test
    void assistantCopyAllIncludesCurrentSessionToolEvidence() throws Exception {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, connectedMcpSettings());
        showAgentResult(panel, ShaftMcpToolResult.success("tool output payload"));
        String exported = assistantExport(panel);

        assertAll(
                () -> assertTrue(exported.contains("## Tool evidence")),
                () -> assertTrue(exported.contains("autobot_local_agent_run")),
                () -> assertTrue(exported.contains("tool output payload")));
    }

    @Test
    void setupSuccessPreservesExistingAssistantSession() throws Exception {
        ShaftSettingsState.Settings settings = blankMcpSettings();
        settings.mcpCommand = "cmd";
        settings.mcpSetupComplete = false;
        Project project = fakeProject();
        ShaftAssistantChatState state = ShaftAssistantChatState.getInstance(project);
        assertNotNull(state);
        state.newSession();
        state.append("user", "Previous assistant conversation", "{}");
        int beforeSetupSessions = state.sessions().size();

        ShaftToolWindowPanel toolWindow = new ShaftToolWindowPanel(project, settings, readyProbe());
        ShaftMcpSetupPanel setupPanel = setupPanel(toolWindow);
        assertNotNull(setupPanel);
        showTestResult(setupPanel, ShaftMcpToolResult.success("Probe OK"));

        assertAll(
                () -> assertEquals(beforeSetupSessions, state.sessions().size()),
                () -> assertNotNull(state.activeSession()),
                () -> assertTrue(state.activeMarkdown().contains("Previous assistant conversation")),
                () -> assertTrue(transcriptMarkdown(toolWindow).contains("Previous assistant conversation")),
                () -> assertNull(toolWindowWorkflowSelector(toolWindow)));
    }

    @Test
    void guidedWorkflowPrefillsContractAlignedToolArguments() {
        List<CapturedInvocation> invocations = new ArrayList<>();
        GuidedWorkflowPanel panel = new GuidedWorkflowPanel(null,
                (toolName, arguments) -> invocations.add(new CapturedInvocation(toolName, arguments)));

        click(panel, "Inspect locator");
        CapturedInvocation inspect = last(invocations);
        assertAll(
                () -> assertEquals("browser_open_intent", inspect.toolName()),
                () -> assertTrue(inspect.arguments().has("userIntent")),
                () -> assertTrue(inspect.arguments().has("maxCharacters")),
                () -> assertFalse(inspect.arguments().has("intent")));

        click(panel, "Generate code");
        CapturedInvocation codegen = last(invocations);
        assertAll(
                () -> assertEquals("capture_code_blocks", codegen.toolName()),
                () -> assertTrue(codegen.arguments().has("outputDirectory")),
                () -> assertTrue(codegen.arguments().has("packageName")),
                () -> assertTrue(codegen.arguments().has("driverVariableName")));

        JComboBox<?> backend = findComboBox(panel);
        assertNotNull(backend);
        backend.setSelectedItem("Playwright");
        click(panel, "Start recording");
        CapturedInvocation playwright = last(invocations);
        assertAll(
                () -> assertEquals("playwright_record_start", playwright.toolName()),
                () -> assertTrue(playwright.arguments().has("outputPath")),
                () -> assertTrue(playwright.arguments().has("includeSensitiveValues")),
                () -> assertFalse(playwright.arguments().has("targetUrl")));
    }

    @Test
    void evidenceTriagePrefillsDoctorAndHealerContracts() {
        List<CapturedInvocation> invocations = new ArrayList<>();
        EvidenceTriagePanel panel = new EvidenceTriagePanel(null,
                (toolName, arguments) -> invocations.add(new CapturedInvocation(toolName, arguments)));

        click(panel, "Analyze Allure");
        CapturedInvocation analyze = last(invocations);
        assertAll(
                () -> assertEquals("doctor_analyze_failed_allure", analyze.toolName()),
                () -> assertTrue(analyze.arguments().has("repositoryRoot")),
                () -> assertTrue(analyze.arguments().has("historicalBundlePaths")),
                () -> assertTrue(analyze.arguments().has("useAi")),
                () -> assertFalse(analyze.arguments().has("repository")));

        click(panel, "Suggest Fix");
        CapturedInvocation suggest = last(invocations);
        assertAll(
                () -> assertEquals("doctor_suggest_fix", suggest.toolName()),
                () -> assertTrue(suggest.arguments().has("jsonReportPath")),
                () -> assertTrue(suggest.arguments().has("repositoryRoot")),
                () -> assertTrue(suggest.arguments().has("useAi")));

        click(panel, "Run Healer");
        CapturedInvocation healer = last(invocations);
        assertAll(
                () -> assertEquals("healer_run_failed_test", healer.toolName()),
                () -> assertEquals("-Dtest=ExampleTest",
                        healer.arguments().getAsJsonArray("testCommand").get(2).getAsString()));

        click(panel, "Propose Locator");
        CapturedInvocation locator = last(invocations);
        assertAll(
                () -> assertEquals("doctor_propose_healed_locator", locator.toolName()),
                () -> assertTrue(locator.arguments().has("repositoryRoot")),
                () -> assertTrue(locator.arguments().has("healingReportPath")),
                () -> assertTrue(locator.arguments().has("sourcePath")));
    }

    private static ShaftSettingsState.Settings blankMcpSettings() {
        ShaftSettingsState.Settings settings = new ShaftSettingsState.Settings();
        settings.mcpCommand = "";
        return settings;
    }

    private static ShaftSettingsState.Settings connectedMcpSettings() {
        ShaftSettingsState.Settings settings = new ShaftSettingsState.Settings();
        settings.mcpCommand = "\"java\" \"@target/shaft-mcp.args\"";
        settings.mcpSetupComplete = true;
        return settings;
    }

    private static ShaftSettingsState.Settings advancedConnectedMcpSettings() {
        ShaftSettingsState.Settings settings = connectedMcpSettings();
        settings.advancedUiEnabled = true;
        return settings;
    }

    private static ShaftMcpSetupPanel.AgentReadinessProbe readyProbe() {
        return (client, runtime) -> ShaftMcpToolResult.success("Codex CLI executable is available on PATH.");
    }

    private static boolean containsText(Component component, String expected) {
        if (component instanceof JLabel label && label.getText() != null && label.getText().contains(expected)) {
            return true;
        }
        if (component instanceof AbstractButton button && button.getText() != null && button.getText().contains(expected)) {
            return true;
        }
        if (component instanceof JTextComponent textComponent
                && textComponent.getText() != null
                && textComponent.getText().contains(expected)) {
            return true;
        }
        if (component instanceof Container container) {
            for (Component child : container.getComponents()) {
                if (containsText(child, expected)) {
                    return true;
                }
            }
        }
        return false;
    }

    private static void collectTextAreas(Component component, List<JBTextArea> textAreas) {
        if (component instanceof JBTextArea textArea) {
            textAreas.add(textArea);
        }
        if (component instanceof Container container) {
            for (Component child : container.getComponents()) {
                collectTextAreas(child, textAreas);
            }
        }
    }

    private static void showToolResult(ShaftFeaturePanel panel, ShaftMcpToolResult result) throws Exception {
        Method showResult = ShaftFeaturePanel.class.getDeclaredMethod(
                "showResult", ShaftMcpToolResult.class, Throwable.class);
        showResult.setAccessible(true);
        showResult.invoke(panel, result, null);
    }

    private static void showInstallResult(ShaftMcpSetupPanel panel, ShaftMcpInstallResult result) throws Exception {
        Method showResult = ShaftMcpSetupPanel.class.getDeclaredMethod(
                "showInstallResult", ShaftMcpInstallResult.class, Throwable.class);
        showResult.setAccessible(true);
        showResult.invoke(panel, result, null);
    }

    private static void showInstallError(ShaftMcpSetupPanel panel, Throwable error) throws Exception {
        Method showResult = ShaftMcpSetupPanel.class.getDeclaredMethod(
                "showInstallResult", ShaftMcpInstallResult.class, Throwable.class);
        showResult.setAccessible(true);
        showResult.invoke(panel, null, error);
    }

    private static void showTestResult(ShaftMcpSetupPanel panel, ShaftMcpToolResult result) throws Exception {
        Method showResult = ShaftMcpSetupPanel.class.getDeclaredMethod(
                "showTestResult", ShaftMcpToolResult.class, Throwable.class);
        showResult.setAccessible(true);
        showResult.invoke(panel, result, null);
    }

    private static ShaftMcpSetupPanel setupPanel(ShaftToolWindowPanel toolWindow) {
        if (toolWindow == null) {
            return null;
        }
        for (Component component : toolWindow.getComponents()) {
            if (component instanceof ShaftMcpSetupPanel setup) {
                return setup;
            }
        }
        return null;
    }

    private static void showAssistantResult(ShaftAssistantPanel panel, ShaftMcpToolResult result) throws Exception {
        showAssistantResult(panel, "autobot_local_agent_run", result);
    }

    private static void showAssistantResult(ShaftAssistantPanel panel, String toolName, ShaftMcpToolResult result) throws Exception {
        showAssistantResult(panel, toolName, result, null);
    }

    private static void showAssistantResult(
            ShaftAssistantPanel panel,
            String toolName,
            ShaftMcpToolResult result,
            Throwable error) throws Exception {
        Method showResult = ShaftAssistantPanel.class.getDeclaredMethod(
                "showResult", String.class, ShaftMcpToolResult.class, Throwable.class);
        showResult.setAccessible(true);
        showResult.invoke(panel, toolName, result, error);
    }

    private static void showSequenceResult(
            ShaftAssistantPanel panel,
            AssistantCommand.ToolCall toolCall,
            ShaftMcpToolResult result) throws Exception {
        Method showResult = ShaftAssistantPanel.class.getDeclaredMethod(
                "showSequenceResult",
                int.class,
                AssistantCommand.ToolCall.class,
                ShaftMcpToolResult.class,
                Throwable.class);
        showResult.setAccessible(true);
        showResult.invoke(panel, 0, toolCall, result, null);
    }

    private static void showCaptureStartDiagnostic(
            ShaftAssistantPanel panel,
            String expectedOutputPath,
            String startOutput,
            ShaftMcpToolResult result) throws Exception {
        Method showDiagnostic = ShaftAssistantPanel.class.getDeclaredMethod(
                "showCaptureStartDiagnostic",
                String.class,
                String.class,
                ShaftMcpToolResult.class,
                Throwable.class);
        showDiagnostic.setAccessible(true);
        showDiagnostic.invoke(panel, expectedOutputPath, startOutput, result, null);
    }

    private static void showAgentResult(ShaftAssistantPanel panel, ShaftMcpToolResult result) throws Exception {
        Method showResult = ShaftAssistantPanel.class.getDeclaredMethod(
                "showAgentResult", ShaftMcpToolResult.class, Throwable.class);
        showResult.setAccessible(true);
        showResult.invoke(panel, result, null);
    }

    private static void showAgentResult(ShaftAssistantPanel panel, int streamToken, ShaftMcpToolResult result)
            throws Exception {
        Method showResult = ShaftAssistantPanel.class.getDeclaredMethod(
                "showAgentResult", int.class, ShaftMcpToolResult.class, Throwable.class);
        showResult.setAccessible(true);
        showResult.invoke(panel, streamToken, result, null);
    }

    private static void setField(Object target, String name, Object value) throws Exception {
        Field field = target.getClass().getDeclaredField(name);
        field.setAccessible(true);
        field.set(target, value);
    }

    private static Object getField(Object target, String name) throws Exception {
        Field field = target.getClass().getDeclaredField(name);
        field.setAccessible(true);
        return field.get(target);
    }


    @SuppressWarnings({"unchecked", "rawtypes"})
    private static Object recordingBackend(String name) throws ClassNotFoundException {
        return Enum.valueOf((Class<Enum>) (Class) Class.forName(
                "com.shaft.intellij.ui.ShaftAssistantPanel$RecordingBackend"), name);
    }

    private static String assistantExport(ShaftAssistantPanel panel) throws Exception {
        Method method = ShaftAssistantPanel.class.getDeclaredMethod("exportTranscriptWithEvidence");
        method.setAccessible(true);
        return (String) method.invoke(panel);
    }

    private static int countOccurrences(String value, String needle) {
        int count = 0;
        int offset = 0;
        while ((offset = value.indexOf(needle, offset)) >= 0) {
            count++;
            offset += needle.length();
        }
        return count;
    }

    private static String mcpText(String text) {
        JsonObject item = new JsonObject();
        item.addProperty("type", "text");
        item.addProperty("text", text);
        JsonArray content = new JsonArray();
        content.add(item);
        JsonObject result = new JsonObject();
        result.add("content", content);
        result.addProperty("isError", false);
        return result.toString();
    }

    private static String outputText(Component component) {
        if (component instanceof JBTextArea textArea && !textArea.isEditable()) {
            return textArea.getText();
        }
        if (component instanceof Container container) {
            for (Component child : container.getComponents()) {
                String found = outputText(child);
                if (!found.isEmpty()) {
                    return found;
                }
            }
        }
        return "";
    }

    private static String transcriptMarkdown(Component component) {
        if (component instanceof AssistantTranscriptView view) {
            return view.markdown();
        }
        if (component instanceof Container container) {
            for (Component child : container.getComponents()) {
                String found = transcriptMarkdown(child);
                if (!found.isEmpty()) {
                    return found;
                }
            }
        }
        return "";
    }

    private static String selectedCategory(ShaftToolWindowPanel toolWindow) {
        Component selectedComponent = selectedWorkflowComponent(toolWindow);
        if (selectedComponent instanceof Container container) {
            JComboBox<?> categorySelector = findCategorySelector(container);
            if (categorySelector != null && categorySelector.getSelectedItem() != null) {
                return categorySelector.getSelectedItem().toString();
            }
        }
        return "";
    }

    private static String selectedWorkflow(ShaftToolWindowPanel toolWindow) {
        JComboBox<ShaftToolWindowPanel.WorkflowView> selector = toolWindowWorkflowSelector(toolWindow);
        Object selected = selector == null ? null : selector.getSelectedItem();
        if (!(selected instanceof ShaftToolWindowPanel.WorkflowView view)) {
            return "";
        }
        return view.label();
    }

    private static Component selectedWorkflowComponent(ShaftToolWindowPanel toolWindow) {
        JComboBox<ShaftToolWindowPanel.WorkflowView> selector = toolWindowWorkflowSelector(toolWindow);
        Object selected = selector == null ? null : selector.getSelectedItem();
        if (!(selected instanceof ShaftToolWindowPanel.WorkflowView view)) {
            return null;
        }
        return view.component();
    }

    private static JComboBox<ShaftToolWindowPanel.WorkflowView> toolWindowWorkflowSelector(ShaftToolWindowPanel toolWindow) {
        return toolWindow.workflowSelector();
    }

    private static JComboBox<?> findCategorySelector(Component component) {
        if (component instanceof ShaftFeaturePanel featurePanel) {
            return featurePanel.categorySelector();
        }
        if (component instanceof JComboBox<?> comboBox
                && comboBox.getItemCount() > 0
                && comboBox.getItemAt(0) instanceof ToolCategory) {
            return comboBox;
        }
        if (component instanceof Container container) {
            for (Component child : container.getComponents()) {
                JComboBox<?> found = findCategorySelector(child);
                if (found != null) {
                    return found;
                }
            }
        }
        return null;
    }

    private static JComboBox<?> findComboBox(Component component) {
        if (component instanceof JComboBox<?> comboBox) {
            return comboBox;
        }
        if (component instanceof Container container) {
            for (Component child : container.getComponents()) {
                JComboBox<?> found = findComboBox(child);
                if (found != null) {
                    return found;
                }
            }
        }
        return null;
    }

    private static void click(Component component, String text) {
        JButton button = findButton(component, text);
        assertNotNull(button, text);
        button.doClick();
    }

    private static void clickAccessible(Component component, String accessibleName) {
        JButton button = findByAccessibleName(component, accessibleName, JButton.class);
        assertNotNull(button, accessibleName);
        button.doClick();
    }

    private static void assertIcon(JButton button) {
        assertNotNull(button);
        assertNotNull(button.getIcon(), button.getText());
        assertTrue(button.getIcon().getIconWidth() > 0, button.getText());
        assertTrue(button.getIcon().getIconHeight() > 0, button.getText());
        assertTrue(paintsVisiblePixels(button.getIcon(), button), button.getToolTipText());
    }

    private static boolean paintsVisiblePixels(Icon icon, JButton button) {
        BufferedImage image = new BufferedImage(icon.getIconWidth(), icon.getIconHeight(), BufferedImage.TYPE_INT_ARGB);
        Graphics2D graphics = image.createGraphics();
        try {
            icon.paintIcon(button, graphics, 0, 0);
        } finally {
            graphics.dispose();
        }
        for (int y = 0; y < image.getHeight(); y++) {
            for (int x = 0; x < image.getWidth(); x++) {
                if ((image.getRGB(x, y) >>> 24) > 0) {
                    return true;
                }
            }
        }
        return false;
    }

    private static void assertIconOnlySymmetric(JButton button) {
        assertIcon(button);
        Dimension size = button.getPreferredSize();
        assertAll(
                () -> assertEquals("", button.getText()),
                () -> assertEquals(size.width, size.height),
                () -> assertEquals(32, size.width),
                () -> assertNotNull(button.getToolTipText()),
                () -> assertFalse(button.getToolTipText().isBlank()),
                () -> assertNotNull(accessibleName(button)),
                () -> assertFalse(accessibleName(button).isBlank()));
    }

    private static List<JButton> collectButtons(Component root) {
        List<JButton> buttons = new ArrayList<>();
        if (root instanceof JButton button && isShaftOwnedButton(button)) {
            buttons.add(button);
        }
        if (root instanceof Container container) {
            for (Component child : container.getComponents()) {
                buttons.addAll(collectButtons(child));
            }
        }
        return buttons;
    }

    private static boolean isShaftOwnedButton(JButton button) {
        if ("Send assistant prompt".equals(accessibleName(button))) {
            return false;
        }
        return hasText(button.getText())
                || hasText(button.getToolTipText())
                || hasText(accessibleName(button));
    }

    private static boolean hasText(String text) {
        return text != null && !text.isBlank();
    }

    private static void notifyShaftMouseListener(JButton button, int eventId) {
        MouseEvent event = new MouseEvent(button, eventId, System.currentTimeMillis(), 0, 1, 1, 0, false);
        for (MouseListener listener : button.getMouseListeners()) {
            if (listener.getClass().getName().startsWith(ShaftAssistantPanel.class.getName())) {
                if (eventId == MouseEvent.MOUSE_ENTERED) {
                    listener.mouseEntered(event);
                } else if (eventId == MouseEvent.MOUSE_EXITED) {
                    listener.mouseExited(event);
                }
            }
        }
    }

    private static JButton findButton(Component component, String text) {
        if (component instanceof JButton button
                && (text.equals(button.getText())
                || text.equals(button.getToolTipText())
                || text.equals(accessibleName(button)))) {
            return button;
        }
        if (component instanceof Container container) {
            for (Component child : container.getComponents()) {
                JButton found = findButton(child, text);
                if (found != null) {
                    return found;
                }
            }
        }
        return null;
    }

    private static <T extends JComponent> T findByAccessibleName(
            Component component,
            String accessibleName,
            Class<T> type) {
        if (type.isInstance(component)
                && accessibleName.equals(accessibleName((JComponent) component))) {
            return type.cast(component);
        }
        if (component instanceof Container container) {
            for (Component child : container.getComponents()) {
                T found = findByAccessibleName(child, accessibleName, type);
                if (found != null) {
                    return found;
                }
            }
        }
        return null;
    }

    private static boolean comboContains(JComboBox<?> comboBox, String expectedText) {
        if (comboBox == null) {
            return false;
        }
        for (int index = 0; index < comboBox.getItemCount(); index++) {
            Object item = comboBox.getItemAt(index);
            if (item != null && item.toString().contains(expectedText)) {
                return true;
            }
        }
        return false;
    }

    private static JTextComponent assistantPrompt(Component component) {
        JTextComponent prompt = textComponent(component, "Assistant prompt");
        assertNotNull(prompt);
        return prompt;
    }

    private static JTextComponent textComponent(Component component, String accessibleName) {
        if (component instanceof JTextComponent textComponent && accessibleName.equals(accessibleName(textComponent))) {
            return textComponent;
        }
        if (component instanceof Container container) {
            for (Component child : container.getComponents()) {
                JTextComponent found = textComponent(child, accessibleName);
                if (found != null) {
                    return found;
                }
            }
        }
        return null;
    }

    private static CapturedInvocation last(List<CapturedInvocation> invocations) {
        assertFalse(invocations.isEmpty());
        return invocations.get(invocations.size() - 1);
    }

    private static void assertAccessibleControls(Component component) {
        List<String> missing = new ArrayList<>();
        collectAccessibilityViolations(component, missing);
        assertTrue(missing.isEmpty(), "Missing accessible names: " + missing);
    }

    private static void collectAccessibilityViolations(Component component, List<String> missing) {
        if (needsAccessibleName(component)) {
            String accessibleName = accessibleName((JComponent) component);
            if (accessibleName == null || accessibleName.isBlank()) {
                missing.add(component.getClass().getSimpleName());
            }
        }
        if (component instanceof Container container) {
            for (Component child : container.getComponents()) {
                collectAccessibilityViolations(child, missing);
            }
        }
    }

    private static boolean needsAccessibleName(Component component) {
        return component instanceof JButton button
                && button.getText() != null
                && !button.getText().isBlank()
                && !button.getClass().getSimpleName().equals("MetalComboBoxButton")
                || component instanceof JComboBox<?>
                || component instanceof JTextComponent;
    }

    private static String accessibleName(JComponent component) {
        AccessibleContext context = component.getAccessibleContext();
        return context == null ? null : context.getAccessibleName();
    }

    private static Project fakeProject() {
        return fakeProject(new ShaftAssistantChatState());
    }

    private static Project fakeProject(ShaftAssistantChatState assistantChatState) {
        return (Project) Proxy.newProxyInstance(Project.class.getClassLoader(), new Class<?>[]{Project.class},
                (proxy, method, arguments) -> {
                    switch (method.getName()) {
                        case "equals":
                            Object other = arguments == null || arguments.length == 0 ? null : arguments[0];
                            return proxy == other;
                        case "hashCode":
                            return System.identityHashCode(proxy);
                        case "getBasePath":
                            return "";
                        case "getName":
                            return "SHAFT test project";
                        case "getService":
                            Class<?> type = arguments == null || arguments.length == 0 ? null : (Class<?>) arguments[0];
                            if (type == ShaftAssistantChatState.class) {
                                return assistantChatState;
                            }
                            return null;
                        default:
                            return defaultValue(method.getReturnType());
                    }
                });
    }

    private static Object defaultValue(Class<?> returnType) {
        if (returnType == boolean.class) {
            return false;
        } else if (returnType == byte.class) {
            return 0;
        } else if (returnType == short.class) {
            return (short) 0;
        } else if (returnType == int.class) {
            return 0;
        } else if (returnType == long.class) {
            return 0L;
        } else if (returnType == float.class) {
            return 0.0F;
        } else if (returnType == double.class) {
            return 0.0D;
        } else if (returnType == char.class) {
            return '\0';
        } else {
            return null;
        }
    }

    private record CapturedInvocation(String toolName, JsonObject arguments) {
    }
}
