package com.shaft.intellij.ui;

import com.google.gson.JsonObject;
import com.google.gson.JsonArray;
import com.google.gson.JsonParser;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.ui.components.JBTextArea;
import com.intellij.openapi.project.Project;
import com.shaft.intellij.mcp.ShaftMcpInvocation;
import com.shaft.intellij.mcp.ShaftMcpToolResult;
import com.shaft.intellij.settings.ShaftSettingsState;
import org.junit.jupiter.api.Test;

import javax.accessibility.AccessibleContext;
import javax.swing.Action;
import javax.swing.AbstractButton;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JEditorPane;
import javax.swing.Icon;
import javax.swing.JLabel;
import javax.swing.KeyStroke;
import javax.swing.JList;
import javax.swing.ListCellRenderer;
import javax.swing.JProgressBar;
import javax.swing.JScrollPane;
import javax.swing.JViewport;
import javax.swing.SwingUtilities;
import javax.swing.UIManager;
import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.Container;
import java.awt.Color;
import java.awt.Cursor;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Graphics2D;
import java.awt.event.ActionEvent;
import java.awt.event.InputEvent;
import java.awt.event.KeyEvent;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.image.BufferedImage;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.lang.reflect.Proxy;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.CancellationException;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Consumer;
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
    void toolWindowShowsFirstRunSetupUntilMcpConnectionIsComplete() throws Exception {
        ShaftToolWindowPanel toolWindow = new ShaftToolWindowPanel(fakeProject(), blankMcpSettings());
        JComponent setupOutput = findByAccessibleName(toolWindow, "SHAFT MCP setup output", JComponent.class);
        JLabel nextStep = findByAccessibleName(toolWindow, "SHAFT MCP setup next step", JLabel.class);
        ShaftMcpSetupPanel setupPanel = setupPanel(toolWindow);
        JTextComponent mcpCommand = (JTextComponent) getField(setupPanel, "mcpCommand");
        JComponent detailsPanel = (JComponent) getField(setupPanel, "detailsPanel");
        JComponent installerDetailsPanel = (JComponent) getField(setupPanel, "installerDetailsPanel");

        assertNull(toolWindowWorkflowSelector(toolWindow));
        assertTrue(containsText(toolWindow, "Runtime"));
        assertTrue(containsText(toolWindow, "1 Pick agent"));
        assertTrue(containsText(toolWindow, "2 Copy command"));
        assertTrue(containsText(toolWindow, "3 Run in terminal"));
        assertTrue(containsText(toolWindow, "4 Check setup"));
        assertTrue(containsText(toolWindow, "Connect SHAFT Assistant"));
        assertTrue(containsText(toolWindow, "Installer source: main"));
        assertNotNull(findByAccessibleName(toolWindow, "Copy command setup state", JLabel.class));
        assertNull(findByAccessibleName(toolWindow, "SHAFT MCP command status", JLabel.class));
        assertFalse(findByAccessibleName(toolWindow, "Assistant runtime setup status", JLabel.class).isVisible());
        assertFalse(findByAccessibleName(toolWindow, "Assistant connection setup status", JLabel.class).isVisible());
        assertNotNull(findByAccessibleName(toolWindow, "Assistant runtime setup status", JLabel.class));
        assertNotNull(findByAccessibleName(toolWindow, "Assistant connection setup status", JLabel.class));
        assertNotNull(nextStep);
        assertNotNull(mcpCommand);
        assertNotNull(setupOutput);
        assertNull(findByAccessibleName(toolWindow, "MCP stdio command", JTextComponent.class));
        assertNotNull(findByAccessibleName(toolWindow, "Show manual MCP install target", JCheckBox.class));
        assertNull(findByAccessibleName(toolWindow, "Install or update SHAFT MCP", JButton.class));
        assertTrue(mcpCommand instanceof JBTextArea);
        assertTrue(((JBTextArea) mcpCommand).getRows() >= 4);
        assertFalse(nextStep.isVisible());
        assertFalse(installerDetailsPanel.isVisible());
        assertFalse(detailsPanel.isVisible());
        assertTrue(findByAccessibleName(toolWindow, "Copy MCP installer command", JButton.class).isVisible());
        assertFalse(findByAccessibleName(toolWindow, "Open terminal for MCP installer", JButton.class).isVisible());
        assertFalse(findByAccessibleName(toolWindow, "Test SHAFT MCP connection", JButton.class).isVisible());
    }

    @Test
    void toolWindowRedirectsToAssistantWhenMcpIsConfigured() {
        ShaftToolWindowPanel toolWindow = new ShaftToolWindowPanel(fakeProject(), connectedMcpSettings());

        assertAll(
                () -> assertNull(setupPanel(toolWindow)),
                () -> assertNull(toolWindowWorkflowSelector(toolWindow)),
                () -> assertNotNull(findByAccessibleName(toolWindow, "Assistant prompt", JTextComponent.class)),
                () -> assertTrue(containsText(toolWindow, "Codex CLI")));
    }

    @Test
    void unverifiedMcpCommandKeepsSetupAndToolsGated() {
        ShaftSettingsState.Settings settings = unverifiedMcpSettings();
        ShaftToolWindowPanel toolWindow = new ShaftToolWindowPanel(fakeProject(), settings);
        ShaftFeaturePanel tools = new ShaftFeaturePanel(fakeProject(), settings);

        clickAccessible(tools, "Run SHAFT tool");

        assertAll(
                () -> assertNull(toolWindowWorkflowSelector(toolWindow)),
                () -> assertTrue(containsText(toolWindow, "Connect SHAFT Assistant")),
                () -> assertTrue(containsText(tools, "Configure SHAFT MCP")),
                () -> assertTrue(outputText(tools).contains("Configure SHAFT MCP in Settings before running Tools requests.")));
    }

    @Test
    void setupPanelPrefillsManualCommandAndKeepsTestEnabledForSelectionChanges() throws Exception {
        ShaftSettingsState.Settings settings = blankMcpSettings();
        settings.mcpCommand = "\"java\" \"@target/shaft-mcp.args\"";
        settings.mcpSetupComplete = false;
        ShaftMcpSetupPanel panel = new ShaftMcpSetupPanel(fakeProject(), settings, () -> {
        });
        JTextComponent command = (JTextComponent) getField(panel, "mcpCommand");
        JButton test = findButton(panel, "Check now");
        JComboBox<?> family = findByAccessibleName(panel, "Assistant family", JComboBox.class);
        JComboBox<?> runtime = findByAccessibleName(panel, "Assistant runtime", JComboBox.class);

        assertAll(
                () -> assertNotNull(command),
                () -> assertNotNull(test),
                () -> assertNotNull(family),
                () -> assertNotNull(runtime),
                () -> assertEquals("\"java\" \"@target/shaft-mcp.args\"", command.getText()),
                () -> assertNull(findButton(panel, "Install / Update SHAFT MCP")),
                () -> assertFalse(containsText(panel, "MCP: Configured")),
                () -> assertFalse(findByAccessibleName(panel, "Assistant runtime setup status", JLabel.class).isVisible()),
                () -> assertFalse(findByAccessibleName(panel, "Assistant connection setup status", JLabel.class).isVisible()),
                () -> assertTrue(test.isVisible()),
                () -> assertTrue(test.isEnabled()));

        family.setSelectedItem("CLAUDE");
        assertAll(
                () -> assertTrue(test.isEnabled()),
                () -> assertFalse(containsText(panel, "MCP: Configured")));

        runtime.setSelectedItem("DESKTOP_APP");
        assertAll(
                () -> assertTrue(test.isEnabled()),
                () -> assertFalse(containsText(panel, "MCP: Configured")));
    }

    @Test
    void setupPanelShowsInstallerCommandAndInfersInstalledStdioCommand() throws Exception {
        Path appData = Files.createTempDirectory("shaft-mcp-app-data");
        Path bootstrap = Files.createTempDirectory("shaft-mcp-bootstrap");
        Path argsFile = appData.resolve("versions").resolve("10.3.20260703").resolve("shaft-mcp.args");
        Path java = bootstrap.resolve("tools").resolve("jdk").resolve("temurin-25-test").resolve("bin")
                .resolve(javaExecutableName());
        Files.createDirectories(argsFile.getParent());
        Files.writeString(argsFile, "-cp\nshaft-mcp.jar\ncom.shaft.mcp.ShaftMcpApplication\n");
        Files.createDirectories(java.getParent());
        Files.writeString(java, "");
        String oldAppData = System.getProperty("shaft.intellij.mcp.applicationDataRoot");
        String oldBootstrap = System.getProperty("shaft.intellij.mcp.bootstrapRoot");
        System.setProperty("shaft.intellij.mcp.applicationDataRoot", appData.toString());
        System.setProperty("shaft.intellij.mcp.bootstrapRoot", bootstrap.toString());
        try {
            ShaftMcpSetupPanel panel = new ShaftMcpSetupPanel(fakeProject(), blankMcpSettings(), () -> {
            });
            AtomicReference<String> copied = new AtomicReference<>();
            AtomicReference<String> toast = new AtomicReference<>();
            setField(panel, "copySink", (Consumer<String>) copied::set);
            setField(panel, "toastSink", (Consumer<String>) toast::set);
            JTextComponent command = (JTextComponent) getField(panel, "mcpCommand");
            JComponent installerDetailsPanel = (JComponent) getField(panel, "installerDetailsPanel");

            assertAll(
                    () -> assertTrue(findByAccessibleName(panel, "MCP installer command", JTextComponent.class)
                            instanceof JBTextArea),
                    () -> assertTrue(command instanceof JBTextArea),
                    () -> assertTrue(((JBTextArea) command).getRows() >= 4),
                    () -> assertFalse(findByAccessibleName(panel, "MCP installer target", JComboBox.class).isVisible()),
                    () -> assertNotNull(findByAccessibleName(panel, "Show manual MCP install target", JCheckBox.class)),
                    () -> assertNotNull(findByAccessibleName(panel, "Copy MCP installer command", JButton.class)),
                    () -> assertNotNull(findByAccessibleName(panel, "Open terminal for MCP installer", JButton.class)),
                    () -> assertNull(findByAccessibleName(panel, "Use inferred MCP command", JButton.class)),
                    () -> assertFalse(installerDetailsPanel.isVisible()),
                    () -> assertTrue(command.getText().isBlank()),
                    () -> assertNull(findButton(panel, "Install / Update SHAFT MCP")));

            clickAccessible(panel, "Copy MCP installer command");
            assertSingleVisiblePrimarySetupAction(panel, "Open terminal for MCP installer");
            assertAll(
                    () -> assertTrue(copied.get().contains("install-shaft-mcp")),
                    () -> assertTrue(copied.get().contains("codex")),
                    () -> assertTrue(copied.get().contains("--install-shaft-skills")),
                    () -> assertTrue(copied.get().contains("/main/scripts/mcp/install-shaft-mcp")),
                    () -> assertFalse(copied.get().contains("SHAFT_MCP_INSTALLER_REF")),
                    () -> assertEquals("Command copied to clipboard", toast.get()),
                    () -> assertFalse(findByAccessibleName(panel, "Copy MCP installer command", JButton.class).isVisible()),
                    () -> assertTrue(findByAccessibleName(panel, "Open terminal for MCP installer", JButton.class).isVisible()),
                    () -> assertFalse(installerDetailsPanel.isVisible()),
                    () -> assertFalse(findByAccessibleName(panel, "Test SHAFT MCP connection", JButton.class).isVisible()));
            if (isWindowsOs()) {
                assertAll(
                        () -> assertTrue(copied.get().contains("-Command '$installer=Join-Path")),
                        () -> assertFalse(copied.get().contains("-Command \"$installer")));
            } else {
                assertTrue(copied.get().contains("sh \"$tmp\" --codex"));
            }

            clickAccessible(panel, "Open terminal for MCP installer");
            String inferred = ShaftMcpSetupPanel.inferInstalledStdioCommand(appData, bootstrap);
            assertSingleVisiblePrimarySetupAction(panel, "Test SHAFT MCP connection");

            assertAll(
                    () -> assertFalse(findByAccessibleName(panel, "Copy MCP installer command", JButton.class).isVisible()),
                    () -> assertFalse(findByAccessibleName(panel, "Open terminal for MCP installer", JButton.class).isVisible()),
                    () -> assertTrue(findByAccessibleName(panel, "Test SHAFT MCP connection", JButton.class).isVisible()),
                    () -> assertFalse(installerDetailsPanel.isVisible()),
                    () -> assertTrue(inferred.contains(java.toString())),
                    () -> assertTrue(inferred.contains("@" + argsFile)));
        } finally {
            restoreProperty("shaft.intellij.mcp.applicationDataRoot", oldAppData);
            restoreProperty("shaft.intellij.mcp.bootstrapRoot", oldBootstrap);
        }
    }

    @Test
    void setupPanelUpdatesInstallerCommandForSelectedAssistantClient() {
        ShaftMcpSetupPanel panel = new ShaftMcpSetupPanel(fakeProject(), blankMcpSettings(), () -> {
        });
        JTextComponent installer = findByAccessibleName(panel, "MCP installer command", JTextComponent.class);
        JComboBox<?> family = findByAccessibleName(panel, "Assistant family", JComboBox.class);
        JComboBox<?> runtime = findByAccessibleName(panel, "Assistant runtime", JComboBox.class);
        JComboBox<?> target = findByAccessibleName(panel, "MCP installer target", JComboBox.class);
        JCheckBox manualTarget = findByAccessibleName(panel, "Show manual MCP install target", JCheckBox.class);

        assertAll(
                () -> assertNotNull(manualTarget),
                () -> assertFalse(target.isVisible()),
                () -> assertEquals("CODEX", target.getSelectedItem()),
                () -> assertTrue(installer.getText().contains("codex")),
                () -> assertTrue(installer.getText().contains("--install-shaft-skills")));

        family.setSelectedItem("CLAUDE");
        runtime.setSelectedItem("DESKTOP_APP");
        assertAll(
                () -> assertEquals("CLAUDE_DESKTOP", target.getSelectedItem()),
                () -> assertTrue(installer.getText().contains("claude-desktop")));

        family.setSelectedItem("COPILOT");
        runtime.setSelectedItem("IDE_PLUGIN");
        assertAll(
                () -> assertEquals("COPILOT_INTELLIJ", target.getSelectedItem()),
                () -> assertTrue(installer.getText().contains("copilot-intellij")));

        manualTarget.doClick();
        target.setSelectedItem("INTELLIJ_PLUGIN");
        assertTrue(installer.getText().contains("intellij-plugin"));
    }

    @Test
    void setupPanelShowsDetectedRecommendedCliAgentWithoutChangingDefaultSelection() {
        ShaftMcpSetupPanel panel = new ShaftMcpSetupPanel(fakeProject(), blankMcpSettings(), () -> {
        }, (client, runtime) -> "CLAUDE_CODE".equals(client)
                ? ShaftMcpToolResult.success("Claude Code CLI executable is available on PATH.")
                : ShaftMcpToolResult.failure("not found"));
        JComboBox<?> family = findByAccessibleName(panel, "Assistant family", JComboBox.class);
        JComboBox<?> target = findByAccessibleName(panel, "MCP installer target", JComboBox.class);

        assertAll(
                () -> assertEquals("CODEX", family.getSelectedItem()),
                () -> assertEquals("CODEX", target.getSelectedItem()),
                () -> assertTrue(containsText(panel, "Recommended: Claude Code CLI detected")));
    }

    @Test
    void setupPanelShowsBlankManualCommandDiagnostic() throws Exception {
        Path appData = Files.createTempDirectory("shaft-mcp-empty-app-data");
        Path bootstrap = Files.createTempDirectory("shaft-mcp-empty-bootstrap");
        String oldAppData = System.getProperty("shaft.intellij.mcp.applicationDataRoot");
        String oldBootstrap = System.getProperty("shaft.intellij.mcp.bootstrapRoot");
        System.setProperty("shaft.intellij.mcp.applicationDataRoot", appData.toString());
        System.setProperty("shaft.intellij.mcp.bootstrapRoot", bootstrap.toString());
        try {
            ShaftMcpSetupPanel panel = new ShaftMcpSetupPanel(fakeProject(), blankMcpSettings(), () -> {
            });
            AtomicReference<String> copied = new AtomicReference<>();
            setField(panel, "copySink", (Consumer<String>) copied::set);
            JComponent detailsPanel = (JComponent) getField(panel, "detailsPanel");

            clickAccessible(panel, "Copy MCP installer command");
            clickAccessible(panel, "Open terminal for MCP installer");
            clickAccessible(panel, "Test SHAFT MCP connection");

            assertAll(
                    () -> assertTrue(containsText(panel, "Assist: Error")),
                    () -> assertTrue(containsText(panel, "Probe failed")),
                    () -> assertTrue(containsText(panel, "No SHAFT MCP command configured.")),
                    () -> assertTrue(containsText(panel,
                            "Run the installer command, then check again. SHAFT will find the local command automatically.")),
                    () -> assertTrue(containsText(panel, "Managed by SHAFT automatically.")),
                    () -> assertTrue(detailsPanel.isVisible()),
                    () -> assertFalse(findByAccessibleName(panel, "Copy setup diagnostic command", JButton.class).isVisible()),
                    () -> assertTrue(findByAccessibleName(panel, "Copy setup diagnostic output", JButton.class).isEnabled()),
                    () -> assertTrue(findByAccessibleName(panel, "Copy SHAFT MCP docs link", JButton.class).isEnabled()),
                    () -> assertTrue(containsText(panel, "Recovery: retry Check setup")),
                    () -> assertNull(findButton(panel, "Install / Update SHAFT MCP")),
                    () -> assertTrue(findByAccessibleName(panel, "Test SHAFT MCP connection", JButton.class).isEnabled()));
            clickAccessible(panel, "Copy setup diagnostic output");
            assertTrue(copied.get().contains("No SHAFT MCP command configured."));
        } finally {
            restoreProperty("shaft.intellij.mcp.applicationDataRoot", oldAppData);
            restoreProperty("shaft.intellij.mcp.bootstrapRoot", oldBootstrap);
        }
    }

    @Test
    void setupPanelShowsClearConnectionSuccess() throws Exception {
        AtomicBoolean connected = new AtomicBoolean();
        ShaftSettingsState.Settings settings = unverifiedMcpSettings();
        ShaftMcpSetupPanel panel = new ShaftMcpSetupPanel(fakeProject(), settings, () -> connected.set(true), readyProbe());
        JComponent detailsPanel = (JComponent) getField(panel, "detailsPanel");

        showTestResult(panel, ShaftMcpToolResult.success("Probe OK\nMCP workspace: C:/work/shaft"));

        assertAll(
                () -> assertTrue(containsText(panel, "Runtime: Codex CLI verified")),
                () -> assertTrue(containsText(panel, "Ready to chat. Verified Codex CLI for workspace C:/work/shaft.")),
                () -> assertFalse(detailsPanel.isVisible()),
                () -> assertNull(findByAccessibleName(panel, "MCP stdio command", JTextComponent.class)),
                () -> assertFalse(connected.get()),
                () -> assertTrue(findByAccessibleName(panel, "Start chatting with SHAFT Assistant", JButton.class).isVisible()),
                () -> assertFalse(findByAccessibleName(panel, "Test SHAFT MCP connection", JButton.class).isVisible()),
                () -> assertTrue(settings.mcpSetupComplete),
                () -> assertTrue(settings.agentGuidanceOptimizationPromptPending));
        clickAccessible(panel, "Start chatting with SHAFT Assistant");
        assertTrue(connected.get());
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
                () -> assertTrue(containsText(panel, "Client readiness failed")),
                () -> assertTrue(containsText(panel, "Category: Client runtime")),
                () -> assertTrue(containsText(panel, "Install the selected client CLI or add it to PATH")),
                () -> assertTrue(containsText(panel, "codex --version")),
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
    void setupPanelShowsInlineProbeErrorStatesAndKeepsRetryEnabled() throws Exception {
        ShaftSettingsState.Settings copilotSettings = connectedMcpSettings();
        copilotSettings.assistantFamily = "COPILOT";
        copilotSettings.assistantRuntime = "IDE_PLUGIN";
        ShaftMcpSetupPanel testPanel = new ShaftMcpSetupPanel(fakeProject(), copilotSettings, () -> {
        });
        AtomicReference<String> copied = new AtomicReference<>();
        setField(testPanel, "copySink", (Consumer<String>) copied::set);

        showTestResult(testPanel, ShaftMcpToolResult.failure(
                "Could not resolve artifact io.github.shafthq:shaft-mcp:jar:1.0.0"));

        assertAll(
                () -> assertTrue(containsText(testPanel, "Assist: Error")),
                () -> assertTrue(containsText(testPanel, "Probe failed")),
                () -> assertTrue(containsText(testPanel, "Category: Maven artifact resolution")),
                () -> assertTrue(containsText(testPanel, "Client: GitHub Copilot")),
                () -> assertTrue(containsText(testPanel, "For GitHub Copilot, check the Copilot MCP client configuration")),
                () -> assertTrue(containsText(testPanel, "Diagnostic command")),
                () -> assertTrue(containsText(testPanel, "Managed by SHAFT automatically.")),
                () -> assertFalse(containsText(testPanel, "\"java\" \"@target/shaft-mcp.args\"")),
                () -> assertFalse(findByAccessibleName(testPanel, "Copy setup diagnostic command", JButton.class).isVisible()),
                () -> assertTrue(findByAccessibleName(testPanel, "Copy setup diagnostic output", JButton.class).isEnabled()),
                () -> assertTrue(findByAccessibleName(testPanel, "Test SHAFT MCP connection", JButton.class).isEnabled()));
        clickAccessible(testPanel, "Copy setup diagnostic output");
        assertTrue(copied.get().contains("Category: Maven artifact resolution"));
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

        toolWindow.prefillTool("capture_record_at_target_code_blocks", arguments);
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
    void assistantLocalAgentResponsesMentionEstimatedTokenUsage() throws Exception {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, connectedMcpSettings());

        showAgentResult(panel, ShaftMcpToolResult.success("Opened DuckDuckGo and validated the SHAFT result."));

        String markdown = transcriptMarkdown(panel);
        assertTrue(markdown.matches("(?s).*\\*\\*Tokens consumed:\\*\\* `\\d+` \\(estimated\\).*"), markdown);
    }

    @Test
    void assistantStreamingLocalAgentFinalResponseMentionsEstimatedTokenUsage() throws Exception {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, connectedMcpSettings());

        appendStreamingLocalAgentBubble(panel, 7);
        showAgentResult(panel, 7, ShaftMcpToolResult.success("Final answer from Codex."));

        String markdown = transcriptMarkdown(panel);
        assertAll(
                () -> assertTrue(markdown.contains("Final answer from Codex.")),
                () -> assertTrue(markdown.matches("(?s).*\\*\\*Tokens consumed:\\*\\* `\\d+` \\(estimated\\).*"),
                        markdown),
                () -> assertFalse(markdown.contains("Running local assistant")));
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
                () -> assertEquals("Try asking me to do something...", status.getText()),
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
    void assistantRestoresProjectServiceChat() {
        ShaftAssistantChatState storedState = new ShaftAssistantChatState();
        storedState.append("user", "persisted prompt", "{}");
        storedState.append("assistant", "persisted answer", "{}");

        ShaftAssistantPanel panel = new ShaftAssistantPanel(fakeProject(storedState), blankMcpSettings());
        JComboBox<?> chats = findByAccessibleName(panel, "Assistant chat", JComboBox.class);
        JButton rerun = findByAccessibleName(panel, "Rerun last assistant prompt", JButton.class);

        assertAll(
                () -> assertTrue(transcriptMarkdown(panel).contains("persisted prompt")),
                () -> assertTrue(transcriptMarkdown(panel).contains("persisted answer")),
                () -> assertNotNull(chats),
                () -> assertEquals(1, chats.getItemCount()),
                () -> assertTrue(rerun.isVisible()),
                () -> assertTrue(rerun.isEnabled()));
    }

    @Test
    void assistantKeepsCurrentSessionHistoryInDropdown() {
        ShaftAssistantChatState chatState = new ShaftAssistantChatState();
        chatState.append("user", "first in-memory chat", "{}");
        chatState.append("assistant", "first response", "{}");
        chatState.newSession();
        chatState.append("user", "second in-memory chat", "{}");
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, blankMcpSettings(), chatState);

        JComboBox<?> chats = findByAccessibleName(panel, "Assistant chat", JComboBox.class);
        assertNotNull(chats);
        chats.setSelectedIndex(1);

        assertAll(
                () -> assertEquals(2, chatState.sessions().size()),
                () -> assertTrue(comboContains(chats, "first in-memory chat")),
                () -> assertTrue(comboContains(chats, "second in-memory chat")),
                () -> assertTrue(transcriptMarkdown(panel).contains("first in-memory chat")),
                () -> assertFalse(transcriptMarkdown(panel).contains("second in-memory chat")));
    }

    @Test
    void assistantChatTitlesTrimToDropdownWidth() {
        JLabel label = new JLabel();
        String title = "generate a very long browser automation test from the current recording";
        String trimmed = ShaftAssistantPanel.trimChatTitleForWidth(title, label.getFontMetrics(label.getFont()), 120);

        assertAll(
                () -> assertTrue(trimmed.endsWith("...")),
                () -> assertTrue(trimmed.length() < title.length()),
                () -> assertTrue(label.getFontMetrics(label.getFont()).stringWidth(trimmed) <= 120));
    }

    @Test
    void connectedSetupRestoresProjectServiceChats() {
        ShaftSettingsState.Settings settings = connectedMcpSettings();
        ShaftAssistantChatState storedState = new ShaftAssistantChatState();
        Project project = fakeProject(storedState);
        storedState.append("user", "start recording", "{}");
        storedState.append("assistant", "Capture browser opened.", "{}");
        storedState.newSession();
        storedState.append("user", "generate reviewed code", "{}");

        ShaftToolWindowPanel toolWindow = new ShaftToolWindowPanel(project, settings);
        JComboBox<?> chats = findByAccessibleName(toolWindow, "Assistant chat", JComboBox.class);

        assertAll(
                () -> assertNull(setupPanel(toolWindow)),
                () -> assertNull(toolWindowWorkflowSelector(toolWindow)),
                () -> assertFalse(transcriptMarkdown(toolWindow).contains("start recording")),
                () -> assertTrue(transcriptMarkdown(toolWindow).contains("generate reviewed code")),
                () -> assertNotNull(chats),
                () -> assertEquals(2, chats.getItemCount()));
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
    void assistantTranscriptHidesLiveAgentOutputUntilFinalResultByDefault() throws Exception {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, blankMcpSettings());

        assistantPrompt(panel).setText("visible realtime user prompt");
        clickAccessible(panel, "Send assistant prompt");
        appendStreamingLocalAgentBubble(panel, 77);
        appendLocalAgentOutput(panel, 77, "visible realtime agent response");
        showAgentResult(panel, 77, ShaftMcpToolResult.success("public class GeneratedTest { void test() {} }"));

        assertAll(
                () -> assertTrue(transcriptMarkdown(panel).contains("visible realtime user prompt")),
                () -> assertFalse(transcriptMarkdown(panel).contains("visible realtime agent response")),
                () -> assertTrue(transcriptMarkdown(panel).contains("```java")),
                () -> assertTrue(transcriptMarkdown(panel).contains("GeneratedTest")),
                () -> assertTrue(containsText(panel, "visible realtime user prompt")),
                () -> assertFalse(containsText(panel, "visible realtime agent response")));
    }

    @Test
    void assistantVerboseModeShowsLiveAgentOutput() throws Exception {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, blankMcpSettings());
        JCheckBox verbose = findByAccessibleName(panel, "Show verbose agent output", JCheckBox.class);
        verbose.setSelected(true);

        appendStreamingLocalAgentBubble(panel, 88);
        appendLocalAgentOutput(panel, 88, "visible verbose agent response");

        assertAll(
                () -> assertTrue(transcriptMarkdown(panel).contains("visible verbose agent response")),
                () -> assertTrue(containsText(panel, "visible verbose agent response")));
    }

    @Test
    void assistantKillStopsLocalAgentStreamingImmediately() throws Exception {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, blankMcpSettings());
        JCheckBox verbose = findByAccessibleName(panel, "Show verbose agent output", JCheckBox.class);
        AtomicBoolean killed = new AtomicBoolean();
        ShaftMcpInvocation invocation = new ShaftMcpInvocation(
                new CompletableFuture<>(),
                () -> {
                },
                () -> killed.set(true));
        verbose.setSelected(true);
        setField(panel, "currentInvocation", invocation);
        panel.setRunning(true, "Thinking...");
        appendStreamingLocalAgentBubble(panel, 99);
        appendLocalAgentOutput(panel, 99, "visible before kill");

        cancelOrKillCurrent(panel);
        cancelOrKillCurrent(panel);
        appendLocalAgentOutput(panel, 99, "late output after kill");

        assertAll(
                () -> assertTrue(killed.get()),
                () -> assertTrue(transcriptMarkdown(panel).contains("visible before kill")),
                () -> assertFalse(transcriptMarkdown(panel).contains("late output after kill")));
    }

    @Test
    void assistantDoesNotPersistRawResponsePayloads() throws Exception {
        ShaftAssistantChatState chatState = new ShaftAssistantChatState();

        chatState.append("assistant", """
                Rendered response
                OPENAI_API_KEY=abc123
                Authorization: Bearer secret-token
                {"apiKey": "json-secret", "password": "json-password"}
                Cookie: session=abc123
                """.stripIndent(),
                "{\"secret\":\"raw payload\"}");
        ShaftAssistantChatState.StateData state = chatState.getState();
        String persistedMarkdown = state.sessions.get(0).messages.get(0).markdown;
        com.intellij.openapi.components.State annotation =
                ShaftAssistantChatState.class.getAnnotation(com.intellij.openapi.components.State.class);

        assertAll(
                () -> assertTrue(persistedMarkdown.contains("Rendered response")),
                () -> assertFalse(persistedMarkdown.contains("abc123")),
                () -> assertFalse(persistedMarkdown.contains("secret-token")),
                () -> assertFalse(persistedMarkdown.contains("json-secret")),
                () -> assertFalse(persistedMarkdown.contains("json-password")),
                () -> assertFalse(persistedMarkdown.contains("session=abc123")),
                () -> assertFalse(persistedMarkdown.contains("raw payload")),
                () -> assertTrue(persistedMarkdown.contains("<redacted>")),
                () -> assertEquals(com.intellij.openapi.components.StoragePathMacros.WORKSPACE_FILE,
                        annotation.storages()[0].value()),
                () -> assertThrows(NoSuchFieldException.class,
                        () -> ShaftAssistantChatState.Message.class.getDeclaredField("raw")));
    }

    @Test
    void assistantChatStateFallsBackFromCorruptPersistedState() {
        ShaftAssistantChatState.StateData state = new ShaftAssistantChatState.StateData();
        ShaftAssistantChatState.Session corrupt = new ShaftAssistantChatState.Session();
        corrupt.id = null;
        corrupt.title = null;
        corrupt.messages = null;
        state.sessions.add(null);
        state.sessions.add(corrupt);
        state.activeSessionId = "missing";

        ShaftAssistantChatState chatState = new ShaftAssistantChatState();
        chatState.loadState(state);
        chatState.append("user", "fresh prompt after fallback", "");

        assertAll(
                () -> assertEquals(1, chatState.sessions().size()),
                () -> assertEquals("fresh prompt after fallback", chatState.activeSession().title),
                () -> assertTrue(chatState.activeMarkdown().contains("fresh prompt after fallback")));
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
        JButton commandInfo = findByAccessibleName(panel, "SHAFT command hints", JButton.class);
        JButton contextInfo = findByAccessibleName(panel, "Assistant context suggestions", JButton.class);
        JCheckBox verbose = findByAccessibleName(panel, "Show verbose agent output", JCheckBox.class);
        JProgressBar spinner = findByAccessibleName(panel, "Assistant thinking spinner", JProgressBar.class);
        JButton sendButton = findByAccessibleName(panel, "Send assistant prompt", JButton.class);
        JLabel status = findByAccessibleName(panel, "Assistant status", JLabel.class);
        JTextComponent prompt = assistantPrompt(panel);
        ListCellRenderer renderer = commandAutocomplete.getRenderer();
        Component renderedGuide = renderer.getListCellRendererComponent(new JList<>(), "/guide", 0, false, false);
        String renderedText = renderedGuide instanceof JLabel label ? label.getText() : "";

        assertAll(
                () -> assertNotNull(commandAutocomplete, "command autocomplete should exist"),
                () -> assertTrue(commandAutocomplete.isEditable(), "command autocomplete should be editable"),
                () -> assertTrue(commandAutocomplete.getToolTipText().contains("tested commands"),
                        "command autocomplete tooltip should mention tested commands"),
                () -> assertFalse(comboContains(commandAutocomplete, "/commands"), "command picker should hide /commands"),
                () -> assertTrue(comboContains(commandAutocomplete, "/codegen"), "command picker should list /codegen"),
                () -> assertTrue(comboContains(commandAutocomplete, "/record-web"), "command picker should list /record-web"),
                () -> assertTrue(comboContains(commandAutocomplete, "/record-mobile"),
                        "command picker should list /record-mobile"),
                () -> assertTrue(comboContains(commandAutocomplete, "/doctor"), "command picker should list /doctor"),
                () -> assertTrue(comboContains(commandAutocomplete, "/guide"), "command picker should list /guide"),
                () -> assertTrue(comboContains(commandAutocomplete, "/guardrails"), "command picker should list /guardrails"),
                () -> assertTrue(comboContains(commandAutocomplete, "/browser"), "command picker should list /browser"),
                () -> assertTrue(comboContains(commandAutocomplete, "/mobile"), "command picker should list /mobile"),
                () -> assertTrue(comboContains(commandAutocomplete, "/upgrade"), "command picker should list /upgrade"),
                () -> assertTrue(comboContains(commandAutocomplete, "/project"), "command picker should list /project"),
                () -> assertTrue(renderedText.contains("Search the SHAFT guide"), renderedText),
                () -> assertTrue(prompt.getAccessibleContext().getAccessibleDescription().contains("guarded local Agent"),
                        "prompt description should mention guarded local Agent"),
                () -> assertTrue(prompt instanceof JBTextArea, "prompt should be a JBTextArea"),
                () -> assertTrue(((JBTextArea) prompt).getRows() >= 6, "prompt should have at least six rows"),
                () -> assertFalse(containsText(panel, "Add context (#), extensions (@), commands (/commands)"),
                        "old starter strip copy should stay removed"),
                () -> assertNotNull(commandInfo, "command help button should exist"),
                () -> assertNotNull(contextInfo, "context suggestions button should exist"),
                () -> assertNotNull(verbose),
                () -> assertFalse(verbose.isSelected()),
                () -> assertTrue(verbose.getToolTipText().contains("live local agent output")),
                () -> assertTrue(contextInfo.getToolTipText().contains("@workflow"), "context tooltip should mention @workflow"),
                () -> assertTrue(commandInfo.getToolTipText().contains("/codegen"), "command tooltip should list /codegen"),
                () -> assertTrue(commandInfo.getToolTipText().contains("/record-web"), "command tooltip should list /record-web"),
                () -> assertTrue(commandInfo.getToolTipText().contains("/record-mobile"), "command tooltip should list /record-mobile"),
                () -> assertTrue(commandInfo.getToolTipText().contains("/doctor"), "command tooltip should list /doctor"),
                () -> assertTrue(commandInfo.getToolTipText().contains("/guide"), "command tooltip should list /guide"),
                () -> assertTrue(commandInfo.getToolTipText().contains("/guardrails"), "command tooltip should list /guardrails"),
                () -> assertTrue(commandInfo.getToolTipText().contains("/browser"), "command tooltip should list /browser"),
                () -> assertTrue(commandInfo.getToolTipText().contains("/mobile"), "command tooltip should list /mobile"),
                () -> assertTrue(commandInfo.getToolTipText().contains("/upgrade"), "command tooltip should list /upgrade"),
                () -> assertTrue(commandInfo.getToolTipText().contains("/project"), "command tooltip should list /project"),
                () -> assertEquals(commandAutocomplete.getParent(), commandInfo.getParent(),
                        "command autocomplete and help should share a row"),
                () -> assertTrue(componentIndex(commandAutocomplete.getParent(), commandInfo)
                        > componentIndex(commandAutocomplete.getParent(), commandAutocomplete),
                        "command help should follow autocomplete"),
                () -> assertTrue(componentIndex(commandAutocomplete.getParent(), contextInfo)
                        > componentIndex(commandAutocomplete.getParent(), commandInfo),
                        "context help should follow command help"),
                () -> assertNotNull(spinner, "spinner should exist"),
                () -> assertTrue(spinner.isIndeterminate(), "spinner should be indeterminate"),
                () -> assertFalse(spinner.isVisible(), "spinner should be hidden while idle"),
                () -> assertNotNull(status, "status should exist"),
                () -> assertTrue(status.getToolTipText().contains("Try asking"), "status tooltip should be idle copy"),
                () -> assertTrue(status.getAccessibleContext().getAccessibleDescription().contains("Try asking"),
                        "status accessible description should be idle copy"),
                () -> assertNotNull(sendButton, "send button should exist"),
                () -> assertTrue(sendButton.getToolTipText().contains("Command+Enter"), "send tooltip should mention Command+Enter"),
                () -> assertTrue(sendButton.getToolTipText().contains("Ctrl+click"), "send tooltip should mention Ctrl+click"),
                () -> assertTrue(sendButton.getParent().getLayout() instanceof FlowLayout, "send row should use FlowLayout"),
                () -> assertEquals(FlowLayout.RIGHT, ((FlowLayout) sendButton.getParent().getLayout()).getAlignment(),
                        "send row should align right"),
                () -> assertEquals("", sendButton.getText(), "send button should stay icon-only"),
                () -> assertNotNull(sendButton.getIcon(), "send button should have an icon"),
                () -> assertTrue(sendButton.getIcon().getIconWidth() > 0, "send icon should have width"),
                () -> assertTrue(sendButton.getIcon().getIconHeight() > 0, "send icon should have height"));
    }

    @Test
    void assistantCommandAutocompleteStartsWithSlashAndFiltersByTypedPrefix() throws Exception {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, blankMcpSettings());
        JComboBox<?> commandAutocomplete = findByAccessibleName(panel, "Assistant command autocomplete", JComboBox.class);

        assertEquals("/", String.valueOf(commandAutocomplete.getEditor().getItem()));

        SwingUtilities.invokeAndWait(() -> commandAutocomplete.getEditor().setItem("/rec"));
        SwingUtilities.invokeAndWait(() -> {
        });

        assertAll(
                () -> assertTrue(comboContains(commandAutocomplete, "/record-web")),
                () -> assertTrue(comboContains(commandAutocomplete, "/record-mobile")),
                () -> assertFalse(comboContains(commandAutocomplete, "/doctor")),
                () -> assertFalse(comboContains(commandAutocomplete, "/project")));

        SwingUtilities.invokeAndWait(() -> commandAutocomplete.getEditor().setItem("/up"));
        SwingUtilities.invokeAndWait(() -> {
        });

        assertAll(
                () -> assertTrue(comboContains(commandAutocomplete, "/upgrade")),
                () -> assertFalse(comboContains(commandAutocomplete, "/record-web")));
    }

    @Test
    void assistantComposerKeepsLongSetupPromptScrollable() throws Exception {
        ShaftSettingsState.Settings settings = connectedMcpSettings();
        settings.agentGuidanceOptimizationPromptPending = true;
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, settings);
        JTextComponent prompt = assistantPrompt(panel);
        JScrollPane promptScroll = enclosingScrollPane(prompt);

        assertNotNull(promptScroll);
        promptScroll.setSize(new Dimension(640, 120));
        promptScroll.doLayout();
        int lineHeight = prompt.getFontMetrics(prompt.getFont()).getHeight();
        int lineCount = prompt.getDocument().getDefaultRootElement().getElementCount();

        assertAll(
                () -> assertTrue(lineCount > 10),
                () -> assertTrue(prompt.getPreferredSize().height > lineHeight * 10,
                        "Long setup prompt should have enough view height to scroll to its final line."));
    }

    @Test
    void assistantEmptyChatOmitsStarterStripAndKeepsPromptUsable() {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, connectedMcpSettings());
        JTextComponent prompt = assistantPrompt(panel);

        assertAll(
                () -> assertNull(findByAccessibleName(panel, "Assistant starter actions", JComponent.class)),
                () -> assertTrue(prompt instanceof JBTextArea),
                () -> assertTrue(((JBTextArea) prompt).getRows() >= 6));
    }

    @Test
    void assistantTimelineRecordsLocalPromptCompletion() {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, blankMcpSettings());
        JList<?> timeline = findByAccessibleName(panel, "Assistant execution timeline", JList.class);

        assistantPrompt(panel).setText("/help");
        clickAccessible(panel, "Send assistant prompt");

        assertAll(
                () -> assertNotNull(timeline),
                () -> assertTrue(listContains(timeline, "Prompt received")),
                () -> assertTrue(listContains(timeline, "Completed")));
    }

    @Test
    void assistantContextSuggestionsAppearOnlyForImplementedTriggers() {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, blankMcpSettings());

        List<ShaftAssistantPanel.ContextSuggestion> workflowSuggestions = panel.contextSuggestionsForTest('@');
        List<ShaftAssistantPanel.ContextSuggestion> slashSuggestions = panel.contextSuggestionsForTest('/');
        List<ShaftAssistantPanel.ContextSuggestion> fileSuggestions = panel.contextSuggestionsForTest('#');
        List<ShaftAssistantPanel.ContextSuggestion> unsupportedSuggestions = panel.contextSuggestionsForTest('$');

        assertAll(
                () -> assertTrue(workflowSuggestions.stream().anyMatch(
                        suggestion -> "@workflow:record-web".equals(suggestion.label())
                                && "/record-web https://example.com".equals(suggestion.insertion()))),
                () -> assertTrue(workflowSuggestions.stream().anyMatch(
                        suggestion -> "@workflow:record-mobile".equals(suggestion.label())
                                && "/record-mobile inspector Android recordings/inspector.json"
                                .equals(suggestion.insertion()))),
                () -> assertTrue(workflowSuggestions.stream().anyMatch(
                        suggestion -> "@workflow:partner".equals(suggestion.label())
                                && "/partner ".equals(suggestion.insertion()))),
                () -> assertTrue(workflowSuggestions.stream().anyMatch(
                        suggestion -> "@tool:guardrails".equals(suggestion.label())
                                && "/guardrails ".equals(suggestion.insertion()))),
                () -> assertTrue(slashSuggestions.stream().anyMatch(
                        suggestion -> "/upgrade".equals(suggestion.label())
                                && "/upgrade .".equals(suggestion.insertion()))),
                () -> assertTrue(slashSuggestions.stream().anyMatch(
                        suggestion -> "/record-web".equals(suggestion.label())
                                && "/record-web https://example.com".equals(suggestion.insertion()))),
                () -> assertTrue(fileSuggestions.isEmpty()),
                () -> assertTrue(unsupportedSuggestions.isEmpty()));
    }

    @Test
    void assistantCommandAutocompleteInsertsCommandExampleIntoPrompt() throws Exception {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, blankMcpSettings());
        JComboBox<?> commandAutocomplete = findByAccessibleName(panel, "Assistant command autocomplete", JComboBox.class);
        JTextComponent prompt = assistantPrompt(panel);

        prompt.setText("please ");
        prompt.setCaretPosition(prompt.getDocument().getLength());
        SwingUtilities.invokeAndWait(() -> commandAutocomplete.setSelectedItem("/upgrade"));
        SwingUtilities.invokeAndWait(() -> {
        });

        assertAll(
                () -> assertEquals("please /upgrade . ", prompt.getText()),
                () -> assertEquals("/", String.valueOf(commandAutocomplete.getEditor().getItem())),
                () -> assertTrue(comboContains(commandAutocomplete, "/doctor")));
    }

    @Test
    void assistantPromptCtrlClickSendsPrompt() {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, blankMcpSettings());
        JTextComponent prompt = assistantPrompt(panel);
        prompt.setText("/help");

        notifyMouseListeners(prompt, new MouseEvent(
                prompt,
                MouseEvent.MOUSE_CLICKED,
                System.currentTimeMillis(),
                InputEvent.CTRL_DOWN_MASK,
                4,
                4,
                1,
                false,
                MouseEvent.BUTTON1));

        assertAll(
                () -> assertEquals("", prompt.getText()),
                () -> assertTrue(transcriptMarkdown(panel).contains("SHAFT Assistant commands:")),
                () -> assertTrue(transcriptMarkdown(panel).contains("/record-mobile")));
    }

    @Test
    void assistantPromptKeyboardShortcutsSendAndCancelRunningPrompt() throws Exception {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, blankMcpSettings());
        JTextComponent prompt = assistantPrompt(panel);
        Action metaEnter = shortcutAction(prompt, KeyEvent.VK_ENTER, InputEvent.META_DOWN_MASK);
        Action escape = shortcutAction(panel, JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT,
                KeyEvent.VK_ESCAPE, 0);
        AtomicInteger cancelCalls = new AtomicInteger();

        prompt.setText("/help");
        metaEnter.actionPerformed(new ActionEvent(prompt, ActionEvent.ACTION_PERFORMED, "send"));

        setField(panel, "currentInvocation", new ShaftMcpInvocation(
                new CompletableFuture<>(), cancelCalls::incrementAndGet, () -> {
        }));
        panel.setRunning(true, "Thinking...");
        escape.actionPerformed(new ActionEvent(panel, ActionEvent.ACTION_PERFORMED, "cancel"));

        assertAll(
                () -> assertEquals("", prompt.getText()),
                () -> assertTrue(transcriptMarkdown(panel).contains("SHAFT Assistant commands:")),
                () -> assertEquals(1, cancelCalls.get()),
                () -> assertTrue(containsText(panel, "Cancelling...")));
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
                "SHAFT command hints",
                "Cancel assistant request");

        assertAll(controls.stream()
                .map(accessibleName -> () -> assertIconOnlySymmetric(
                        findByAccessibleName(panel, accessibleName, JButton.class))));
    }

    @Test
    void assistantActionChromeAppearsOnlyWhenUseful() throws Exception {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, connectedMcpSettings());

        JButton copyResponse = findByAccessibleName(panel, "Copy last assistant response", JButton.class);
        JButton copyRaw = findByAccessibleName(panel, "Copy last raw assistant response", JButton.class);
        JButton copyAll = findByAccessibleName(panel, "Copy assistant transcript", JButton.class);
        JButton clear = findByAccessibleName(panel, "Clear assistant transcript", JButton.class);
        JButton rerun = findByAccessibleName(panel, "Rerun last assistant prompt", JButton.class);
        JButton cancel = findByAccessibleName(panel, "Cancel assistant request", JButton.class);
        JComponent timelinePanel = (JComponent) getField(panel, "timelinePanel");

        assertAll(
                () -> assertFalse(copyResponse.isVisible()),
                () -> assertFalse(copyRaw.isVisible()),
                () -> assertFalse(copyAll.isVisible()),
                () -> assertFalse(clear.isVisible()),
                () -> assertFalse(rerun.isVisible()),
                () -> assertFalse(cancel.isVisible()),
                () -> assertFalse(timelinePanel.isVisible()));

        showAssistantResult(panel, ShaftMcpToolResult.success("Rendered assistant output"));

        assertAll(
                () -> assertTrue(copyResponse.isVisible()),
                () -> assertTrue(copyAll.isVisible()),
                () -> assertTrue(clear.isVisible()),
                () -> assertIconOnlySymmetric(copyAll),
                () -> assertIconOnlySymmetric(clear),
                () -> assertTrue(copyRaw.isVisible()),
                () -> assertFalse(cancel.isVisible()));

        panel.setRunning(true, "Thinking...");

        assertAll(
                () -> assertTrue(cancel.isVisible()),
                () -> assertTrue(timelinePanel.isVisible()));
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
                .filter(button -> !isSetupPrimaryAction(button))
                .filter(button -> !"Reset and reinstall SHAFT MCP".equals(accessibleName(button)))
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
    void iconButtonsExposeVisibleHoverPressedAndDisabledFeedback() {
        JButton button = new JButton("Run");
        ShaftIconButtons.apply(button, ShaftIcons.SEND);

        Color ready = button.getBackground();
        button.getModel().setRollover(true);
        Color hover = button.getBackground();
        button.getModel().setArmed(true);
        button.getModel().setPressed(true);
        Color pressed = button.getBackground();
        int enabledCursor = button.getCursor().getType();
        button.setEnabled(false);
        Color disabled = button.getBackground();
        int disabledCursor = button.getCursor().getType();

        assertAll(
                () -> assertTrue(button.isOpaque()),
                () -> assertEquals(Cursor.HAND_CURSOR, enabledCursor),
                () -> assertEquals(Cursor.DEFAULT_CURSOR, disabledCursor),
                () -> assertNotEquals(ready, hover),
                () -> assertNotEquals(hover, pressed),
                () -> assertNotEquals(ready, disabled));
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
                () -> assertEquals("Send assistant prompt (Ctrl+Enter, Command+Enter, or Ctrl+click)",
                        sendButton.getToolTipText()));
    }

    @Test
    void assistantCancelButtonArmsKillForActiveInvocation() throws Exception {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, blankMcpSettings());
        JButton cancelButton = findButton(panel, "Cancel");
        AtomicInteger cancelCalls = new AtomicInteger();
        AtomicInteger killCalls = new AtomicInteger();
        setField(panel, "currentInvocation", new ShaftMcpInvocation(
                new CompletableFuture<>(), cancelCalls::incrementAndGet, killCalls::incrementAndGet));

        panel.setRunning(true, "Thinking...");
        clickAccessible(panel, "Cancel assistant request");

        assertAll(
                () -> assertEquals(1, cancelCalls.get()),
                () -> assertEquals(0, killCalls.get()),
                () -> assertEquals("Kill assistant session", cancelButton.getToolTipText()),
                () -> assertNotNull(findByAccessibleName(panel, "Kill assistant session", JButton.class)));

        clickAccessible(panel, "Kill assistant session");

        assertAll(
                () -> assertEquals(1, cancelCalls.get()),
                () -> assertEquals(1, killCalls.get()),
                () -> assertTrue(containsText(panel, "Killing...")));
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
                () -> assertIcon(findButton(setupPanel, "Copy MCP installer command")),
                () -> assertIcon(findButton(setupPanel, "Open terminal for MCP installer")),
                () -> assertIcon(findButton(setupPanel, "Test SHAFT MCP connection")),
                () -> assertIcon(findButton(setupPanel, "Start chatting with SHAFT Assistant")),
                () -> assertIcon(findButton(featurePanel, "Run")),
                () -> assertIcon(findButton(featurePanel, "Cancel")),
                () -> assertIcon(findButton(featurePanel, "Restore defaults")),
                () -> assertIcon(findButton(featurePanel, "Copy output")),
                () -> assertIcon(findButton(featurePanel, "Refresh tools")),
                () -> assertIcon(findButton(setupPanel, "Copy command")),
                () -> assertIcon(findButton(setupPanel, "Copy output")),
                () -> assertIcon(findButton(guidedPanel, "Plan coding partner")),
                () -> assertIcon(findButton(guidedPanel, "Start recording")),
                () -> assertIcon(findButton(guidedPanel, "Stop recording")),
                () -> assertIcon(findButton(guidedPanel, "Review code")),
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
                () -> assertTrue(containsText(panel, "Codex CLI")),
                () -> assertEquals("Agent: Local / Codex / CLI",
                        findByAccessibleName(panel, "Current agent configuration", JLabel.class).getToolTipText()),
                () -> assertNull(findButton(panel, "Test connection and start chatting")),
                () -> assertFalse(openedSetup.get()));

        configure.doClick();
        assertTrue(openedSetup.get());
    }

    @Test
    void assistantTranscriptInitialStateIsBlank() {
        AssistantTranscriptView transcript = new AssistantTranscriptView();

        assertAll(
                () -> assertTrue(transcript.markdown().isBlank()),
                () -> assertFalse(containsText(transcript, "Start with")),
                () -> assertFalse(containsText(transcript, "/guide")),
                () -> assertFalse(containsText(transcript, "/browser")),
                () -> assertFalse(containsText(transcript, "/record")),
                () -> assertFalse(containsText(transcript, "/doctor")));
    }

    @Test
    void assistantTranscriptIsSelfContainedWithoutExternalMarkdownRuntime() {
        List<String> externalMarkdownFields = new ArrayList<>();
        for (Field field : AssistantTranscriptView.class.getDeclaredFields()) {
            if (field.getType().getName().startsWith("org.commonmark")) {
                externalMarkdownFields.add(field.getName() + ":" + field.getType().getName());
            }
        }

        assertTrue(externalMarkdownFields.isEmpty(), externalMarkdownFields.toString());
    }

    @Test
    void assistantTranscriptRendersRoleBasedMessageBubbles() {
        AssistantTranscriptView transcript = new AssistantTranscriptView();
        transcript.append("user", "Hello assistant\n\n```java\nclass UserPrompt {}\n```");
        transcript.append("assistant", "Hi user");

        List<JComponent> bubbles = transcriptBubbles(transcript);
        List<JEditorPane> panes = transcriptHtmlPanes(transcript);
        String rendered = transcriptRenderedHtml(transcript);
        assertAll(
                () -> assertEquals(2, bubbles.size()),
                () -> assertEquals(2, panes.size()),
                () -> assertEquals("user",
                        bubbles.get(0).getClientProperty(AssistantTranscriptView.TRANSCRIPT_BUBBLE_PROPERTY)),
                () -> assertEquals("assistant",
                        bubbles.get(1).getClientProperty(AssistantTranscriptView.TRANSCRIPT_BUBBLE_PROPERTY)),
                () -> assertEquals(BorderLayout.EAST,
                        ((BorderLayout) bubbles.get(0).getParent().getLayout()).getConstraints(bubbles.get(0))),
                () -> assertEquals(BorderLayout.WEST,
                        ((BorderLayout) bubbles.get(1).getParent().getLayout()).getConstraints(bubbles.get(1))),
                () -> assertEquals("user",
                        panes.get(0).getClientProperty(AssistantTranscriptView.TRANSCRIPT_ROLE_PROPERTY)),
                () -> assertEquals("assistant",
                        panes.get(1).getClientProperty(AssistantTranscriptView.TRANSCRIPT_ROLE_PROPERTY)),
                () -> assertTrue(rendered.contains("background:")),
                () -> assertTrue(rendered.contains("class=\"shaft-code-copy\"")),
                () -> assertTrue(rendered.contains("data-copy-code=\"class UserPrompt")),
                () -> assertTrue(rendered.contains("aria-label=\"Copy code\"")),
                () -> assertTrue(rendered.contains("<svg width=\"16\" height=\"16\"")),
                () -> assertTrue(rendered.contains("class=\"shaft-code-copy-icon\"")),
                () -> assertTrue(rendered.contains("&#x2398;")),
                () -> assertTrue(rendered.contains("class=\"shaft-code-highlighted language-java\""), rendered),
                () -> assertTrue(rendered.contains("<span style=\""), rendered),
                () -> assertFalse(rendered.contains(">Copy code<")),
                () -> assertTrue(containsText(transcript, "Hello assistant")),
                () -> assertTrue(containsText(transcript, "Hi user")),
                () -> assertFalse(containsText(transcript, "Start with")),
                () -> assertFalse(rendered.contains("cellpadding=\"8\"")),
                () -> assertFalse(rendered.contains("border=\"1\"")),
                () -> assertFalse(rendered.contains("<table")));
    }

    @Test
    void assistantTranscriptKeepsBubblesInsideNarrowToolWindow() {
        AssistantTranscriptView transcript = new AssistantTranscriptView();
        transcript.setSize(new Dimension(360, 520));
        transcript.append("user", "generate code that opens DuckDuckGo, searches SHAFT Engine, and opens the first result");
        transcript.append("assistant", """
                Confirmed target: https://duckduckgo.com/

                ```java
                public class DuckDuckGoSearchTest {
                    private final SHAFT.GUI.WebDriver driver = new SHAFT.GUI.WebDriver();
                }
                ```
                """.stripIndent().trim());
        transcript.doLayout();

        for (JComponent bubble : transcriptBubbles(transcript)) {
            assertTrue(bubble.getPreferredSize().width <= 336,
                    "Narrow transcript bubble should fit inside 360 px tool window: " + bubble.getPreferredSize());
        }
    }

    @Test
    void assistantTranscriptWrapsLongMarkdownLinksAndPathsInsideBubble() {
        AssistantTranscriptView transcript = new AssistantTranscriptView();
        transcript.setSize(new Dimension(360, 520));
        transcript.append("assistant", """
                Created
                [AGENTS.md](C:/Users/Mohab/IdeaProjects/GitHub/LevelingUpRound34/AGENTS.md) captures canonical guidance surface.
                """.stripIndent().trim());
        transcript.doLayout();

        String rendered = transcriptRenderedHtml(transcript);
        List<JComponent> bubbles = transcriptBubbles(transcript);
        List<JEditorPane> panes = transcriptHtmlPanes(transcript);

        assertAll(
                () -> assertEquals(1, bubbles.size()),
                () -> assertEquals(1, panes.size()),
                () -> assertTrue(rendered.contains("overflow-wrap: anywhere"), rendered),
                () -> assertTrue(rendered.contains("word-wrap: break-word"), rendered),
                () -> assertTrue(rendered.contains("C:&#8203;/&#8203;Users"), rendered),
                () -> assertTrue(bubbles.get(0).getPreferredSize().width <= 336,
                        "Long path bubble should fit inside 360 px tool window: "
                                + bubbles.get(0).getPreferredSize()),
                () -> assertTrue(panes.get(0).getPreferredSize().width <= 314,
                        "Long path pane should fit inside bubble content width: "
                                + panes.get(0).getPreferredSize()));
    }

    @Test
    void assistantTranscriptUsesReadActionForIntellijRendering() {
        assertTrue(AssistantTranscriptView.renderReadActionForTest(
                () -> ApplicationManager.getApplication() == null
                        || ApplicationManager.getApplication().isReadAccessAllowed()));
    }

    @Test
    void assistantTranscriptRendersLatestMessageInSwingView() {
        AssistantTranscriptView transcript = new AssistantTranscriptView();
        transcript.append("user", "Show the sent prompt");

        assertTrue(containsText(transcript, "Show the sent prompt"));
    }

    @Test
    void assistantTranscriptAssistantBubbleHasVisibleRoundedSurface() {
        AssistantTranscriptView transcript = new AssistantTranscriptView();
        transcript.append("assistant", "Assistant response");

        List<JComponent> bubbles = transcriptBubbles(transcript);
        assertAll(
                () -> assertEquals(1, bubbles.size()),
                () -> assertEquals("assistant",
                        bubbles.get(0).getClientProperty(AssistantTranscriptView.TRANSCRIPT_BUBBLE_PROPERTY)),
                () -> assertNotNull(bubbles.get(0).getBorder()),
                () -> assertNotNull(bubbles.get(0).getBackground()),
                () -> assertTrue(bubbles.get(0).getAccessibleContext().getAccessibleName().contains("Assistant")),
                () -> assertFalse(transcriptRenderedHtml(transcript).contains("<table")));
    }

    @Test
    void assistantTranscriptCodeCopyControlDoesNotPaintRectangularOutline() {
        AssistantTranscriptView transcript = new AssistantTranscriptView();
        transcript.append("assistant", """
                ```java
                class AssistantResponse {}
                ```
                """.stripIndent().trim());

        String rendered = transcriptRenderedHtml(transcript);
        String copyControlCss = rendered.substring(
                rendered.indexOf(".shaft-code-copy {"),
                rendered.indexOf(".shaft-code-copy-icon"));
        assertAll(
                () -> assertFalse(copyControlCss.contains("border:")),
                () -> assertFalse(copyControlCss.contains("background:")),
                () -> assertTrue(copyControlCss.contains("line-height: 24px")),
                () -> assertTrue(rendered.contains("line-height:24px")));
    }

    @Test
    void assistantTranscriptCodeBlocksUseLightThemePalette() {
        Object oldTextAreaBackground = UIManager.get("TextArea.background");
        Object oldTextAreaForeground = UIManager.get("TextArea.foreground");
        Object oldEditorPaneBackground = UIManager.get("EditorPane.background");
        Object oldBorder = UIManager.get("Component.borderColor");
        UIManager.put("TextArea.background", Color.WHITE);
        UIManager.put("TextArea.foreground", new Color(0x1F2328));
        UIManager.put("EditorPane.background", new Color(0x3C3F41));
        UIManager.put("Component.borderColor", new Color(0xC9CCD1));
        try {
            AssistantTranscriptView transcript = new AssistantTranscriptView();
            transcript.append("assistant", """
                    ```java
                    public class AssistantResponse {}
                    ```
                    """.stripIndent().trim());

            String rendered = transcriptRenderedHtml(transcript);

            assertAll(
                    () -> assertTrue(rendered.contains("background: #f6f8fa"), rendered),
                    () -> assertTrue(rendered.contains("background: #eef2f7"), rendered),
                    () -> assertTrue(rendered.contains("color:#000080;"), rendered),
                    () -> assertFalse(rendered.contains("#24272b"), rendered),
                    () -> assertFalse(rendered.contains("#2f3338"), rendered));
        } finally {
            restoreUiValue("TextArea.background", oldTextAreaBackground);
            restoreUiValue("TextArea.foreground", oldTextAreaForeground);
            restoreUiValue("EditorPane.background", oldEditorPaneBackground);
            restoreUiValue("Component.borderColor", oldBorder);
        }
    }

    @Test
    void setupPanelShowsOnePrimaryWorkflowActionAtEachStep() throws Exception {
        Path appData = Files.createTempDirectory("shaft-mcp-empty-app-data");
        Path bootstrap = Files.createTempDirectory("shaft-mcp-empty-bootstrap");
        String oldAppData = System.getProperty("shaft.intellij.mcp.applicationDataRoot");
        String oldBootstrap = System.getProperty("shaft.intellij.mcp.bootstrapRoot");
        System.setProperty("shaft.intellij.mcp.applicationDataRoot", appData.toString());
        System.setProperty("shaft.intellij.mcp.bootstrapRoot", bootstrap.toString());
        try {
            ShaftMcpSetupPanel panel = new ShaftMcpSetupPanel(fakeProject(), blankMcpSettings(), () -> {
            });
            setField(panel, "copySink", (Consumer<String>) value -> {
            });
            assertSingleVisiblePrimarySetupAction(panel, "Copy MCP installer command");

            clickAccessible(panel, "Copy MCP installer command");
            assertSingleVisiblePrimarySetupAction(panel, "Open terminal for MCP installer");

            clickAccessible(panel, "Open terminal for MCP installer");
            assertSingleVisiblePrimarySetupAction(panel, "Test SHAFT MCP connection");

            clickAccessible(panel, "Test SHAFT MCP connection");
            assertSingleVisiblePrimarySetupAction(panel, "Test SHAFT MCP connection");
        } finally {
            restoreProperty("shaft.intellij.mcp.applicationDataRoot", oldAppData);
            restoreProperty("shaft.intellij.mcp.bootstrapRoot", oldBootstrap);
        }

        ShaftSettingsState.Settings settings = unverifiedMcpSettings();
        ShaftMcpSetupPanel connectedPanel = new ShaftMcpSetupPanel(fakeProject(), settings, () -> {
        }, readyProbe());
        assertSingleVisiblePrimarySetupAction(connectedPanel, "Test SHAFT MCP connection");

        showTestResult(connectedPanel, ShaftMcpToolResult.success("Probe OK"));
        assertSingleVisiblePrimarySetupAction(connectedPanel, "Start chatting with SHAFT Assistant");
    }

    @Test
    void setupResetAndReinstallClearsStateAndCopiesInstallerCommand() throws Exception {
        ShaftSettingsState.Settings settings = unverifiedMcpSettings();
        settings.mcpSetupComplete = true;
        settings.agentGuidanceOptimizationPromptPending = true;
        ShaftMcpSetupPanel panel = new ShaftMcpSetupPanel(fakeProject(), settings, () -> {
        }, readyProbe());
        AtomicReference<String> copied = new AtomicReference<>("");
        setField(panel, "copySink", (Consumer<String>) copied::set);
        JTextComponent mcpCommand = (JTextComponent) getField(panel, "mcpCommand");

        showTestResult(panel, ShaftMcpToolResult.success("Probe OK"));
        clickAccessible(panel, "Reset and reinstall SHAFT MCP");

        assertAll(
                () -> assertEquals("", mcpCommand.getText()),
                () -> assertFalse(settings.mcpSetupComplete),
                () -> assertFalse(settings.agentGuidanceOptimizationPromptPending),
                () -> assertTrue(copied.get().contains("install-shaft-mcp")),
                () -> assertTrue(copied.get().contains("--install-shaft-skills")),
                () -> assertSingleVisiblePrimarySetupAction(panel, "Open terminal for MCP installer"),
                () -> assertTrue(containsText(panel, "Installer command copied. Run it in terminal, then check.")));
    }

    @Test
    void completedSetupShowsResetAndReinstallWhenReopened() {
        ShaftToolWindowPanel toolWindow = new ShaftToolWindowPanel(fakeProject(), connectedMcpSettings());

        clickAccessible(toolWindow, "Open SHAFT MCP setup");

        JButton resetAndReinstall = findByAccessibleName(toolWindow, "Reset and reinstall SHAFT MCP", JButton.class);
        assertAll(
                () -> assertEquals("Reset / reinstall", resetAndReinstall.getText()),
                () -> assertEquals("Clear the saved MCP command and copy a fresh installer command",
                        resetAndReinstall.getToolTipText()),
                () -> assertTrue(resetAndReinstall.isVisible()),
                () -> assertTrue(resetAndReinstall.isEnabled()));
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

        String rendered = transcriptRenderedHtml(transcript);

        assertAll(
                () -> assertTrue(rendered.contains("<h2")),
                () -> assertTrue(rendered.contains("<hr")),
                () -> assertTrue(rendered.contains("language-json")),
                () -> assertTrue(rendered.contains("language-java")),
                () -> assertTrue(rendered.contains("language-xml")),
                () -> assertEquals(3, countOccurrences(rendered, "class=\"shaft-code-copy\"")),
                () -> assertTrue(rendered.contains("data-copy-code=\"public class LoginTest")),
                () -> assertTrue(rendered.contains("href=\"shaft-copy-code:public+class+LoginTest")),
                () -> assertTrue(rendered.contains("aria-label=\"Copy code\"")),
                () -> assertTrue(rendered.contains("<svg width=\"16\" height=\"16\"")),
                () -> assertTrue(rendered.contains("class=\"shaft-code-copy-icon\"")),
                () -> assertTrue(rendered.contains("&#x2398;")),
                () -> assertTrue(rendered.contains("class=\"shaft-code-highlighted language-java\""), rendered),
                () -> assertTrue(rendered.contains("<span style=\""), rendered),
                () -> assertFalse(rendered.contains(">Copy code<")));
    }

    @Test
    void assistantTranscriptCanReplaceLiveAgentOutputWithFinalMessage() {
        AssistantTranscriptView transcript = new AssistantTranscriptView();
        transcript.append("assistant", "Running Codex CLI...");
        transcript.replaceLast("assistant", "Validated DuckDuckGo title.");

        assertAll(
                () -> assertEquals("Validated DuckDuckGo title.", transcript.markdown()),
                () -> assertFalse(transcriptRenderedHtml(transcript).contains("Running Codex CLI")),
                () -> assertTrue(transcriptRenderedHtml(transcript).contains("Validated DuckDuckGo title.")));
    }

    @Test
    void assistantTranscriptRestoresMessagesFromChatState() {
        ShaftAssistantChatState state = new ShaftAssistantChatState();
        state.append("user", "User prompt", "{}");
        state.append("assistant", "Assistant reply", "{}");

        AssistantTranscriptView transcript = new AssistantTranscriptView();
        transcript.setMessages(state.activeMessages());

        assertEquals("User prompt\n\nAssistant reply", transcript.markdown());
        assertFalse(transcriptRenderedHtml(transcript).isBlank());
    }

    @Test
    void assistantAgentModeSourceEditApprovalOnlyBlocksUnsandboxedCustomCommands() {
        assertAll(
                () -> assertFalse(ShaftAssistantPanel.requiresSourceEditApprovalBeforeSend(
                        true, false, false, "", "edit this test", "")),
                () -> assertTrue(ShaftAssistantPanel.requiresSourceEditApprovalBeforeSend(
                        true, false, false, "custom-agent --unsafe", "edit this test", "")),
                () -> assertFalse(ShaftAssistantPanel.requiresSourceEditApprovalBeforeSend(
                        true, false, true, "custom-agent --unsafe", "edit this test", "")),
                () -> assertFalse(ShaftAssistantPanel.requiresSourceEditApprovalBeforeSend(
                        false, false, false, "custom-agent --unsafe", "edit this test", "")),
                () -> assertFalse(ShaftAssistantPanel.requiresSourceEditApprovalBeforeSend(
                        true, true, false, "custom-agent --unsafe", "edit this test", "")));
    }

    @Test
    void assistantAskOrPlanMcpPromptTellsUserToSwitchToAgentMode() {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, blankMcpSettings());
        JComboBox<?> assistantMode = findByAccessibleName(panel, "Assistant mode", JComboBox.class);
        assertNotNull(assistantMode);
        assistantMode.setSelectedItem("PLAN");

        assistantPrompt(panel).setText("open duckduckgo and search for SHAFT Engine");
        clickAccessible(panel, "Send assistant prompt");

        assertTrue(transcriptMarkdown(panel).contains("Switch to **Agent** mode"));
    }

    @Test
    void assistantAgentModeSourceEditWarningUsesActiveContextForContinuationPrompt() {
        assertAll(
                () -> assertTrue(ShaftAssistantPanel.requiresSourceEditApprovalBeforeSend(
                        true, false, false, "custom-agent --unsafe", "try again", "fix this Java test")),
                () -> assertFalse(ShaftAssistantPanel.requiresSourceEditApprovalBeforeSend(
                        true, false, false, "", "try again", "fix this Java test")));
    }

    @Test
    void assistantAgentModeDoesNotWarnForBrowserOnlyTasksWhenSourceEditsNotApproved() {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, blankMcpSettings());
        JComboBox<?> blockedMode = findByAccessibleName(panel, "Assistant mode", JComboBox.class);
        assertNotNull(blockedMode);
        blockedMode.setSelectedItem("AGENT");
        assistantPrompt(panel).setText("open https://example.com and update me on the login form");
        clickAccessible(panel, "Send assistant prompt");

        assertFalse(transcriptMarkdown(panel).contains("enable **Allow source edits**"));
    }

    @Test
    void assistantModeSelectorCanChangeModesInDefaultComposer() {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, blankMcpSettings());
        JComboBox<?> assistantMode = findByAccessibleName(panel, "Assistant mode", JComboBox.class);

        assertAll(
                () -> assertNotNull(assistantMode),
                () -> assertTrue(assistantMode.isVisible()),
                () -> assertTrue(assistantMode.isEnabled()));

        assistantMode.setSelectedItem("PLAN");
        assertEquals("PLAN", assistantMode.getSelectedItem());

        assistantMode.setSelectedItem("AGENT");
        assertEquals("AGENT", assistantMode.getSelectedItem());

        JCheckBox allowEdits = findByAccessibleName(panel, "Approve source mutation for Agent mode", JCheckBox.class);
        assertAll(
                () -> assertNotNull(allowEdits),
                () -> assertTrue(allowEdits.isVisible()),
                () -> assertTrue(allowEdits.isEnabled()));
    }

    @Test
    void assistantAgentModeShowsSourceEditApprovalForLockedDesktopRuntime() {
        ShaftSettingsState.Settings settings = connectedMcpSettings();
        settings.assistantRuntime = "DESKTOP_APP";
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, settings, new ShaftAssistantChatState(), () -> {
        });
        JComboBox<?> assistantMode = findByAccessibleName(panel, "Assistant mode", JComboBox.class);
        JCheckBox allowEdits = findByAccessibleName(panel, "Approve source mutation for Agent mode", JCheckBox.class);

        assertAll(
                () -> assertNotNull(assistantMode),
                () -> assertNotNull(allowEdits));

        assistantMode.setSelectedItem("AGENT");

        assertAll(
                () -> assertTrue(allowEdits.isVisible()),
                () -> assertTrue(allowEdits.isEnabled()));
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
        ShaftAssistantChatState chatState = new ShaftAssistantChatState();
        chatState.append("user", "Previous assistant conversation", "{}");
        ShaftToolWindowPanel toolWindow = new ShaftToolWindowPanel(project, settings, readyProbe(), chatState);

        ShaftMcpSetupPanel setupPanel = setupPanel(toolWindow);
        assertNotNull(setupPanel);
        showTestResult(setupPanel, ShaftMcpToolResult.success("Probe OK"));
        clickAccessible(setupPanel, "Start chatting with SHAFT Assistant");

        assertAll(
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

        click(panel, "Review code");
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

    private static ShaftSettingsState.Settings unverifiedMcpSettings() {
        ShaftSettingsState.Settings settings = connectedMcpSettings();
        settings.mcpSetupComplete = false;
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

    private static String transcriptRenderedHtml(Component component) {
        StringBuilder html = new StringBuilder();
        for (JEditorPane pane : transcriptHtmlPanes(component)) {
            Object rendered = pane.getClientProperty(AssistantTranscriptView.TRANSCRIPT_RENDERED_HTML_PROPERTY);
            html.append(rendered instanceof String value ? value : pane.getText()).append('\n');
        }
        return html.toString();
    }

    private static List<JEditorPane> transcriptHtmlPanes(Component component) {
        List<JEditorPane> panes = new ArrayList<>();
        collectTranscriptHtmlPanes(component, panes);
        return panes;
    }

    private static void collectTranscriptHtmlPanes(Component component, List<JEditorPane> panes) {
        if (component instanceof JEditorPane pane
                && pane.getClientProperty(AssistantTranscriptView.TRANSCRIPT_RENDERED_HTML_PROPERTY) != null) {
            panes.add(pane);
        }
        if (component instanceof Container container) {
            for (Component child : container.getComponents()) {
                collectTranscriptHtmlPanes(child, panes);
            }
        }
    }

    private static List<JComponent> transcriptBubbles(Component component) {
        List<JComponent> bubbles = new ArrayList<>();
        collectComponentsWithClientProperty(component, AssistantTranscriptView.TRANSCRIPT_BUBBLE_PROPERTY, bubbles);
        return bubbles;
    }

    private static void collectComponentsWithClientProperty(Component component, String property, List<JComponent> found) {
        if (component instanceof JComponent jComponent && jComponent.getClientProperty(property) != null) {
            found.add(jComponent);
        }
        if (component instanceof Container container) {
            for (Component child : container.getComponents()) {
                collectComponentsWithClientProperty(child, property, found);
            }
        }
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

    private static void appendStreamingLocalAgentBubble(ShaftAssistantPanel panel, int streamToken) throws Exception {
        Method method = ShaftAssistantPanel.class.getDeclaredMethod("appendStreamingLocalAgentBubble", int.class);
        method.setAccessible(true);
        method.invoke(panel, streamToken);
    }

    private static void appendLocalAgentOutput(ShaftAssistantPanel panel, int streamToken, String line) throws Exception {
        Method method = ShaftAssistantPanel.class.getDeclaredMethod("appendLocalAgentOutput", int.class, String.class);
        method.setAccessible(true);
        method.invoke(panel, streamToken, line);
    }

    private static void cancelOrKillCurrent(ShaftAssistantPanel panel) throws Exception {
        Method method = ShaftAssistantPanel.class.getDeclaredMethod("cancelOrKillCurrent");
        method.setAccessible(true);
        method.invoke(panel);
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

    private static Action shortcutAction(JTextComponent component, int keyCode, int modifiers) {
        Object key = component.getInputMap().get(KeyStroke.getKeyStroke(keyCode, modifiers));
        assertNotNull(key);
        Action action = component.getActionMap().get(key);
        assertNotNull(action);
        return action;
    }

    private static Action shortcutAction(JComponent component, int condition, int keyCode, int modifiers) {
        Object key = component.getInputMap(condition).get(KeyStroke.getKeyStroke(keyCode, modifiers));
        assertNotNull(key);
        Action action = component.getActionMap().get(key);
        assertNotNull(action);
        return action;
    }

    private static void assertIcon(JButton button) {
        assertNotNull(button);
        String name = button.getText() + "/" + accessibleName(button);
        assertNotNull(button.getIcon(), name);
        assertTrue(button.getIcon().getIconWidth() > 0, name);
        assertTrue(button.getIcon().getIconHeight() > 0, name);
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

    private static String javaExecutableName() {
        return isWindowsOs() ? "java.exe" : "java";
    }

    private static boolean isWindowsOs() {
        return System.getProperty("os.name", "").toLowerCase().contains("win");
    }

    private static void restoreProperty(String key, String value) {
        if (value == null) {
            System.clearProperty(key);
        } else {
            System.setProperty(key, value);
        }
    }

    private static void restoreUiValue(String key, Object value) {
        if (value == null) {
            UIManager.getDefaults().remove(key);
        } else {
            UIManager.put(key, value);
        }
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
                || hasText(accessibleName(button))
                || (button.getIcon() != null && hasText(button.getToolTipText()));
    }

    private static boolean isSetupPrimaryAction(JButton button) {
        return List.of(
                "Copy MCP installer command",
                "Open terminal for MCP installer",
                "Test SHAFT MCP connection",
                "Start chatting with SHAFT Assistant").contains(accessibleName(button));
    }

    private static void assertSingleVisiblePrimarySetupAction(Component component, String accessibleName) {
        List<JButton> visibleActions = collectButtons(component).stream()
                .filter(ShaftPanelSetupTest::isSetupPrimaryAction)
                .filter(button -> button.isVisible() && button.isEnabled())
                .toList();
        assertEquals(1, visibleActions.size());
        assertEquals(accessibleName, accessibleName(visibleActions.get(0)));
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

    private static void notifyMouseListeners(Component component, MouseEvent event) {
        for (MouseListener listener : component.getMouseListeners()) {
            listener.mouseClicked(event);
        }
    }

    private static int componentIndex(Container container, Component component) {
        Component[] children = container.getComponents();
        for (int index = 0; index < children.length; index++) {
            if (children[index] == component) {
                return index;
            }
        }
        return -1;
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

    private static boolean listContains(JList<?> list, String expectedText) {
        if (list == null) {
            return false;
        }
        for (int index = 0; index < list.getModel().getSize(); index++) {
            Object item = list.getModel().getElementAt(index);
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

    private static JScrollPane enclosingScrollPane(Component component) {
        Container parent = component.getParent();
        while (parent != null) {
            if (parent instanceof JViewport viewport && viewport.getParent() instanceof JScrollPane scrollPane) {
                return scrollPane;
            }
            parent = parent.getParent();
        }
        return null;
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
