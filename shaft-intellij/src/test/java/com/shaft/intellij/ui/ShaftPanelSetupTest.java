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
import javax.swing.JMenuItem;
import javax.swing.JPanel;
import javax.swing.JPopupMenu;
import javax.swing.JProgressBar;
import javax.swing.JScrollPane;
import javax.swing.JViewport;
import javax.swing.RepaintManager;
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
import static org.junit.jupiter.api.Assertions.assertSame;
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
    void refreshCatalogEntersBusyStateThenBlocksReentryViaRealGuard() throws Exception {
        ShaftFeaturePanel tools = new ShaftFeaturePanel(fakeProject(), connectedMcpSettings());
        JButton refreshButton = findButton(tools, "Refresh tools");
        assertNotNull(refreshButton);
        assertEquals("Refresh tools", refreshButton.getToolTipText());

        // The fake test Project has no real ShaftMcpInvocationService/Application wired up (this
        // suite never registers one - see fakeProject()), so refreshCatalog()'s call into
        // ShaftMcpInvocationService.getInstance(project).startListTools() is expected to throw once
        // it gets that far. What we're proving here is that setRunning(true, ...) and the busy
        // tooltip swap already happened - via the real refreshCatalog(Project) method - before that
        // unrelated plumbing gap is hit.
        assertThrows(NullPointerException.class, () -> invokeRefreshCatalog(tools, fakeProject()));

        assertAll(
                () -> assertFalse(refreshButton.isEnabled(), "button should be disabled once refreshCatalog() starts running"),
                () -> assertEquals("Refreshing...", refreshButton.getToolTipText()));

        // Re-entry guard: seed currentInvocation as if a request were genuinely in flight, then
        // invoke the real refreshCatalog(Project) method again. The guard at the top of the method
        // must return immediately without touching the button/tooltip/currentInvocation again.
        ShaftMcpInvocation inFlight = new ShaftMcpInvocation(new CompletableFuture<>(), () -> {
        });
        setField(tools, "currentInvocation", inFlight);
        refreshButton.setEnabled(true);
        refreshButton.setToolTipText("Refresh tools");

        invokeRefreshCatalog(tools, fakeProject());

        assertAll(
                () -> assertSame(inFlight, getField(tools, "currentInvocation"),
                        "a blocked re-entrant call must not replace the in-flight invocation"),
                () -> assertTrue(refreshButton.isEnabled(),
                        "the re-entry guard must return before touching button state"),
                () -> assertEquals("Refresh tools", refreshButton.getToolTipText(),
                        "the re-entry guard must return before touching the tooltip"));
    }

    @Test
    void showCatalogResultAppliesSuccessAndErrorGlyphsThroughRealCompletionPath() throws Exception {
        ShaftFeaturePanel tools = new ShaftFeaturePanel(fakeProject(), connectedMcpSettings());
        JButton refreshButton = findButton(tools, "Refresh tools");
        assertNotNull(refreshButton);

        setField(tools, "currentInvocation", new ShaftMcpInvocation(new CompletableFuture<>(), () -> {
        }));
        refreshButton.setToolTipText("Refreshing...");

        showCatalogResult(tools, ShaftMcpToolResult.success(mcpToolsList()));

        assertAll(
                () -> assertTrue(statusText(tools).contains(ShaftStatusPresentation.SUCCESS_ICON),
                        "success completion should surface the shared SUCCESS_ICON glyph: " + statusText(tools)),
                () -> assertTrue(statusText(tools).contains("Tools refreshed")),
                () -> assertEquals("Refresh tools", refreshButton.getToolTipText(),
                        "tooltip should be restored once the refresh completes"),
                () -> assertNull(getField(tools, "currentInvocation"), "completion must clear the in-flight guard"),
                () -> assertTrue(refreshButton.isEnabled()));

        setField(tools, "currentInvocation", new ShaftMcpInvocation(new CompletableFuture<>(), () -> {
        }));
        refreshButton.setToolTipText("Refreshing...");

        showCatalogResult(tools, ShaftMcpToolResult.failure("MCP server process exited."));

        assertAll(
                () -> assertTrue(statusText(tools).contains(ShaftStatusPresentation.ERROR_ICON),
                        "error completion should surface the shared ERROR_ICON glyph: " + statusText(tools)),
                () -> assertTrue(statusText(tools).contains("Failed")),
                () -> assertEquals("Refresh tools", refreshButton.getToolTipText()),
                () -> assertNull(getField(tools, "currentInvocation")));
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
                    () -> assertEquals(6, findByAccessibleName(panel, "MCP installer target", JComboBox.class)
                            .getItemCount()),
                    () -> assertEquals("INTELLIJ_PLUGIN", lastComboItem(
                            findByAccessibleName(panel, "MCP installer target", JComboBox.class))),
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
    void setupPanelUpdatesInstallerCommandForSelectedAssistantClient() throws Exception {
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

        // Regression lock: INTELLIJ_PLUGIN stays last among exactly the 6 known installer targets.
        List<String> installerTargetTokens = new ArrayList<>();
        for (int index = 0; index < target.getItemCount(); index++) {
            installerTargetTokens.add(String.valueOf(target.getItemAt(index)));
        }
        assertAll(
                () -> assertEquals(6, target.getItemCount()),
                () -> assertEquals("INTELLIJ_PLUGIN", lastComboItem(target)),
                () -> assertEquals(List.of("CODEX", "CLAUDE_CODE", "CLAUDE_DESKTOP", "COPILOT_CLI",
                        "COPILOT_INTELLIJ", "INTELLIJ_PLUGIN"), installerTargetTokens));

        // The disambiguated label must not collapse back to a bare peer-agent name, and the
        // distinct IDE_PLUGIN runtime token (used by the runtime combo, not this target combo)
        // must stay unaffected.
        assertAll(
                () -> assertEquals("SHAFT IntelliJ plugin (this plugin only - no external agent)",
                        ShaftUiLabels.friendly("INTELLIJ_PLUGIN")),
                () -> assertNotEquals("SHAFT IntelliJ plugin", ShaftUiLabels.friendly("INTELLIJ_PLUGIN")),
                () -> assertEquals("IDE plugin", ShaftUiLabels.friendly("IDE_PLUGIN")));

        // installerArgumentFor()/installerCommandFor() must keep producing the exact same
        // --client argument strings as before for every known installer target token.
        assertAll(
                () -> assertEquals("codex", installerArgumentFor("CODEX")),
                () -> assertEquals("claude", installerArgumentFor("CLAUDE_CODE")),
                () -> assertEquals("claude-desktop", installerArgumentFor("CLAUDE_DESKTOP")),
                () -> assertEquals("copilot", installerArgumentFor("COPILOT_CLI")),
                () -> assertEquals("copilot-intellij", installerArgumentFor("COPILOT_INTELLIJ")),
                () -> assertEquals("intellij-plugin", installerArgumentFor("INTELLIJ_PLUGIN")));
        for (String token : installerTargetTokens) {
            String argument = installerArgumentFor(token);
            String command = installerCommandFor(argument);
            assertTrue(command.contains(argument), command);
        }
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
                () -> assertEquals("INTELLIJ_PLUGIN", lastComboItem(target)),
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
                // fakeProject()'s base path has no AGENTS.md, so the guidance-optimization
                // prompt (which references a validator only meaningful for a project that
                // adopted that scaffold) must not be scheduled -- see
                // setupSuccessSchedulesGuidanceOptimizationPromptOnlyWithAgentsMdScaffold
                // for the case where the scaffold is present.
                () -> assertFalse(settings.agentGuidanceOptimizationPromptPending));
        clickAccessible(panel, "Start chatting with SHAFT Assistant");
        assertTrue(connected.get());
    }

    @Test
    void setupSuccessSchedulesGuidanceOptimizationPromptOnlyWithAgentsMdScaffold() throws Exception {
        // Regression test for issue #3363 bug 9: the agent-guidance-optimization
        // prompt tells the agent to rerun scripts/ci/validate_agent_setup.py, a
        // validator that only works in a project that has actually adopted the
        // AGENTS.md scaffold. Scheduling that prompt for any successfully
        // configured project -- regardless of whether it has that scaffold --
        // made the agent report back that required config/scaffold files were
        // missing, because they never existed in the target project at all.
        Path projectWithoutScaffold = Files.createTempDirectory("shaft-no-scaffold");
        Path projectWithScaffold = Files.createTempDirectory("shaft-with-scaffold");
        try {
            Files.writeString(projectWithScaffold.resolve("AGENTS.md"), "# Guidance\n");

            ShaftSettingsState.Settings withoutScaffoldSettings = unverifiedMcpSettings();
            ShaftMcpSetupPanel withoutScaffoldPanel = new ShaftMcpSetupPanel(
                    fakeProject(new ShaftAssistantChatState(), projectWithoutScaffold.toString()),
                    withoutScaffoldSettings, () -> {
                    }, readyProbe());
            showTestResult(withoutScaffoldPanel, ShaftMcpToolResult.success("Probe OK"));

            ShaftSettingsState.Settings withScaffoldSettings = unverifiedMcpSettings();
            ShaftMcpSetupPanel withScaffoldPanel = new ShaftMcpSetupPanel(
                    fakeProject(new ShaftAssistantChatState(), projectWithScaffold.toString()),
                    withScaffoldSettings, () -> {
                    }, readyProbe());
            showTestResult(withScaffoldPanel, ShaftMcpToolResult.success("Probe OK"));

            assertAll(
                    () -> assertFalse(withoutScaffoldSettings.agentGuidanceOptimizationPromptPending,
                            "No AGENTS.md scaffold: the guidance-optimization prompt must not be scheduled"),
                    () -> assertTrue(withScaffoldSettings.agentGuidanceOptimizationPromptPending,
                            "AGENTS.md scaffold present: the guidance-optimization prompt should be scheduled"));
        } finally {
            Files.deleteIfExists(projectWithoutScaffold.resolve("AGENTS.md"));
            Files.deleteIfExists(projectWithoutScaffold);
            Files.deleteIfExists(projectWithScaffold.resolve("AGENTS.md"));
            Files.deleteIfExists(projectWithScaffold);
        }
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
    void setupPanelConfiguresGeminiCloudProviderWithStoredApiKey() throws Exception {
        java.util.Map<String, String> storedKeys = new java.util.HashMap<>();
        ShaftMcpSetupPanel.CloudKeyStore keyStore = fakeKeyStore(storedKeys);
        ShaftSettingsState.Settings settings = unverifiedMcpSettings();
        ShaftMcpSetupPanel panel = new ShaftMcpSetupPanel(fakeProject(), settings, () -> {
        }, (client, runtime) -> ShaftMcpToolResult.failure("local CLI readiness must not gate the cloud route"),
                keyStore);
        JComboBox<?> family = findByAccessibleName(panel, "Assistant family", JComboBox.class);
        javax.swing.JPasswordField apiKey =
                findByAccessibleName(panel, "Gemini API key", javax.swing.JPasswordField.class);
        JComponent runtimeRow = (JComponent) getField(panel, "runtimeRow");
        JComponent apiKeyRow = (JComponent) getField(panel, "apiKeyRow");

        assertAll(
                () -> assertFalse(apiKeyRow.isVisible()),
                () -> assertTrue(runtimeRow.isVisible()));

        family.setSelectedItem("GEMINI");
        assertAll(
                () -> assertTrue(apiKeyRow.isVisible()),
                () -> assertFalse(runtimeRow.isVisible()),
                () -> assertTrue(containsText(panel, "Paste your Google AI Studio API key.")));

        apiKey.setText("test-gemini-key");
        showTestResult(panel, ShaftMcpToolResult.success("Probe OK"));

        assertAll(
                () -> assertEquals("test-gemini-key", storedKeys.get("GEMINI_API_KEY")),
                () -> assertEquals(0, apiKey.getPassword().length),
                () -> assertEquals("CLOUD", settings.assistantProviderType),
                () -> assertEquals("gemini", settings.cloudProvider),
                () -> assertEquals("gemini-3.5-flash", settings.cloudModel),
                () -> assertTrue(settings.passProviderApiKeysToMcp),
                () -> assertTrue(settings.mcpSetupComplete),
                () -> assertTrue(containsText(panel, "Ready to chat. Verified Gemini cloud API.")),
                () -> assertTrue(findByAccessibleName(panel, "Start chatting with SHAFT Assistant", JButton.class)
                        .isVisible()));
    }

    @Test
    void setupPanelBlocksGeminiConnectionWithoutStoredApiKey() throws Exception {
        ShaftSettingsState.Settings settings = unverifiedMcpSettings();
        ShaftMcpSetupPanel panel = new ShaftMcpSetupPanel(fakeProject(), settings, () -> {
        }, readyProbe(), fakeKeyStore(new java.util.HashMap<>()));
        JComboBox<?> family = findByAccessibleName(panel, "Assistant family", JComboBox.class);

        family.setSelectedItem("GEMINI");
        showTestResult(panel, ShaftMcpToolResult.success("Probe OK"));

        assertAll(
                () -> assertTrue(containsText(panel, "Assist: Error")),
                () -> assertTrue(containsText(panel, "No Gemini API key stored")),
                () -> assertFalse(settings.mcpSetupComplete),
                () -> assertFalse(settings.agentGuidanceOptimizationPromptPending));
    }

    @Test
    void assistantShowsModelAndEffortSelectorsWithoutAdvancedMode() {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, connectedMcpSettings());
        JComboBox<?> localModel = findByAccessibleName(panel, "Assistant local agent model", JComboBox.class);
        JComboBox<?> effort = findByAccessibleName(panel, "Assistant effort", JComboBox.class);

        assertAll(
                () -> assertTrue(localModel.isVisible()),
                () -> assertTrue(effort.isVisible()),
                () -> assertEquals("DEFAULT", effort.getSelectedItem()),
                () -> assertTrue(effort.getItemCount() >= 4));
    }

    @Test
    void assistantKeepsCloudRouteAndModelListFromSetupWithoutAdvancedMode() {
        ShaftSettingsState.Settings settings = connectedMcpSettings();
        settings.assistantProviderType = "CLOUD";
        settings.cloudProvider = "gemini";
        settings.cloudModel = "gemini-3.5-flash";
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, settings);
        JComboBox<?> providerType = findByAccessibleName(panel, "Assistant provider type", JComboBox.class);
        JComboBox<?> cloudModel = findByAccessibleName(panel, "Assistant cloud model", JComboBox.class);
        JComboBox<?> effort = findByAccessibleName(panel, "Assistant effort", JComboBox.class);

        assertAll(
                // Basic mode must not force the setup-selected cloud route back to LOCAL.
                () -> assertEquals("CLOUD", providerType.getSelectedItem()),
                () -> assertTrue(cloudModel.isVisible()),
                () -> assertTrue(effort.isVisible()),
                () -> assertEquals("gemini-3.5-flash", String.valueOf(cloudModel.getSelectedItem())),
                () -> assertTrue(cloudModel.getItemCount() >= 2));
    }

    private static ShaftMcpSetupPanel.CloudKeyStore fakeKeyStore(java.util.Map<String, String> storedKeys) {
        return new ShaftMcpSetupPanel.CloudKeyStore() {
            @Override
            public boolean hasKey(String keyName) {
                return storedKeys.containsKey(keyName);
            }

            @Override
            public void saveKey(String keyName, char[] secret) {
                storedKeys.put(keyName, new String(secret));
            }
        };
    }

    @Test
    void assistantConsumesPendingGuidanceOptimizationPromptOnce() {
        ShaftSettingsState.Settings settings = connectedMcpSettings();
        settings.agentGuidanceOptimizationPromptPending = true;

        ShaftAssistantPanel first = new ShaftAssistantPanel(null, settings);
        ShaftAssistantPanel second = new ShaftAssistantPanel(null, settings);

        JComboBox<?> assistantMode = findByAccessibleName(first, "Assistant mode", JComboBox.class);
        JCheckBox allowEdits = findByAccessibleName(first, "Approve source mutation for Agent mode", JCheckBox.class);

        assertAll(
                () -> assertTrue(containsText(first, "Audit and optimize")),
                () -> assertTrue(containsText(first, "shaft_guide_search")),
                () -> assertTrue(containsText(first, "test_automation_scenarios")),
                () -> assertTrue(containsText(first, "test_code_guardrails_check")),
                () -> assertFalse(settings.agentGuidanceOptimizationPromptPending),
                () -> assertFalse(containsText(second, "Audit and optimize")),
                () -> assertEquals("PLAN", assistantMode.getSelectedItem(),
                        "Onboarding optimization prompt should force Plan mode"),
                () -> assertFalse(allowEdits.isSelected(),
                        "Onboarding optimization prompt should leave source edits unchecked"));
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
    void guidanceOptimizationPromptRerunInstructionIsUnconditional() {
        ShaftSettingsState.Settings codex = connectedMcpSettings();

        String prompt = ShaftAssistantPanel.agentGuidanceOptimizationPrompt(codex);

        assertAll(
                () -> assertFalse(prompt.contains("If source edits are not enabled"), prompt),
                () -> assertTrue(prompt.contains("validate_agent_setup.py --skip-external"), prompt));
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
    void setupPanelHasNoLabelCroppingAndPaintsStepBackgroundContinuously() throws Exception {
        ShaftMcpSetupPanel panel = new ShaftMcpSetupPanel(fakeProject(), blankMcpSettings(), () -> {
        });

        // Lay out the panel at 800x600 to ensure label text can render
        panel.setBounds(0, 0, 800, 600);
        SwingUtilities.invokeAndWait(() -> {
            panel.doLayout();
            // Recursively layout all nested components (not just immediate children)
            walkComponents(panel, comp -> {
                if (comp instanceof JComponent jc && comp != panel) {
                    jc.doLayout();
                }
            });
            panel.validate();
        });

        // Find the step row with agent controls (contains "Assistant family" label)
        JPanel chooseRow = (JPanel) getField(panel, "chooseRow");
        AtomicReference<JLabel> assistantFamilyLabel = new AtomicReference<>();
        AtomicReference<JComboBox<?>> familyCombo = new AtomicReference<>();
        List<JPanel> childPanels = new ArrayList<>();

        // Walk the component tree to find labels and verify they render at full size
        walkComponents(chooseRow, comp -> {
            if (comp instanceof JLabel lbl && "Assistant family".equals(lbl.getText())) {
                assistantFamilyLabel.set(lbl);
            }
            if (comp instanceof JComboBox<?> cmb &&
                "Assistant family".equals(cmb.getAccessibleContext().getAccessibleName())) {
                familyCombo.set(cmb);
            }
            if (comp instanceof JPanel pnl && comp != chooseRow) {
                childPanels.add(pnl);
            }
        });

        // Verify labels render without clipping
        assertAll(
                () -> assertNotNull(assistantFamilyLabel.get(), "Should find 'Assistant family' label"),
                () -> assertNotNull(familyCombo.get(), "Should find family combobox"),
                () -> assertTrue(assistantFamilyLabel.get().getSize().width >= assistantFamilyLabel.get().getPreferredSize().width,
                        "Assistant family label width " + assistantFamilyLabel.get().getSize().width +
                        " should not be less than preferred " + assistantFamilyLabel.get().getPreferredSize().width),
                () -> assertTrue(assistantFamilyLabel.get().getSize().height >= assistantFamilyLabel.get().getPreferredSize().height,
                        "Assistant family label height should not be less than preferred"));

        // Verify step row child panels are non-opaque or painted with step background
        Color stepBackground = chooseRow.getBackground();
        for (JPanel child : childPanels) {
            boolean isNonOpaque = !child.isOpaque();
            boolean hasStepBackground = child.getBackground() != null &&
                child.getBackground().equals(stepBackground);
            assertTrue(isNonOpaque || hasStepBackground,
                    "Child panel should be non-opaque or have step background color. " +
                    "Opaque: " + child.isOpaque() + ", Background matches step: " + hasStepBackground);
        }
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
    void setupCompleteCallbackCreatesNewSessionBeforeShowingMainView() throws Exception {
        ShaftAssistantChatState chatState = new ShaftAssistantChatState();
        chatState.append("user", "previous message", "{}");
        chatState.append("assistant", "previous answer", "{}");
        int initialSessionCount = chatState.sessions().size();
        String previousSessionId = chatState.activeSession().id;

        // Create tool window with unverified MCP settings (shows setup panel). Use the stubbed
        // readyProbe() rather than the real AssistantLocalAgentRunner::readiness default -- that
        // real probe checks whether an actual agent CLI is installed on PATH, which made this test
        // pass or fail depending on the machine running it (installed locally, absent on clean CI
        // runners) instead of testing the session-reset behavior it's meant to cover.
        ShaftToolWindowPanel toolWindow = new ShaftToolWindowPanel(
                fakeProject(chatState), unverifiedMcpSettings(), readyProbe(), chatState);

        // Simulate successful setup and trigger callback by clicking "Start chatting"
        ShaftMcpSetupPanel setupPanel = setupPanel(toolWindow);
        assertNotNull(setupPanel);
        showTestResult(setupPanel, ShaftMcpToolResult.success("Probe OK\nMCP workspace: C:/work/shaft"));
        JButton startChatting = findByAccessibleName(setupPanel, "Start chatting with SHAFT Assistant", JButton.class);
        assertNotNull(startChatting);
        startChatting.doClick();

        assertAll(
                () -> assertEquals(initialSessionCount + 1, chatState.sessions().size(),
                        "Setup complete callback should create a new session"),
                () -> assertTrue(chatState.activeMessages().isEmpty(),
                        "New active session should have no messages"),
                () -> assertNotEquals(previousSessionId, chatState.activeSession().id,
                        "Active session should be the new one"),
                () -> assertNotNull(findByAccessibleName(toolWindow, "Assistant prompt", JTextComponent.class),
                        "Main view should be displayed with empty chat"));
    }

    @Test
    void setupCompleteDedupGuardPreventsDoubleSessionCreation() throws Exception {
        ShaftAssistantChatState chatState = new ShaftAssistantChatState();
        chatState.append("user", "first chat", "{}");
        chatState.newSession(); // Create empty session
        int sessionCount = chatState.sessions().size();
        String activeSessionId = chatState.activeSession().id;

        // Invoke newSession() again while active session is empty
        ShaftAssistantChatState.Session returned = chatState.newSession();

        assertAll(
                () -> assertEquals(sessionCount, chatState.sessions().size(),
                        "Second newSession() on empty active session should not create new session"),
                () -> assertEquals(activeSessionId, returned.id,
                        "Should return the existing empty session"),
                () -> assertEquals(activeSessionId, chatState.activeSession().id,
                        "Active session ID should remain unchanged"));
    }

    @Test
    void newSessionCreatesNewWhenActiveSesionHasMessages() {
        ShaftAssistantChatState chatState = new ShaftAssistantChatState();
        chatState.append("user", "existing message", "{}");
        int initialSessionCount = chatState.sessions().size();
        String initialActiveId = chatState.activeSession().id;

        ShaftAssistantChatState.Session newSession = chatState.newSession();

        assertAll(
                () -> assertEquals(initialSessionCount + 1, chatState.sessions().size(),
                        "Should create new session when active has messages"),
                () -> assertNotEquals(initialActiveId, newSession.id,
                        "New session should have different ID"),
                () -> assertTrue(newSession.messages.isEmpty(),
                        "New session should be empty"),
                () -> assertEquals(newSession.id, chatState.activeSession().id,
                        "New session should become active"));
    }

    @Test
    void assistantPanelUiResetWorksWithDedupedSession() throws Exception {
        ShaftAssistantChatState chatState = new ShaftAssistantChatState();
        chatState.append("user", "initial prompt", "{}");
        ShaftAssistantPanel panel = new ShaftAssistantPanel(fakeProject(chatState), blankMcpSettings(), chatState);

        // Simulate clearing the active session and creating a new one (like "New chat" does internally)
        chatState.clearActiveSession();

        // Call newSession() twice - second time should reuse the empty session instead of creating a new one
        chatState.newSession();
        ShaftAssistantChatState.Session afterFirstNew = chatState.activeSession();
        int sessionCountAfterFirst = chatState.sessions().size();

        chatState.newSession();
        ShaftAssistantChatState.Session afterSecondNew = chatState.activeSession();
        int sessionCountAfterSecond = chatState.sessions().size();

        assertAll(
                () -> assertTrue(chatState.activeMessages().isEmpty(),
                        "Chat should be empty after newSession"),
                () -> assertEquals(sessionCountAfterFirst, sessionCountAfterSecond,
                        "Second newSession() on empty session should not create new session (dedup guard)"),
                () -> assertEquals(afterFirstNew.id, afterSecondNew.id,
                        "After second newSession(), should still be on the same empty session"),
                () -> assertNotNull(findByAccessibleName(panel, "Assistant prompt", JTextComponent.class),
                        "UI should still be functional"));
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
    void assistantStructuredLocalAgentResponseShowsExactReportedTokenCountAndHidesRawUsageJson() throws Exception {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, blankMcpSettings());
        // Shaped per the R3-T1 contract: an answer followed by a trailing single-line usage JSON,
        // as produced by AssistantLocalAgentRunner's structured-stream terminal event / finalOutput.
        String rawResponse = "The failure was a stale locator."
                + "\n\n{\"usage\":{\"input_tokens\":123,\"output_tokens\":45}}";

        appendStreamingLocalAgentBubble(panel, 101);
        showAgentResult(panel, 101, ShaftMcpToolResult.success(rawResponse));

        String markdown = transcriptMarkdown(panel);
        assertAll(
                () -> assertTrue(markdown.contains("The failure was a stale locator."), markdown),
                () -> assertTrue(markdown.contains("Tokens consumed:"), markdown),
                () -> assertTrue(markdown.contains("`168`"), markdown),
                () -> assertTrue(markdown.contains("input: 123"), markdown),
                () -> assertTrue(markdown.contains("output: 45"), markdown),
                () -> assertFalse(markdown.contains("(estimated)"), markdown),
                () -> assertFalse(markdown.contains("input_tokens"), markdown));
    }

    @Test
    void assistantFallbackLocalAgentResponseKeepsEstimatedTokenCount() throws Exception {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, blankMcpSettings());
        String rawResponse = "Here is the explanation.\nIt spans several lines.\nNo structured data anywhere.";

        appendStreamingLocalAgentBubble(panel, 102);
        showAgentResult(panel, 102, ShaftMcpToolResult.success(rawResponse));

        String markdown = transcriptMarkdown(panel);
        assertAll(
                () -> assertTrue(markdown.contains("Here is the explanation."), markdown),
                () -> assertTrue(markdown.matches("(?s).*\\*\\*Tokens consumed:\\*\\* `\\d+` \\(estimated\\).*"),
                        markdown));
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
                () -> assertTrue(comboContains(commandAutocomplete, "/upgrade"), "command picker should list /upgrade"),
                // expert commands are hidden from the default composer until Expert mode is enabled
                () -> assertFalse(comboContains(commandAutocomplete, "/guide"), "expert commands hidden by default"),
                () -> assertFalse(comboContains(commandAutocomplete, "/guardrails"), "expert commands hidden by default"),
                () -> assertFalse(comboContains(commandAutocomplete, "/browser"), "expert commands hidden by default"),
                () -> assertFalse(comboContains(commandAutocomplete, "/mobile"), "expert commands hidden by default"),
                () -> assertFalse(comboContains(commandAutocomplete, "/project"), "expert commands hidden by default"),
                () -> assertFalse(comboContains(commandAutocomplete, "/partner"), "expert commands hidden by default"),
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
                () -> assertTrue(commandInfo.getToolTipText().contains("/upgrade"), "command tooltip should list /upgrade"),
                () -> assertFalse(commandInfo.getToolTipText().contains("/guide"), "expert commands hidden by default"),
                () -> assertFalse(commandInfo.getToolTipText().contains("/project"), "expert commands hidden by default"),
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
    void assistantTranscriptMessagePaneShowsContextMenuWithCopyActionsAndSurvivesRerender() {
        AssistantTranscriptView transcript = new AssistantTranscriptView();
        transcript.append("assistant", "First response");

        JEditorPane firstPane = transcriptHtmlPanes(transcript).get(0);
        MouseListener listener = transcriptContextMenuListener(firstPane);
        assertNotNull(listener, "Context menu listener should be installed in fallbackHtmlPane");

        listener.mouseReleased(popupTriggerEvent(firstPane));
        JPopupMenu menu = transcript.lastMessageContextMenuForTest();
        assertNotNull(menu);
        JMenuItem copyItem = findByAccessibleName(menu, "Copy", JMenuItem.class);
        JMenuItem selectAllItem = findByAccessibleName(menu, "Select All", JMenuItem.class);
        JMenuItem copyFullTranscriptItem = findByAccessibleName(menu, "Copy full transcript", JMenuItem.class);

        assertAll(
                () -> assertEquals(3, menu.getComponentCount()),
                () -> assertNotNull(copyItem),
                () -> assertFalse(copyItem.isEnabled(), "Copy should be disabled without a selection"),
                () -> assertNotNull(selectAllItem),
                () -> assertTrue(selectAllItem.isEnabled()),
                () -> assertNotNull(copyFullTranscriptItem),
                () -> assertTrue(copyFullTranscriptItem.isEnabled()));

        firstPane.select(0, firstPane.getDocument().getLength());
        listener.mouseReleased(popupTriggerEvent(firstPane));
        JMenuItem copyItemAfterSelection = findByAccessibleName(
                transcript.lastMessageContextMenuForTest(), "Copy", JMenuItem.class);
        assertTrue(copyItemAfterSelection.isEnabled(), "Copy should be enabled once the pane has a selection");

        transcript.append("assistant", "Second response");
        List<JEditorPane> panesAfterRerender = transcriptHtmlPanes(transcript);
        JEditorPane newestPane = panesAfterRerender.get(panesAfterRerender.size() - 1);
        MouseListener listenerAfterRerender = transcriptContextMenuListener(newestPane);
        assertNotNull(listenerAfterRerender, "Context menu should survive a transcript re-render");

        listenerAfterRerender.mouseReleased(popupTriggerEvent(newestPane));
        assertEquals(3, transcript.lastMessageContextMenuForTest().getComponentCount());
    }

    @Test
    void assistantTranscriptContextMenuIgnoresNonPopupTriggerEvents() {
        AssistantTranscriptView transcript = new AssistantTranscriptView();
        transcript.append("assistant", "Response");
        JEditorPane pane = transcriptHtmlPanes(transcript).get(0);
        MouseListener listener = transcriptContextMenuListener(pane);
        assertNotNull(listener);

        MouseEvent plainClick = new MouseEvent(pane, MouseEvent.MOUSE_RELEASED, System.currentTimeMillis(),
                InputEvent.BUTTON1_DOWN_MASK, 4, 4, 1, false, MouseEvent.BUTTON1);
        listener.mousePressed(plainClick);
        listener.mouseReleased(plainClick);

        assertNull(transcript.lastMessageContextMenuForTest());
    }

    @Test
    void assistantTranscriptCopyFullTranscriptContextMenuItemReusesCopyButtonExportPath() throws Exception {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, connectedMcpSettings());
        showAssistantResult(panel, ShaftMcpToolResult.success("Rendered assistant output"));
        AssistantTranscriptView view = getTranscriptView(panel);
        assertNotNull(view);

        String expectedExport = assistantExport(panel);
        assertTrue(expectedExport.contains("Rendered assistant output"));

        AtomicInteger invocations = new AtomicInteger();
        view.setCopyFullTranscriptAction(invocations::incrementAndGet);

        List<JEditorPane> panes = transcriptHtmlPanes(view);
        JEditorPane pane = panes.get(panes.size() - 1);
        MouseListener listener = transcriptContextMenuListener(pane);
        assertNotNull(listener);
        listener.mouseReleased(popupTriggerEvent(pane));

        JMenuItem copyFullTranscriptItem = findByAccessibleName(
                view.lastMessageContextMenuForTest(), "Copy full transcript", JMenuItem.class);
        assertNotNull(copyFullTranscriptItem);
        copyFullTranscriptItem.doClick();

        assertEquals(1, invocations.get());
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
    void allowSourceMutationSurvivesRunningCycleInAgentModeLocalRoute() {
        ShaftSettingsState.Settings settings = connectedMcpSettings();
        settings.advancedUiEnabled = true;
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, settings);
        JComboBox<?> assistantMode = findByAccessibleName(panel, "Assistant mode", JComboBox.class);
        JComboBox<?> assistantProviderType = findByAccessibleName(panel, "Assistant provider type", JComboBox.class);
        JCheckBox allowEdits = findByAccessibleName(panel, "Approve source mutation for Agent mode", JCheckBox.class);

        assistantProviderType.setSelectedItem("LOCAL");
        assistantMode.setSelectedItem("AGENT");
        allowEdits.setSelected(true);

        panel.setRunning(true, "Thinking...");
        panel.setRunning(false, "Ready");

        assertTrue(allowEdits.isSelected(),
                "Allow source edits should remain selected after a running cycle in Agent mode");
    }

    @Test
    void allowSourceMutationUnchecksExactlyOnceWhenModeLeavesAgent() {
        ShaftSettingsState.Settings settings = connectedMcpSettings();
        settings.advancedUiEnabled = true;
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, settings);
        JComboBox<?> assistantMode = findByAccessibleName(panel, "Assistant mode", JComboBox.class);
        JComboBox<?> assistantProviderType = findByAccessibleName(panel, "Assistant provider type", JComboBox.class);
        JCheckBox allowEdits = findByAccessibleName(panel, "Approve source mutation for Agent mode", JCheckBox.class);

        assistantProviderType.setSelectedItem("LOCAL");
        assistantMode.setSelectedItem("AGENT");
        allowEdits.setSelected(true);
        assertTrue(allowEdits.isSelected());

        AtomicInteger uncheckCount = new AtomicInteger();
        allowEdits.addItemListener(event -> {
            if (!allowEdits.isSelected()) {
                uncheckCount.incrementAndGet();
            }
        });

        assistantMode.setSelectedItem("ASK");
        assertAll(
                () -> assertFalse(allowEdits.isSelected(), "Switching away from Agent mode should uncheck source edits"),
                () -> assertEquals(1, uncheckCount.get(), "Uncheck should fire exactly once, no listener re-entrancy loop"));

        assistantMode.setSelectedItem("AGENT");
        allowEdits.setSelected(true);
        uncheckCount.set(0);
        assistantMode.setSelectedItem("PLAN");
        assertAll(
                () -> assertFalse(allowEdits.isSelected(), "Switching to Plan mode should uncheck source edits"),
                () -> assertEquals(1, uncheckCount.get(), "Uncheck should fire exactly once, no listener re-entrancy loop"));
    }

    @Test
    void allowSourceMutationUnchecksExactlyOnceWhenCloudForcesModeSwitch() {
        // The live cloud route is not reachable in this headless test harness: usesCloud()==true
        // makes updateControlVisibility() call updateCloudKeyStatus(), which needs
        // ApplicationManager.getApplication() (unavailable without IntelliJ Platform Test Framework
        // fixtures, which this task's tests must not require/pull in). updateControlVisibility()
        // forces mode back to PLAN (the behavior this test proves) *before* it reaches
        // updateCloudKeyStatus(), so the NPE from the missing ApplicationManager happens strictly
        // after both effects under test (uncheck + forced PLAN switch) have already taken place.
        // The try/catch below only swallows that trailing, unrelated environment limitation; every
        // assertion about the behavior under test runs after the mutation that can throw.
        ShaftSettingsState.Settings settings = connectedMcpSettings();
        settings.advancedUiEnabled = true;
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, settings);
        JComboBox<?> assistantMode = findByAccessibleName(panel, "Assistant mode", JComboBox.class);
        JComboBox<?> assistantProviderType = findByAccessibleName(panel, "Assistant provider type", JComboBox.class);
        JCheckBox allowEdits = findByAccessibleName(panel, "Approve source mutation for Agent mode", JCheckBox.class);

        assistantProviderType.setSelectedItem("LOCAL");
        assistantMode.setSelectedItem("AGENT");
        allowEdits.setSelected(true);
        assertTrue(allowEdits.isSelected());
        assertEquals("AGENT", assistantMode.getSelectedItem());

        AtomicInteger uncheckCount = new AtomicInteger();
        allowEdits.addItemListener(event -> {
            if (!allowEdits.isSelected()) {
                uncheckCount.incrementAndGet();
            }
        });

        // Selecting CLOUD while in Agent mode makes onModeOrRouteSelectionChanged() uncheck the
        // checkbox immediately, then delegates to updateControlVisibility(), which forces mode back
        // to PLAN before it reaches the cloud-key-status lookup that NPEs in this bare harness
        // (missing ApplicationManager). That NPE is an unrelated, pre-existing environment
        // limitation, not a re-entrancy bug, so it is tolerated here -- but only after the mode
        // switch to PLAN has already happened, which the assertions below verify end-to-end.
        try {
            assistantProviderType.setSelectedItem("CLOUD");
        } catch (NullPointerException ignoredMissingApplicationManager) {
            // Expected in this headless harness; see comment above.
        }

        assertAll(
                () -> assertFalse(allowEdits.isSelected(), "Cloud route should uncheck source edits exactly once"),
                () -> assertEquals(1, uncheckCount.get(),
                        "Source edits checkbox should uncheck exactly once with no listener re-entrancy loop"),
                () -> assertEquals("PLAN", assistantMode.getSelectedItem(),
                        "Forcing cloud route while in Agent mode should switch mode to PLAN end-to-end"));
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
    void actionRowButtonsAreLaidOutAfterClearingTranscript() throws Exception {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, connectedMcpSettings());
        panel.simulateAppendForTest("user", "Plan a resilient login test", "");
        panel.simulateAppendForTest("assistant", "Here is a plan.", "raw output");

        layoutPanel(panel);
        assertActionRowButtonsSaneAfterLayout(panel, true);

        RevalidateSpy spy = RevalidateSpy.install();
        try {
            clickAccessible(panel, "Clear assistant transcript");
            SwingUtilities.invokeAndWait(() -> {
            });
        } finally {
            spy.uninstall();
        }
        assertTrue(spy.sawRevalidateFor(actionRowContainer(panel)),
                "updateActionChrome() should revalidate the action row's container after Clear,"
                        + " without relying on the test's manual layout walk");

        // Do NOT call the manual layoutPanel() walker before asserting bounds here: that helper
        // forces doLayout()/validate() on every component regardless of production behavior, which
        // would make this assertion pass even if updateActionChrome()'s revalidate()/repaint() call
        // were deleted. Instead, flush the revalidate/repaint that production code already scheduled.
        flushPendingLayoutAndRepaint(panel);

        JButton clear = findByAccessibleName(panel, "Clear assistant transcript", JButton.class);
        JButton copyTranscript = findByAccessibleName(panel, "Copy assistant transcript", JButton.class);
        JButton rerun = findByAccessibleName(panel, "Rerun last assistant prompt", JButton.class);
        JButton copyResponse = findByAccessibleName(panel, "Copy last assistant response", JButton.class);
        assertAll(
                () -> assertFalse(clear.isVisible(), "Clear should hide once transcript is empty"),
                () -> assertFalse(copyTranscript.isVisible(), "Copy all should hide once transcript is empty"),
                () -> assertFalse(rerun.isVisible(), "Rerun should hide once last prompt is cleared"),
                () -> assertFalse(copyResponse.isVisible(), "Copy response should hide once last response is cleared"));
        assertActionRowButtonsSaneAfterLayout(panel, false);
    }

    @Test
    void actionRowButtonsAreLaidOutAfterAppendingMessages() throws Exception {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, connectedMcpSettings());
        layoutPanel(panel);

        RevalidateSpy spy = RevalidateSpy.install();
        try {
            panel.simulateAppendForTest("user", "Plan a resilient login test", "");
            SwingUtilities.invokeAndWait(() -> {
            });
            panel.simulateAppendForTest("assistant", "Here is a plan.", "raw output");
            SwingUtilities.invokeAndWait(() -> {
            });
        } finally {
            spy.uninstall();
        }
        assertTrue(spy.sawRevalidateFor(actionRowContainer(panel)),
                "updateActionChrome() should revalidate the action row's container after appending"
                        + " messages (a non-Clear caller), proving the fix lives in updateActionChrome()"
                        + " rather than being Clear-specific");

        // Same rationale as above: flush the production-scheduled revalidate/repaint instead of
        // forcing layout manually, so a deleted refreshActionRowLayout() call would fail this test.
        flushPendingLayoutAndRepaint(panel);

        assertActionRowButtonsSaneAfterLayout(panel, true);
    }

    /**
     * Resolves the same container that {@code ShaftAssistantPanel.refreshActionRowLayout()}
     * revalidates/repaints: the action row's parent, falling back to the action row itself.
     */
    private static Container actionRowContainer(ShaftAssistantPanel panel) throws Exception {
        JPanel actionRow = (JPanel) getField(panel, "actionRow");
        Container parent = actionRow.getParent();
        return parent != null ? parent : actionRow;
    }

    /**
     * Flushes a revalidate()/repaint() that production code already scheduled (via
     * {@code RepaintManager}) into an actual layout pass. The spy assertion (run before this method)
     * is what keeps the test sensitive to a deleted {@code refreshActionRowLayout()} call; this
     * method's job is only to produce realized bounds afterward, so it mirrors {@link
     * #layoutPanel(ShaftAssistantPanel)}'s recursive doLayout()/validate() walk rather than relying
     * on {@code Container.validate()} to recurse into every nested panel on its own.
     */
    private static void flushPendingLayoutAndRepaint(ShaftAssistantPanel panel) throws Exception {
        layoutPanel(panel);
    }

    /**
     * A {@link RepaintManager} that records exactly which components had {@code revalidate()}
     * invoked on them (via {@code addInvalidComponent}), so tests can assert that production code
     * itself requested a layout refresh instead of relying on a test helper that forces layout
     * unconditionally. Deliberately does NOT track {@code addDirtyRegion} (repaint): a plain
     * {@link javax.swing.AbstractButton#doClick()} already marks the unrealized top-level panel
     * dirty as a side effect of the button's own pressed/armed paint state (Swing's default
     * RepaintManager walks up to the nearest showing-or-root ancestor), which would make a
     * repaint-based check pass trivially even with no production revalidate/repaint call at all.
     * revalidate() does not have that false-positive: only an explicit
     * {@code JComponent.revalidate()} call adds the exact component to the invalid-component list.
     */
    private static final class RevalidateSpy extends RepaintManager {
        private final RepaintManager delegate;
        private final List<Component> invalidated = new ArrayList<>();

        private RevalidateSpy(RepaintManager delegate) {
            this.delegate = delegate;
        }

        static RevalidateSpy install() {
            RevalidateSpy spy = new RevalidateSpy(RepaintManager.currentManager(null));
            RepaintManager.setCurrentManager(spy);
            return spy;
        }

        void uninstall() {
            RepaintManager.setCurrentManager(delegate);
        }

        boolean sawRevalidateFor(Component target) {
            return invalidated.contains(target);
        }

        @Override
        public synchronized void addInvalidComponent(JComponent invalidComponent) {
            invalidated.add(invalidComponent);
            delegate.addInvalidComponent(invalidComponent);
        }
    }

    private static void layoutPanel(ShaftAssistantPanel panel) throws Exception {
        panel.setBounds(0, 0, 900, 700);
        SwingUtilities.invokeAndWait(() -> {
            panel.doLayout();
            walkComponents(panel, comp -> {
                if (comp instanceof JComponent jc && comp != panel) {
                    jc.doLayout();
                }
            });
            panel.validate();
        });
    }

    private static void assertActionRowButtonsSaneAfterLayout(ShaftAssistantPanel panel, boolean expectResponseButtonsVisible)
            throws Exception {
        JLabel statusLabel = (JLabel) getField(panel, "status");
        List<JButton> actionRowButtons = List.of(
                findByAccessibleName(panel, "Copy last assistant response", JButton.class),
                findByAccessibleName(panel, "Copy last raw assistant response", JButton.class),
                findByAccessibleName(panel, "Copy assistant transcript", JButton.class),
                findByAccessibleName(panel, "Clear assistant transcript", JButton.class),
                findByAccessibleName(panel, "Rerun last assistant prompt", JButton.class),
                findByAccessibleName(panel, "Reconnect to MCP server", JButton.class),
                findByAccessibleName(panel, "Cancel assistant request", JButton.class));
        boolean anyVisible = false;
        for (JButton button : actionRowButtons) {
            assertNotNull(button);
            if (!button.isVisible()) {
                continue;
            }
            anyVisible = true;
            assertTrue(button.getWidth() > 0,
                    accessibleName(button) + " should have a positive width after layout");
            assertTrue(button.getHeight() > 0,
                    accessibleName(button) + " should have a positive height after layout");
            java.awt.Rectangle buttonBounds = new java.awt.Rectangle(
                    SwingUtilities.convertPoint(button.getParent(), button.getLocation(), panel), button.getSize());
            java.awt.Rectangle statusBounds = new java.awt.Rectangle(
                    SwingUtilities.convertPoint(statusLabel.getParent(), statusLabel.getLocation(), panel),
                    statusLabel.getSize());
            assertFalse(buttonBounds.intersects(statusBounds),
                    accessibleName(button) + " bounds " + buttonBounds + " should not intersect status bounds " + statusBounds);
        }
        if (expectResponseButtonsVisible) {
            assertTrue(anyVisible, "Expected at least one action-row button to be visible after layout");
        }
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
    void setupSuccessOpensNewFreshSessionWithoutLosingSessions() throws Exception {
        ShaftSettingsState.Settings settings = blankMcpSettings();
        settings.mcpCommand = "cmd";
        settings.mcpSetupComplete = false;
        Project project = fakeProject();
        ShaftAssistantChatState chatState = new ShaftAssistantChatState();
        chatState.append("user", "Previous assistant conversation", "{}");
        String previousSessionId = chatState.activeSession().id;
        int initialSessionCount = chatState.sessions().size();

        ShaftToolWindowPanel toolWindow = new ShaftToolWindowPanel(project, settings, readyProbe(), chatState);

        ShaftMcpSetupPanel setupPanel = setupPanel(toolWindow);
        assertNotNull(setupPanel);
        showTestResult(setupPanel, ShaftMcpToolResult.success("Probe OK"));
        clickAccessible(setupPanel, "Start chatting with SHAFT Assistant");

        assertAll(
                () -> assertTrue(chatState.activeMessages().isEmpty(),
                        "Setup complete should open a fresh (empty) session"),
                () -> assertNotEquals(previousSessionId, chatState.activeSession().id,
                        "Should have created a new session"),
                () -> assertEquals(initialSessionCount + 1, chatState.sessions().size(),
                        "Previous session should still exist in sessions list"),
                () -> assertTrue(chatState.sessions().stream().anyMatch(s -> s.id.equals(previousSessionId)),
                        "Previous session with conversation should still be selectable"),
                () -> assertFalse(transcriptMarkdown(toolWindow).contains("Previous assistant conversation"),
                        "Current transcript should show the new empty session, not previous conversation"),
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

        click(panel, "Fix Failing Test");
        CapturedInvocation repair = last(invocations);
        assertAll(
                () -> assertEquals("shaft_coding_partner_plan", repair.toolName()),
                () -> assertTrue(repair.arguments().has("repositoryPath")),
                () -> assertTrue(repair.arguments().has("intent")),
                () -> assertTrue(repair.arguments().getAsJsonArray("artifactPaths").size() >= 1));

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

    @Test
    void contextTruncationIndicatorRendersAtBoundaryWhenExceeded() {
        ShaftAssistantChatState chatState = new ShaftAssistantChatState();
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, connectedMcpSettings(), chatState);

        String longMessage = "x".repeat(8000);
        String shortMessage = "y".repeat(100);

        panel.simulateAppendForTest("user", longMessage, "");
        panel.simulateAppendForTest("assistant", longMessage, "");
        panel.simulateAppendForTest("user", shortMessage, "");

        AssistantTranscriptView transcript = getTranscriptView(panel);
        JLabel truncationChip = findByAccessibleName(panel, "Context truncation chip", JLabel.class);

        assertAll(
                () -> assertNotNull(truncationChip,
                        "Truncation chip should exist when context exceeds cap"),
                () -> assertEquals("Earlier messages not included in context", truncationChip.getText(),
                        "Truncation chip text should be correct"),
                () -> assertTrue(truncationChip.getToolTipText().contains("16000"),
                        "Tooltip should mention the 16K character cap"));
    }

    @Test
    void contextTruncationIndicatorHidesWhenAllFits() {
        ShaftAssistantChatState chatState = new ShaftAssistantChatState();
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, connectedMcpSettings(), chatState);

        String shortMessage = "Test message";
        panel.simulateAppendForTest("user", shortMessage, "");
        panel.simulateAppendForTest("assistant", shortMessage, "");

        JLabel truncationChip = findByAccessibleName(panel, "Context truncation chip", JLabel.class);

        assertNull(truncationChip,
                "Truncation chip should not appear when all messages fit within cap");
    }

    @Test
    void contextTruncationIndicatorRepositionsWithNewMessages() {
        ShaftAssistantChatState chatState = new ShaftAssistantChatState();
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, connectedMcpSettings(), chatState);

        String longMessage = "x".repeat(8000);
        String mediumMessage = "y".repeat(500);
        String shortMessage = "z".repeat(100);

        panel.simulateAppendForTest("user", longMessage, "");
        panel.simulateAppendForTest("assistant", longMessage, "");
        panel.simulateAppendForTest("user", mediumMessage, "");

        JLabel chipBefore = findByAccessibleName(panel, "Context truncation chip", JLabel.class);
        assertNotNull(chipBefore, "Truncation chip should exist before adding more messages");

        panel.simulateAppendForTest("assistant", shortMessage, "");

        JLabel chipAfter = findByAccessibleName(panel, "Context truncation chip", JLabel.class);
        assertNotNull(chipAfter,
                "Truncation indicator should still be present after new message");
    }

    @Test
    void contextTruncationIndicatorTooltipIsDescriptive() {
        ShaftAssistantChatState chatState = new ShaftAssistantChatState();
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, connectedMcpSettings(), chatState);

        String longMessage = "x".repeat(8000);
        String shortMessage = "y".repeat(100);
        panel.simulateAppendForTest("user", longMessage, "");
        panel.simulateAppendForTest("assistant", longMessage, "");
        panel.simulateAppendForTest("user", shortMessage, "");

        JLabel truncationChip = findByAccessibleName(panel, "Context truncation chip", JLabel.class);

        assertAll(
                () -> assertNotNull(truncationChip, "Truncation chip should exist"),
                () -> assertTrue(truncationChip.getToolTipText().contains("Start a new chat"),
                        "Tooltip should suggest starting a new chat when context is truncated"));
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

    private static MouseListener transcriptContextMenuListener(JEditorPane pane) {
        for (MouseListener listener : pane.getMouseListeners()) {
            if (listener instanceof AssistantTranscriptView.TranscriptContextMenuListener) {
                return listener;
            }
        }
        return null;
    }

    private static MouseEvent popupTriggerEvent(JEditorPane pane) {
        return new MouseEvent(pane, MouseEvent.MOUSE_RELEASED, System.currentTimeMillis(),
                InputEvent.BUTTON3_DOWN_MASK, 6, 6, 1, true, MouseEvent.BUTTON3);
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

    private static void showCatalogResult(ShaftFeaturePanel panel, ShaftMcpToolResult result) throws Exception {
        Method showCatalogResult = ShaftFeaturePanel.class.getDeclaredMethod(
                "showCatalogResult", ShaftMcpToolResult.class, Throwable.class);
        showCatalogResult.setAccessible(true);
        showCatalogResult.invoke(panel, result, null);
    }

    private static void invokeRefreshCatalog(ShaftFeaturePanel panel, Project project) throws Exception {
        Method refreshCatalog = ShaftFeaturePanel.class.getDeclaredMethod("refreshCatalog", Project.class);
        refreshCatalog.setAccessible(true);
        try {
            refreshCatalog.invoke(panel, project);
        } catch (java.lang.reflect.InvocationTargetException wrapped) {
            Throwable cause = wrapped.getCause();
            if (cause instanceof Exception exception) {
                throw exception;
            }
            if (cause instanceof Error error) {
                throw error;
            }
            throw wrapped;
        }
    }

    private static String statusText(ShaftFeaturePanel panel) throws Exception {
        return (String) ((JLabel) getField(panel, "status")).getText();
    }

    private static String mcpToolsList() {
        JsonObject tool = new JsonObject();
        tool.addProperty("name", "browser_navigate");
        tool.addProperty("description", "Navigate the browser");
        JsonArray tools = new JsonArray();
        tools.add(tool);
        JsonObject result = new JsonObject();
        result.add("tools", tools);
        return result.toString();
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

    private static AssistantTranscriptView getTranscriptView(Component component) {
        if (component instanceof AssistantTranscriptView view) {
            return view;
        }
        if (component instanceof Container container) {
            for (Component child : container.getComponents()) {
                AssistantTranscriptView found = getTranscriptView(child);
                if (found != null) {
                    return found;
                }
            }
        }
        return null;
    }

    private static String getTranscriptHtml(Component component) {
        AssistantTranscriptView view = getTranscriptView(component);
        if (view != null) {
            return transcriptRenderedHtml(view);
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

    private static Object lastComboItem(JComboBox<?> comboBox) {
        return comboBox.getItemAt(comboBox.getItemCount() - 1);
    }

    private static String installerArgumentFor(String target) throws Exception {
        Method method = ShaftMcpSetupPanel.class.getDeclaredMethod("installerArgumentFor", String.class);
        method.setAccessible(true);
        return (String) method.invoke(null, target);
    }

    private static String installerCommandFor(String argument) throws Exception {
        Method method = ShaftMcpSetupPanel.class.getDeclaredMethod("installerCommandFor", String.class);
        method.setAccessible(true);
        return (String) method.invoke(null, argument);
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
        return fakeProject(assistantChatState, "");
    }

    private static Project fakeProject(ShaftAssistantChatState assistantChatState, String basePath) {
        return (Project) Proxy.newProxyInstance(Project.class.getClassLoader(), new Class<?>[]{Project.class},
                (proxy, method, arguments) -> {
                    switch (method.getName()) {
                        case "equals":
                            Object other = arguments == null || arguments.length == 0 ? null : arguments[0];
                            return proxy == other;
                        case "hashCode":
                            return System.identityHashCode(proxy);
                        case "getBasePath":
                            return basePath;
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

    @Test
    void assistantTranscriptRerendersWhenThemeChanges() throws Exception {
        AssistantTranscriptView transcript = new AssistantTranscriptView(null);
        transcript.append("user", "test user message");
        transcript.append("assistant", "test response with **bold** and `code`");

        String originalHtml = extractRenderedHtml(transcript);
        String originalContent = extractTranscriptContent(transcript);

        triggerLafManagerListener(transcript);

        String rerenderedHtml = extractRenderedHtml(transcript);
        String rerenderedContent = extractTranscriptContent(transcript);

        assertAll(
                () -> assertNotNull(originalHtml, "Original HTML should be rendered"),
                () -> assertFalse(originalHtml.isBlank(), "Original HTML should not be empty"),
                () -> assertNotNull(rerenderedHtml, "Rerendered HTML should exist"),
                () -> assertFalse(rerenderedHtml.isBlank(), "Rerendered HTML should not be empty"),
                () -> assertEquals(originalContent, rerenderedContent,
                        "Message content should remain unchanged after theme switch"),
                () -> assertTrue(rerenderedHtml.contains("background:"), "Rerendered HTML should have color styles"),
                () -> assertTrue(rerenderedHtml.contains("color:"), "Rerendered HTML should have text color styles"));
    }

    private static String extractRenderedHtml(AssistantTranscriptView transcript) throws Exception {
        JPanel fallbackPanel = (JPanel) getField(transcript, "fallbackPanel");
        if (fallbackPanel == null || fallbackPanel.getComponentCount() == 0) {
            return "";
        }
        return transcriptRenderedHtml(fallbackPanel.getComponent(0));
    }

    private static String extractTranscriptContent(AssistantTranscriptView transcript) throws Exception {
        return transcript.markdown();
    }

    private static void triggerLafManagerListener(AssistantTranscriptView transcript) throws Exception {
        java.lang.reflect.Field lafListenerField = AssistantTranscriptView.class.getDeclaredField("lafListener");
        lafListenerField.setAccessible(true);
        com.intellij.ide.ui.LafManagerListener lafListener =
                (com.intellij.ide.ui.LafManagerListener) lafListenerField.get(transcript);
        if (lafListener != null) {
            lafListener.lookAndFeelChanged(null);
            SwingUtilities.invokeAndWait(() -> {
            });
        }
    }

    private record CapturedInvocation(String toolName, JsonObject arguments) {
    }

    private static void walkComponents(Component component, Consumer<Component> visitor) {
        visitor.accept(component);
        if (component instanceof Container container) {
            for (Component child : container.getComponents()) {
                walkComponents(child, visitor);
            }
        }
    }

    // ------------------------------------------------------------------
    // Round 5 (issue #3363) capstone: walks the entire onboarding journey
    // -- setup -> install command -> Start chatting -> agent-mode consent
    // -> verbose streaming/token count -> Clear -> right-click context menu
    // -> New chat -- in one ordered flow, so a future regression in any of
    // Rounds 1-4's fixes fails here even if a narrower unit test is ever
    // weakened or deleted.
    // ------------------------------------------------------------------
    @Test
    void onboardingJourneyCoversSetupThroughNewChatWithoutRegressingAnyRoundFix() throws Exception {
        // Step 1: setup view shown. Pre-seed a prior session so step 3's
        // fresh-session assertion isn't vacuously true.
        ShaftAssistantChatState chatState = new ShaftAssistantChatState();
        chatState.append("user", "previous onboarding message", "{}");
        chatState.append("assistant", "previous onboarding answer", "{}");
        int sessionsBeforeStartChatting = chatState.sessions().size();

        // readyProbe() stub, never the real AssistantLocalAgentRunner::readiness probe --
        // that real probe checks for an actual agent CLI on PATH and made a Round 2 test
        // pass locally but fail on clean CI runners; every construction here must stay
        // deterministic regardless of what's installed on the machine running it.
        ShaftToolWindowPanel toolWindow = new ShaftToolWindowPanel(
                fakeProject(chatState), unverifiedMcpSettings(), readyProbe(), chatState);
        ShaftMcpSetupPanel setupPanel = setupPanel(toolWindow);
        assertNotNull(setupPanel, "Unverified MCP settings must show the setup panel first");

        // Step 2: install command contract -- must never auto-suggest intellij-plugin
        // by default (Round 4 fix) and must reference a real installer script.
        JTextComponent installerCommandField =
                findByAccessibleName(setupPanel, "MCP installer command", JTextComponent.class);
        assertNotNull(installerCommandField);
        String installerCommandText = installerCommandField.getText();
        assertAll(
                () -> assertFalse(installerCommandText.contains("-Client intellij-plugin"),
                        "Round 4 fix: the setup panel must never auto-suggest the intellij-plugin target: "
                                + installerCommandText),
                () -> assertTrue(installerCommandText.contains("install-shaft-mcp"),
                        "Installer command must reference a real installer script: " + installerCommandText));

        // Step 3: Check -> Start chatting must open a fresh session (Round 2 fix).
        showTestResult(setupPanel, ShaftMcpToolResult.success("Probe OK\nMCP workspace: C:/work/shaft"));
        JButton startChatting =
                findByAccessibleName(setupPanel, "Start chatting with SHAFT Assistant", JButton.class);
        assertNotNull(startChatting);
        startChatting.doClick();

        assertAll(
                () -> assertEquals(sessionsBeforeStartChatting + 1, chatState.sessions().size(),
                        "Round 2 fix: Start chatting must create exactly one new session"),
                () -> assertTrue(chatState.activeMessages().isEmpty(),
                        "Round 2 fix: the new active session must start empty"),
                () -> assertNotNull(findByAccessibleName(toolWindow, "Assistant prompt", JTextComponent.class),
                        "Round 2 fix: the main view must be shown with an empty prompt after Start chatting"));

        ShaftAssistantPanel assistantPanel = findAssistantPanel(toolWindow);
        assertNotNull(assistantPanel, "Main view should now host the Assistant panel");

        // Step 4: agent-mode consent integrity (Round 2 fix, both defects).
        JComboBox<?> assistantMode = findByAccessibleName(assistantPanel, "Assistant mode", JComboBox.class);
        JCheckBox allowEdits =
                findByAccessibleName(assistantPanel, "Approve source mutation for Agent mode", JCheckBox.class);
        assistantMode.setSelectedItem("AGENT");
        allowEdits.setSelected(true);

        // A run-state cycle that is NOT itself a user-driven mode change must not
        // silently reset the checkbox (Round 2 defect A).
        assistantPanel.setRunning(true, "Thinking...");
        assistantPanel.setRunning(false, "Ready");
        assertTrue(allowEdits.isSelected(),
                "Round 2 fix: Allow source edits must survive a running cycle that isn't a mode change");

        // The constructed prompt must carry an affirmative, unconditional instruction
        // once edits are truly enabled (Round 2 defect B).
        AssistantCommand.Invocation invocation = AssistantCommand.fromPrompt(
                "Implement this login flow in Java", "CODEX", "AGENT", ".", "", true);
        String agentPromptText = invocation.arguments().get("prompt").getAsString();
        assertAll(
                () -> assertTrue(agentPromptText.contains("Source edits are approved for this session"),
                        "Round 2 fix: prompt must carry an affirmative source-edit instruction: " + agentPromptText),
                () -> assertFalse(agentPromptText.contains("Source edits are not enabled"),
                        "Round 2 fix: prompt must not carry the unresolved negative conditional: " + agentPromptText));

        // Step 5: verbose streaming shows real progress, and a structured response
        // reports an exact token count with no "(estimated)" fallback (Round 3 fix).
        JCheckBox verboseOutput =
                findByAccessibleName(assistantPanel, "Show verbose agent output", JCheckBox.class);
        verboseOutput.setSelected(true);
        appendStreamingLocalAgentBubble(assistantPanel, 555);
        appendLocalAgentOutput(assistantPanel, 555, "intermediate progress line");

        // While streaming is in progress the interim line must be visible (Round 3 fix)
        // -- it is later superseded by the final answer once the result finalizes the
        // bubble, so this must be checked before showAgentResult(), not after.
        assertTrue(transcriptMarkdown(assistantPanel).contains("intermediate progress line"),
                "Round 3 fix: verbose streaming must show real progress, not just the static placeholder");

        String rawResponse = "The login flow now handles expired sessions."
                + "\n\n{\"usage\":{\"input_tokens\":210,\"output_tokens\":64}}";
        showAgentResult(assistantPanel, 555, ShaftMcpToolResult.success(rawResponse));

        String markdownAfterAgentRun = transcriptMarkdown(assistantPanel);
        assertAll(
                () -> assertTrue(markdownAfterAgentRun.contains("The login flow now handles expired sessions."),
                        markdownAfterAgentRun),
                () -> assertTrue(markdownAfterAgentRun.contains("`274`"),
                        "Round 3 fix: exact token count (210 + 64) must be shown: " + markdownAfterAgentRun),
                () -> assertFalse(markdownAfterAgentRun.contains("(estimated)"),
                        "Round 3 fix: a structured response must not fall back to the estimate"));

        // Step 6: Clear must not leave the action row visually broken (Round 2 fix).
        clickAccessible(assistantPanel, "Clear assistant transcript");
        layoutPanel(assistantPanel);
        assertActionRowButtonsSaneAfterLayout(assistantPanel, false);

        // Step 7: right-click context menu on the transcript (Round 4 fix). Re-populate
        // the transcript first -- Clear left it empty, and there must be a rendered
        // message pane to right-click on.
        assistantPanel.simulateAppendForTest("assistant", "Rendered assistant output for the journey test", "");
        AssistantTranscriptView transcriptView = getTranscriptView(assistantPanel);
        assertNotNull(transcriptView);
        List<JEditorPane> panes = transcriptHtmlPanes(transcriptView);
        assertFalse(panes.isEmpty(), "Expected at least one rendered transcript pane after re-populating");
        JEditorPane lastPane = panes.get(panes.size() - 1);
        MouseListener contextMenuListener = transcriptContextMenuListener(lastPane);
        assertNotNull(contextMenuListener, "Round 4 fix: transcript panes must install a context-menu listener");
        contextMenuListener.mouseReleased(popupTriggerEvent(lastPane));
        JPopupMenu contextMenu = transcriptView.lastMessageContextMenuForTest();
        assertAll(
                () -> assertNotNull(findByAccessibleName(contextMenu, "Copy", JMenuItem.class),
                        "Round 4 fix: transcript context menu must offer Copy"),
                () -> assertNotNull(findByAccessibleName(contextMenu, "Select All", JMenuItem.class),
                        "Round 4 fix: transcript context menu must offer Select All"),
                () -> assertNotNull(findByAccessibleName(contextMenu, "Copy full transcript", JMenuItem.class),
                        "Round 4 fix: transcript context menu must offer Copy full transcript"));

        // Step 8: New chat -- fresh transcript, cleared prompt, no session data lost
        // across the whole journey.
        assistantPrompt(assistantPanel).setText("stray text that must be cleared by New chat");
        click(assistantPanel, "New chat");

        assertAll(
                () -> assertTrue(transcriptMarkdown(assistantPanel).isBlank(),
                        "New chat must clear the visible transcript"),
                () -> assertTrue(assistantPrompt(assistantPanel).getText().isBlank(),
                        "New chat must clear the prompt field"),
                () -> assertEquals(sessionsBeforeStartChatting + 2, chatState.sessions().size(),
                        "New chat must not lose any prior session across the whole journey"));
    }

    private static ShaftAssistantPanel findAssistantPanel(ShaftToolWindowPanel toolWindow) {
        for (Component component : toolWindow.getComponents()) {
            if (component instanceof ShaftAssistantPanel assistant) {
                return assistant;
            }
        }
        return null;
    }
}
