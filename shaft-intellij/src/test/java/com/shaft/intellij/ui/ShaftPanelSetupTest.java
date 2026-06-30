package com.shaft.intellij.ui;

import com.google.gson.JsonObject;
import com.google.gson.JsonArray;
import com.google.gson.JsonParser;
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
import javax.swing.JLabel;
import java.awt.Component;
import java.awt.Container;
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
                () -> assertTrue(containsText(panel, "Open Settings")));
    }

    @Test
    void toolsExplainMissingMcpConfiguration() {
        ShaftFeaturePanel panel = new ShaftFeaturePanel(null, blankMcpSettings());

        assertAll(
                () -> assertTrue(containsText(panel, "Configure SHAFT MCP")),
                () -> assertTrue(containsText(panel, "Open Settings")));
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
        assertTrue(containsText(toolWindow, "1. Assistant family"));
        assertTrue(containsText(toolWindow, "2. Assistant runtime"));
        assertTrue(containsText(toolWindow, "3. Install or update SHAFT MCP"));
        assertTrue(containsText(toolWindow, "Install / Update SHAFT MCP"));
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
                () -> assertTrue(containsText(panel, "Installed")),
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
                () -> assertFalse(containsText(panel, "Installed")));

        family.setSelectedItem("CODEX");
        assertAll(
                () -> assertFalse(install.isEnabled()),
                () -> assertTrue(test.isEnabled()),
                () -> assertTrue(containsText(panel, "Installed")));

        runtime.setSelectedItem("DESKTOP_APP");
        assertAll(
                () -> assertTrue(install.isEnabled()),
                () -> assertFalse(test.isEnabled()),
                () -> assertFalse(containsText(panel, "Installed")));
    }

    @Test
    void setupPanelShowsClearConnectionSuccess() throws Exception {
        AtomicBoolean connected = new AtomicBoolean();
        ShaftMcpSetupPanel panel = new ShaftMcpSetupPanel(fakeProject(), connectedMcpSettings(), () -> connected.set(true));

        showTestResult(panel, ShaftMcpToolResult.success("Probe OK"));

        assertAll(
                () -> assertTrue(containsText(panel, "Probe OK")),
                () -> assertTrue(containsText(panel, "SUCCESS: Connected to SHAFT MCP.")),
                () -> assertTrue(containsText(panel, "Connected")),
                () -> assertTrue(connected.get()));
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
                () -> assertTrue(containsText(panel, "Copy raw")));
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
        assertAll(
                () -> assertTrue(markdown.contains("public class RecordedFlowTest")),
                () -> assertTrue(markdown.contains("Review before writing files")),
                () -> assertTrue(markdown.contains("`approve`, `okay`, or `generate`")));
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
        click(panel, "Send");

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
        click(panel, "Send");
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
    void assistantTextButtonsDoNotReserveInvisibleIconSpace() {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, blankMcpSettings());

        assertAll(
                () -> assertNull(findButton(panel, "Test connection and start chatting")),
                () -> assertNull(findButton(panel, "Copy response").getIcon()),
                () -> assertNull(findButton(panel, "Copy raw").getIcon()),
                () -> assertNull(findButton(panel, "Copy all").getIcon()),
                () -> assertNull(findButton(panel, "Clear").getIcon()),
                () -> assertNull(findButton(panel, "Rerun").getIcon()),
                () -> assertNull(findButton(panel, "Cancel").getIcon()),
                () -> assertNull(findButton(panel, "Send").getIcon()));
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
    void assistantTranscriptRendersRoleBasedMessageBubbles() {
        AssistantTranscriptView transcript = new AssistantTranscriptView();
        transcript.append("user", "Hello assistant");
        transcript.append("assistant", "Hi user");

        String html = transcript.html();
        assertAll(
                () -> assertTrue(html.contains("class=\"message-row user\"")),
                () -> assertTrue(html.contains("class=\"message-row assistant\"")),
                () -> assertTrue(html.contains("class=\"message-bubble user\"")),
                () -> assertTrue(html.contains("class=\"message-bubble assistant\"")),
                () -> assertTrue(html.contains("cellpadding=\"8\"")),
                () -> assertTrue(html.contains("border=\"0\"")),
                () -> assertTrue(html.contains("bgcolor=")),
                () -> assertTrue(html.contains("background-color:")),
                () -> assertFalse(html.contains("border=\"1\"")),
                () -> assertTrue(html.contains("class=\"message-hint\"")),
                () -> assertFalse(html.contains("border-radius")),
                () -> assertTrue(html.contains("Hello assistant")),
                () -> assertTrue(html.contains("Hi user")),
                () -> assertTrue(html.indexOf("Hi user") < html.indexOf("Type a question")),
                () -> assertTrue(html.contains("align=\"left\"")));
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
        click(blockedPanel, "Send");
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
        click(approvedPanel, "Send");
        assertFalse(transcriptMarkdown(approvedPanel).contains("tick **Allow source edits**"));
    }

    @Test
    void assistantAgentModeDoesNotWarnForBrowserOnlyTasksWhenSourceEditsNotApproved() {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, blankMcpSettings());
        JComboBox<?> blockedMode = findByAccessibleName(panel, "Assistant mode", JComboBox.class);
        assertNotNull(blockedMode);
        blockedMode.setSelectedItem("AGENT");
        assistantPrompt(panel).setText("open https://example.com and update me on the login form");
        click(panel, "Send");

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

        ShaftToolWindowPanel toolWindow = new ShaftToolWindowPanel(project, settings);
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

    private static void showAgentResult(ShaftAssistantPanel panel, ShaftMcpToolResult result) throws Exception {
        Method showResult = ShaftAssistantPanel.class.getDeclaredMethod(
                "showAgentResult", ShaftMcpToolResult.class, Throwable.class);
        showResult.setAccessible(true);
        showResult.invoke(panel, result, null);
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

    private static String assistantExport(ShaftAssistantPanel panel) throws Exception {
        Method method = ShaftAssistantPanel.class.getDeclaredMethod("exportTranscriptWithEvidence");
        method.setAccessible(true);
        return (String) method.invoke(panel);
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

    private static JButton findButton(Component component, String text) {
        if (component instanceof JButton button && text.equals(button.getText())) {
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
