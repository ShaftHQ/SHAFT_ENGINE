package com.shaft.intellij.settings;

import com.intellij.ui.components.JBTextField;
import com.shaft.intellij.mcp.McpInvocationError;
import com.shaft.intellij.mcp.ShaftMcpToolResult;
import com.shaft.intellij.ui.ShaftStatusPresentation;
import org.junit.jupiter.api.Test;

import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JCheckBox;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JPasswordField;
import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import java.util.function.Function;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

class ShaftSettingsConfigurableTest {
    @Test
    void settingsConfigurableContainsProviderClearAccessibilityAndWarnings() throws Exception {
        String source = Files.readString(Path.of(
                "src/main/java/com/shaft/intellij/settings/ShaftSettingsConfigurable.java"));

        assertFalse(source.isBlank());
        assertTrue(source.contains("Clear stored OpenAI API key"));
        assertTrue(source.contains("Clear stored Anthropic API key"));
        assertTrue(source.contains("Clear stored Gemini API key"));
        assertTrue(source.contains("Clear stored GitHub API key"));
        assertFalse(source.contains("ShaftMcpInstaller"));
        assertFalse(source.contains("Install or update SHAFT MCP"));
        assertFalse(source.contains("Connect selected runtime MCP"));
        assertFalse(source.contains("Connect GitHub Copilot MCP"));
        assertTrue(source.contains("Assistant provider type"));
        assertTrue(source.contains("Assistant family"));
        assertTrue(source.contains("Assistant runtime"));
        assertTrue(source.contains("Current agent configuration"));
        assertTrue(source.contains("Configure assistant agent"));
        assertTrue(source.contains("Enable advanced workflows and provider options"));
        assertTrue(source.contains("github"));
        assertTrue(source.contains("SHAFT AI provider"));
        assertTrue(source.contains("setAccessibleDescription(\"Mark this provider key as ready to clear on apply.\")"));
        assertTrue(source.contains("Visit the SHAFT MCP user guide, install the MCP integration"));
        assertFalse(source.contains("installForPluginAndClient"));
        assertTrue(source.contains("Passing keys exposes them only to the SHAFT MCP process."));
        assertTrue(source.contains("key storage status"));
        assertTrue(source.contains("Stored in Password Safe."));
    }

    @Test
    void settingsPanelExposesCredentialControlsWithAccessibilityMetadata() {
        ShaftSettingsState.Settings settings = new ShaftSettingsState.Settings();
        settings.advancedUiEnabled = true;
        ShaftSettingsConfigurable configurable = new ShaftSettingsConfigurable(
                settings, new InMemoryCredentials());
        JComponent panel = (JComponent) configurable.createComponent();

        assertAll(
                () -> assertTrue(containsText(panel, "Connection")),
                () -> assertTrue(containsText(panel, "Execution")),
                () -> assertTrue(containsText(panel, "Credentials")),
                () -> assertTrue(containsText(panel, "Advanced")));

        JButton testMcp = (JButton) findByAccessibleName(panel, "Test MCP");
        assertNotNull(testMcp);
        assertIcon(testMcp);
        assertNotNull(testMcp.getAccessibleContext().getAccessibleDescription());
        assertNotEquals(0, testMcp.getAccessibleContext().getAccessibleDescription().length());

        JBTextField command = findByAccessibleName(panel, "MCP stdio command", JBTextField.class);
        assertNotNull(command);
        assertNull(findByAccessibleName(panel, "Install or update SHAFT MCP"));
        assertNull(findByAccessibleName(panel, "Connect GitHub Copilot MCP"));
        assertNull(findByAccessibleName(panel, "Connect selected runtime MCP"));
        assertTrue(containsText(panel,
                "Visit the SHAFT MCP user guide, install the MCP integration, paste the stdio command, then test the connection."));

        List<JButton> clearButtons = collectButtons(panel);
        assertEquals(4, clearButtons.size());
        assertTrue(clearButtons.stream().anyMatch(button ->
                "Clear stored OpenAI API key".equals(button.getAccessibleContext().getAccessibleName())));
        assertTrue(clearButtons.stream().anyMatch(button ->
                "Clear stored Anthropic API key".equals(button.getAccessibleContext().getAccessibleName())));
        assertTrue(clearButtons.stream().anyMatch(button ->
                "Clear stored Gemini API key".equals(button.getAccessibleContext().getAccessibleName())));
        assertTrue(clearButtons.stream().anyMatch(button ->
                "Clear stored GitHub API key".equals(button.getAccessibleContext().getAccessibleName())));
        clearButtons.forEach(ShaftSettingsConfigurableTest::assertIcon);

        assertNotNull(findByAccessibleName(panel, "OpenAI key storage status"));
        assertNotNull(findByAccessibleName(panel, "Anthropic key storage status"));
        assertNotNull(findByAccessibleName(panel, "Gemini key storage status"));
        assertNotNull(findByAccessibleName(panel, "GitHub key storage status"));
        assertNotNull(findByAccessibleName(panel, "SHAFT AI provider"));
        assertNotNull(findByAccessibleName(panel, "SHAFT AI provider model"));
        assertNotNull(findByAccessibleName(panel, "Assistant provider type"));
        assertNotNull(findByAccessibleName(panel, "Assistant family"));
        assertNotNull(findByAccessibleName(panel, "Assistant runtime"));

        JComboBox<?> assistantCloudProvider = findByAccessibleName(panel, "Assistant cloud provider", JComboBox.class);
        JBTextField assistantCloudModel = findByAccessibleName(panel, "Assistant cloud model", JBTextField.class);
        JBTextField shaftAiModel = findByAccessibleName(panel, "SHAFT AI provider model", JBTextField.class);
        assertEquals("gemini", assistantCloudProvider.getSelectedItem());
        assertEquals("gemini-3.5-flash", assistantCloudModel.getText());
        assertEquals("Cloud model, for example gemini-3.5-flash", assistantCloudModel.getEmptyText().getText());
        assertEquals("Provider model, for example gemini-3.5-flash", shaftAiModel.getEmptyText().getText());

        for (JButton button : collectAllButtons(panel)) {
            // Recovery actions keep visible labels: which of Retry / Restart MCP server / View logs
            // applies depends on the failure category, which an icon alone cannot convey (#3626).
            if ("SHAFT MCP test recovery action".equals(button.getAccessibleContext().getAccessibleName())) {
                continue;
            }
            assertIconOnlySymmetric(button);
        }
    }

    @Test
    void settingsSectionHeadersAreVisuallyDistinctFromFieldLabels() {
        ShaftSettingsState.Settings settings = new ShaftSettingsState.Settings();
        settings.advancedUiEnabled = true;
        ShaftSettingsConfigurable configurable = new ShaftSettingsConfigurable(
                settings, new InMemoryCredentials());
        JComponent panel = (JComponent) configurable.createComponent();

        JLabel advancedSection = findByAccessibleName(panel, "Advanced settings section", JLabel.class);
        JLabel credentialsSection = findByAccessibleName(panel, "Credentials settings section", JLabel.class);

        assertAll(
                () -> assertNotNull(advancedSection),
                () -> assertNotNull(credentialsSection),
                () -> assertTrue(advancedSection.getFont().isBold(), "section header should be bold"),
                () -> assertTrue(credentialsSection.getFont().isBold(), "section header should be bold"));
    }

    @Test
    void testStatusReflectsProbeOutcomeAndResetsWhenCommandChanges() {
        ShaftSettingsConfigurable configurable = new ShaftSettingsConfigurable(
                new ShaftSettingsState.Settings(), new InMemoryCredentials());
        JComponent panel = (JComponent) configurable.createComponent();

        JLabel status = findByAccessibleName(panel, "SHAFT MCP test status", JLabel.class);
        JBTextField command = findByAccessibleName(panel, "MCP stdio command", JBTextField.class);

        assertAll(
                () -> assertEquals("Not tested", status.getText()),
                () -> assertFalse(status.isEnabled(), "idle status should use the muted disabled look"));

        // Simulate a completed successful test, then confirm editing the command invalidates it again.
        status.setEnabled(true);
        status.setText("Connected");
        status.setForeground(new java.awt.Color(0x0A7F26));
        command.setText(command.getText() + "-edited");

        assertAll(
                () -> assertEquals("Not tested", status.getText()),
                () -> assertFalse(status.isEnabled(), "editing the command should invalidate the prior test result"));
    }

    @Test
    void settingsPanelHidesAdvancedControlsByDefault() {
        ShaftSettingsConfigurable configurable = new ShaftSettingsConfigurable(
                new ShaftSettingsState.Settings(), new InMemoryCredentials());
        JComponent panel = (JComponent) configurable.createComponent();

        JCheckBox advanced = findByAccessibleName(panel, "Enable advanced SHAFT UI", JCheckBox.class);
        JComboBox<?> providerType = findByAccessibleName(panel, "Assistant provider type", JComboBox.class);
        JComboBox<?> shaftAiProvider = findByAccessibleName(panel, "SHAFT AI provider", JComboBox.class);
        JPasswordField openAiField = findByAccessibleName(panel, "OpenAI API key", JPasswordField.class);
        JButton clearOpenAi = findByAccessibleName(panel, "Clear stored OpenAI API key", JButton.class);

        assertAll(
                () -> assertNotNull(advanced),
                () -> assertFalse(advanced.isSelected()),
                () -> assertTrue(advanced.isVisible()),
                () -> assertNull(findByAccessibleName(panel, "Connect selected runtime MCP", JButton.class)),
                () -> assertNull(findByAccessibleName(panel, "Connect GitHub Copilot MCP", JButton.class)),
                () -> assertFalse(providerType.isVisible()),
                () -> assertFalse(shaftAiProvider.isVisible()),
                () -> assertFalse(openAiField.isVisible()),
                () -> assertFalse(clearOpenAi.isVisible()),
                () -> assertTrue(findByAccessibleName(panel, "Assistant family", JComboBox.class).isVisible()),
                () -> assertTrue(findByAccessibleName(panel, "Assistant runtime", JComboBox.class).isVisible()));
    }

    @Test
    void advancedUiFlagIsPersistedBySettingsPanel() throws Exception {
        ShaftSettingsState.Settings settings = new ShaftSettingsState.Settings();
        ShaftSettingsConfigurable configurable = new ShaftSettingsConfigurable(settings, new InMemoryCredentials());
        JComponent panel = (JComponent) configurable.createComponent();

        JCheckBox advanced = findByAccessibleName(panel, "Enable advanced SHAFT UI", JCheckBox.class);
        assertNotNull(advanced);

        advanced.setSelected(true);

        assertTrue(configurable.isModified());
        configurable.apply();
        configurable.reset();

        assertAll(
                () -> assertTrue(settings.advancedUiEnabled),
                () -> assertTrue(advanced.isSelected()));
    }

    @Test
    void settingsShowsConnectedAgentConfigurationReadOnlyUntilConfigureIsClicked() {
        ShaftSettingsState.Settings settings = new ShaftSettingsState.Settings();
        settings.mcpCommand = "\"java\" \"@target/shaft-mcp.args\"";
        settings.mcpSetupComplete = true;
        settings.assistantFamily = "CLAUDE";
        settings.assistantRuntime = "DESKTOP_APP";
        ShaftSettingsConfigurable configurable = new ShaftSettingsConfigurable(settings, new InMemoryCredentials());
        JComponent panel = (JComponent) configurable.createComponent();

        JLabel currentAgent = findByAccessibleName(panel, "Current agent configuration", JLabel.class);
        JButton configure = findByAccessibleName(panel, "Configure assistant agent", JButton.class);
        JComboBox<?> family = findByAccessibleName(panel, "Assistant family", JComboBox.class);
        JComboBox<?> runtime = findByAccessibleName(panel, "Assistant runtime", JComboBox.class);

        assertNotNull(currentAgent);
        assertNotNull(configure);
        assertNotNull(family);
        assertNotNull(runtime);
        assertTrue(currentAgent.isVisible());
        assertTrue(configure.isVisible());
        assertTrue(currentAgent.getText().contains("Agent: Local / Claude / Desktop app"));
        assertFalse(family.isVisible());
        assertFalse(runtime.isVisible());
        // Issue #3603: the accessible name stays the short, stable "Current agent configuration"
        // (test-id-safe), but a screen reader also needs the live agent/runtime text.
        assertEquals(currentAgent.getText(), currentAgent.getAccessibleContext().getAccessibleDescription());
        boolean modifiedBeforeConfigure = configurable.isModified();

        configure.doClick();

        assertFalse(currentAgent.isVisible());
        assertFalse(configure.isVisible());
        assertTrue(family.isVisible());
        assertTrue(runtime.isVisible());
        assertEquals(modifiedBeforeConfigure, configurable.isModified());
    }

    @Test
    void currentAgentConfigurationAccessibleDescriptionTracksLiveConfigurationAcrossUpdates() {
        // Issue #3603 live-update proof: reconfiguring the route must update the description to
        // the NEW live text, not just retain what was captured when the panel was first built.
        ShaftSettingsState.Settings settings = new ShaftSettingsState.Settings();
        settings.mcpCommand = "\"java\" \"@target/shaft-mcp.args\"";
        settings.mcpSetupComplete = true;
        settings.assistantFamily = "CLAUDE";
        settings.assistantRuntime = "DESKTOP_APP";
        ShaftSettingsConfigurable configurable = new ShaftSettingsConfigurable(settings, new InMemoryCredentials());
        JComponent panel = (JComponent) configurable.createComponent();

        JLabel currentAgent = findByAccessibleName(panel, "Current agent configuration", JLabel.class);
        assertNotNull(currentAgent);
        String firstDescription = currentAgent.getAccessibleContext().getAccessibleDescription();
        assertAll(
                () -> assertEquals(currentAgent.getText(), firstDescription),
                () -> assertTrue(firstDescription.contains("Claude"), firstDescription));

        settings.assistantFamily = "CODEX";
        settings.assistantRuntime = "CLI";
        configurable.reset();
        String secondDescription = currentAgent.getAccessibleContext().getAccessibleDescription();

        assertAll(
                () -> assertEquals(currentAgent.getText(), secondDescription),
                () -> assertTrue(secondDescription.contains("Codex"), secondDescription),
                () -> assertNotEquals(firstDescription, secondDescription,
                        "the description must track the live agent configuration after it changes"));
    }

    @Test
    void keyStatusLabelsAccessibleDescriptionsTrackLiveStoredStateAcrossUpdates() {
        // Issue #3605: keyStatusLabel(...) used to hard-code a static, generic accessible
        // description ("Shows whether a key is stored for this provider.") that never reflected
        // the live "Stored in Password Safe."/"No stored key." text -- exactly the anti-pattern
        // this sweep fixes. updateStoredState() is the single choke point all 4 provider labels
        // route through.
        InMemoryCredentials credentials = new InMemoryCredentials();
        ShaftSettingsState.Settings settings = new ShaftSettingsState.Settings();
        ShaftSettingsConfigurable configurable = new ShaftSettingsConfigurable(settings, credentials);
        JComponent panel = (JComponent) configurable.createComponent();

        JLabel openAiStatus = findByAccessibleName(panel, "OpenAI key storage status", JLabel.class);
        JLabel anthropicStatus = findByAccessibleName(panel, "Anthropic key storage status", JLabel.class);
        JLabel geminiStatus = findByAccessibleName(panel, "Gemini key storage status", JLabel.class);
        JLabel githubStatus = findByAccessibleName(panel, "GitHub key storage status", JLabel.class);
        assertAll(
                () -> assertNotNull(openAiStatus),
                () -> assertNotNull(anthropicStatus),
                () -> assertNotNull(geminiStatus),
                () -> assertNotNull(githubStatus));

        // createComponent() (line ~324) already calls reset() internally before returning, so by
        // the time a caller can observe the label, keyStatusLabel(...)'s fleeting "Checking..."
        // constructor state has already been replaced by updateStoredState()'s real result -- here,
        // "No stored key." from the fresh InMemoryCredentials. What matters is that the description
        // already mirrors that live text rather than the old static generic explanation.
        String noKeyDescription = openAiStatus.getAccessibleContext().getAccessibleDescription();
        assertAll(
                () -> assertEquals(openAiStatus.getText(), noKeyDescription),
                () -> assertEquals("No stored key.", noKeyDescription));

        // Live-update proof: storing a key and resetting again must move all 4 descriptions to
        // their NEW "Stored in Password Safe." text, not retain "No stored key.".
        credentials.setApiKey("OPENAI_API_KEY", "sk-test".toCharArray());
        credentials.setApiKey("ANTHROPIC_API_KEY", "sk-test".toCharArray());
        credentials.setApiKey("GEMINI_API_KEY", "sk-test".toCharArray());
        credentials.setApiKey("GITHUB_TOKEN", "sk-test".toCharArray());
        configurable.reset();
        String storedOpenAiDescription = openAiStatus.getAccessibleContext().getAccessibleDescription();
        assertAll(
                () -> assertEquals(openAiStatus.getText(), storedOpenAiDescription),
                () -> assertEquals("Stored in Password Safe.", storedOpenAiDescription),
                () -> assertNotEquals(noKeyDescription, storedOpenAiDescription,
                        "the description must track the live stored-key state after it changes again"),
                () -> assertEquals("Stored in Password Safe.",
                        anthropicStatus.getAccessibleContext().getAccessibleDescription()),
                () -> assertEquals("Stored in Password Safe.",
                        geminiStatus.getAccessibleContext().getAccessibleDescription()),
                () -> assertEquals("Stored in Password Safe.",
                        githubStatus.getAccessibleContext().getAccessibleDescription()));
    }

    @Test
    void testStatusAndTestRecoveryAccessibleDescriptionsTrackLiveProbeOutcomeAcrossUpdates() throws Exception {
        // Issue #3605: testStatus/testRecovery's accessible descriptions must mirror each live
        // update -- "Testing..." while a probe is in flight, the glyph+outcome text once it
        // completes, and "Not tested" again once the command changes and resetTestStatus() fires
        // -- not just the first value captured.
        ShaftSettingsState.Settings settings = new ShaftSettingsState.Settings();
        settings.mcpCommand = "\"" + Path.of("does", "not", "exist", "shaft-mcp-missing.exe") + "\"";
        ShaftSettingsConfigurable configurable = new ShaftSettingsConfigurable(settings, new InMemoryCredentials());
        JComponent panel = (JComponent) configurable.createComponent();

        JButton testMcp = findByAccessibleName(panel, "Test MCP", JButton.class);
        JLabel testStatus = findByAccessibleName(panel, "SHAFT MCP test status", JLabel.class);
        JLabel testRecovery = findByAccessibleName(panel, "SHAFT MCP test recovery", JLabel.class);
        JBTextField command = findByAccessibleName(panel, "MCP stdio command", JBTextField.class);
        assertAll(
                () -> assertNotNull(testMcp),
                () -> assertNotNull(testStatus),
                () -> assertNotNull(testRecovery));

        // Real click drives testMcpConnection()'s synchronous "Testing..." update; the background
        // probe never completes within this synchronous test (mirrors
        // testMcpButtonEntersBusyStateAndBlocksReentryWhileInFlight's established assumption).
        testMcp.doClick();
        String testingDescription = testStatus.getAccessibleContext().getAccessibleDescription();
        assertAll(
                () -> assertEquals("Testing...", testStatus.getText()),
                () -> assertEquals(testStatus.getText(), testingDescription));

        // showProbeResult(...)'s success branch is the real completion path testMcpConnection()
        // hands off to on its background thread; invoked directly here mirrors
        // showProbeResultAppliesSuccessGlyphThroughRealCompletionPath's approach.
        invokeShowProbeResult(configurable, (JPanel) panel, testStatus,
                ShaftMcpToolResult.success("Handshake acknowledged."));
        String connectedDescription = testStatus.getAccessibleContext().getAccessibleDescription();
        assertAll(
                () -> assertEquals(ShaftStatusPresentation.SUCCESS_ICON + " Connected", testStatus.getText()),
                () -> assertEquals(testStatus.getText(), connectedDescription),
                () -> assertNotEquals(testingDescription, connectedDescription,
                        "the description must track the live probe outcome after it changes"));

        // showProbeResult(...)'s failure branch updates both testStatus and testRecovery.
        invokeShowProbeResult(configurable, (JPanel) panel, testStatus,
                ShaftMcpToolResult.failure("MCP server process exited."));
        String failedDescription = testStatus.getAccessibleContext().getAccessibleDescription();
        String recoveryDescription = testRecovery.getAccessibleContext().getAccessibleDescription();
        assertAll(
                () -> assertEquals(ShaftStatusPresentation.ERROR_ICON + " Failed", testStatus.getText()),
                () -> assertEquals(testStatus.getText(), failedDescription),
                () -> assertNotEquals(connectedDescription, failedDescription,
                        "the description must track the live probe outcome after it changes again"),
                () -> assertEquals(testRecovery.getText(), recoveryDescription),
                () -> assertTrue(recoveryDescription.contains("MCP server process exited."), recoveryDescription));

        // resetTestStatus() is the reset path: editing the command must move testStatus back to
        // "Not tested" and keep the description in sync, not stale at "Failed".
        command.setText(command.getText() + "-edited");
        String resetDescription = testStatus.getAccessibleContext().getAccessibleDescription();
        assertAll(
                () -> assertEquals("Not tested", testStatus.getText()),
                () -> assertEquals(testStatus.getText(), resetDescription),
                () -> assertNotEquals(failedDescription, resetDescription,
                        "the description must track the live reset back to \"Not tested\""));
    }

    @Test
    void testRecoveryActionBecomesVisibleAndConfiguredOnFailureThenHidesOnReset() throws Exception {
        ShaftSettingsState.Settings settings = new ShaftSettingsState.Settings();
        settings.mcpCommand = "\"" + Path.of("does", "not", "exist", "shaft-mcp-missing.exe") + "\"";
        ShaftSettingsConfigurable configurable = new ShaftSettingsConfigurable(settings, new InMemoryCredentials());
        JComponent panel = (JComponent) configurable.createComponent();

        JLabel testStatus = findByAccessibleName(panel, "SHAFT MCP test status", JLabel.class);
        JBTextField command = findByAccessibleName(panel, "MCP stdio command", JBTextField.class);
        JButton testRecoveryAction = findByAccessibleName(panel, "SHAFT MCP test recovery action", JButton.class);
        assertNotNull(testRecoveryAction);
        assertFalse(testRecoveryAction.isVisible(), "the recovery action starts hidden until a probe fails");

        // ShaftMcpToolResult.failure(String) (the overload used elsewhere in this file) never sets
        // errorCategory, so the recovery mapping would be non-deterministic; use the explicit
        // categorized overload so this pins PROCESS_EXITED -> "Restart MCP server" deterministically.
        invokeShowProbeResult(configurable, (JPanel) panel, testStatus,
                ShaftMcpToolResult.failure("MCP server process exited.",
                        McpInvocationError.PROCESS_EXITED, "Restart MCP server"));

        assertAll(
                () -> assertTrue(testRecoveryAction.isVisible(), "the recovery action must show on a failed probe"),
                () -> assertEquals("Restart MCP server", testRecoveryAction.getText()));

        // Editing the MCP command mirrors resetTestStatus()'s established trigger (see
        // testStatusReflectsProbeOutcomeAndResetsWhenCommandChanges) and must hide the action again.
        command.setText(command.getText() + "-edited");

        assertFalse(testRecoveryAction.isVisible(), "the recovery action must hide once the command is edited");
    }

    @Test
    void mcpCommandStartsEditableWhenNoWizardValueIsConfiguredYet() {
        ShaftSettingsConfigurable configurable = new ShaftSettingsConfigurable(
                new ShaftSettingsState.Settings(), new InMemoryCredentials());
        JComponent panel = (JComponent) configurable.createComponent();

        JBTextField command = findByAccessibleName(panel, "MCP stdio command", JBTextField.class);
        JCheckBox manualEdit = findByAccessibleName(panel, "Edit MCP command manually", JCheckBox.class);

        assertAll(
                () -> assertTrue(command.isEditable(), "a fresh install has nothing to protect yet"),
                () -> assertFalse(manualEdit.isVisible(), "the toggle only makes sense once there is a managed value"));
    }

    @Test
    void mcpCommandStartsReadOnlyWhenAlreadyWizardConfiguredAndUnlocksViaManualEditToggle() {
        ShaftSettingsState.Settings settings = new ShaftSettingsState.Settings();
        settings.mcpCommand = "\"java\" \"@target/shaft-mcp.args\"";
        ShaftSettingsConfigurable configurable = new ShaftSettingsConfigurable(settings, new InMemoryCredentials());
        JComponent panel = (JComponent) configurable.createComponent();

        JBTextField command = findByAccessibleName(panel, "MCP stdio command", JBTextField.class);
        JCheckBox manualEdit = findByAccessibleName(panel, "Edit MCP command manually", JCheckBox.class);

        assertAll(
                () -> assertFalse(command.isEditable(), "a managed command starts read-only"),
                () -> assertTrue(command.isVisible(), "the configured value must still be visible, only not editable"),
                () -> assertEquals(settings.mcpCommand, command.getText()),
                () -> assertTrue(manualEdit.isVisible()),
                () -> assertFalse(manualEdit.isSelected()));

        manualEdit.doClick();

        assertTrue(command.isEditable(), "checking the toggle should unlock direct editing");
    }

    @Test
    void clearButtonClearsFieldAndFlagsSettingsModified() {
        ShaftSettingsConfigurable configurable = new ShaftSettingsConfigurable(
                new ShaftSettingsState.Settings(), new InMemoryCredentials());
        JComponent panel = (JComponent) configurable.createComponent();

        JPasswordField openAiField = findByAccessibleName(panel, "OpenAI API key", JPasswordField.class);
        JButton clearOpenAi = findByAccessibleName(panel, "Clear stored OpenAI API key", JButton.class);
        assertNotNull(openAiField);
        assertNotNull(clearOpenAi);

        openAiField.setText("candidate-key");
        clearOpenAi.doClick();

        assertEquals(0, openAiField.getPassword().length);
        assertTrue(configurable.isModified());
    }

    @Test
    void testMcpButtonEntersBusyStateAndBlocksReentryWhileInFlight() throws Exception {
        ShaftSettingsState.Settings settings = new ShaftSettingsState.Settings();
        // A non-blank command routes ShaftMcpConnectionProbe.test through its
        // CompletableFuture.supplyAsync branch (a blank command resolves inline on the calling
        // thread instead, which would race this assertion). The executable does not exist, so the
        // background ProcessBuilder.start() call fails almost immediately with an IOException
        // instead of running/waiting on a real MCP handshake - we only need the synchronous busy
        // state that testMcpConnection() sets up before handing off to that background thread.
        settings.mcpCommand = "\"" + Path.of("does", "not", "exist", "shaft-mcp-missing.exe") + "\"";
        ShaftSettingsConfigurable configurable = new ShaftSettingsConfigurable(settings, new InMemoryCredentials());
        JComponent panel = (JComponent) configurable.createComponent();

        JButton testMcp = findByAccessibleName(panel, "Test MCP", JButton.class);
        assertNotNull(testMcp);

        testMcp.doClick();

        assertAll(
                () -> assertFalse(testMcp.isEnabled(), "button should be disabled while the probe is in flight"),
                () -> assertEquals("Testing...", testMcp.getToolTipText()),
                () -> assertEquals(Boolean.TRUE, getField(configurable, "testMcpInFlight")),
                () -> assertFalse(settings.mcpSetupComplete, "must not read ready while a check is in flight"));

        // Re-entry guard: invoking the real completion-guarded method again while
        // testMcpInFlight is still true must be a no-op, not start a second probe.
        invokeTestMcpConnection(configurable);

        assertAll(
                () -> assertFalse(testMcp.isEnabled(), "second click while in flight must not re-enable or restart"),
                () -> assertEquals("Testing...", testMcp.getToolTipText(),
                        "tooltip must still read the busy label after a blocked re-entrant call"),
                () -> assertEquals(Boolean.TRUE, getField(configurable, "testMcpInFlight")));
    }

    @Test
    void showProbeResultAppliesSuccessGlyphThroughRealCompletionPath() throws Exception {
        ShaftSettingsState.Settings settings = new ShaftSettingsState.Settings();
        settings.mcpSetupComplete = false;
        ShaftSettingsConfigurable configurable = new ShaftSettingsConfigurable(settings, new InMemoryCredentials());
        JComponent panel = (JComponent) configurable.createComponent();
        JLabel statusLabel = findByAccessibleName(panel, "SHAFT MCP test status", JLabel.class);
        assertNotNull(statusLabel);
        statusLabel.setText("Testing...");

        invokeShowProbeResult(configurable, (JPanel) panel, statusLabel,
                ShaftMcpToolResult.success("Handshake acknowledged."));

        assertAll(
                () -> assertEquals(ShaftStatusPresentation.SUCCESS_ICON + " Connected", statusLabel.getText()),
                () -> assertTrue(settings.mcpSetupComplete, "a successful probe should mark MCP setup complete"));
    }

    @Test
    void showProbeResultShowsInlineRecoveryTextOnFailure() throws Exception {
        ShaftSettingsState.Settings settings = new ShaftSettingsState.Settings();
        settings.mcpSetupComplete = false;
        ShaftSettingsConfigurable configurable = new ShaftSettingsConfigurable(settings, new InMemoryCredentials());
        JComponent panel = (JComponent) configurable.createComponent();
        JLabel statusLabel = findByAccessibleName(panel, "SHAFT MCP test status", JLabel.class);
        JLabel recoveryLabel = findByAccessibleName(panel, "SHAFT MCP test recovery", JLabel.class);
        assertNotNull(statusLabel);
        assertNotNull(recoveryLabel);
        statusLabel.setText("Testing...");

        // showProbeResult's failure branch now reports the outcome via an inline recovery JLabel
        // (mirroring ShaftMcpSetupPanel's recoveryStatus pattern) instead of a blocking
        // Messages.showErrorDialog(host, ...) call, so the method completes normally.
        invokeShowProbeResult(configurable, (JPanel) panel, statusLabel,
                ShaftMcpToolResult.failure("MCP server process exited."));

        assertAll(
                () -> assertEquals(ShaftStatusPresentation.ERROR_ICON + " Failed", statusLabel.getText()),
                () -> assertFalse(settings.mcpSetupComplete),
                () -> assertTrue(recoveryLabel.isVisible(), "recovery label should be shown on failure"),
                () -> assertTrue(recoveryLabel.getText().contains("MCP server process exited."),
                        "recovery label should contain the failure detail, got: " + recoveryLabel.getText()));
    }

    @Test
    void showProviderKeyResultRendersSuccessGlyphAndColor() throws Exception {
        ShaftSettingsConfigurable configurable = new ShaftSettingsConfigurable(
                new ShaftSettingsState.Settings(), new InMemoryCredentials());
        JComponent panel = (JComponent) configurable.createComponent();
        JLabel openAiStatus = findByAccessibleName(panel, "OpenAI key storage status", JLabel.class);
        assertNotNull(openAiStatus);

        invokeShowProviderKeyResult(openAiStatus, "OpenAI", ProviderKeyProbe.Result.ok());

        assertAll(
                () -> assertTrue(openAiStatus.getText().contains("OK"), openAiStatus.getText()),
                () -> assertEquals(ShaftStatusPresentation.success(), openAiStatus.getForeground()));
    }

    @Test
    void showProviderKeyResultRendersFailureReasonAndColor() throws Exception {
        ShaftSettingsConfigurable configurable = new ShaftSettingsConfigurable(
                new ShaftSettingsState.Settings(), new InMemoryCredentials());
        JComponent panel = (JComponent) configurable.createComponent();
        JLabel openAiStatus = findByAccessibleName(panel, "OpenAI key storage status", JLabel.class);
        assertNotNull(openAiStatus);

        invokeShowProviderKeyResult(openAiStatus, "OpenAI", ProviderKeyProbe.Result.fail("Invalid or expired API key."));

        assertAll(
                () -> assertTrue(openAiStatus.getText().contains("Invalid or expired API key."), openAiStatus.getText()),
                () -> assertEquals(ShaftStatusPresentation.error(), openAiStatus.getForeground()));
    }

    @Test
    void testProviderKeyDoesNotProbeWhenFieldAndStoredKeyAreBlank() throws Exception {
        ShaftSettingsConfigurable configurable = new ShaftSettingsConfigurable(
                new ShaftSettingsState.Settings(), new InMemoryCredentials());
        JComponent panel = (JComponent) configurable.createComponent();
        JPasswordField openAiField = findByAccessibleName(panel, "OpenAI API key", JPasswordField.class);
        JLabel openAiStatus = findByAccessibleName(panel, "OpenAI key storage status", JLabel.class);
        JButton testOpenAi = findByAccessibleName(panel, "Test OpenAI API key", JButton.class);
        assertAll(
                () -> assertNotNull(openAiField),
                () -> assertNotNull(openAiStatus),
                () -> assertNotNull(testOpenAi));

        Function<char[], ProviderKeyProbe.Result> neverRun = key -> {
            throw new AssertionError("probe should not have run");
        };

        invokeTestProviderKey(configurable, "OPENAI_API_KEY", openAiField, openAiStatus, testOpenAi, "OpenAI", neverRun);

        assertAll(
                () -> assertTrue(openAiStatus.getText().contains("No key to test."), openAiStatus.getText()),
                () -> assertTrue(testOpenAi.isEnabled(), "the button must be re-enabled once the check is skipped"));
    }

    @Test
    void testProviderKeyDisablesButtonSynchronouslyWhileProbeInFlight() throws Exception {
        ShaftSettingsConfigurable configurable = new ShaftSettingsConfigurable(
                new ShaftSettingsState.Settings(), new InMemoryCredentials());
        JComponent panel = (JComponent) configurable.createComponent();
        JPasswordField openAiField = findByAccessibleName(panel, "OpenAI API key", JPasswordField.class);
        JLabel openAiStatus = findByAccessibleName(panel, "OpenAI key storage status", JLabel.class);
        JButton testOpenAi = findByAccessibleName(panel, "Test OpenAI API key", JButton.class);
        assertAll(
                () -> assertNotNull(openAiField),
                () -> assertNotNull(openAiStatus),
                () -> assertNotNull(testOpenAi));

        openAiField.setText("candidate-key");

        // The probe function's own behavior does not matter here -- button.setEnabled(false) runs
        // synchronously before any async dispatch, so this assertion holds immediately without
        // waiting for the background probe (mirrors testMcpButtonEntersBusyStateAndBlocksReentryWhileInFlight).
        invokeTestProviderKey(configurable, "OPENAI_API_KEY", openAiField, openAiStatus, testOpenAi, "OpenAI",
                key -> ProviderKeyProbe.Result.ok());

        assertFalse(testOpenAi.isEnabled(), "button should be disabled synchronously once the probe starts");
    }

    @Test
    void testProviderKeyFallsBackToStoredKeyWhenFieldIsBlank() throws Exception {
        InMemoryCredentials credentials = new InMemoryCredentials();
        credentials.setApiKey("OPENAI_API_KEY", "stored-secret".toCharArray());
        ShaftSettingsConfigurable configurable = new ShaftSettingsConfigurable(
                new ShaftSettingsState.Settings(), credentials);
        JComponent panel = (JComponent) configurable.createComponent();
        JPasswordField openAiField = findByAccessibleName(panel, "OpenAI API key", JPasswordField.class);
        JLabel openAiStatus = findByAccessibleName(panel, "OpenAI key storage status", JLabel.class);
        JButton testOpenAi = findByAccessibleName(panel, "Test OpenAI API key", JButton.class);
        assertAll(
                () -> assertNotNull(openAiField),
                () -> assertNotNull(openAiStatus),
                () -> assertNotNull(testOpenAi));
        assertEquals(0, openAiField.getPassword().length, "field must stay blank for the fallback path to trigger");

        String[] capturedKey = new String[1];
        CountDownLatch probeInvoked = new CountDownLatch(1);
        Function<char[], ProviderKeyProbe.Result> capturingProbe = key -> {
            capturedKey[0] = new String(key);
            probeInvoked.countDown();
            return ProviderKeyProbe.Result.ok();
        };

        invokeTestProviderKey(configurable, "OPENAI_API_KEY", openAiField, openAiStatus, testOpenAi, "OpenAI",
                capturingProbe);

        // The stored-key lookup resolves synchronously/inline here (InMemoryCredentials.apiKeyAsync
        // returns an already-completed future, mirroring ShaftCredentialService.apiKeyAsync landing on
        // the EDT), so the label already reads "Testing..." the instant the reflective call above
        // returns. Only the actual probe dispatch runs on ShaftPluginExecutor's background pool, so a
        // bounded latch await -- not a sleep loop -- is used to observe that hand-off deterministically.
        assertEquals("Testing...", openAiStatus.getText());
        assertTrue(probeInvoked.await(5, TimeUnit.SECONDS), "the probe should have run against the stored key");
        assertEquals("stored-secret", capturedKey[0]);
    }

    private static void invokeShowProviderKeyResult(
            JLabel statusLabel,
            String providerLabel,
            ProviderKeyProbe.Result result) throws Exception {
        Method showProviderKeyResult = ShaftSettingsConfigurable.class.getDeclaredMethod(
                "showProviderKeyResult", JLabel.class, String.class, ProviderKeyProbe.Result.class);
        showProviderKeyResult.setAccessible(true); // NOPMD - reflective test invocation of a private rendering helper
        showProviderKeyResult.invoke(null, statusLabel, providerLabel, result);
    }

    private static void invokeTestProviderKey(
            ShaftSettingsConfigurable configurable,
            String providerKey,
            JPasswordField field,
            JLabel statusLabel,
            JButton button,
            String providerLabel,
            Function<char[], ProviderKeyProbe.Result> probeFn) throws Exception {
        Method testProviderKey = ShaftSettingsConfigurable.class.getDeclaredMethod(
                "testProviderKey", String.class, JPasswordField.class, JLabel.class, JButton.class, String.class,
                Function.class);
        testProviderKey.setAccessible(true); // NOPMD - reflective test invocation of a private orchestration method
        testProviderKey.invoke(configurable, providerKey, field, statusLabel, button, providerLabel, probeFn);
    }

    private static void invokeShowProbeResult(
            ShaftSettingsConfigurable configurable,
            JPanel host,
            JLabel statusLabel,
            ShaftMcpToolResult result) throws Exception {
        Method showProbeResult = ShaftSettingsConfigurable.class.getDeclaredMethod(
                "showProbeResult", JPanel.class, JLabel.class, ShaftMcpToolResult.class);
        showProbeResult.setAccessible(true); // NOPMD - reflective test invocation of a private completion handler, matching the established pattern in ShaftPanelSetupTest
        showProbeResult.invoke(configurable, host, statusLabel, result);
    }

    private static void invokeTestMcpConnection(ShaftSettingsConfigurable configurable) throws Exception {
        Method testMcpConnection = ShaftSettingsConfigurable.class.getDeclaredMethod("testMcpConnection");
        testMcpConnection.setAccessible(true); // NOPMD - reflective test invocation of a private completion handler, matching the established pattern in ShaftPanelSetupTest
        testMcpConnection.invoke(configurable);
    }

    private static Object getField(Object target, String name) throws Exception {
        Field field = target.getClass().getDeclaredField(name);
        field.setAccessible(true); // NOPMD - test-only field inspection, matching the established getField/setField helpers in ShaftPanelSetupTest
        return field.get(target);
    }

    private static List<JButton> collectButtons(Component root) {
        List<JButton> buttons = new ArrayList<>();
        if (root instanceof JButton button && button.getAccessibleContext() != null &&
                button.getAccessibleContext().getAccessibleName() != null &&
                button.getAccessibleContext().getAccessibleName().contains("Clear stored ")) {
            buttons.add(button);
        }
        if (root instanceof Container container) {
            for (Component child : container.getComponents()) {
                buttons.addAll(collectButtons(child));
            }
        }
        return buttons;
    }

    private static List<JButton> collectAllButtons(Component root) {
        List<JButton> buttons = new ArrayList<>();
        if (root instanceof JButton button && isShaftOwnedButton(button)) {
            buttons.add(button);
        }
        if (root instanceof Container container) {
            for (Component child : container.getComponents()) {
                buttons.addAll(collectAllButtons(child));
            }
        }
        return buttons;
    }

    private static boolean isShaftOwnedButton(JButton button) {
        return hasText(button.getText())
                || hasText(button.getToolTipText())
                || hasText(button.getAccessibleContext().getAccessibleName());
    }

    private static boolean hasText(String text) {
        return text != null && !text.isBlank();
    }

    private static void assertIcon(JButton button) {
        assertNotNull(button);
        assertNotNull(button.getIcon(), button.getText());
        assertTrue(button.getIcon().getIconWidth() > 0, button.getText());
        assertTrue(button.getIcon().getIconHeight() > 0, button.getText());
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
                () -> assertNotNull(button.getAccessibleContext().getAccessibleName()),
                () -> assertFalse(button.getAccessibleContext().getAccessibleName().isBlank()));
    }

    private static Component findByAccessibleName(Component root, String accessibleName) {
        if (root.getAccessibleContext() != null
                && accessibleName.equals(root.getAccessibleContext().getAccessibleName())) {
            return root;
        }
        if (root instanceof Container container) {
            for (Component child : container.getComponents()) {
                Component found = findByAccessibleName(child, accessibleName);
                if (found != null) {
                    return found;
                }
            }
        }
        return null;
    }

    private static <T extends Component> T findByAccessibleName(Component root, String accessibleName, Class<T> type) {
        if (type.isInstance(root)
                && root.getAccessibleContext() != null
                && accessibleName.equals(root.getAccessibleContext().getAccessibleName())) {
            return type.cast(root);
        }
        if (root instanceof Container container) {
            for (Component child : container.getComponents()) {
                T found = findByAccessibleName(child, accessibleName, type);
                if (found != null) {
                    return found;
                }
            }
        }
        return null;
    }

    private static boolean containsText(Component component, String expected) {
        if (component instanceof JLabel label && label.getText() != null && label.getText().contains(expected)) {
            return true;
        }
        if (component instanceof JButton button && button.getText() != null && button.getText().contains(expected)) {
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

    private static final class InMemoryCredentials implements ShaftSettingsConfigurable.CredentialAccess {
        private final java.util.Map<String, String> storedKeys = new java.util.HashMap<>();

        // Kept as plain (non-override) methods so existing test call sites that talk to this
        // concrete type directly (see keyStatusLabelsAccessibleDescriptionsTrackLiveStoredStateAcrossUpdates)
        // can still exercise the store synchronously without waiting on a future.
        public void setApiKey(String provider, char[] secret) {
            if (secret == null || new String(secret).isBlank()) {
                storedKeys.remove(provider);
            } else {
                storedKeys.put(provider, new String(secret));
            }
        }

        public boolean hasApiKey(String provider) {
            return storedKeys.containsKey(provider);
        }

        @Override
        public CompletableFuture<Void> setApiKeyAsync(String provider, char[] secret) {
            setApiKey(provider, secret);
            return CompletableFuture.completedFuture(null);
        }

        @Override
        public CompletableFuture<Boolean> hasApiKeyAsync(String provider) {
            return CompletableFuture.completedFuture(hasApiKey(provider));
        }

        @Override
        public CompletableFuture<String> apiKeyAsync(String provider) {
            return CompletableFuture.completedFuture(storedKeys.getOrDefault(provider, ""));
        }
    }
}
