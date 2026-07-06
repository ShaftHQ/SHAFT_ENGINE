package com.shaft.intellij.settings;

import com.intellij.ui.components.JBTextField;
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
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
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

        collectAllButtons(panel).forEach(ShaftSettingsConfigurableTest::assertIconOnlySymmetric);
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
        boolean modifiedBeforeConfigure = configurable.isModified();

        configure.doClick();

        assertFalse(currentAgent.isVisible());
        assertFalse(configure.isVisible());
        assertTrue(family.isVisible());
        assertTrue(runtime.isVisible());
        assertEquals(modifiedBeforeConfigure, configurable.isModified());
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
                () -> assertEquals(Boolean.TRUE, getField(configurable, "testMcpInFlight")));

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
    void showProbeResultAppliesErrorGlyphThroughRealCompletionPathBeforeReportingTheFailure() throws Exception {
        ShaftSettingsState.Settings settings = new ShaftSettingsState.Settings();
        settings.mcpSetupComplete = false;
        ShaftSettingsConfigurable configurable = new ShaftSettingsConfigurable(settings, new InMemoryCredentials());
        JComponent panel = (JComponent) configurable.createComponent();
        JLabel statusLabel = findByAccessibleName(panel, "SHAFT MCP test status", JLabel.class);
        assertNotNull(statusLabel);
        statusLabel.setText("Testing...");

        // showProbeResult's failure branch reports the outcome via Messages.showErrorDialog(host, ...).
        // This module has no test-only seam (e.g. TestDialogManager.setTestDialog) wired up for that
        // call - there is no existing precedent for stubbing Messages in this suite. The JUnit5 IntelliJ
        // test framework attached to this module's test JVM enforces its own EDT-thread assertion
        // before DialogWrapper ever reaches real AWT/Swing dialog code, so invoking this off the EDT (as
        // any plain JUnit test body runs) surfaces as a real, deterministic IntelliJ threading exception
        // instead of popping a blocking modal. We still invoke the real, private
        // showProbeResult(JPanel, JLabel, ShaftMcpToolResult) method via reflection - not a hand-written
        // stand-in - and the assertions below confirm the ERROR_ICON glyph the method sets on the label
        // (which happens before the dialog call in production code) really came from that invocation.
        InvocationTargetException wrapped = assertThrows(InvocationTargetException.class,
                () -> invokeShowProbeResult(configurable, (JPanel) panel, statusLabel,
                        ShaftMcpToolResult.failure("MCP server process exited.")));

        assertAll(
                () -> assertNotNull(wrapped.getCause()),
                () -> assertTrue(wrapped.getCause() instanceof RuntimeException,
                        "expected the real Messages.showErrorDialog call to fail with a runtime exception, got: "
                                + wrapped.getCause()),
                () -> assertTrue(wrapped.getCause().getMessage() != null
                                && wrapped.getCause().getMessage().contains("Event Dispatch Thread"),
                        "expected IntelliJ's EDT-only threading assertion inside Messages.showErrorDialog, got: "
                                + wrapped.getCause()),
                () -> assertEquals(ShaftStatusPresentation.ERROR_ICON + " Failed", statusLabel.getText()),
                () -> assertFalse(settings.mcpSetupComplete));
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
        private final Set<String> storedKeys = new HashSet<>();

        @Override
        public void setApiKey(String provider, char[] secret) {
            if (secret == null || new String(secret).isBlank()) {
                storedKeys.remove(provider);
            } else {
                storedKeys.add(provider);
            }
        }

        @Override
        public boolean hasApiKey(String provider) {
            return storedKeys.contains(provider);
        }
    }
}
