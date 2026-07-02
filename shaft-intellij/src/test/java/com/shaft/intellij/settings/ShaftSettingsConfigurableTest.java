package com.shaft.intellij.settings;

import org.junit.jupiter.api.Test;

import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JCheckBox;
import javax.swing.JLabel;
import javax.swing.JPasswordField;
import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
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
        assertTrue(source.contains("Install or update SHAFT MCP"));
        assertTrue(source.contains("Connect selected runtime MCP"));
        assertTrue(source.contains("Connect GitHub Copilot MCP"));
        assertTrue(source.contains("Assistant provider type"));
        assertTrue(source.contains("Assistant family"));
        assertTrue(source.contains("Assistant runtime"));
        assertTrue(source.contains("Current agent configuration"));
        assertTrue(source.contains("Configure assistant agent"));
        assertTrue(source.contains("Enable advanced workflows and provider options"));
        assertTrue(source.contains("github"));
        assertTrue(source.contains("SHAFT AI provider"));
        assertTrue(source.contains("setAccessibleDescription(\"Mark this provider key as ready to clear on apply.\")"));
        assertTrue(source.contains("fill the plugin stdio command, and connect the selected local assistant"));
        assertTrue(source.contains("installForPluginAndClient(installerClientForSelection())"));
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

        JButton installMcp = (JButton) findByAccessibleName(panel, "Install or update SHAFT MCP");
        assertNotNull(installMcp);
        assertIcon(installMcp);
        assertNotNull(installMcp.getAccessibleContext().getAccessibleDescription());
        assertNotEquals(0, installMcp.getAccessibleContext().getAccessibleDescription().length());

        JButton connectCopilot = (JButton) findByAccessibleName(panel, "Connect GitHub Copilot MCP");
        assertNotNull(connectCopilot);
        assertIcon(connectCopilot);
        assertTrue(connectCopilot.isVisible());
        assertNotNull(connectCopilot.getAccessibleContext().getAccessibleDescription());
        assertNotEquals(0, connectCopilot.getAccessibleContext().getAccessibleDescription().length());

        JButton connectRuntime = (JButton) findByAccessibleName(panel, "Connect selected runtime MCP");
        assertNotNull(connectRuntime);
        assertIcon(connectRuntime);
        assertTrue(connectRuntime.isVisible());
        assertNotNull(connectRuntime.getAccessibleContext().getAccessibleDescription());
        assertNotEquals(0, connectRuntime.getAccessibleContext().getAccessibleDescription().length());

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

        collectAllButtons(panel).forEach(ShaftSettingsConfigurableTest::assertIconOnlySymmetric);
    }

    @Test
    void settingsPanelHidesAdvancedControlsByDefault() {
        ShaftSettingsConfigurable configurable = new ShaftSettingsConfigurable(
                new ShaftSettingsState.Settings(), new InMemoryCredentials());
        JComponent panel = (JComponent) configurable.createComponent();

        JCheckBox advanced = findByAccessibleName(panel, "Enable advanced SHAFT UI", JCheckBox.class);
        JButton connectRuntime = findByAccessibleName(panel, "Connect selected runtime MCP", JButton.class);
        JButton connectCopilot = findByAccessibleName(panel, "Connect GitHub Copilot MCP", JButton.class);
        JComboBox<?> providerType = findByAccessibleName(panel, "Assistant provider type", JComboBox.class);
        JComboBox<?> shaftAiProvider = findByAccessibleName(panel, "SHAFT AI provider", JComboBox.class);
        JPasswordField openAiField = findByAccessibleName(panel, "OpenAI API key", JPasswordField.class);
        JButton clearOpenAi = findByAccessibleName(panel, "Clear stored OpenAI API key", JButton.class);

        assertAll(
                () -> assertNotNull(advanced),
                () -> assertFalse(advanced.isSelected()),
                () -> assertTrue(advanced.isVisible()),
                () -> assertFalse(connectRuntime.isVisible()),
                () -> assertFalse(connectCopilot.isVisible()),
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
