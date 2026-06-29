package com.shaft.intellij.settings;

import org.junit.jupiter.api.Test;

import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JPasswordField;
import java.awt.Component;
import java.awt.Container;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

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
        assertTrue(source.contains("Connect GitHub Copilot MCP"));
        assertTrue(source.contains("SHAFT AI provider"));
        assertTrue(source.contains("setAccessibleDescription(\"Mark this provider key as ready to clear on apply.\")"));
        assertTrue(source.contains("Install or update shaft-mcp and fill the plugin stdio command automatically."));
        assertTrue(source.contains("Passing keys exposes them only to the SHAFT MCP process."));
        assertTrue(source.contains("key storage status"));
        assertTrue(source.contains("Stored in Password Safe."));
    }

    @Test
    void settingsPanelExposesCredentialControlsWithAccessibilityMetadata() {
        ShaftSettingsConfigurable configurable = new ShaftSettingsConfigurable(
                new ShaftSettingsState.Settings(), new InMemoryCredentials());
        JComponent panel = (JComponent) configurable.createComponent();

        JButton testMcp = (JButton) findByAccessibleName(panel, "Test MCP");
        assertNotNull(testMcp);
        assertNotNull(testMcp.getAccessibleContext().getAccessibleDescription());
        assertNotEquals(0, testMcp.getAccessibleContext().getAccessibleDescription().length());

        JButton installMcp = (JButton) findByAccessibleName(panel, "Install or update SHAFT MCP");
        assertNotNull(installMcp);
        assertNotNull(installMcp.getAccessibleContext().getAccessibleDescription());
        assertNotEquals(0, installMcp.getAccessibleContext().getAccessibleDescription().length());

        JButton connectCopilot = (JButton) findByAccessibleName(panel, "Connect GitHub Copilot MCP");
        assertNotNull(connectCopilot);
        assertNotNull(connectCopilot.getAccessibleContext().getAccessibleDescription());
        assertNotEquals(0, connectCopilot.getAccessibleContext().getAccessibleDescription().length());

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

        assertNotNull(findByAccessibleName(panel, "OpenAI key storage status"));
        assertNotNull(findByAccessibleName(panel, "Anthropic key storage status"));
        assertNotNull(findByAccessibleName(panel, "Gemini key storage status"));
        assertNotNull(findByAccessibleName(panel, "GitHub key storage status"));
        assertNotNull(findByAccessibleName(panel, "SHAFT AI provider"));
        assertNotNull(findByAccessibleName(panel, "SHAFT AI provider model"));
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
