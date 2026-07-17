package com.shaft.intellij.ui;

import com.shaft.intellij.settings.ShaftSettingsState;
import org.junit.jupiter.api.Test;

import javax.swing.JPanel;
import java.lang.reflect.Field;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Covers issue #3694: the notices banner strip (setup notice + fresh-project hint) above the chat
 * header used {@link java.awt.GridLayout}, which -- unlike {@link java.awt.BorderLayout} or
 * {@link javax.swing.BoxLayout} -- reserves layout space for invisible children instead of
 * collapsing them to zero height. In a normal, already-configured, non-fresh project (the
 * everyday "fresh/new-chat Assistant panel" state), both banners are {@code setVisible(false)},
 * yet {@code GridLayout(0, 1)} still reserved two rows' worth of blank height above the
 * "New chat" dropdown, reading as a large empty gap between the panel header and the dropdown.
 */
class ShaftAssistantPanelLayoutTest {

    @Test
    void noticesContainerCollapsesToZeroHeightWhenBothBannersAreHidden() throws ReflectiveOperationException {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(
                null, readySettingsForExistingProject(), ShaftAssistantChatState.getInstance(null));

        JPanel notices = noticesOf(panel);

        assertTrue(notices.getComponentCount() >= 1, "the notices strip should still contain the banner panels");
        assertEquals(0, notices.getPreferredSize().height,
                "both banners are hidden (MCP configured, no project to flag as fresh), so the notices "
                        + "strip must not reserve any height above the chat header");
    }

    /** MCP configured (hides the setup notice) with a {@code null} project (never "fresh", hides that notice too). */
    private static ShaftSettingsState.Settings readySettingsForExistingProject() {
        ShaftSettingsState.Settings settings = new ShaftSettingsState.Settings();
        settings.mcpCommand = "\"java\" \"@target/shaft-mcp.args\"";
        settings.mcpSetupComplete = true;
        return settings;
    }

    private static JPanel noticesOf(ShaftAssistantPanel panel) throws ReflectiveOperationException {
        Field field = ShaftAssistantPanel.class.getDeclaredField("notices");
        field.setAccessible(true); // NOPMD - test-only field injection, matching the established pattern in ShaftPanelSetupTest
        return (JPanel) field.get(panel);
    }
}
