package com.shaft.intellij.ui;

import com.shaft.intellij.settings.ShaftSettingsState;
import org.junit.jupiter.api.Test;

import javax.swing.JComponent;
import javax.swing.JPanel;
import javax.swing.JToggleButton;
import java.awt.Component;
import java.awt.Container;
import java.awt.BorderLayout;
import java.lang.reflect.Field;
import java.util.Arrays;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.assertAll;

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

    /**
     * Issue #4316: {@code currentAgentConfiguration} (the "Claude CLI"-style label) and
     * {@code configure} (the settings gear) used to be added to {@code routeRow} as two bare
     * controls, reading as clutter among the ~13 other dropdowns/checkboxes in that row. They now
     * share one bordered chip, mirroring the {@code allowSourceMutationChip} idiom already used in
     * this file for {@code allowSourceMutation}.
     */
    @Test
    void currentAgentChipGroupsLabelAndGearButtonWhenRouteIsLocked() throws ReflectiveOperationException {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(
                null, readySettingsForExistingProject(), ShaftAssistantChatState.getInstance(null), () -> { });

        JPanel chip = currentAgentChipOf(panel);
        Component label = fieldOf(panel, "currentAgentConfiguration");
        Component gear = fieldOf(panel, "configure");

        assertTrue(chip.isVisible(), "route is locked (MCP configured + setup flow present), so the chip "
                + "grouping the current-agent label and settings gear must be visible");
        assertTrue(containsComponent(chip, label), "chip must contain the current-agent-configuration label");
        assertTrue(containsComponent(chip, gear), "chip must contain the configure/settings gear button");
    }

    /** Same route-locked gate {@code currentAgentConfiguration}/{@code configure} already used individually. */
    @Test
    void currentAgentChipHiddenWhenRouteIsNotLocked() throws ReflectiveOperationException {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(
                null, new ShaftSettingsState.Settings(), ShaftAssistantChatState.getInstance(null));

        JPanel chip = currentAgentChipOf(panel);

        assertFalse(chip.isVisible(), "no setup flow/MCP configuration -- the route is not locked, so the "
                + "grouped chip must stay hidden exactly like the two controls it wraps used to");
    }

    @Test
    void runSettingsDisclosureStartsCollapsedAndKeepsRouteControlsTogether() throws ReflectiveOperationException {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(
                null, readySettingsForExistingProject(), ShaftAssistantChatState.getInstance(null));

        JToggleButton toggle = (JToggleButton) fieldOf(panel, "runSettingsToggle");
        JPanel settings = (JPanel) fieldOf(panel, "runSettingsPanel");
        Component mode = fieldOf(panel, "mode");

        assertAll(
                () -> assertEquals("Run settings", toggle.getAccessibleContext().getAccessibleName()),
                () -> assertFalse(toggle.isSelected(), "the everyday composer must start compact"),
                () -> assertFalse(settings.isVisible(), "route and configuration controls belong behind Run settings"),
                () -> assertTrue(containsDescendant(settings, mode), "mode must remain in the settings disclosure"),
                () -> assertTrue(toggle.getText().contains("CLI"),
                        "the collapsed summary must name the effective agent/runtime"),
                () -> assertTrue(toggle.getText().toLowerCase(java.util.Locale.ROOT).contains("effort"),
                        "the collapsed summary must name the selected effort"));

        toggle.doClick();

        assertTrue(settings.isVisible(), "Run settings must expand with the keyboard-accessible toggle");

        ((javax.swing.JComboBox<?>) mode).setSelectedItem("PLAN");
        assertTrue(toggle.getText().contains("Plan"), "the summary must update with the selected mode");
    }

    @Test
    void expandedRunSettingsUseOneAlignedNativeSettingsGroup() throws ReflectiveOperationException {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(
                null, readySettingsForExistingProject(), ShaftAssistantChatState.getInstance(null));
        JPanel settings = (JPanel) fieldOf(panel, "runSettingsPanel");

        assertTrue(Arrays.stream(settings.getComponents()).allMatch(component -> component instanceof Container row
                        && row.getLayout() instanceof BorderLayout),
                "Run settings must use aligned label/control rows, not independent FlowLayout rows");
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

    private static JPanel currentAgentChipOf(ShaftAssistantPanel panel) throws ReflectiveOperationException {
        return (JPanel) fieldOf(panel, "currentAgentChip");
    }

    private static Component fieldOf(ShaftAssistantPanel panel, String name) throws ReflectiveOperationException {
        Field field = ShaftAssistantPanel.class.getDeclaredField(name);
        field.setAccessible(true); // NOPMD - test-only field injection, matching the established pattern in ShaftPanelSetupTest
        return (Component) field.get(panel);
    }

    private static boolean containsComponent(JComponent container, Component target) {
        return Arrays.stream(container.getComponents()).anyMatch(child -> child == target);
    }

    private static boolean containsDescendant(Container container, Component target) {
        return Arrays.stream(container.getComponents()).anyMatch(child -> child == target
                || child instanceof Container nested && containsDescendant(nested, target));
    }
}
