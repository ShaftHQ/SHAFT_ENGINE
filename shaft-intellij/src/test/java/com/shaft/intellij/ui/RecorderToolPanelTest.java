package com.shaft.intellij.ui;

import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import com.shaft.intellij.settings.ShaftSettingsState;
import org.junit.jupiter.api.Test;

import javax.accessibility.AccessibleContext;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.text.JTextComponent;
import java.awt.Component;
import java.awt.Container;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Covers issue #3665 part B: the curated standalone Recorder tab. {@link ShaftFeaturePanelCatalogTest}'s
 * javadoc documents that a real end-to-end {@code ShaftMcpInvocationService.getInstance(project)
 * .startTool(...)} round trip cannot be exercised in this Gradle unit-test JVM (no {@code
 * ApplicationManager.getApplication()} bootstrap) -- this suite follows that same suite's established
 * pattern: pin the guard order (MCP unconfigured / no open project, both resolved before ever reaching
 * the service call) via the status label, and pin the exact argument shape each button would send via
 * {@link RecorderToolPanel}'s package-private {@code capture*Arguments()} builder methods.
 */
class RecorderToolPanelTest {

    @Test
    void quickStartFieldsExposeAccessibleNamesAndSensibleDefaults() {
        RecorderToolPanel panel = new RecorderToolPanel(null, unreadyMcpSettings());

        JTextComponent targetUrl = findByAccessibleName(panel, "Target URL", JTextComponent.class);
        JComboBox<?> browser = findByAccessibleName(panel, "Recorder browser", JComboBox.class);
        JCheckBox headless = findByAccessibleName(panel, "Headless browser", JCheckBox.class);
        JTextComponent outputPath = findByAccessibleName(panel, "Output path", JTextComponent.class);

        assertAll(
                () -> assertNotNull(targetUrl),
                () -> assertEquals("", targetUrl.getText()),
                () -> assertNotNull(browser),
                () -> assertEquals(2, browser.getItemCount(), "Only Chrome and Edge are accepted server-side"),
                () -> assertEquals("Chrome", browser.getItemAt(0)),
                () -> assertEquals("Edge", browser.getItemAt(1)),
                () -> assertEquals("Chrome", browser.getSelectedItem()),
                () -> assertNotNull(headless),
                () -> assertFalse(headless.isSelected(), "Recording should default to a visible browser window"),
                () -> assertNotNull(outputPath),
                () -> assertEquals("recordings/intellij-capture.json", outputPath.getText(),
                        "Must match ToolTemplates.recorder()'s capture_start template default"));
    }

    @Test
    void quickStartButtonsAndStatusLabelAreReachable() {
        RecorderToolPanel panel = new RecorderToolPanel(null, unreadyMcpSettings());

        assertAll(
                () -> assertNotNull(findButton(panel, "Start recording")),
                () -> assertNotNull(findButton(panel, "Stop recording")),
                () -> assertNotNull(findButton(panel, "Check status")),
                () -> assertNotNull(findButton(panel, "Review code")),
                () -> assertNotNull(findByAccessibleName(panel, "Recorder status", javax.swing.JLabel.class)));
    }

    @Test
    void captureStartArgumentsMatchTheCuratedTemplateShape() {
        RecorderToolPanel panel = new RecorderToolPanel(null, unreadyMcpSettings());
        setText(panel, "Target URL", "https://example.com");
        select(findByAccessibleName(panel, "Recorder browser", JComboBox.class), "Edge");
        findByAccessibleName(panel, "Headless browser", JCheckBox.class).setSelected(true);
        setText(panel, "Output path", "recordings/custom.json");

        JsonObject arguments = panel.captureStartArguments();

        assertAll(
                () -> assertEquals("recordings/custom.json", arguments.get("outputPath").getAsString()),
                () -> assertEquals("https://example.com", arguments.get("targetUrl").getAsString()),
                () -> assertEquals("Edge", arguments.get("browser").getAsString()),
                () -> assertTrue(arguments.get("headless").getAsBoolean()),
                () -> assertFalse(arguments.has("sessionGoal"),
                        "capture_start's curated default template has no sessionGoal field"));
    }

    @Test
    void captureStopAndStatusArgumentsMatchTheCuratedTemplateShape() {
        JsonObject stop = RecorderToolPanel.captureStopArguments();
        JsonObject status = RecorderToolPanel.captureStatusArguments();

        assertAll(
                () -> assertFalse(stop.get("discard").getAsBoolean()),
                () -> assertEquals(0, status.entrySet().size(), "capture_status takes no arguments"));
    }

    @Test
    void captureCodeBlocksArgumentsReadTheOutputPathField() {
        RecorderToolPanel panel = new RecorderToolPanel(null, unreadyMcpSettings());
        setText(panel, "Output path", "recordings/custom.json");

        JsonObject arguments = panel.captureCodeBlocksArguments();

        assertAll(
                () -> assertEquals("recordings/custom.json", arguments.get("sessionPath").getAsString()),
                () -> assertEquals("driver", arguments.get("driverVariableName").getAsString()),
                () -> assertFalse(arguments.get("overwrite").getAsBoolean()));
    }

    @Test
    void startButtonReportsUnconfiguredMcpWithoutCrashing() {
        RecorderToolPanel panel = new RecorderToolPanel(null, unreadyMcpSettings());
        javax.swing.JLabel status = findByAccessibleName(panel, "Recorder status", javax.swing.JLabel.class);

        findButton(panel, "Start recording").doClick();

        assertTrue(status.getText().contains("Configure SHAFT MCP"), status.getText());
    }

    @Test
    void startButtonReportsNoOpenProjectOnceMcpIsConfigured() {
        RecorderToolPanel panel = new RecorderToolPanel(null, readyMcpSettings());
        javax.swing.JLabel status = findByAccessibleName(panel, "Recorder status", javax.swing.JLabel.class);

        findButton(panel, "Start recording").doClick();

        assertTrue(status.getText().contains("Open an IntelliJ project"), status.getText());
    }

    @Test
    void stopStatusAndReviewButtonsAlsoGuardOnUnconfiguredMcpWithoutCrashing() {
        RecorderToolPanel panel = new RecorderToolPanel(null, unreadyMcpSettings());
        javax.swing.JLabel status = findByAccessibleName(panel, "Recorder status", javax.swing.JLabel.class);

        findButton(panel, "Stop recording").doClick();
        assertTrue(status.getText().contains("Configure SHAFT MCP"), status.getText());

        findButton(panel, "Check status").doClick();
        assertTrue(status.getText().contains("Configure SHAFT MCP"), status.getText());

        findButton(panel, "Review code").doClick();
        assertTrue(status.getText().contains("Configure SHAFT MCP"), status.getText());
    }

    @Test
    void advancedSectionIsCollapsedByDefaultAndContainsTheEmbeddedFeaturePanel() {
        RecorderToolPanel panel = new RecorderToolPanel(null, unreadyMcpSettings());
        JCheckBox toggle = findByAccessibleName(panel, "Show advanced Recorder tools", JCheckBox.class);

        assertAll(
                () -> assertNotNull(toggle),
                () -> assertFalse(toggle.isSelected(), "Advanced section must default to collapsed"),
                () -> assertFalse(isVisibleInHierarchy(panel.featurePanel()),
                        "Embedded ShaftFeaturePanel must be hidden until Advanced is toggled"),
                () -> assertTrue(containsComponent(panel, panel.featurePanel()),
                        "Embedded ShaftFeaturePanel must still be part of the component tree"));

        toggle.doClick();
        assertAll(
                () -> assertTrue(toggle.isSelected()),
                () -> assertTrue(isVisibleInHierarchy(panel.featurePanel()),
                        "Toggling Advanced must reveal the embedded ShaftFeaturePanel"));
    }

    @Test
    void prefillToolDelegatesToTheEmbeddedFeaturePanel() {
        RecorderToolPanel panel = new RecorderToolPanel(null, unreadyMcpSettings());
        JsonObject arguments = JsonParser.parseString(
                "{\"sessionPath\":\"recordings/intellij-capture.json\"}").getAsJsonObject();

        boolean matched = panel.prefillTool("capture_code_blocks", arguments);

        assertTrue(matched, "capture_code_blocks is a real Recorder-category template");
    }

    @Test
    void prefillToolExpandsAdvancedOnlyForToolsOutsideQuickStart() {
        RecorderToolPanel panel = new RecorderToolPanel(null, unreadyMcpSettings());
        JCheckBox toggle = findByAccessibleName(panel, "Show advanced Recorder tools", JCheckBox.class);
        JsonObject arguments = new JsonObject();
        arguments.addProperty("discard", false);

        // capture_stop is curated in Quick Start: prefilling it must not force Advanced open.
        assertTrue(panel.prefillTool("capture_stop", arguments));
        assertFalse(toggle.isSelected(), "Quick Start tools must not force Advanced open");

        // capture_checkpoint has no Quick Start control: prefilling it must expand Advanced so the
        // user actually sees what got prefilled.
        JsonObject checkpointArguments = new JsonObject();
        checkpointArguments.addProperty("description", "Important flow checkpoint");
        checkpointArguments.addProperty("kind", "USER_MARKER");
        assertTrue(panel.prefillTool("capture_checkpoint", checkpointArguments));
        assertTrue(toggle.isSelected(), "A tool outside Quick Start must expand Advanced so it is visible");
    }

    @Test
    void prefillToolReturnsFalseForAToolNoRecorderTemplateOwns() {
        RecorderToolPanel panel = new RecorderToolPanel(null, unreadyMcpSettings());

        assertFalse(panel.prefillTool("shaft_project_create", new JsonObject()));
    }

    private static ShaftSettingsState.Settings unreadyMcpSettings() {
        return new ShaftSettingsState.Settings();
    }

    private static ShaftSettingsState.Settings readyMcpSettings() {
        ShaftSettingsState.Settings settings = new ShaftSettingsState.Settings();
        settings.mcpCommand = "\"java\" \"@target/shaft-mcp.args\"";
        settings.mcpSetupComplete = true;
        return settings;
    }

    private static boolean containsComponent(Container parent, Component target) {
        for (Component child : parent.getComponents()) {
            if (child.equals(target)) {
                return true;
            }
            if (child instanceof Container container && containsComponent(container, target)) {
                return true;
            }
        }
        return false;
    }

    private static boolean isVisibleInHierarchy(Component component) {
        for (Component current = component; current != null; current = current.getParent()) {
            if (!current.isVisible()) {
                return false;
            }
        }
        return true;
    }

    private static void setText(Component component, String accessibleName, String value) {
        JTextComponent field = findByAccessibleName(component, accessibleName, JTextComponent.class);
        assertNotNull(field, "Missing field: " + accessibleName);
        field.setText(value);
    }

    private static void select(JComboBox<?> comboBox, String label) {
        for (int index = 0; index < comboBox.getItemCount(); index++) {
            if (label.equals(comboBox.getItemAt(index).toString())) {
                comboBox.setSelectedIndex(index);
                return;
            }
        }
        throw new AssertionError("Missing combo item: " + label);
    }

    private static JButton findButton(Component component, String accessibleName) {
        if (component instanceof JButton button && accessibleName.equals(accessibleName(button))) {
            return button;
        }
        if (component instanceof Container container) {
            for (Component child : container.getComponents()) {
                JButton found = findButton(child, accessibleName);
                if (found != null) {
                    return found;
                }
            }
        }
        return null;
    }

    private static <T extends JComponent> T findByAccessibleName(
            Component component, String accessibleName, Class<T> type) {
        if (type.isInstance(component) && accessibleName.equals(accessibleName((JComponent) component))) {
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

    private static String accessibleName(JComponent component) {
        AccessibleContext context = component.getAccessibleContext();
        return context == null ? "" : context.getAccessibleName();
    }
}
