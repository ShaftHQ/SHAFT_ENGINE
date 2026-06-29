package com.shaft.intellij.ui;

import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import com.intellij.ui.components.JBTabbedPane;
import com.intellij.ui.components.JBTextArea;
import com.intellij.openapi.project.Project;
import com.shaft.intellij.settings.ShaftSettingsState;
import org.junit.jupiter.api.Test;

import javax.accessibility.AccessibleContext;
import javax.swing.AbstractButton;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JButton;
import javax.swing.JLabel;
import java.awt.Component;
import java.awt.Container;
import java.lang.reflect.Proxy;
import java.util.ArrayList;
import java.util.List;
import javax.swing.text.JTextComponent;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
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
    void toolWindowShowsWorkflowTabLabels() {
        ShaftToolWindowPanel toolWindow = new ShaftToolWindowPanel(fakeProject(), blankMcpSettings());

        JBTabbedPane tabs = toolWindowTabbedPane(toolWindow);
        assertNotNull(tabs);
        List<String> labels = new ArrayList<>();
        for (int index = 0; index < tabs.getTabCount(); index++) {
            labels.add(tabs.getTitleAt(index));
        }

        assertEquals(List.of("Assistant", "Recorder", "Inspector", "Evidence", "Projects", "Advanced Tools"), labels);
    }

    @Test
    void prefillToolSelectsMatchingWorkflowTabAndCategory() {
        ShaftToolWindowPanel toolWindow = new ShaftToolWindowPanel(fakeProject(), blankMcpSettings());
        JBTabbedPane tabs = toolWindowTabbedPane(toolWindow);
        assertNotNull(tabs);
        JsonObject arguments = JsonParser.parseString("{}").getAsJsonObject();

        toolWindow.prefillTool("capture_start", arguments);
        assertEquals("Recorder", tabs.getTitleAt(tabs.getSelectedIndex()));
        assertEquals("Recorder", selectedCategory(toolWindow));

        toolWindow.prefillTool("mobile_get_accessibility_tree", arguments);
        assertEquals("Inspector", tabs.getTitleAt(tabs.getSelectedIndex()));
        assertEquals("Inspector", selectedCategory(toolWindow));

        toolWindow.prefillTool("doctor_analyze_trace", arguments);
        assertEquals("Evidence", tabs.getTitleAt(tabs.getSelectedIndex()));
        assertEquals("Evidence", selectedCategory(toolWindow));

        toolWindow.prefillTool("shaft_project_create", arguments);
        assertEquals("Projects", tabs.getTitleAt(tabs.getSelectedIndex()));
        assertEquals("Projects", selectedCategory(toolWindow));
    }

    @Test
    void assistantAndToolsControlsExposeAccessibleMetadata() {
        assertAccessibleControls(new ShaftAssistantPanel(null, blankMcpSettings()));
        assertAccessibleControls(new ShaftFeaturePanel(null, blankMcpSettings()));
    }

    private static ShaftSettingsState.Settings blankMcpSettings() {
        ShaftSettingsState.Settings settings = new ShaftSettingsState.Settings();
        settings.mcpCommand = "";
        return settings;
    }

    private static boolean containsText(Component component, String expected) {
        if (component instanceof JLabel label && label.getText() != null && label.getText().contains(expected)) {
            return true;
        }
        if (component instanceof AbstractButton button && button.getText() != null && button.getText().contains(expected)) {
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

    private static String selectedCategory(ShaftToolWindowPanel toolWindow) {
        JBTabbedPane tabs = toolWindowTabbedPane(toolWindow);
        assertNotNull(tabs);
        int selected = tabs.getSelectedIndex();
        if (selected < 0) {
            return "";
        }
        Component selectedComponent = tabs.getComponentAt(selected);
        if (selectedComponent instanceof Container container) {
            JComboBox<?> categorySelector = findCategorySelector(container);
            if (categorySelector != null && categorySelector.getSelectedItem() != null) {
                return categorySelector.getSelectedItem().toString();
            }
        }
        return "";
    }

    private static JBTabbedPane toolWindowTabbedPane(ShaftToolWindowPanel toolWindow) {
        return toolWindow.tabbedPane();
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
        return component instanceof JButton button && button.getText() != null && !button.getClass().getSimpleName().equals("MetalComboBoxButton")
                || component instanceof JComboBox<?>
                || component instanceof JTextComponent;
    }

    private static String accessibleName(JComponent component) {
        AccessibleContext context = component.getAccessibleContext();
        return context == null ? null : context.getAccessibleName();
    }

    private static Project fakeProject() {
        return (Project) Proxy.newProxyInstance(Project.class.getClassLoader(), new Class<?>[]{Project.class},
                (proxy, method, arguments) -> {
                    return switch (method.getName()) {
                        case "equals" -> proxy == arguments[0];
                        case "hashCode" -> System.identityHashCode(proxy);
                        case "getBasePath" -> "";
                        case "getName" -> "SHAFT test project";
                        default -> defaultValue(method.getReturnType());
                    };
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
}
