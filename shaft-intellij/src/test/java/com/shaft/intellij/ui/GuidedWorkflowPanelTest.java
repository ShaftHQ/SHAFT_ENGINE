package com.shaft.intellij.ui;

import com.google.gson.JsonObject;
import org.junit.jupiter.api.Test;

import javax.accessibility.AccessibleContext;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import java.awt.Component;
import java.awt.Container;
import java.util.ArrayList;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

class GuidedWorkflowPanelTest {
    @Test
    void starterTemplatesPrefillSafeWorkflowArguments() {
        List<CapturedInvocation> invocations = new ArrayList<>();
        GuidedWorkflowPanel panel = new GuidedWorkflowPanel(null,
                (toolName, arguments) -> invocations.add(new CapturedInvocation(toolName, arguments)));
        JComboBox<?> templates = findByAccessibleName(panel, "Workflow template", JComboBox.class);
        JButton useTemplate = findButton(panel, "Use template");

        assertNotNull(templates);
        assertNotNull(useTemplate);
        assertEquals(5, templates.getItemCount());

        select(templates, "Record browser flow and generate Page Object code");
        useTemplate.doClick();
        CapturedInvocation capture = last(invocations);
        assertAll(
                () -> assertEquals("test_automation_scenarios", capture.toolName()),
                () -> assertEquals("capture", capture.arguments().get("area").getAsString()),
                () -> assertTrue(capture.arguments().get("intent").getAsString().contains("Page Object")),
                () -> assertTrue(capture.arguments().get("intent").getAsString().contains("Do not write")));

        select(templates, "Analyze failed Allure results");
        useTemplate.doClick();
        CapturedInvocation allure = last(invocations);
        assertAll(
                () -> assertEquals("doctor_analyze_failed_allure", allure.toolName()),
                () -> assertFalse(allure.arguments().get("useAi").getAsBoolean()),
                () -> assertFalse(allure.arguments().get("allowLocalAi").getAsBoolean()),
                () -> assertEquals(0, allure.arguments().getAsJsonArray("allowedSourcePaths").size()));

        select(templates, "Convert Selenium snippet to SHAFT syntax");
        useTemplate.doClick();
        CapturedInvocation selenium = last(invocations);
        assertAll(
                () -> assertEquals("test_automation_scenarios", selenium.toolName()),
                () -> assertEquals("web", selenium.arguments().get("area").getAsString()),
                () -> assertTrue(selenium.arguments().get("intent").getAsString().contains("SHAFT.GUI.WebDriver")),
                () -> assertTrue(selenium.arguments().get("intent").getAsString().contains("review-only")));

        select(templates, "Create a new SHAFT project");
        useTemplate.doClick();
        CapturedInvocation project = last(invocations);
        assertAll(
                () -> assertEquals("shaft_project_create", project.toolName()),
                () -> assertEquals("TestNG", project.arguments().get("runner").getAsString()),
                () -> assertFalse(project.arguments().get("overwrite").getAsBoolean()));

        select(templates, "Inspect current page locators");
        useTemplate.doClick();
        CapturedInvocation dom = last(invocations);
        assertAll(
                () -> assertEquals("browser_get_page_dom", dom.toolName()),
                () -> assertEquals(12_000, dom.arguments().get("maxCharacters").getAsInt()),
                () -> assertFalse(dom.arguments().has("targetUrl")));
    }

    @Test
    void starterTemplateControlsExposeAccessibleMetadata() {
        GuidedWorkflowPanel panel = new GuidedWorkflowPanel(null, (tool, arguments) -> {
        });
        JComboBox<?> templates = findByAccessibleName(panel, "Workflow template", JComboBox.class);
        JButton useTemplate = findButton(panel, "Use template");
        JButton reviewCode = findButton(panel, "Review code");

        assertAll(
                () -> assertNotNull(templates),
                () -> assertFalse(accessibleDescription(templates).isBlank()),
                () -> assertNotNull(useTemplate),
                () -> assertEquals("Use template", accessibleName(useTemplate)),
                () -> assertFalse(accessibleDescription(useTemplate).isBlank()),
                () -> assertNotNull(reviewCode),
                () -> assertTrue(accessibleDescription(reviewCode).contains("reviewed SHAFT code")));
    }

    private static void select(JComboBox<?> comboBox, String label) {
        for (int index = 0; index < comboBox.getItemCount(); index++) {
            Object item = comboBox.getItemAt(index);
            if (label.equals(item.toString())) {
                comboBox.setSelectedIndex(index);
                return;
            }
        }
        throw new AssertionError("Missing combo item: " + label);
    }

    private static CapturedInvocation last(List<CapturedInvocation> invocations) {
        assertFalse(invocations.isEmpty());
        return invocations.get(invocations.size() - 1);
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

    private static String accessibleName(JComponent component) {
        AccessibleContext context = component.getAccessibleContext();
        return context == null ? "" : context.getAccessibleName();
    }

    private static String accessibleDescription(JComponent component) {
        AccessibleContext context = component.getAccessibleContext();
        return context == null || context.getAccessibleDescription() == null
                ? ""
                : context.getAccessibleDescription();
    }

    private record CapturedInvocation(String toolName, JsonObject arguments) {
    }
}
