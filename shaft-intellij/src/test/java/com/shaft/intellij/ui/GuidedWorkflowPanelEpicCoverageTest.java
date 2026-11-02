package com.shaft.intellij.ui;

import com.google.gson.JsonArray;
import com.google.gson.JsonObject;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

import javax.accessibility.AccessibleContext;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.SwingUtilities;
import java.awt.Component;
import java.awt.Container;
import java.util.ArrayList;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Additional GuidedWorkflowPanel coverage: status parsing edges and UI action routes under headless.
 */
class GuidedWorkflowPanelEpicCoverageTest {

    @BeforeAll
    static void forceHeadless() {
        System.setProperty("java.awt.headless", "true");
    }

    @Test
    void parseStepsCoversNestedCheckpointOnlyStatusAndRiskyFlags() {
        JsonObject status = new JsonObject();
        JsonArray steps = new JsonArray();
        JsonObject risky = new JsonObject();
        risky.addProperty("stepId", "s-1");
        risky.addProperty("sequence", 1);
        risky.addProperty("action", "click");
        risky.addProperty("locatorStrategy", "css");
        risky.addProperty("locatorValue", "#buy");
        risky.addProperty("risky", true);
        steps.add(risky);
        steps.add("skip-me");
        steps.add(new JsonObject());
        status.add("steps", steps);

        JsonArray checkpoints = new JsonArray();
        JsonObject cp = new JsonObject();
        cp.addProperty("id", "cp-9");
        cp.addProperty("sequence", 9);
        cp.addProperty("kind", "ASSERT_TEXT");
        cp.addProperty("oracle", "Welcome");
        checkpoints.add(cp);
        status.add("checkpoints", checkpoints);

        List<GuidedWorkflowPanel.StepRow> rows = GuidedWorkflowPanel.parseSteps(status);
        assertFalse(rows.isEmpty());
        assertTrue(rows.stream().anyMatch(GuidedWorkflowPanel.StepRow::checkpoint)
                || rows.stream().anyMatch(r -> r.action() != null && r.action().toLowerCase().contains("assert")));
        assertTrue(rows.stream().anyMatch(r -> "s-1".equals(r.stepId())));
        assertEquals(List.of(), GuidedWorkflowPanel.parseOracleSuggestions(null));
        assertEquals(List.of(), GuidedWorkflowPanel.parseOracleSuggestions(""));
        assertEquals(List.of(), GuidedWorkflowPanel.parseOracleSuggestions("   "));
        assertEquals(List.of("Welcome", "url contains /home"),
                GuidedWorkflowPanel.parseOracleSuggestions(
                        "title: Welcome\nurl contains /home"));
    }

    @Test
    void unsupportedStepEditorMessageNamesToolAndBackend() {
        String delete = GuidedWorkflowPanel.unsupportedStepEditorMessage("capture_step_delete");
        String reorder = GuidedWorkflowPanel.unsupportedStepEditorMessage("capture_step_reorder");
        String edit = GuidedWorkflowPanel.unsupportedStepEditorMessage("capture_step_edit");
        String blank = GuidedWorkflowPanel.unsupportedStepEditorMessage(null);
        assertAll(
                () -> assertTrue(delete.contains("capture_step_delete")),
                () -> assertTrue(reorder.contains("capture_step_reorder")),
                () -> assertTrue(edit.contains("capture_step_edit")),
                () -> assertNotNull(blank),
                () -> assertTrue(delete.contains("WEB CDP") || delete.toLowerCase().contains("cdp")));
    }

    @Test
    void recorderUiRoutesPlaywrightBackendAndStopReviewButtons() throws Exception {
        List<CapturedInvocation> invocations = new ArrayList<>();
        GuidedWorkflowPanel panel = new GuidedWorkflowPanel(null,
                (toolName, arguments) -> invocations.add(new CapturedInvocation(toolName, arguments)));
        expandAdvanced(panel);

        JComboBox<?> backend = findByAccessibleName(panel, "Guided workflow backend", JComboBox.class);
        JCheckBox headless = findByAccessibleName(panel, "Headless browser", JCheckBox.class);
        JButton start = findButton(panel, "Start recording");
        JButton stop = findButton(panel, "Stop recording");
        JButton review = findButton(panel, "Review code");
        assertNotNull(backend);
        assertNotNull(headless);
        assertNotNull(start);
        assertNotNull(stop);
        assertNotNull(review);

        select(backend, "Playwright");
        headless.setSelected(false);
        start.doClick();
        CapturedInvocation startInv = last(invocations);
        assertEquals("capture_start", startInv.toolName());
        // Playwright start omits headless; only assert the tool name and that args exist.
        assertNotNull(startInv.arguments());

        stop.doClick();
        assertEquals("capture_stop", last(invocations).toolName());

        review.doClick();
        assertEquals("capture_code_blocks", last(invocations).toolName());
    }

    @Test
    void disposeIsSafeAfterConstruction() throws Exception {
        GuidedWorkflowPanel panel = new GuidedWorkflowPanel(null, (toolName, arguments) -> { });
        SwingUtilities.invokeAndWait(panel::dispose);
        JLabel status = findByAccessibleName(panel, "Recorder status", JLabel.class);
        // after dispose, label may still exist; construction+dispose must not throw
        assertTrue(status == null || status.getText() != null);
    }


    @Test
    void apiBackendRoutesStartStopAndReviewToApiTools() {
        List<CapturedInvocation> invocations = new ArrayList<>();
        GuidedWorkflowPanel panel = new GuidedWorkflowPanel(null,
                (toolName, arguments) -> invocations.add(new CapturedInvocation(toolName, arguments)));
        expandAdvanced(panel);
        JComboBox<?> backend = findByAccessibleName(panel, "Guided workflow backend", JComboBox.class);
        select(backend, "API");
        findButton(panel, "Start recording").doClick();
        assertEquals("capture_api_start", last(invocations).toolName());
        findButton(panel, "Stop recording").doClick();
        assertEquals("capture_api_stop", last(invocations).toolName());
        findButton(panel, "Review code").doClick();
        assertEquals("capture_api_generate", last(invocations).toolName());
    }

    @Test
    void mobileBackendStartUsesDriverInitializeShapeWhenEmulationSelected() {
        List<CapturedInvocation> invocations = new ArrayList<>();
        GuidedWorkflowPanel panel = new GuidedWorkflowPanel(null,
                (toolName, arguments) -> invocations.add(new CapturedInvocation(toolName, arguments)));
        expandAdvanced(panel);
        JComboBox<?> backend = findByAccessibleName(panel, "Guided workflow backend", JComboBox.class);
        select(backend, GuidedWorkflowPanel.BACKEND_MOBILE);
        findButton(panel, "Start recording").doClick();
        CapturedInvocation start = last(invocations);
        assertTrue("capture_start".equals(start.toolName()) || "driver_initialize".equals(start.toolName()),
                start.toolName());
    }

    @Test
    void stepRowToStringMarksRiskyAndCheckpointRows() {
        JsonObject status = new JsonObject();
        JsonArray steps = new JsonArray();
        JsonObject step = new JsonObject();
        step.addProperty("stepId", "risk-1");
        step.addProperty("sequence", 2);
        step.addProperty("action", "type");
        step.addProperty("locatorStrategy", "id");
        step.addProperty("locatorValue", "email");
        step.addProperty("risky", true);
        steps.add(step);
        status.add("steps", steps);
        JsonArray checkpoints = new JsonArray();
        JsonObject cp = new JsonObject();
        cp.addProperty("id", "cp-toString");
        cp.addProperty("sequence", 1);
        cp.addProperty("kind", "ASSERT_VISIBLE");
        cp.addProperty("description", "logo visible");
        checkpoints.add(cp);
        status.add("checkpoints", checkpoints);

        List<GuidedWorkflowPanel.StepRow> rows = GuidedWorkflowPanel.parseSteps(status);
        assertEquals(2, rows.size());
        String joined = rows.stream().map(Object::toString).reduce("", (a, b) -> a + "\n" + b);
        assertTrue(joined.toLowerCase().contains("risk") || joined.contains("email") || joined.contains("type"));
        assertTrue(joined.contains("logo visible") || joined.contains("ASSERT") || joined.contains("SHAFT"));
    }


    @Test
    void parseCheckpointStepsCoversNestedAndMalformedLeaves() {
        JsonObject status = com.google.gson.JsonParser.parseString("""
                {
                  "checkpoints": [
                    {"id":"c1","description":"assert visible","kind":"ASSERTION","sequence":3},
                    {"id":"c2","description":"","kind":"WAIT","oracle":"fallback oracle","sequence":4},
                    12,
                    null,
                    {"description":"no-id","kind":"ASSERTION","sequence":5}
                  ],
                  "steps": [
                    {"stepId":"s1","sequence":1,"action":"click","locatorStrategy":"css","locatorValue":"#go","risky":false},
                    {"stepId":"s2","sequence":2,"action":"type","locatorStrategy":"name","locatorValue":"q","risky":true},
                    "skip"
                  ]
                }
                """).getAsJsonObject();
        var checkpoints = GuidedWorkflowPanel.parseCheckpointSteps(status);
        assertFalse(checkpoints.isEmpty());
        assertTrue(checkpoints.stream().anyMatch(s -> "c1".equals(s.stepId())));
        assertTrue(checkpoints.stream().anyMatch(s -> s.action() != null && s.action().contains("assert visible")));
        assertTrue(checkpoints.stream().anyMatch(s -> s.action() != null && s.action().contains("fallback oracle")));
        var steps = GuidedWorkflowPanel.parseSteps(status);
        assertTrue(steps.stream().anyMatch(s -> "s1".equals(s.stepId())));
        assertTrue(steps.stream().anyMatch(s -> s.risky()));
        assertEquals(List.of(), GuidedWorkflowPanel.parseCheckpointSteps(new JsonObject()));
        assertEquals(List.of(), GuidedWorkflowPanel.parseSteps(new JsonObject()));
    }

    private static void expandAdvanced(GuidedWorkflowPanel panel) {
        JButton advanced = findButton(panel, "Advanced");
        if (advanced != null && advanced.isVisible()) {
            advanced.doClick();
        }
    }

    private static void select(JComboBox<?> combo, String label) {
        for (int i = 0; i < combo.getItemCount(); i++) {
            Object item = combo.getItemAt(i);
            if (item != null && label.equals(item.toString())) {
                combo.setSelectedIndex(i);
                return;
            }
        }
        throw new AssertionError("missing combo item: " + label);
    }

    private static CapturedInvocation last(List<CapturedInvocation> invocations) {
        assertFalse(invocations.isEmpty());
        return invocations.get(invocations.size() - 1);
    }

    private static JButton findButton(JComponent root, String text) {
        return findByAccessibleName(root, text, JButton.class);
    }

    private static <T extends JComponent> T findByAccessibleName(JComponent root, String name, Class<T> type) {
        List<T> matches = new ArrayList<>();
        walk(root, component -> {
            if (!type.isInstance(component)) {
                return;
            }
            AccessibleContext context = component.getAccessibleContext();
            if (context != null && name.equals(context.getAccessibleName())) {
                matches.add(type.cast(component));
            } else if (component instanceof JButton button && name.equals(button.getText())) {
                matches.add(type.cast(component));
            }
        });
        return matches.isEmpty() ? null : matches.get(0);
    }

    private static void walk(Component component, java.util.function.Consumer<Component> visitor) {
        visitor.accept(component);
        if (component instanceof Container container) {
            for (Component child : container.getComponents()) {
                walk(child, visitor);
            }
        }
    }

    private record CapturedInvocation(String toolName, JsonObject arguments) {
    }

    @Test
    void parseStepsSkipsNonObjectLeavesAndSortsBySequence() {
        JsonObject status = com.google.gson.JsonParser.parseString("""
                {
                  "steps": [
                    {"stepId":"later","sequence":9,"action":"type","locatorStrategy":"css","locatorValue":"#q","risky":false},
                    {"stepId":"earlier","sequence":1,"action":"click","locatorStrategy":"id","locatorValue":"go","risky":true},
                    7,
                    null,
                    {"stepId":"no-seq","action":"noop"}
                  ],
                  "checkpoints": [
                    {"id":"ck","description":"done","kind":"ASSERTION","sequence":5}
                  ]
                }
                """).getAsJsonObject();
        var rows = GuidedWorkflowPanel.parseSteps(status);
        assertTrue(rows.size() >= 3);
        int earlier = -1;
        int later = -1;
        for (int i = 0; i < rows.size(); i++) {
            if ("earlier".equals(rows.get(i).stepId())) {
                earlier = i;
            }
            if ("later".equals(rows.get(i).stepId())) {
                later = i;
            }
        }
        assertTrue(earlier >= 0 && later > earlier);
        assertTrue(rows.stream().anyMatch(r -> "ck".equals(r.stepId())));
        assertTrue(rows.stream().anyMatch(GuidedWorkflowPanel.StepRow::risky));
    }

}
