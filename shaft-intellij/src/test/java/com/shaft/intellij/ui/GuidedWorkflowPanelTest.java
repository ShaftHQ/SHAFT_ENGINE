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
        expandAdvanced(panel);
        JComboBox<?> templates = findByAccessibleName(panel, "Workflow template", JComboBox.class);
        JButton useTemplate = findButton(panel, "Use template");

        assertNotNull(templates);
        assertNotNull(useTemplate);
        assertEquals(7, templates.getItemCount());

        select(templates, "Weekly flaky triage (maintenance loop)");
        useTemplate.doClick();
        CapturedInvocation weeklyTriage = last(invocations);
        assertAll(
                () -> assertEquals("doctor_analyze_failed_allure", weeklyTriage.toolName()),
                () -> assertEquals("target/shaft-doctor/weekly",
                        weeklyTriage.arguments().get("outputDirectory").getAsString()),
                () -> assertEquals("target/shaft-doctor/history",
                        weeklyTriage.arguments().get("historicalBundlePaths").getAsJsonArray().get(0).getAsString()),
                () -> assertFalse(weeklyTriage.arguments().get("useAi").getAsBoolean()));

        select(templates, "Record browser flow and generate Page Object code");
        useTemplate.doClick();
        CapturedInvocation capture = last(invocations);
        assertAll(
                () -> assertEquals("test_automation_scenarios", capture.toolName()),
                () -> assertEquals("capture", capture.arguments().get("area").getAsString()),
                () -> assertTrue(capture.arguments().get("intent").getAsString().contains("Page Object")),
                () -> assertTrue(capture.arguments().get("intent").getAsString().contains("Do not write")));

        select(templates, "Start mobile emulation session for recording");
        useTemplate.doClick();
        CapturedInvocation mobileEmulation = last(invocations);
        assertAll(
                () -> assertEquals("mobile_initialize_web_emulation", mobileEmulation.toolName()),
                () -> assertEquals("CHROME", mobileEmulation.arguments().get("browser").getAsString()),
                () -> assertEquals("Pixel 5", mobileEmulation.arguments().get("deviceName").getAsString()),
                () -> assertFalse(mobileEmulation.arguments().get("headless").getAsBoolean()));

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
    void recorderButtonsRouteByBackendAndRespectHeadlessToggle() {
        List<CapturedInvocation> invocations = new ArrayList<>();
        GuidedWorkflowPanel panel = new GuidedWorkflowPanel(null,
                (toolName, arguments) -> invocations.add(new CapturedInvocation(toolName, arguments)));
        expandAdvanced(panel);
        JComboBox<?> backend = findByAccessibleName(panel, "Guided workflow backend", JComboBox.class);
        javax.swing.JCheckBox headless = findByAccessibleName(panel, "Headless browser", javax.swing.JCheckBox.class);
        JButton start = findButton(panel, "Start recording");
        JButton stop = findButton(panel, "Stop recording");
        JButton review = findButton(panel, "Review code");
        assertNotNull(backend);
        assertNotNull(headless);
        assertNotNull(start);
        assertNotNull(stop);
        assertNotNull(review);

        assertFalse(headless.isSelected(), "Recording should default to a visible browser window");
        select(backend, "WebDriver");
        start.doClick();
        CapturedInvocation webStart = last(invocations);
        assertAll(
                () -> assertEquals("capture_start", webStart.toolName()),
                () -> assertFalse(webStart.arguments().get("headless").getAsBoolean()));

        headless.setSelected(true);
        start.doClick();
        assertTrue(last(invocations).arguments().get("headless").getAsBoolean());

        select(backend, "Mobile (web emulation)");
        start.doClick();
        CapturedInvocation mobileStart = last(invocations);
        assertAll(
                () -> assertEquals("mobile_record_start", mobileStart.toolName()),
                () -> assertFalse(mobileStart.arguments().get("includeSensitiveValues").getAsBoolean()));

        stop.doClick();
        CapturedInvocation mobileStop = last(invocations);
        assertAll(
                () -> assertEquals("mobile_record_stop", mobileStop.toolName()),
                () -> assertFalse(mobileStop.arguments().get("discard").getAsBoolean()));

        review.doClick();
        CapturedInvocation mobileReview = last(invocations);
        assertAll(
                () -> assertEquals("mobile_recording_code_blocks", mobileReview.toolName()),
                () -> assertEquals("driver", mobileReview.arguments().get("driverVariableName").getAsString()));

        select(backend, "Playwright");
        stop.doClick();
        assertEquals("playwright_record_stop", last(invocations).toolName());
        review.doClick();
        assertEquals("playwright_recording_code_blocks", last(invocations).toolName());
    }

    @Test
    void insertAtCaretAndCreateTestClassGenerateFromTheSameToolAsReviewCode() {
        // Issue #3548 item 1: without a live project (no MCP connection, no open editor -- the
        // state every headless unit test runs in), invocationService() is null and these buttons
        // fall back to the same review-only prefill "Review code" already uses, so the recorder
        // stays testable with PickLocatorAtCaretActionTest's pure-logic style even though the
        // WriteCommandAction/PSI insertion path itself is not unit-testable here.
        List<CapturedInvocation> invocations = new ArrayList<>();
        GuidedWorkflowPanel panel = new GuidedWorkflowPanel(null,
                (toolName, arguments) -> invocations.add(new CapturedInvocation(toolName, arguments)));
        expandAdvanced(panel);
        JButton insertAtCaret = findButton(panel, "Insert at caret");
        JButton createTestClass = findButton(panel, "Create test class");
        assertNotNull(insertAtCaret);
        assertNotNull(createTestClass);

        insertAtCaret.doClick();
        CapturedInvocation insert = last(invocations);
        assertAll(
                () -> assertEquals("capture_code_blocks", insert.toolName()),
                () -> assertEquals("driver", insert.arguments().get("driverVariableName").getAsString()));

        createTestClass.doClick();
        CapturedInvocation create = last(invocations);
        assertEquals("capture_code_blocks", create.toolName());

        select(findByAccessibleName(panel, "Guided workflow backend", JComboBox.class), "Mobile (web emulation)");
        insertAtCaret.doClick();
        assertEquals("mobile_recording_code_blocks", last(invocations).toolName());

        select(findByAccessibleName(panel, "Guided workflow backend", JComboBox.class), "Playwright");
        createTestClass.doClick();
        assertEquals("playwright_recording_code_blocks", last(invocations).toolName());
    }

    @Test
    void webRecordingStartCarriesIntentAsSessionGoal() {
        List<CapturedInvocation> invocations = new ArrayList<>();
        GuidedWorkflowPanel panel = new GuidedWorkflowPanel(null,
                (toolName, arguments) -> invocations.add(new CapturedInvocation(toolName, arguments)));
        expandAdvanced(panel);
        setText(panel, "Intent", "Log in as a valid user");
        findButton(panel, "Start recording").doClick();

        CapturedInvocation start = last(invocations);
        assertAll(
                () -> assertEquals("capture_start", start.toolName()),
                () -> assertEquals("Log in as a valid user", start.arguments().get("sessionGoal").getAsString()));
    }

    @Test
    void headlessToggleIsPersistedInSettingsAndInitializedFromThem() {
        com.shaft.intellij.settings.ShaftSettingsState.Settings settings =
                new com.shaft.intellij.settings.ShaftSettingsState.Settings();
        settings.recorderHeadless = true;
        GuidedWorkflowPanel panel = new GuidedWorkflowPanel(null, (tool, arguments) -> {
        }, settings);
        expandAdvanced(panel);
        javax.swing.JCheckBox headless = findByAccessibleName(panel, "Headless browser", javax.swing.JCheckBox.class);

        assertNotNull(headless);
        assertTrue(headless.isSelected(), "Headless toggle must initialize from the persisted preference");
        headless.setSelected(false);
        assertFalse(settings.recorderHeadless, "Toggling headless must persist to settings");
    }

    @Test
    void irrelevantFieldsAreDisabledPerBackend() {
        GuidedWorkflowPanel panel = new GuidedWorkflowPanel(null, (tool, arguments) -> {
        });
        expandAdvanced(panel);
        JComboBox<?> backend = findByAccessibleName(panel, "Guided workflow backend", JComboBox.class);
        javax.swing.JCheckBox headless = findByAccessibleName(panel, "Headless browser", javax.swing.JCheckBox.class);
        javax.swing.text.JTextComponent targetUrl =
                findByAccessibleName(panel, "Target URL", javax.swing.text.JTextComponent.class);
        assertNotNull(backend);
        assertNotNull(headless);
        assertNotNull(targetUrl);

        select(backend, "WebDriver");
        assertTrue(targetUrl.isEnabled());
        assertTrue(headless.isEnabled());

        select(backend, "Playwright");
        assertFalse(targetUrl.isEnabled(), "Playwright recorder start does not take a target URL");
        assertFalse(headless.isEnabled(), "Playwright recorder start does not take a headless option");

        select(backend, "Mobile (web emulation)");
        assertTrue(targetUrl.isEnabled());
        assertTrue(headless.isEnabled());
    }

    @Test
    void mobileEmulationTemplateSwitchesBackendToMobile() {
        List<CapturedInvocation> invocations = new ArrayList<>();
        GuidedWorkflowPanel panel = new GuidedWorkflowPanel(null,
                (toolName, arguments) -> invocations.add(new CapturedInvocation(toolName, arguments)));
        expandAdvanced(panel);
        JComboBox<?> backend = findByAccessibleName(panel, "Guided workflow backend", JComboBox.class);
        JComboBox<?> templates = findByAccessibleName(panel, "Workflow template", JComboBox.class);
        assertNotNull(backend);
        assertNotNull(templates);

        select(templates, "Start mobile emulation session for recording");
        findButton(panel, "Use template").doClick();

        assertEquals("mobile_initialize_web_emulation", last(invocations).toolName());
        assertEquals("Mobile (web emulation)", String.valueOf(backend.getSelectedItem()));
    }

    @Test
    void recorderStatusStripIsPresentAndIdleByDefault() {
        GuidedWorkflowPanel panel = new GuidedWorkflowPanel(null, (tool, arguments) -> {
        });
        javax.swing.JLabel status = findByAccessibleName(panel, "Recorder status", javax.swing.JLabel.class);
        assertNotNull(status);
        assertTrue(status.getText().contains("idle"), status.getText());
    }

    @Test
    void starterTemplateControlsExposeAccessibleMetadata() {
        GuidedWorkflowPanel panel = new GuidedWorkflowPanel(null, (tool, arguments) -> {
        });
        expandAdvanced(panel);
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

    @Test
    void codingPartnerWorkspacePrefillsRepositoryAwarePlan() {
        List<CapturedInvocation> invocations = new ArrayList<>();
        GuidedWorkflowPanel panel = new GuidedWorkflowPanel(null,
                (toolName, arguments) -> invocations.add(new CapturedInvocation(toolName, arguments)));
        expandAdvanced(panel);
        JButton planPartnerWork = findButton(panel, "Plan coding partner");

        assertNotNull(planPartnerWork);
        planPartnerWork.doClick();

        CapturedInvocation invocation = last(invocations);
        assertAll(
                () -> assertEquals("shaft_coding_partner_plan", invocation.toolName()),
                () -> assertEquals(".", invocation.arguments().get("repositoryPath").getAsString()),
                () -> assertEquals("Log in as a valid user", invocation.arguments().get("intent").getAsString()),
                () -> assertEquals("WebDriver", invocation.arguments().get("backend").getAsString()),
                () -> assertEquals(10, invocation.arguments().get("maxResults").getAsInt()),
                () -> assertEquals(0, invocation.arguments().getAsJsonArray("artifactPaths").size()));
    }

    @Test
    void advancedOptionsAreCollapsedByDefault() {
        GuidedWorkflowPanel panel = new GuidedWorkflowPanel(null, (tool, arguments) -> {
        });
        javax.swing.text.JTextComponent targetUrl =
                findByAccessibleName(panel, "Target URL", javax.swing.text.JTextComponent.class);
        javax.swing.JLabel status = findByAccessibleName(panel, "Recorder status", javax.swing.JLabel.class);
        javax.swing.JCheckBox toggle =
                findByAccessibleName(panel, "Show advanced Guided options", javax.swing.JCheckBox.class);
        JComboBox<?> backend = findByAccessibleName(panel, "Guided workflow backend", JComboBox.class);
        JButton planPartnerWork = findButton(panel, "Plan coding partner");

        assertAll(
                () -> assertNotNull(targetUrl),
                () -> assertTrue(isVisibleInHierarchy(targetUrl), "Target URL must stay on the primary surface"),
                () -> assertNotNull(status),
                () -> assertTrue(isVisibleInHierarchy(status), "Recorder status must stay on the primary surface"),
                () -> assertNotNull(toggle),
                () -> assertFalse(toggle.isSelected(), "Advanced options must default to collapsed"),
                () -> assertNotNull(backend),
                () -> assertFalse(isVisibleInHierarchy(backend), "Backend selector must be hidden by default"),
                () -> assertNotNull(planPartnerWork),
                () -> assertFalse(isVisibleInHierarchy(planPartnerWork),
                        "Coding Partner section must be hidden by default"));
    }

    @Test
    void advancedToggleExpandsAndCollapsesTheAdvancedSurface() {
        GuidedWorkflowPanel panel = new GuidedWorkflowPanel(null, (tool, arguments) -> {
        });
        javax.swing.JCheckBox toggle =
                findByAccessibleName(panel, "Show advanced Guided options", javax.swing.JCheckBox.class);
        JComboBox<?> backend = findByAccessibleName(panel, "Guided workflow backend", JComboBox.class);
        assertNotNull(toggle);
        assertNotNull(backend);

        assertFalse(isVisibleInHierarchy(backend));

        toggle.doClick();
        assertAll(
                () -> assertTrue(toggle.isSelected()),
                () -> assertTrue(isVisibleInHierarchy(backend), "Backend selector must show once expanded"));

        toggle.doClick();
        assertAll(
                () -> assertFalse(toggle.isSelected()),
                () -> assertFalse(isVisibleInHierarchy(backend), "Backend selector must hide again once collapsed"));
    }

    @Test
    void advancedOptionsHintShowsOnlyWhileCollapsed() {
        // Issue #3601 G3: templates/backend/session controls all live behind Advanced options, so
        // a collapsed-by-default user gets a plain-language nudge toward the toggle -- purely
        // informational, and gone the moment Advanced options is opened.
        GuidedWorkflowPanel panel = new GuidedWorkflowPanel(null, (tool, arguments) -> {
        });
        javax.swing.JCheckBox toggle =
                findByAccessibleName(panel, "Show advanced Guided options", javax.swing.JCheckBox.class);
        javax.swing.JLabel hint =
                findByAccessibleName(panel, "Advanced options hint", javax.swing.JLabel.class);
        assertNotNull(toggle);
        assertNotNull(hint);

        assertAll(
                () -> assertFalse(toggle.isSelected(), "Advanced options must default to collapsed"),
                () -> assertTrue(isVisibleInHierarchy(hint), "Hint must show while Advanced options is collapsed"));

        toggle.doClick();
        assertAll(
                () -> assertTrue(toggle.isSelected()),
                () -> assertFalse(isVisibleInHierarchy(hint), "Hint must hide once Advanced options is expanded"));

        toggle.doClick();
        assertAll(
                () -> assertFalse(toggle.isSelected()),
                () -> assertTrue(isVisibleInHierarchy(hint), "Hint must reappear once Advanced options is collapsed again"));
    }

    @Test
    void allAccessibleNamesRemainReachableOnceExpanded() {
        GuidedWorkflowPanel panel = new GuidedWorkflowPanel(null, (tool, arguments) -> {
        });
        expandAdvanced(panel);

        assertAll(
                () -> assertNotNull(findByAccessibleName(panel, "Target URL", javax.swing.text.JTextComponent.class)),
                () -> assertNotNull(findByAccessibleName(panel, "Guided workflow backend", JComboBox.class)),
                () -> assertNotNull(findByAccessibleName(panel, "Workflow template", JComboBox.class)),
                () -> assertNotNull(findByAccessibleName(panel, "Intent", javax.swing.text.JTextComponent.class)),
                () -> assertNotNull(findByAccessibleName(panel, "Current source path", javax.swing.text.JTextComponent.class)),
                () -> assertNotNull(findByAccessibleName(panel, "Evidence paths", javax.swing.text.JTextComponent.class)),
                () -> assertNotNull(findByAccessibleName(panel, "Session path", javax.swing.text.JTextComponent.class)),
                () -> assertNotNull(findByAccessibleName(panel, "Headless browser", javax.swing.JCheckBox.class)),
                () -> assertNotNull(findByAccessibleName(panel, "Recorder status", javax.swing.JLabel.class)),
                () -> assertNotNull(findByAccessibleName(panel, "Generated code or guardrail input", javax.swing.text.JTextComponent.class)),
                () -> assertNotNull(findButton(panel, "Try SHAFT on a sample page")),
                () -> assertNotNull(findButton(panel, "Start recording")),
                () -> assertNotNull(findButton(panel, "Stop recording")),
                () -> assertNotNull(findButton(panel, "Review code")),
                () -> assertNotNull(findButton(panel, "Insert at caret")),
                () -> assertNotNull(findButton(panel, "Create test class")),
                () -> assertNotNull(findButton(panel, "Plan coding partner")),
                () -> assertNotNull(findButton(panel, "Find reuse")),
                () -> assertNotNull(findButton(panel, "Inspect locator")),
                () -> assertNotNull(findButton(panel, "Guardrail check")));
    }

    private static boolean isVisibleInHierarchy(Component component) {
        for (Component current = component; current != null; current = current.getParent()) {
            if (!current.isVisible()) {
                return false;
            }
        }
        return true;
    }

    private static void expandAdvanced(GuidedWorkflowPanel panel) {
        javax.swing.JCheckBox toggle =
                findByAccessibleName(panel, "Show advanced Guided options", javax.swing.JCheckBox.class);
        assertNotNull(toggle, "Missing Advanced options toggle");
        if (!toggle.isSelected()) {
            toggle.doClick();
        }
    }

    private static void setText(GuidedWorkflowPanel panel, String accessibleName, String value) {
        javax.swing.text.JTextComponent field =
                findByAccessibleName(panel, accessibleName, javax.swing.text.JTextComponent.class);
        assertNotNull(field, "Missing field: " + accessibleName);
        field.setText(value);
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
