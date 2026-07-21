package com.shaft.intellij.ui;

import com.google.gson.JsonArray;
import com.google.gson.JsonObject;
import com.intellij.openapi.project.Project;
import com.intellij.ui.components.JBList;
import org.junit.jupiter.api.Test;

import javax.accessibility.AccessibleContext;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import java.awt.Component;
import java.awt.Container;
import java.io.IOException;
import java.lang.reflect.Proxy;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
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
        JsonObject mobileOptions = mobileEmulation.arguments().getAsJsonObject("mobileOptions");
        assertAll(
                () -> assertEquals("driver_initialize", mobileEmulation.toolName()),
                // driver_initialize's engine selector (issue #3899: the plugin must send the schema's
                // uppercase Java-enum-name wire casing, e.g. MOBILE_WEB, not a lowercase variant).
                () -> assertEquals("MOBILE_WEB", mobileEmulation.arguments().get("engine").getAsString()),
                () -> assertNotNull(mobileOptions, "mobile fields must nest under mobileOptions: "
                        + mobileEmulation.arguments()),
                // Default combo selection is "Chrome" (matches CaptureBrowser#parse and
                // MobileService#browserType, both case-insensitive) -- not the old hardcoded "CHROME".
                () -> assertEquals("Chrome", mobileOptions.get("browser").getAsString()),
                () -> assertEquals("Pixel 5", mobileOptions.get("deviceName").getAsString()),
                () -> assertFalse(mobileOptions.get("headless").getAsBoolean()));

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
                () -> assertEquals("capture_start", mobileStart.toolName()),
                // Issue #3908 live-repro finding: capture_start has no mode/includeSensitiveValues
                // arguments (absorbed into sessionGoal / dropped, #3881) -- the stale flat shape this
                // used to send fails live schema validation ("property 'mode' is not defined in the
                // schema").
                () -> assertFalse(mobileStart.arguments().has("mode")),
                () -> assertFalse(mobileStart.arguments().has("includeSensitiveValues")),
                () -> assertTrue(mobileStart.arguments().has("sessionGoal")));

        stop.doClick();
        CapturedInvocation mobileStop = last(invocations);
        assertAll(
                () -> assertEquals("capture_stop", mobileStop.toolName()),
                () -> assertFalse(mobileStop.arguments().get("discard").getAsBoolean()));

        review.doClick();
        CapturedInvocation mobileReview = last(invocations);
        assertAll(
                () -> assertEquals("capture_code_blocks", mobileReview.toolName()),
                () -> assertEquals("driver", mobileReview.arguments().get("driverVariableName").getAsString()),
                // Issue #3916 live-repro finding: the unified capture_code_blocks tool (post-#3881)
                // has no recordingPath argument -- it fails live schema validation ("property
                // 'recordingPath' is not defined in the schema"). Mobile/Playwright must send
                // sessionPath plus an explicit backend, exactly like the WebDriver branch below.
                () -> assertFalse(mobileReview.arguments().has("recordingPath")),
                () -> assertEquals("recordings/intellij-capture.json",
                        mobileReview.arguments().get("sessionPath").getAsString()),
                () -> assertEquals("mobile", mobileReview.arguments().get("backend").getAsString()));

        select(backend, "Playwright");
        start.doClick();
        CapturedInvocation playwrightStart = last(invocations);
        assertAll(
                () -> assertEquals("capture_start", playwrightStart.toolName()),
                () -> assertFalse(playwrightStart.arguments().has("mode")),
                () -> assertFalse(playwrightStart.arguments().has("includeSensitiveValues")),
                () -> assertTrue(playwrightStart.arguments().has("sessionGoal")));
        stop.doClick();
        assertEquals("capture_stop", last(invocations).toolName());
        review.doClick();
        CapturedInvocation playwrightReview = last(invocations);
        assertAll(
                () -> assertEquals("capture_code_blocks", playwrightReview.toolName()),
                () -> assertFalse(playwrightReview.arguments().has("recordingPath")),
                () -> assertEquals("recordings/intellij-capture.json",
                        playwrightReview.arguments().get("sessionPath").getAsString()),
                () -> assertEquals("playwright", playwrightReview.arguments().get("backend").getAsString()));
    }

    @Test
    void apiRecorderButtonsWireCaptureApiToolsMirroringWebDriverAndMobile() {
        // Issue #3939: GuidedWorkflowPanel had zero UI entry points for API recording even though
        // capture_api_start/capture_api_status/capture_api_stop/capture_api_generate (CaptureService,
        // shaft-mcp) already work and are proven by GuidedWorkflowLiveE2ETest
        // #apiRecorderCapturesNetworkTrafficAndGeneratesShaftApiCode. Start/Stop/Review code must route
        // to those tools for the new API backend, exactly like the existing backends route to
        // capture_start/capture_stop/capture_code_blocks.
        List<CapturedInvocation> invocations = new ArrayList<>();
        GuidedWorkflowPanel panel = new GuidedWorkflowPanel(null,
                (toolName, arguments) -> invocations.add(new CapturedInvocation(toolName, arguments)));
        expandAdvanced(panel);
        JComboBox<?> backend = findByAccessibleName(panel, "Guided workflow backend", JComboBox.class);
        JButton start = findButton(panel, "Start recording");
        JButton stop = findButton(panel, "Stop recording");
        JButton review = findButton(panel, "Review code");
        assertNotNull(backend);

        select(backend, "API");
        start.doClick();
        CapturedInvocation apiStart = last(invocations);
        JsonObject networkOptions = apiStart.arguments().getAsJsonObject("networkOptions");
        assertAll(
                () -> assertEquals("capture_api_start", apiStart.toolName()),
                () -> assertFalse(apiStart.arguments().get("headless").getAsBoolean()),
                () -> assertNotNull(networkOptions, "capture_api_start requires networkOptions: "
                        + apiStart.arguments()),
                () -> assertTrue(networkOptions.get("enabled").getAsBoolean()),
                () -> assertTrue(networkOptions.get("captureResponseBodies").getAsBoolean()),
                () -> assertFalse(networkOptions.get("captureRequestBodies").getAsBoolean()));

        stop.doClick();
        CapturedInvocation apiStop = last(invocations);
        assertAll(
                () -> assertEquals("capture_api_stop", apiStop.toolName()),
                () -> assertFalse(apiStop.arguments().get("discard").getAsBoolean()));

        review.doClick();
        CapturedInvocation apiGenerate = last(invocations);
        assertAll(
                // capture_code_blocks' backend parameter only recognizes web/playwright/mobile and would
                // silently misread an API-network recording as a WebDriver UI capture (GuidedWorkflowLiveE2ETest
                // javadoc); the dedicated capture_api_generate tool is the real API codegen entry point.
                () -> assertEquals("capture_api_generate", apiGenerate.toolName()),
                () -> assertEquals("recordings/intellij-capture.json",
                        apiGenerate.arguments().get("sessionPath").getAsString()),
                () -> assertFalse(apiGenerate.arguments().get("overwrite").getAsBoolean()),
                () -> assertFalse(apiGenerate.arguments().get("replay").getAsBoolean()),
                () -> assertTrue(apiGenerate.arguments().has("style")),
                () -> assertTrue(apiGenerate.arguments().has("validationDepth")),
                () -> assertTrue(apiGenerate.arguments().has("excludedTransactionIds")),
                () -> assertTrue(apiGenerate.arguments().has("pinnedJsonPaths")));
    }

    @Test
    void apiBackendEnablesTargetFieldsAndDisablesTheUnusedSessionPathField() {
        // capture_api_start's WEB branch ignores its outputPath argument entirely (the session path
        // is only known once the server reports it back), so unlike the other backends the Session
        // path field has nothing to send for API recording and must be disabled rather than silently
        // ignored.
        GuidedWorkflowPanel panel = new GuidedWorkflowPanel(null, (tool, arguments) -> {
        });
        expandAdvanced(panel);
        JComboBox<?> backend = findByAccessibleName(panel, "Guided workflow backend", JComboBox.class);
        javax.swing.JCheckBox headless = findByAccessibleName(panel, "Headless browser", javax.swing.JCheckBox.class);
        javax.swing.text.JTextComponent targetUrl =
                findByAccessibleName(panel, "Target URL", javax.swing.text.JTextComponent.class);
        JComboBox<?> recorderBrowser = findByAccessibleName(panel, "Recorder browser", JComboBox.class);
        javax.swing.text.JTextComponent sessionPath =
                findByAccessibleName(panel, "Session path", javax.swing.text.JTextComponent.class);
        assertNotNull(backend);

        select(backend, "API");
        assertAll(
                () -> assertTrue(targetUrl.isEnabled()),
                () -> assertTrue(headless.isEnabled()),
                () -> assertTrue(recorderBrowser.isEnabled()),
                () -> assertFalse(sessionPath.isEnabled(),
                        "capture_api_start ignores outputPath; the field has nothing to send"));
    }

    @Test
    void recorderBrowserPickerIsVisibleByDefaultAndDrivesBothStartToolArguments() {
        // Issue #3660: a real browser picker (not a hardcoded literal) replaces the old
        // "Chrome"/"CHROME" constants in both webdriverCaptureStartArguments() and
        // mobileWebEmulation(), and it must be reachable without expanding Advanced options.
        List<CapturedInvocation> invocations = new ArrayList<>();
        GuidedWorkflowPanel panel = new GuidedWorkflowPanel(null,
                (toolName, arguments) -> invocations.add(new CapturedInvocation(toolName, arguments)));
        JComboBox<?> recorderBrowser = findByAccessibleName(panel, "Recorder browser", JComboBox.class);
        assertNotNull(recorderBrowser);
        assertAll(
                () -> assertEquals(2, recorderBrowser.getItemCount(), "Only Chrome and Edge are accepted server-side"),
                () -> assertEquals("Chrome", recorderBrowser.getSelectedItem(), "Matches the previous hardcoded default"),
                () -> assertTrue(isVisibleInHierarchy(recorderBrowser),
                        "Browser picker must be visible without expanding Advanced options"));

        expandAdvanced(panel);
        JComboBox<?> backend = findByAccessibleName(panel, "Guided workflow backend", JComboBox.class);
        JComboBox<?> templates = findByAccessibleName(panel, "Workflow template", JComboBox.class);
        JButton start = findButton(panel, "Start recording");
        JButton useTemplate = findButton(panel, "Use template");

        select(backend, "WebDriver");
        start.doClick();
        assertEquals("Chrome", last(invocations).arguments().get("browser").getAsString(),
                "webdriverCaptureStartArguments() must read the selected combo item");

        select(recorderBrowser, "Edge");
        start.doClick();
        assertEquals("Edge", last(invocations).arguments().get("browser").getAsString(),
                "Changing the combo must change capture_start's browser argument");

        select(templates, "Start mobile emulation session for recording");
        useTemplate.doClick();
        CapturedInvocation mobileEmulation = last(invocations);
        assertAll(
                () -> assertEquals("driver_initialize", mobileEmulation.toolName()),
                () -> assertEquals("Edge",
                        mobileEmulation.arguments().getAsJsonObject("mobileOptions").get("browser").getAsString(),
                        "mobileWebEmulation() must read the same selected combo item"));
    }

    @Test
    void headlessPolicyLockHintBecomesVisibleWhenTeamPolicyLocksHeadless() throws IOException {
        // Issue #3660: applyTeamRecorderPolicy() already locks the headless checkbox via a tooltip;
        // the hint label must also become visible so the lock is not tooltip-only.
        Path lockedProjectRoot = Files.createTempDirectory("shaft-guided-workflow-policy-locked");
        Path shaftDir = lockedProjectRoot.resolve(".shaft");
        Files.createDirectories(shaftDir);
        Files.writeString(shaftDir.resolve("recorder-policy.json"), "{\"headless\": true}");

        GuidedWorkflowPanel lockedPanel = new GuidedWorkflowPanel(fakeProject(lockedProjectRoot.toString()),
                (tool, arguments) -> {
                });
        javax.swing.JLabel lockedHint =
                findByAccessibleName(lockedPanel, "Headless policy lock hint", javax.swing.JLabel.class);
        javax.swing.JCheckBox lockedHeadless =
                findByAccessibleName(lockedPanel, "Headless browser", javax.swing.JCheckBox.class);
        assertAll(
                () -> assertNotNull(lockedHint),
                () -> assertTrue(isVisibleInHierarchy(lockedHint), "Hint must show once policy locks headless"),
                () -> assertTrue(lockedHeadless.isSelected(), "Headless value must come from the policy file"),
                () -> assertFalse(lockedHeadless.isEnabled(), "Checkbox stays locked by policy"));

        Path unlockedProjectRoot = Files.createTempDirectory("shaft-guided-workflow-policy-unlocked");
        GuidedWorkflowPanel unlockedPanel = new GuidedWorkflowPanel(fakeProject(unlockedProjectRoot.toString()),
                (tool, arguments) -> {
                });
        javax.swing.JLabel unlockedHint =
                findByAccessibleName(unlockedPanel, "Headless policy lock hint", javax.swing.JLabel.class);
        assertAll(
                () -> assertNotNull(unlockedHint),
                () -> assertFalse(isVisibleInHierarchy(unlockedHint),
                        "Hint must stay hidden without a recorder-policy.json"));
    }

    @Test
    void stepEditingButtonsWireCorrectToolNameAndArgumentsPerBackend() {
        // Issue #3639/#3866: Delete/Move Up/Move Down must resolve to the unconditional
        // capture_step_delete/capture_step_reorder tools (CaptureService#stepDelete/stepReorder), which
        // already dispatch on the MCP session's active engine server-side -- the panel no longer picks a
        // playwright_/mobile_ prefix itself. The list must still re-render from whatever status fixture
        // is fed in -- no separate client-side step store.
        List<CapturedInvocation> invocations = new ArrayList<>();
        GuidedWorkflowPanel panel = new GuidedWorkflowPanel(null,
                (toolName, arguments) -> invocations.add(new CapturedInvocation(toolName, arguments)));
        expandAdvanced(panel);
        JComboBox<?> backend = findByAccessibleName(panel, "Guided workflow backend", JComboBox.class);
        JBList<?> stepList = findByAccessibleName(panel, "Recorded steps", JBList.class);
        JButton delete = findButton(panel, "Delete");
        JButton moveUp = findButton(panel, "Move Up");
        JButton moveDown = findButton(panel, "Move Down");
        assertNotNull(backend);
        assertNotNull(stepList);
        assertNotNull(delete);
        assertNotNull(moveUp);
        assertNotNull(moveDown);

        select(backend, "Playwright");
        panel.renderStepsFromStatus(recordedStepsStatus("pw-step-1"));
        assertAll(
                () -> assertEquals(1, stepList.getModel().getSize()),
                () -> assertFalse(delete.isEnabled(), "no row selected yet"),
                () -> assertFalse(moveUp.isEnabled(), "no row selected yet"),
                () -> assertFalse(moveDown.isEnabled(), "no row selected yet"));

        stepList.setSelectedIndex(0);
        assertAll(
                () -> assertTrue(delete.isEnabled()),
                () -> assertTrue(moveUp.isEnabled()),
                () -> assertTrue(moveDown.isEnabled()));

        delete.doClick();
        CapturedInvocation playwrightDelete = last(invocations);
        assertAll(
                () -> assertEquals("capture_step_delete", playwrightDelete.toolName()),
                () -> assertEquals("pw-step-1", playwrightDelete.arguments().get("stepId").getAsString()));

        moveUp.doClick();
        CapturedInvocation playwrightMoveUp = last(invocations);
        assertAll(
                () -> assertEquals("capture_step_reorder", playwrightMoveUp.toolName()),
                () -> assertEquals("pw-step-1", playwrightMoveUp.arguments().get("stepId").getAsString()),
                () -> assertEquals("up", playwrightMoveUp.arguments().get("direction").getAsString()));

        moveDown.doClick();
        CapturedInvocation playwrightMoveDown = last(invocations);
        assertEquals("down", playwrightMoveDown.arguments().get("direction").getAsString());

        // Switching to Mobile and feeding a fresh status fixture proves the list re-renders as a pure
        // projection of the latest poll, and the SAME unconditional tool names are used -- there is no
        // mobile_ prefix to switch to.
        select(backend, "Mobile (web emulation)");
        panel.renderStepsFromStatus(recordedStepsStatus("mobile-step-1"));
        assertEquals(1, stepList.getModel().getSize());
        stepList.setSelectedIndex(0);

        delete.doClick();
        CapturedInvocation mobileDelete = last(invocations);
        assertAll(
                () -> assertEquals("capture_step_delete", mobileDelete.toolName()),
                () -> assertEquals("mobile-step-1", mobileDelete.arguments().get("stepId").getAsString()));

        moveUp.doClick();
        CapturedInvocation mobileMoveUp = last(invocations);
        assertAll(
                () -> assertEquals("capture_step_reorder", mobileMoveUp.toolName()),
                () -> assertEquals("mobile-step-1", mobileMoveUp.arguments().get("stepId").getAsString()),
                () -> assertEquals("up", mobileMoveUp.arguments().get("direction").getAsString()));
    }

    @Test
    void stepEditingIsDisabledForWebDriverBackendWithNoStepDeleteTool() {
        // Critical scope boundary (issue #3639): capture_step_delete/capture_step_reorder throw an
        // actionable error for the WEB CDP engine (CaptureService#noStepEditorFor) since a capture_start
        // WebDriver recording has no step editor, so the WebDriver backend must never let these buttons
        // fire, even if a row is present and selected in the list.
        List<CapturedInvocation> invocations = new ArrayList<>();
        GuidedWorkflowPanel panel = new GuidedWorkflowPanel(null,
                (toolName, arguments) -> invocations.add(new CapturedInvocation(toolName, arguments)));
        expandAdvanced(panel);
        JComboBox<?> backend = findByAccessibleName(panel, "Guided workflow backend", JComboBox.class);
        JBList<?> stepList = findByAccessibleName(panel, "Recorded steps", JBList.class);
        JButton delete = findButton(panel, "Delete");
        JButton moveUp = findButton(panel, "Move Up");
        JButton moveDown = findButton(panel, "Move Down");

        select(backend, "WebDriver");
        assertFalse(stepList.isEnabled(), "WebDriver has no per-step summaries or step_delete/reorder tools");

        panel.renderStepsFromStatus(recordedStepsStatus("orphan-step"));
        stepList.setSelectedIndex(0);
        assertAll(
                () -> assertFalse(delete.isEnabled()),
                () -> assertFalse(moveUp.isEnabled()),
                () -> assertFalse(moveDown.isEnabled()));

        delete.doClick();
        moveUp.doClick();
        moveDown.doClick();
        assertTrue(invocations.isEmpty(),
                "disabled step buttons must never fire a step_delete/step_reorder request");
    }

    @Test
    void parseStepsReadsPerStepFieldsAndTreatsMissingStepsAsEmpty() {
        // Pure parsing coverage (mirrors ApiRecordingSessionPanel#parseTransactions): confirms the
        // wire field names read off playwright_record_status/mobile_record_status's "steps" array,
        // and that a capture_status-shaped payload (no "steps" key at all) parses to empty rather
        // than throwing -- the empirical basis for disabling the UI on the WebDriver backend.
        List<GuidedWorkflowPanel.StepRow> rows =
                GuidedWorkflowPanel.parseSteps(recordedStepsStatus("s-42"));
        assertEquals(1, rows.size());
        GuidedWorkflowPanel.StepRow row = rows.get(0);
        assertAll(
                () -> assertEquals("s-42", row.stepId()),
                () -> assertEquals(1L, row.sequence()),
                () -> assertEquals("click", row.action()),
                () -> assertEquals("xpath", row.locatorStrategy()),
                () -> assertEquals("//button[@id='s-42']", row.locatorValue()),
                () -> assertFalse(row.risky()));

        JsonObject captureStatusShape = new JsonObject();
        captureStatusShape.addProperty("eventCount", 3);
        assertTrue(GuidedWorkflowPanel.parseSteps(captureStatusShape).isEmpty());
        assertTrue(GuidedWorkflowPanel.parseSteps(null).isEmpty());
    }

    private static JsonObject recordedStepsStatus(String stepId) {
        JsonObject status = new JsonObject();
        status.addProperty("active", true);
        status.addProperty("actionCount", 1);
        JsonArray steps = new JsonArray();
        JsonObject step = new JsonObject();
        step.addProperty("stepId", stepId);
        step.addProperty("sequence", 1);
        step.addProperty("action", "click");
        step.addProperty("locatorStrategy", "xpath");
        step.addProperty("locatorValue", "//button[@id='" + stepId + "']");
        step.addProperty("risky", false);
        steps.add(step);
        status.add("steps", steps);
        return status;
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
        assertEquals("capture_code_blocks", last(invocations).toolName());

        select(findByAccessibleName(panel, "Guided workflow backend", JComboBox.class), "Playwright");
        createTestClass.doClick();
        assertEquals("capture_code_blocks", last(invocations).toolName());
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
        JComboBox<?> recorderBrowser = findByAccessibleName(panel, "Recorder browser", JComboBox.class);
        assertNotNull(backend);
        assertNotNull(headless);
        assertNotNull(targetUrl);
        assertNotNull(recorderBrowser);

        select(backend, "WebDriver");
        assertTrue(targetUrl.isEnabled());
        assertTrue(headless.isEnabled());
        assertTrue(recorderBrowser.isEnabled());

        select(backend, "Playwright");
        assertFalse(targetUrl.isEnabled(), "Playwright recorder start does not take a target URL");
        assertFalse(headless.isEnabled(), "Playwright recorder start does not take a headless option");
        assertFalse(recorderBrowser.isEnabled(), "Playwright recorder start does not take a browser parameter");

        select(backend, "Mobile (web emulation)");
        assertTrue(targetUrl.isEnabled());
        assertTrue(headless.isEnabled());
        assertTrue(recorderBrowser.isEnabled());
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

        assertEquals("driver_initialize", last(invocations).toolName());
        assertEquals("Mobile (web emulation)", String.valueOf(backend.getSelectedItem()));
    }

    @Test
    void recorderStatusStripIsPresentAndIdleByDefault() {
        GuidedWorkflowPanel panel = new GuidedWorkflowPanel(null, (tool, arguments) -> {
        });
        javax.swing.JLabel status = findByAccessibleName(panel, "Recorder status", javax.swing.JLabel.class);
        assertNotNull(status);
        assertTrue(status.getText().contains("idle"), status.getText());
        // Issue #3603: the accessible name stays the short, stable "Recorder status" (test-id-safe),
        // but a screen reader also needs the live recorder status text, and that description must
        // keep tracking every later update -- not just the first -- since setRecorderStatus() is the
        // single choke point every status change runs through.
        String initialDescription = accessibleDescription(status);
        assertEquals(status.getText(), initialDescription);

        findButton(panel, "Try SHAFT on a sample page").doClick();

        String updatedDescription = accessibleDescription(status);
        assertAll(
                () -> assertEquals(status.getText(), updatedDescription),
                () -> assertNotEquals(initialDescription, updatedDescription,
                        "the description must track the live status text after it changes"));
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
                () -> assertTrue(isVisibleInHierarchy(hint), "Hint must show while Advanced options is collapsed"),
                // Issue #3603: the accessible name stays the short, stable "Advanced options hint"
                // (test-id-safe), but a screen reader also needs the actual hint text.
                () -> assertEquals(hint.getText(), accessibleDescription(hint)));

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

    /**
     * A {@link Project} stub whose {@code getBasePath()} returns {@code basePath} (mirrors this
     * suite's established {@code fakeProject} pattern, see {@code
     * ShaftToolWindowPanelWorkflowPersistenceTest}) -- the only project method
     * {@code applyTeamRecorderPolicy()} reads.
     */
    private static Project fakeProject(String basePath) {
        return (Project) Proxy.newProxyInstance(Project.class.getClassLoader(), new Class<?>[]{Project.class},
                (proxy, method, arguments) -> {
                    switch (method.getName()) {
                        case "equals":
                            return proxy == (arguments == null || arguments.length == 0 ? null : arguments[0]);
                        case "hashCode":
                            return System.identityHashCode(proxy);
                        case "getBasePath":
                            return basePath;
                        case "getName":
                            return "guided-workflow-panel-test-project";
                        default:
                            return defaultValue(method.getReturnType());
                    }
                });
    }

    private static Object defaultValue(Class<?> returnType) {
        if (!returnType.isPrimitive()) {
            return null;
        }
        if (returnType == boolean.class) {
            return false;
        }
        return 0;
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
