package com.shaft.intellij.ui;

import com.google.gson.JsonArray;
import com.google.gson.JsonObject;
import com.intellij.openapi.Disposable;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.fileEditor.FileEditorManager;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.ui.components.JBTextArea;
import com.intellij.ui.components.JBTextField;
import com.intellij.util.Alarm;
import com.intellij.util.ui.JBUI;
import com.shaft.intellij.mcp.ShaftMcpInvocationService;
import com.shaft.intellij.mcp.ShaftMcpToolResult;
import com.shaft.intellij.settings.ShaftSettingsState;

import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.Icon;
import java.awt.BorderLayout;
import java.awt.FlowLayout;
import java.awt.Font;
import java.awt.GridLayout;

/**
 * Guided recorder, locator, and code-generation entry points backed by existing MCP tools.
 */
final class GuidedWorkflowPanel extends JPanel implements Disposable {
    static final String BACKEND_WEBDRIVER = "WebDriver";
    static final String BACKEND_PLAYWRIGHT = "Playwright";
    static final String BACKEND_MOBILE = "Mobile (web emulation)";
    private static final int STATUS_POLL_INTERVAL_MILLIS = 2_000;
    /** Stop idle polling after ~5 minutes if the prepared recording request is never run. */
    private static final int MAX_POLLS_BEFORE_ACTIVE = 150;

    private final Project project;
    private final JComboBox<String> backend;
    private final JComboBox<WorkflowTemplate> templateSelector;
    private final JBTextField targetUrl;
    private final JBTextField intent;
    private final JBTextField currentSourcePath;
    private final JBTextField artifactPaths;
    private final JBTextField sessionPath;
    private final JCheckBox headlessBrowser;
    private final JBTextArea codeSnippet;
    private final JLabel recorderStatus;
    private final ToolPrefill prefill;
    private final ShaftSettingsState.Settings settings;
    // Created lazily on the first poll so headless panel tests never touch platform executors.
    private Alarm statusPollAlarm;
    private volatile boolean pollingActive;
    private boolean recorderSeenActive;
    private int pollsWithoutActivity;
    private String statusToolName = "";

    GuidedWorkflowPanel(Project project, ToolPrefill prefill) {
        this(project, prefill, resolveSettings());
    }

    GuidedWorkflowPanel(Project project, ToolPrefill prefill, ShaftSettingsState.Settings settings) {
        super(new BorderLayout(8, 8));
        this.project = project;
        this.prefill = prefill;
        this.settings = settings == null ? new ShaftSettingsState.Settings() : settings;
        setBorder(JBUI.Borders.empty(8));

        backend = new JComboBox<>(new String[]{BACKEND_WEBDRIVER, BACKEND_PLAYWRIGHT, BACKEND_MOBILE});
        backend.getAccessibleContext().setAccessibleName("Guided workflow backend");
        templateSelector = new JComboBox<>(WorkflowTemplate.values());
        templateSelector.setPrototypeDisplayValue(WorkflowTemplate.CREATE_NEW_SHAFT_PROJECT);
        templateSelector.getAccessibleContext().setAccessibleName("Workflow template");
        templateSelector.addActionListener(event -> updateTemplateDescription());
        targetUrl = field("Target URL", "");
        intent = field("Intent", "Log in as a valid user");
        intent.setToolTipText(
                "Describes the recorded journey; also names the generated test class and method.");
        currentSourcePath = field("Current source path", "");
        artifactPaths = field("Evidence paths", "");
        sessionPath = field("Session path", "recordings/intellij-capture.json");
        headlessBrowser = new JCheckBox("Headless browser", this.settings.recorderHeadless);
        headlessBrowser.getAccessibleContext().setAccessibleName("Headless browser");
        headlessBrowser.getAccessibleContext().setAccessibleDescription(
                "Record without a visible browser window. Keep unchecked to interact with the recorded browser.");
        headlessBrowser.setToolTipText(
                "Record without a visible browser window; useful for agent-driven or CI recordings. "
                        + "Remembered across sessions and honored by /record-web and /mobile web.");
        headlessBrowser.addItemListener(event ->
                this.settings.recorderHeadless = headlessBrowser.isSelected());
        codeSnippet = new JBTextArea(6, 32);
        codeSnippet.getAccessibleContext().setAccessibleName("Generated code or guardrail input");
        codeSnippet.getAccessibleContext().setAccessibleDescription(
                "Paste Java code for review-only guardrail checks.");
        codeSnippet.setLineWrap(true);
        codeSnippet.setWrapStyleWord(true);
        recorderStatus = new JLabel("Recorder idle.");
        recorderStatus.getAccessibleContext().setAccessibleName("Recorder status");
        recorderStatus.setBorder(JBUI.Borders.empty(2, 0));
        updateTemplateDescription();
        backend.addActionListener(event -> updateFieldRelevance());
        updateFieldRelevance();

        JPanel fields = new JPanel(new GridLayout(0, 1, 4, 4));
        fields.add(row("Backend", 'B', backend));
        fields.add(row("Template", 'T', templateControls(), templateSelector));
        fields.add(row("Target URL", 'U', targetUrl));
        fields.add(row("Intent", 'I', intent));
        fields.add(row("Current source", 'R', currentSourcePath));
        fields.add(row("Evidence paths", 'E', artifactPaths));
        fields.add(row("Session path", 'S', sessionPath));
        fields.add(row("Browser", 'H', headlessBrowser));
        fields.add(row("Status", 'A', recorderStatus));

        JPanel partner = section("Coding Partner",
                button("Plan coding partner", "Plan repository-aware SHAFT reuse, missing code, proof, and validation", ShaftIcons.CODE,
                        this::planPartnerWork),
                button("Find reuse", "Find existing Java test and page-object anchors before generating code", ShaftIcons.SEARCH,
                        this::findReuse));
        JPanel recorder = section("Recorder",
                button("Try SHAFT on a sample page",
                        "Record a bundled local sample page - the 90-second first-recording tour", ShaftIcons.SEND,
                        this::trySampleRecording),
                button("Start recording", "Start a SHAFT recording", ShaftIcons.SEND, this::startRecording),
                button("Stop recording", "Stop the active SHAFT recording", ShaftIcons.CANCEL, this::stopRecording),
                button("Review code", "Generate reviewed SHAFT code blocks from a recording", ShaftIcons.CODE, this::generateCode));
        JPanel locator = section("Locator",
                button("Inspect locator", "Inspect the page and propose locator candidates", ShaftIcons.SEARCH, this::inspectLocator),
                button("Guardrail check", "Check generated SHAFT code for automation anti-patterns", ShaftIcons.CHECK, this::guardrailCheck));

        JPanel center = new JPanel(new BorderLayout(6, 6));
        center.add(fields, BorderLayout.NORTH);
        center.add(row("Code", 'C', codeSnippet), BorderLayout.CENTER);

        JPanel actions = new JPanel(new GridLayout(1, 3, 8, 8));
        actions.add(partner);
        actions.add(recorder);
        actions.add(locator);

        add(introLabel("Guided workflows prepare reviewed SHAFT MCP requests."), BorderLayout.NORTH);
        add(center, BorderLayout.CENTER);
        add(actions, BorderLayout.SOUTH);
    }

    private static ShaftSettingsState.Settings resolveSettings() {
        try {
            if (ApplicationManager.getApplication() == null) {
                return new ShaftSettingsState.Settings();
            }
            ShaftSettingsState.Settings settings = ShaftSettingsState.getInstance().getState();
            return settings == null ? new ShaftSettingsState.Settings() : settings;
        } catch (RuntimeException | Error headlessTestEnvironment) {
            return new ShaftSettingsState.Settings();
        }
    }

    private static JLabel introLabel(String text) {
        JLabel label = new JLabel(text);
        label.setFont(label.getFont().deriveFont(Font.BOLD, label.getFont().getSize2D() + 1f));
        label.setBorder(JBUI.Borders.emptyBottom(8));
        label.getAccessibleContext().setAccessibleName(text);
        return label;
    }

    private static JBTextField field(String accessibleName, String value) {
        JBTextField field = new JBTextField(value);
        field.getAccessibleContext().setAccessibleName(accessibleName);
        return field;
    }

    private static JPanel row(String labelText, char mnemonic, JComponent component) {
        return row(labelText, mnemonic, component, component);
    }

    private static JPanel row(String labelText, char mnemonic, JComponent component, JComponent labelFor) {
        JPanel row = new JPanel(new BorderLayout(4, 4));
        JLabel label = new JLabel(labelText);
        label.setDisplayedMnemonic(mnemonic);
        label.setLabelFor(labelFor);
        row.add(label, BorderLayout.WEST);
        row.add(component, BorderLayout.CENTER);
        return row;
    }

    private JPanel templateControls() {
        JPanel controls = new JPanel(new BorderLayout(4, 0));
        controls.add(templateSelector, BorderLayout.CENTER);
        controls.add(button("Use template", "Prefill the selected workflow template", ShaftIcons.SEND, this::applyTemplate),
                BorderLayout.EAST);
        return controls;
    }

    private static JPanel section(String title, JButton... buttons) {
        JPanel panel = new JPanel(new FlowLayout(FlowLayout.LEFT, 6, 0));
        panel.add(new JLabel(title));
        for (JButton button : buttons) {
            panel.add(button);
        }
        return panel;
    }

    private static JButton button(String text, String description, Icon icon, Runnable action) {
        JButton button = new JButton();
        ShaftIconButtons.apply(button, description, text, icon);
        button.getAccessibleContext().setAccessibleDescription(description);
        button.addActionListener(event -> action.run());
        return button;
    }

    private void updateTemplateDescription() {
        WorkflowTemplate template = selectedTemplate();
        if (template != null) {
            templateSelector.getAccessibleContext().setAccessibleDescription(template.description);
        }
    }

    /**
     * Grays out fields that the selected backend's start request does not read, so a misfilled
     * field can never silently change what gets prefilled.
     */
    private void updateFieldRelevance() {
        boolean playwrightBackend = playwright();
        targetUrl.setEnabled(!playwrightBackend);
        targetUrl.setToolTipText(playwrightBackend
                ? "The Playwright recorder start request does not take a target URL; navigate after starting."
                : "Initial http, https, or file URL the recording opens.");
        headlessBrowser.setEnabled(!playwrightBackend);
        headlessBrowser.setToolTipText(playwrightBackend
                ? "The Playwright recorder start request does not take a headless option."
                : "Record without a visible browser window; useful for agent-driven or CI recordings. "
                + "Remembered across sessions and honored by /record-web and /mobile web.");
        sessionPath.setToolTipText(mobile()
                ? "Mobile recording JSON output path (mobile_record_start outputPath)."
                : playwrightBackend
                ? "Playwright recording JSON output path (playwright_record_start outputPath)."
                : "Capture session JSON output path (capture_start outputPath).");
    }

    private void applyTemplate() {
        WorkflowTemplate template = selectedTemplate();
        if (template == null) {
            return;
        }
        switch (template) {
            case RECORD_BROWSER_FLOW -> prefill.prefill("test_automation_scenarios", capturePageObjectWorkflow());
            case START_MOBILE_EMULATION -> {
                // The mobile flow records against the emulated session, so keep the backend in sync.
                backend.setSelectedItem(BACKEND_MOBILE);
                prefill.prefill("mobile_initialize_web_emulation", mobileWebEmulation());
            }
            case ANALYZE_FAILED_ALLURE -> prefill.prefill("doctor_analyze_failed_allure", failedAllureAnalysis());
            case WEEKLY_FLAKY_TRIAGE -> prefill.prefill("doctor_analyze_failed_allure", weeklyFlakyTriage());
            case CONVERT_SELENIUM_SNIPPET -> prefill.prefill("test_automation_scenarios", seleniumConversionWorkflow());
            case CREATE_NEW_SHAFT_PROJECT -> prefill.prefill("shaft_project_create", newShaftProject());
            case INSPECT_CURRENT_PAGE_LOCATORS -> prefill.prefill("browser_get_page_dom", currentPageDomInspection());
            default -> throw new IllegalStateException("Unsupported workflow template: " + template);
        }
    }

    private WorkflowTemplate selectedTemplate() {
        Object selected = templateSelector.getSelectedItem();
        return selected instanceof WorkflowTemplate template ? template : null;
    }

    private static JsonObject capturePageObjectWorkflow() {
        JsonObject arguments = new JsonObject();
        arguments.addProperty("area", "capture");
        arguments.addProperty("intent",
                "Record a browser flow and generate review-only Page Object SHAFT code from the capture. "
                        + "Do not write source files before explicit approval.");
        arguments.addProperty("maxResults", 5);
        return arguments;
    }

    private static JsonObject seleniumConversionWorkflow() {
        JsonObject arguments = new JsonObject();
        arguments.addProperty("area", "web");
        arguments.addProperty("intent",
                "Convert a pasted Selenium WebDriver Java snippet to SHAFT syntax using SHAFT.GUI.WebDriver, "
                        + "driver.browser(), driver.element(), driver.element().touch(), and SHAFT.GUI.Locator. "
                        + "Do not use SHAFT.GUI.Locator.xpath; use the locator builder or By.xpath only as a last fallback. "
                        + "Return review-only code and do not write source files.");
        arguments.addProperty("maxResults", 5);
        return arguments;
    }

    private static JsonObject failedAllureAnalysis() {
        JsonObject arguments = new JsonObject();
        arguments.add("allureResultPaths", array("allure-results"));
        arguments.add("historicalBundlePaths", array());
        arguments.addProperty("outputDirectory", "target/shaft-doctor");
        arguments.addProperty("includeScreenshots", true);
        arguments.addProperty("includePageSnapshots", true);
        arguments.addProperty("minimumAllureResults", 1);
        arguments.addProperty("repositoryRoot", ".");
        arguments.add("allowedSourcePaths", array());
        arguments.addProperty("useAi", false);
        arguments.addProperty("allowLocalAi", false);
        arguments.addProperty("allowRemoteAi", false);
        arguments.addProperty("driverVariableName", "driver");
        return arguments;
    }

    /**
     * Weekly maintenance-loop triage (issue #3425 C5): the same deterministic Doctor batch as
     * {@link #failedAllureAnalysis()} plus historical bundles so repeat offenders surface as
     * trends. The template description points at healer_run_failed_test as the follow-up per
     * flaky test and at scheduling the loop through a weekly agent.
     */
    private static JsonObject weeklyFlakyTriage() {
        JsonObject arguments = failedAllureAnalysis();
        arguments.remove("historicalBundlePaths");
        arguments.add("historicalBundlePaths", array("target/shaft-doctor/history"));
        arguments.addProperty("outputDirectory", "target/shaft-doctor/weekly");
        return arguments;
    }

    private static JsonObject newShaftProject() {
        JsonObject arguments = new JsonObject();
        arguments.addProperty("outputDirectory", "shaft-web-testng");
        arguments.addProperty("runner", "TestNG");
        arguments.addProperty("platform", "web");
        arguments.addProperty("groupId", "io.github.yourUsername");
        arguments.addProperty("artifactId", "shaft-web-testng");
        arguments.addProperty("version", "1.0.0");
        arguments.add("optionalModules", array());
        arguments.addProperty("includeGithubActions", true);
        arguments.addProperty("includeDependabot", true);
        arguments.addProperty("overwrite", false);
        return arguments;
    }

    private JsonObject mobileWebEmulation() {
        JsonObject arguments = new JsonObject();
        arguments.addProperty("targetUrl", targetUrl.getText().trim());
        arguments.addProperty("browser", "CHROME");
        arguments.addProperty("deviceName", "Pixel 5");
        arguments.addProperty("width", 0);
        arguments.addProperty("height", 0);
        arguments.addProperty("pixelRatio", 0);
        arguments.addProperty("userAgent", "");
        arguments.addProperty("headless", headlessBrowser.isSelected());
        return arguments;
    }

    private static JsonObject currentPageDomInspection() {
        JsonObject arguments = new JsonObject();
        arguments.addProperty("maxCharacters", 12_000);
        return arguments;
    }

    private static JsonArray array(String... values) {
        JsonArray array = new JsonArray();
        for (String value : values) {
            array.add(value);
        }
        return array;
    }

    /**
     * The 90-second golden path (issue #3425 A1): one click extracts the bundled sample page,
     * points a visible WebDriver recording at it, and tells the user exactly what to do next.
     * The sample is a local file, so the first contact needs no site, no credentials, and leaks
     * nothing off the machine.
     */
    private void trySampleRecording() {
        java.nio.file.Path samplePage;
        try {
            samplePage = extractSamplePage();
        } catch (Exception extractionFailure) {
            setRecorderStatus("Could not prepare the sample page: " + extractionFailure.getMessage());
            return;
        }
        backend.setSelectedItem(BACKEND_WEBDRIVER);
        targetUrl.setText(samplePage.toUri().toString());
        intent.setText("Search the sample bookstore and add the first result to the cart");
        sessionPath.setText("recordings/sample-tour.json");
        headlessBrowser.setSelected(false);
        String tourGuidance = "Sample tour: a browser is opening on the bundled bookstore page. Search for a "
                + "book, add it to the cart, add an assertion on the cart status, then press Stop in the SHAFT "
                + "overlay and click Review code here.";
        ShaftMcpInvocationService invocationService = invocationService();
        if (invocationService == null) {
            startRecording();
            setRecorderStatus(tourGuidance + " (Run the prepared capture_start request to begin.)");
            return;
        }
        invocationService.startTool("capture_start", webdriverCaptureStartArguments())
                .future()
                .whenComplete((result, error) -> onEdt(() -> {
                    if (failed(result, error)) {
                        setRecorderStatus("Sample recording failed to start: " + failureText(result, error));
                        return;
                    }
                    setRecorderStatus(tourGuidance);
                    startStatusPolling("capture_status");
                }));
    }

    private java.nio.file.Path extractSamplePage() throws java.io.IOException {
        java.nio.file.Path directory = java.nio.file.Files.createTempDirectory("shaft-sample");
        java.nio.file.Path page = directory.resolve("shaft-sample-page.html");
        try (java.io.InputStream sample = GuidedWorkflowPanel.class
                .getResourceAsStream("/sample/shaft-sample-page.html")) {
            if (sample == null) {
                throw new java.io.IOException("bundled sample page resource is missing");
            }
            java.nio.file.Files.copy(sample, page, java.nio.file.StandardCopyOption.REPLACE_EXISTING);
        }
        return page;
    }

    private void startRecording() {
        if (mobile()) {
            startMobileRecording();
            return;
        }
        JsonObject arguments = new JsonObject();
        arguments.addProperty("outputPath", sessionPath.getText().trim());
        if (playwright()) {
            arguments.addProperty("mode", "default");
            arguments.addProperty("includeSensitiveValues", false);
            prefill.prefill("playwright_record_start", arguments);
            startStatusPolling("playwright_record_status");
        } else {
            prefill.prefill("capture_start", webdriverCaptureStartArguments());
            startStatusPolling("capture_status");
        }
    }

    private JsonObject webdriverCaptureStartArguments() {
        JsonObject arguments = new JsonObject();
        arguments.addProperty("outputPath", sessionPath.getText().trim());
        arguments.addProperty("targetUrl", targetUrl.getText().trim());
        arguments.addProperty("browser", "Chrome");
        arguments.addProperty("headless", headlessBrowser.isSelected());
        arguments.addProperty("sessionGoal", intent.getText().trim());
        return arguments;
    }

    private JsonObject mobileRecordStartArguments() {
        JsonObject arguments = new JsonObject();
        arguments.addProperty("outputPath", sessionPath.getText().trim());
        arguments.addProperty("mode", "default");
        arguments.addProperty("includeSensitiveValues", false);
        return arguments;
    }

    /**
     * One-click mobile recording: the emulated Chrome session and the recorder are started as one
     * chained action, with the second call gated on the first succeeding. Without an MCP-connected
     * project (headless tests, the live E2E panel driver) the panel falls back to preparing the
     * recorder-start request for review, matching the other backends.
     */
    private void startMobileRecording() {
        ShaftMcpInvocationService invocationService = invocationService();
        if (invocationService == null) {
            prefill.prefill("mobile_record_start", mobileRecordStartArguments());
            startStatusPolling("mobile_record_status");
            return;
        }
        setRecorderStatus("Starting emulated mobile session at "
                + displayUrl(targetUrl.getText().trim()) + "...");
        invocationService.startTool("mobile_initialize_web_emulation", mobileWebEmulation())
                .future()
                .whenComplete((sessionResult, sessionError) -> onEdt(() -> {
                    if (failed(sessionResult, sessionError)) {
                        setRecorderStatus("Mobile emulation session failed: "
                                + failureText(sessionResult, sessionError));
                        return;
                    }
                    setRecorderStatus("Emulated session ready. Starting mobile recorder...");
                    invocationService.startTool("mobile_record_start", mobileRecordStartArguments())
                            .future()
                            .whenComplete((recordResult, recordError) -> onEdt(() -> {
                                if (failed(recordResult, recordError)) {
                                    setRecorderStatus("Mobile recorder failed to start: "
                                            + failureText(recordResult, recordError));
                                    return;
                                }
                                setRecorderStatus("Mobile recording started.");
                                startStatusPolling("mobile_record_status");
                            }));
                }));
    }

    private void planPartnerWork() {
        JsonObject arguments = new JsonObject();
        arguments.addProperty("repositoryPath", ".");
        arguments.addProperty("intent", intent.getText().trim());
        arguments.addProperty("backend", backend.getSelectedItem().toString());
        arguments.addProperty("currentSourcePath", currentSourcePath());
        arguments.addProperty("selectedText", selectedText());
        arguments.add("artifactPaths", delimitedArray(artifactPaths.getText()));
        arguments.addProperty("maxResults", 10);
        prefill.prefill("shaft_coding_partner_plan", arguments);
    }

    private void findReuse() {
        JsonObject arguments = new JsonObject();
        arguments.addProperty("repositoryPath", ".");
        arguments.addProperty("maxResults", 10);
        prefill.prefill("capture_target_candidates", arguments);
    }

    private void stopRecording() {
        JsonObject arguments = new JsonObject();
        arguments.addProperty("discard", false);
        if (mobile()) {
            prefill.prefill("mobile_record_stop", arguments);
        } else {
            prefill.prefill(playwright() ? "playwright_record_stop" : "capture_stop", arguments);
        }
    }

    private void generateCode() {
        JsonObject arguments = new JsonObject();
        if (mobile()) {
            arguments.addProperty("recordingPath", sessionPath.getText().trim());
            arguments.addProperty("driverVariableName", "driver");
            prefill.prefill("mobile_recording_code_blocks", arguments);
        } else if (playwright()) {
            arguments.addProperty("recordingPath", sessionPath.getText().trim());
            arguments.addProperty("driverVariableName", "driver");
            prefill.prefill("playwright_recording_code_blocks", arguments);
        } else {
            arguments.addProperty("sessionPath", sessionPath.getText().trim());
            arguments.addProperty("outputDirectory", ".");
            arguments.addProperty("packageName", "tests.generated");
            arguments.addProperty("className", "GeneratedShaftTest");
            arguments.addProperty("overwrite", false);
            arguments.addProperty("driverVariableName", "driver");
            prefill.prefill("capture_code_blocks", arguments);
        }
    }

    private void inspectLocator() {
        JsonObject arguments = new JsonObject();
        arguments.addProperty("targetUrl", targetUrl.getText().trim());
        arguments.addProperty("userIntent", intent.getText().trim());
        arguments.addProperty("maxCharacters", 12_000);
        arguments.addProperty("maxElements", 10);
        prefill.prefill("browser_open_intent", arguments);
    }

    private void guardrailCheck() {
        JsonObject arguments = new JsonObject();
        arguments.addProperty("language", "java");
        arguments.addProperty("code", codeSnippet.getText());
        prefill.prefill("test_code_guardrails_check", arguments);
    }

    // ------------------------------------------------------------------
    // Live recorder status strip
    // ------------------------------------------------------------------

    private ShaftMcpInvocationService invocationService() {
        if (project == null) {
            return null;
        }
        try {
            return ShaftMcpInvocationService.getInstance(project);
        } catch (RuntimeException | Error unavailable) {
            return null;
        }
    }

    /**
     * Polls the backend's status tool every couple of seconds and mirrors the live recorder state
     * (event/action counts, pending debounced signals, current URL) in the status row, so headless
     * recordings are observable without a browser window. Polling stops once a previously active
     * recording ends, after an idle timeout, or on panel disposal.
     */
    private void startStatusPolling(String toolName) {
        if (invocationService() == null) {
            return;
        }
        statusToolName = toolName;
        recorderSeenActive = false;
        pollsWithoutActivity = 0;
        if (!pollingActive) {
            pollingActive = true;
            setRecorderStatus("Waiting for the recording to start (run the prepared request)...");
            scheduleNextStatusPoll();
        }
    }

    private void scheduleNextStatusPoll() {
        if (statusPollAlarm == null) {
            statusPollAlarm = new Alarm(Alarm.ThreadToUse.POOLED_THREAD, this);
        }
        if (!pollingActive || statusPollAlarm.isDisposed()) {
            pollingActive = false;
            return;
        }
        statusPollAlarm.addRequest(this::pollStatusOnce, STATUS_POLL_INTERVAL_MILLIS);
    }

    private void pollStatusOnce() {
        ShaftMcpInvocationService invocationService = invocationService();
        if (!pollingActive || invocationService == null) {
            pollingActive = false;
            return;
        }
        invocationService.startTool(statusToolName, new JsonObject())
                .future()
                .whenComplete((result, error) -> onEdt(() -> applyStatusPoll(result, error)));
    }

    private void applyStatusPoll(ShaftMcpToolResult result, Throwable error) {
        if (!pollingActive) {
            return;
        }
        if (failed(result, error)) {
            setRecorderStatus("Recorder status unavailable: " + failureText(result, error));
            scheduleNextStatusPoll();
            return;
        }
        JsonObject status = AssistantMarkdown.jsonObjectFromMcpOutput(result.output());
        boolean active = isRecorderActive(status);
        if (active) {
            recorderSeenActive = true;
            pollsWithoutActivity = 0;
            setRecorderStatus(activeStatusText(status));
            scheduleNextStatusPoll();
            return;
        }
        if (recorderSeenActive) {
            pollingActive = false;
            setRecorderStatus("Recording finished - " + countText(status)
                    + ". Use Review code to generate the reviewed test.");
            return;
        }
        pollsWithoutActivity++;
        if (pollsWithoutActivity >= MAX_POLLS_BEFORE_ACTIVE) {
            pollingActive = false;
            setRecorderStatus("No active recording detected. Recorder idle.");
            return;
        }
        scheduleNextStatusPoll();
    }

    private static boolean isRecorderActive(JsonObject status) {
        if (status == null) {
            return false;
        }
        if (status.has("active")) {
            return status.get("active").getAsBoolean();
        }
        String state = status.has("state") ? status.get("state").getAsString() : "";
        return "ACTIVE".equalsIgnoreCase(state) || "STARTING".equalsIgnoreCase(state)
                || "STOPPING".equalsIgnoreCase(state);
    }

    private static String activeStatusText(JsonObject status) {
        StringBuilder text = new StringBuilder("Recording ACTIVE - ").append(countText(status));
        if (status != null && status.has("currentUrl")
                && !status.get("currentUrl").getAsString().isBlank()) {
            text.append(" - ").append(displayUrl(status.get("currentUrl").getAsString()));
        }
        return text.append(". Stop recording ends the session.").toString();
    }

    private static String countText(JsonObject status) {
        if (status == null) {
            return "0 events";
        }
        if (status.has("actionCount")) {
            return status.get("actionCount").getAsInt() + " action(s)";
        }
        int events = status.has("eventCount") ? status.get("eventCount").getAsInt() : 0;
        int pending = status.has("pendingSignalCount") ? status.get("pendingSignalCount").getAsInt() : 0;
        return pending > 0 ? events + " event(s) (+" + pending + " pending)" : events + " event(s)";
    }

    private static String displayUrl(String url) {
        String value = url == null ? "" : url.trim();
        if (value.isBlank()) {
            return "(no URL)";
        }
        return value.length() <= 60 ? value : value.substring(0, 57) + "...";
    }

    private static boolean failed(ShaftMcpToolResult result, Throwable error) {
        return error != null || result == null || !result.success();
    }

    private static String failureText(ShaftMcpToolResult result, Throwable error) {
        if (error != null) {
            return String.valueOf(error.getMessage());
        }
        return result == null ? "no result" : displayUrl(result.output());
    }

    private void setRecorderStatus(String text) {
        recorderStatus.setText(text);
        recorderStatus.setToolTipText(text);
    }

    private static void onEdt(Runnable action) {
        var application = ApplicationManager.getApplication();
        if (application == null) {
            action.run();
        } else {
            application.invokeLater(action);
        }
    }

    JLabel recorderStatusLabel() {
        return recorderStatus;
    }

    @Override
    public void dispose() {
        pollingActive = false;
        if (statusPollAlarm != null && !statusPollAlarm.isDisposed()) {
            statusPollAlarm.cancelAllRequests();
        }
    }

    private boolean playwright() {
        return BACKEND_PLAYWRIGHT.equals(backend.getSelectedItem());
    }

    private boolean mobile() {
        return BACKEND_MOBILE.equals(backend.getSelectedItem());
    }

    private String currentSourcePath() {
        String typedPath = currentSourcePath.getText().trim();
        if (!typedPath.isBlank() || project == null) {
            return typedPath;
        }
        VirtualFile[] selectedFiles = FileEditorManager.getInstance(project).getSelectedFiles();
        return selectedFiles.length == 0 ? "" : selectedFiles[0].getPath();
    }

    private String selectedText() {
        String pastedText = codeSnippet.getText().trim();
        if (!pastedText.isBlank() || project == null) {
            return pastedText;
        }
        Editor editor = FileEditorManager.getInstance(project).getSelectedTextEditor();
        return editor == null || editor.getSelectionModel().getSelectedText() == null
                ? ""
                : editor.getSelectionModel().getSelectedText();
    }

    private static JsonArray delimitedArray(String values) {
        JsonArray array = new JsonArray();
        if (values == null || values.isBlank()) {
            return array;
        }
        for (String value : values.split("[,;\\n]+")) {
            String trimmed = value.trim();
            if (!trimmed.isBlank()) {
                array.add(trimmed);
            }
        }
        return array;
    }

    @FunctionalInterface
    interface ToolPrefill {
        void prefill(String toolName, JsonObject arguments);
    }

    private enum WorkflowTemplate {
        RECORD_BROWSER_FLOW(
                "Record browser flow and generate Page Object code",
                "Prefills a review-only capture workflow plan for Page Object code generation."),
        START_MOBILE_EMULATION(
                "Start mobile emulation session for recording",
                "Prefills a Chrome mobile web-emulation session and switches the backend to Mobile."),
        ANALYZE_FAILED_ALLURE(
                "Analyze failed Allure results",
                "Prefills deterministic Doctor analysis with AI and source edits disabled."),
        WEEKLY_FLAKY_TRIAGE(
                "Weekly flaky triage (maintenance loop)",
                "Prefills a batch Doctor analysis with historical bundles for trend detection; follow up with "
                        + "healer_run_failed_test per flaky test. Pair it with a weekly scheduled agent that sends "
                        + "/doctor and consolidates the report."),
        CONVERT_SELENIUM_SNIPPET(
                "Convert Selenium snippet to SHAFT syntax",
                "Prefills a review-only SHAFT syntax conversion workflow."),
        CREATE_NEW_SHAFT_PROJECT(
                "Create a new SHAFT project",
                "Prefills a new project request with overwrite disabled."),
        INSPECT_CURRENT_PAGE_LOCATORS(
                "Inspect current page locators",
                "Prefills a bounded current-page DOM inspection.");

        private final String label;
        private final String description;

        WorkflowTemplate(String label, String description) {
            this.label = label;
            this.description = description;
        }

        @Override
        public String toString() {
            return label;
        }
    }
}
