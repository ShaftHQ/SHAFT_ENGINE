package com.shaft.intellij.ui;

import com.google.gson.JsonArray;
import com.google.gson.JsonObject;
import com.intellij.openapi.Disposable;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.command.WriteCommandAction;
import com.intellij.openapi.editor.Document;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.fileEditor.FileDocumentManager;
import com.intellij.openapi.fileEditor.FileEditorManager;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiDocumentManager;
import com.intellij.psi.PsiFile;
import com.intellij.ui.components.JBTextArea;
import com.intellij.ui.components.JBTextField;
import com.intellij.util.Alarm;
import com.intellij.util.ui.JBUI;
import com.shaft.intellij.java.JavaTargetContextResolver;
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
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

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
    // Stable per-instance identity so overlapping recordings across surfaces don't collapse onto
    // one process-wide flag (issue #3591 item 3).
    private final String recordingKey = "guided#" + Integer.toHexString(System.identityHashCode(this));
    // Created lazily on the first poll so headless panel tests never touch platform executors.
    private Alarm statusPollAlarm;
    private volatile boolean pollingActive;
    private boolean headlessLockedByPolicy;
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
                        + "Remembered across sessions and honored by the assistant web and mobile recording flows.");
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
        // Status is the primary surface signal (issue #3500 G2): louder than the collapsed form.
        recorderStatus.setFont(recorderStatus.getFont().deriveFont(Font.BOLD, recorderStatus.getFont().getSize2D() + 2f));
        updateTemplateDescription();
        applyTeamRecorderPolicy();
        backend.addActionListener(event -> updateFieldRelevance());
        updateFieldRelevance();

        // Progressive disclosure (issue #3500 G1, #3496 B6): the primary surface is just the
        // target, the recorder controls, the sample-page tour, and the live status. Everything
        // else a returning power user needs stays one click away behind Advanced options.
        JPanel primaryFields = new JPanel(new GridLayout(0, 1, 4, 4));
        primaryFields.add(row("Target URL", 'U', targetUrl));
        primaryFields.add(row("Status", 'A', recorderStatus));

        JPanel advancedFields = new JPanel(new GridLayout(0, 1, 4, 4));
        advancedFields.add(row("Backend", 'B', backend));
        advancedFields.add(row("Template", 'T', templateControls(), templateSelector));
        advancedFields.add(row("Intent", 'I', intent));
        advancedFields.add(row("Current source", 'R', currentSourcePath));
        advancedFields.add(row("Evidence paths", 'E', artifactPaths));
        advancedFields.add(row("Session path", 'S', sessionPath));
        advancedFields.add(row("Browser", 'H', headlessBrowser));

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
                button("Review code", "Generate reviewed SHAFT code blocks from a recording", ShaftIcons.CODE, this::generateCode),
                // Closes the recorder -> editor loop (issue #3548 item 1): "Review code" only
                // prefills the Tools panel for manual copy; these execute the same *_code_blocks
                // tool and write the result into the editor, mirroring the Assistant Capture-review
                // strip's "Insert into open class"/"Create test class" seams.
                button("Insert at caret", "Insert generated SHAFT code at the editor caret", ShaftIcons.EDIT,
                        this::insertCodeAtCaret),
                button("Create test class", "Create a test class from the generated recording", ShaftIcons.CODE,
                        this::createTestClassFromRecording));
        JPanel locator = section("Locator",
                button("Inspect locator", "Inspect the page and propose locator candidates", ShaftIcons.SEARCH, this::inspectLocator),
                button("Guardrail check", "Check generated SHAFT code for automation anti-patterns", ShaftIcons.CHECK, this::guardrailCheck));

        JPanel advancedActions = new JPanel(new GridLayout(1, 2, 8, 8));
        advancedActions.add(partner);
        advancedActions.add(locator);

        JPanel advancedCenter = new JPanel(new BorderLayout(6, 6));
        advancedCenter.add(advancedFields, BorderLayout.NORTH);
        advancedCenter.add(row("Code", 'C', codeSnippet), BorderLayout.CENTER);

        JPanel advancedPanel = new JPanel(new BorderLayout(6, 6));
        advancedPanel.setBorder(JBUI.Borders.emptyTop(8));
        advancedPanel.add(advancedCenter, BorderLayout.CENTER);
        advancedPanel.add(advancedActions, BorderLayout.SOUTH);
        // Collapsed by default; a project that already opted into advanced/expert mode
        // (ShaftSettingsState.Settings#advancedUiEnabled) starts expanded instead.
        advancedPanel.setVisible(this.settings.advancedUiEnabled);

        JCheckBox advancedToggle = new JCheckBox("Advanced options", this.settings.advancedUiEnabled);
        advancedToggle.getAccessibleContext().setAccessibleName("Show advanced Guided options");
        advancedToggle.getAccessibleContext().setAccessibleDescription(
                "Show backend, template, intent, current source, evidence paths, session path, browser, "
                        + "generated code, coding partner, and locator controls.");
        advancedToggle.addItemListener(event -> {
            advancedPanel.setVisible(advancedToggle.isSelected());
            revalidate();
            repaint();
        });

        JPanel primaryPanel = new JPanel(new BorderLayout(6, 6));
        primaryPanel.add(primaryFields, BorderLayout.NORTH);
        primaryPanel.add(recorder, BorderLayout.CENTER);
        primaryPanel.add(advancedToggle, BorderLayout.SOUTH);

        JPanel body = new JPanel(new BorderLayout(6, 6));
        body.add(primaryPanel, BorderLayout.NORTH);
        body.add(advancedPanel, BorderLayout.CENTER);

        add(introLabel("Guided workflows prepare reviewed SHAFT MCP requests."), BorderLayout.NORTH);
        add(body, BorderLayout.CENTER);
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
     * Mirrors the checked-in team recorder policy ({@code .shaft/recorder-policy.json}, issue
     * #3425 C4) in the panel: the headless default is applied and locked, and the default session
     * path moves into the team output directory. Enforcement happens server-side in SHAFT MCP's
     * {@code capture_start}; this keeps the UI honest about what will actually happen.
     */
    private void applyTeamRecorderPolicy() {
        if (project == null || project.getBasePath() == null) {
            return;
        }
        java.nio.file.Path policyFile = java.nio.file.Path.of(
                project.getBasePath(), ".shaft", "recorder-policy.json");
        if (!java.nio.file.Files.isRegularFile(policyFile)) {
            return;
        }
        try {
            JsonObject policy = com.google.gson.JsonParser
                    .parseString(java.nio.file.Files.readString(policyFile)).getAsJsonObject();
            if (policy.has("headless") && policy.get("headless").isJsonPrimitive()
                    && policy.get("headless").getAsJsonPrimitive().isBoolean()) {
                headlessBrowser.setSelected(policy.get("headless").getAsBoolean());
                headlessLockedByPolicy = true;
                headlessBrowser.setToolTipText(
                        "Locked by this repository's team policy (.shaft/recorder-policy.json)");
            }
            String outputDirectory = policy.has("outputDirectory")
                    && policy.get("outputDirectory").isJsonPrimitive()
                    ? policy.get("outputDirectory").getAsString().trim()
                    : "";
            if (!outputDirectory.isBlank()) {
                sessionPath.setText(outputDirectory + "/intellij-capture.json");
            }
            setRecorderStatus("Team recorder policy applied from .shaft/recorder-policy.json.");
        } catch (Exception malformedPolicy) {
            setRecorderStatus("Could not read .shaft/recorder-policy.json: " + malformedPolicy.getMessage());
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
        headlessBrowser.setEnabled(!playwrightBackend && !headlessLockedByPolicy);
        headlessBrowser.setToolTipText(playwrightBackend
                ? "The Playwright recorder start request does not take a headless option."
                : "Record without a visible browser window; useful for agent-driven or CI recordings. "
                + "Remembered across sessions and honored by the assistant web and mobile recording flows.");
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
        if (!headlessLockedByPolicy) {
            headlessBrowser.setSelected(false);
        }
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
        boolean playwrightBackend = playwright();
        String startTool = playwrightBackend ? "playwright_record_start" : "capture_start";
        String statusTool = playwrightBackend ? "playwright_record_status" : "capture_status";
        JsonObject arguments;
        if (playwrightBackend) {
            arguments = new JsonObject();
            arguments.addProperty("outputPath", sessionPath.getText().trim());
            arguments.addProperty("mode", "default");
            arguments.addProperty("includeSensitiveValues", false);
        } else {
            arguments = webdriverCaptureStartArguments();
        }
        // With an MCP-connected project, Start recording runs the request directly (matching the
        // sample tour and the mobile chain) instead of parking a prepared request the user still
        // has to run; headless panel tests and disconnected projects keep the prefill fallback.
        ShaftMcpInvocationService invocationService = invocationService();
        if (invocationService == null) {
            prefill.prefill(startTool, arguments);
            startStatusPolling(statusTool);
            return;
        }
        setRecorderStatus("Starting the recording...");
        invocationService.startTool(startTool, arguments)
                .future()
                .whenComplete((result, error) -> onEdt(() -> {
                    if (failed(result, error)) {
                        setRecorderStatus("Recording failed to start: " + failureText(result, error));
                        return;
                    }
                    setRecorderStatus("Recording started. Interact with the browser, then press Stop recording.");
                    startStatusPolling(statusTool);
                }));
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
        String stopTool = mobile()
                ? "mobile_record_stop"
                : playwright() ? "playwright_record_stop" : "capture_stop";
        ShaftMcpInvocationService invocationService = invocationService();
        if (invocationService == null) {
            prefill.prefill(stopTool, arguments);
            return;
        }
        setRecorderStatus("Stopping the recording...");
        invocationService.startTool(stopTool, arguments)
                .future()
                .whenComplete((result, error) -> onEdt(() -> {
                    if (failed(result, error)) {
                        setRecorderStatus("Recording failed to stop: " + failureText(result, error));
                        return;
                    }
                    setRecorderStatus("Recording stopped. Use Review code to generate the reviewed test.");
                }));
    }

    private void generateCode() {
        prefill.prefill(codeBlocksToolName(), codeBlocksArguments());
    }

    /**
     * Returns the {@code *_code_blocks} MCP tool matching the selected backend, shared by
     * "Review code", "Insert at caret", and "Create test class" so the three actions always
     * generate from the same recording.
     */
    private String codeBlocksToolName() {
        if (mobile()) {
            return "mobile_recording_code_blocks";
        }
        if (playwright()) {
            return "playwright_recording_code_blocks";
        }
        return "capture_code_blocks";
    }

    private JsonObject codeBlocksArguments() {
        JsonObject arguments = new JsonObject();
        if (mobile() || playwright()) {
            arguments.addProperty("recordingPath", sessionPath.getText().trim());
            arguments.addProperty("driverVariableName", "driver");
        } else {
            arguments.addProperty("sessionPath", sessionPath.getText().trim());
            arguments.addProperty("outputDirectory", ".");
            arguments.addProperty("packageName", "tests.generated");
            arguments.addProperty("className", "GeneratedShaftTest");
            arguments.addProperty("overwrite", false);
            arguments.addProperty("driverVariableName", "driver");
        }
        return arguments;
    }

    /**
     * "Insert at caret" (issue #3548 item 1): executes the same {@code *_code_blocks} tool "Review
     * code" only prefills, then writes the generated {@code TEST_METHOD} snippet (falling back to
     * the full class) into the editor at the caret -- the same {@code FileDocumentManager
     * .requestWriting} + {@code WriteCommandAction} seam {@code PickLocatorAtCaretAction} already
     * proved, not a new insertion mechanism.
     */
    private void insertCodeAtCaret() {
        ShaftMcpInvocationService invocationService = invocationService();
        if (invocationService == null) {
            prefill.prefill(codeBlocksToolName(), codeBlocksArguments());
            return;
        }
        if (selectedJavaEditor() == null) {
            setRecorderStatus("Open a Java file in the editor first.");
            return;
        }
        setRecorderStatus("Generating code to insert at caret...");
        invocationService.startTool(codeBlocksToolName(), codeBlocksArguments())
                .future()
                .whenComplete((result, error) -> onEdt(() -> applyInsertAtCaret(result, error)));
    }

    private void applyInsertAtCaret(ShaftMcpToolResult result, Throwable error) {
        if (failed(result, error)) {
            setRecorderStatus("Could not generate code to insert: " + failureText(result, error));
            return;
        }
        JsonObject raw = AssistantMarkdown.jsonObjectFromMcpOutput(result.output());
        String snippet = firstBlockByKind(raw, "TEST_METHOD");
        if (snippet.isBlank()) {
            snippet = firstFullClassBlock(raw);
        }
        if (snippet.isBlank()) {
            setRecorderStatus("The generated result has no insertable code block.");
            return;
        }
        Editor editor = selectedJavaEditor();
        if (editor == null) {
            setRecorderStatus("Open a Java file in the editor first.");
            return;
        }
        Document document = editor.getDocument();
        if (!FileDocumentManager.getInstance().requestWriting(document, project)) {
            setRecorderStatus("The current file could not be made writable.");
            return;
        }
        int offset = Math.max(0, Math.min(editor.getCaretModel().getOffset(), document.getTextLength()));
        String toInsert = snippet;
        WriteCommandAction.writeCommandAction(project)
                .withName("Insert SHAFT Recorded Code")
                .run(() -> document.insertString(offset, toInsert));
        setRecorderStatus("Inserted generated code at the caret.");
    }

    /**
     * "Create test class" (issue #3548 item 1): executes the same {@code *_code_blocks} tool, then
     * writes the {@code FULL_CLASS} block into {@code src/test/java} (never overwriting) and opens
     * it, mirroring {@code ShaftAssistantPanel#createTestClassFromReview}.
     */
    private void createTestClassFromRecording() {
        ShaftMcpInvocationService invocationService = invocationService();
        if (invocationService == null) {
            prefill.prefill(codeBlocksToolName(), codeBlocksArguments());
            return;
        }
        if (project == null || project.getBasePath() == null) {
            setRecorderStatus("No open project.");
            return;
        }
        setRecorderStatus("Generating test class...");
        invocationService.startTool(codeBlocksToolName(), codeBlocksArguments())
                .future()
                .whenComplete((result, error) -> onEdt(() -> applyCreateTestClass(result, error)));
    }

    private void applyCreateTestClass(ShaftMcpToolResult result, Throwable error) {
        if (failed(result, error)) {
            setRecorderStatus("Could not generate test class: " + failureText(result, error));
            return;
        }
        JsonObject raw = AssistantMarkdown.jsonObjectFromMcpOutput(result.output());
        String code = firstFullClassBlock(raw);
        if (code.isBlank()) {
            setRecorderStatus("The generated result has no full-class code block.");
            return;
        }
        Matcher classMatcher = Pattern.compile("class\\s+(\\w+)").matcher(code);
        Matcher packageMatcher = Pattern.compile("package\\s+([\\w.]+)\\s*;").matcher(code);
        if (!classMatcher.find()) {
            setRecorderStatus("Could not determine the generated class name.");
            return;
        }
        String body = packageMatcher.find() ? code : "package tests.generated;\n\n" + code;
        String packagePath = (packageMatcher.reset().find() ? packageMatcher.group(1) : "tests.generated")
                .replace('.', '/');
        Path target = Path.of(project.getBasePath(), "src", "test", "java")
                .resolve(packagePath).resolve(classMatcher.group(1) + ".java");
        try {
            if (Files.exists(target)) {
                setRecorderStatus("Already exists - opened " + target.getFileName() + " (not overwritten).");
            } else {
                Files.createDirectories(target.getParent());
                Files.writeString(target, body);
                setRecorderStatus("Created " + target.getFileName() + " in src/test/java.");
            }
            openFileInEditor(target);
        } catch (IOException writeFailure) {
            setRecorderStatus("Could not write test class: " + writeFailure.getMessage());
        }
    }

    /**
     * Returns the editor selected in the editor tab strip, gated on it holding a resolvable Java
     * target (same gate {@code PickLocatorAtCaretAction#isAvailable} uses), or null when there is
     * none -- callers treat null as "open a Java file first".
     */
    private Editor selectedJavaEditor() {
        if (project == null) {
            return null;
        }
        Editor editor = FileEditorManager.getInstance(project).getSelectedTextEditor();
        if (editor == null) {
            return null;
        }
        PsiFile file = PsiDocumentManager.getInstance(project).getPsiFile(editor.getDocument());
        return JavaTargetContextResolver.resolve(file, editor.getCaretModel().getOffset()) != null ? editor : null;
    }

    private void openFileInEditor(Path path) {
        try {
            var virtualFile = com.intellij.openapi.vfs.LocalFileSystem.getInstance()
                    .refreshAndFindFileByNioFile(path);
            if (virtualFile != null && project != null) {
                FileEditorManager.getInstance(project).openFile(virtualFile, true);
            }
        } catch (RuntimeException | Error headlessTestEnvironment) {
            // Best effort: the path was already reported in the status label.
        }
    }

    /**
     * Returns the {@code code} of the first {@code codeBlocks[]} entry whose {@code kind} matches,
     * or {@code ""} when none match. Kind (not id) is used because the id differs per backend
     * (e.g. {@code capture-test-method} vs {@code playwright-replay-method}).
     */
    private static String firstBlockByKind(JsonObject raw, String kind) {
        if (raw == null || !raw.has("codeBlocks") || !raw.get("codeBlocks").isJsonArray()) {
            return "";
        }
        for (var element : raw.getAsJsonArray("codeBlocks")) {
            if (!element.isJsonObject()) {
                continue;
            }
            JsonObject block = element.getAsJsonObject();
            if (kind.equals(jsonString(block, "kind"))) {
                String code = blockCode(block);
                if (!code.isBlank()) {
                    return code;
                }
            }
        }
        return "";
    }

    /**
     * Returns the {@code capture-full-class} block's code, falling back to the first block whose
     * code contains {@code "class "} when the id is missing (mirrors {@code
     * ShaftAssistantPanel#firstJavaClassBlock}).
     */
    private static String firstFullClassBlock(JsonObject raw) {
        if (raw == null || !raw.has("codeBlocks") || !raw.get("codeBlocks").isJsonArray()) {
            return "";
        }
        String fallback = "";
        for (var element : raw.getAsJsonArray("codeBlocks")) {
            if (!element.isJsonObject()) {
                continue;
            }
            JsonObject block = element.getAsJsonObject();
            String code = blockCode(block);
            if (code.isBlank() || !code.contains("class ")) {
                continue;
            }
            if ("capture-full-class".equals(jsonString(block, "id"))) {
                return code;
            }
            if (fallback.isBlank()) {
                fallback = code;
            }
        }
        return fallback;
    }

    // McpCodeBlock's source is in "code"; a few callers key on "content" instead, so both are
    // checked (mirrors ShaftAssistantPanel#firstJavaClassBlock).
    private static String blockCode(JsonObject block) {
        String code = jsonString(block, "code");
        return code.isBlank() ? jsonString(block, "content") : code;
    }

    private static String jsonString(JsonObject object, String key) {
        return object.has(key) && object.get(key).isJsonPrimitive() ? object.get(key).getAsString() : "";
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
        // Feeds the shared readiness strip's recording badge (issue #3500 A4).
        if (active) {
            ShaftRecordingActivity.started(recordingKey);
        } else {
            ShaftRecordingActivity.stopped(recordingKey);
        }
        if (active) {
            recorderSeenActive = true;
            pollsWithoutActivity = 0;
            setRecorderStatus(activeStatusText(status));
            scheduleNextStatusPoll();
            return;
        }
        if (recorderSeenActive) {
            pollingActive = false;
            String state = status != null && status.has("state") ? status.get("state").getAsString() : "";
            if ("INCOMPLETE".equalsIgnoreCase(state) || "FAILED".equalsIgnoreCase(state)) {
                setRecorderStatus("Recording ended unexpectedly (" + state.toUpperCase(java.util.Locale.ROOT)
                        + ") - " + countText(status)
                        + ". The browser or recorder died before Stop; re-record the flow before generating code.");
                return;
            }
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

    // Mirrors the overlay's pill glossary (#3496 B7): mode, steps count, and human-cased
    // readiness read identically in both surfaces, and either surface can stop safely.
    private static String activeStatusText(JsonObject status) {
        StringBuilder text = new StringBuilder("Recording · ").append(countText(status));
        String readiness = readinessLabel(status);
        if (!readiness.isBlank()) {
            text.append(" · ").append(readiness);
        }
        if (status != null && status.has("currentUrl")
                && !status.get("currentUrl").getAsString().isBlank()) {
            text.append(" · ").append(displayUrl(status.get("currentUrl").getAsString()));
        }
        return text.append(". Stop here or in the browser overlay - both save the session.").toString();
    }

    private static String readinessLabel(JsonObject status) {
        if (status == null || !status.has("readiness")) {
            return "";
        }
        return switch (status.get("readiness").getAsString().toUpperCase(java.util.Locale.ROOT)) {
            case "READY" -> "Ready";
            case "RISKY" -> "Risky";
            case "BLOCKED" -> "Blocked";
            default -> "";
        };
    }

    // Recorded units read "steps" in every user-facing surface (shared authoring glossary,
    // #3496/#3501) even though the wire fields keep their eventCount/actionCount names.
    private static String countText(JsonObject status) {
        if (status == null) {
            return "0 steps";
        }
        int steps = status.has("actionCount")
                ? status.get("actionCount").getAsInt()
                : status.has("eventCount") ? status.get("eventCount").getAsInt() : 0;
        int pending = status.has("pendingSignalCount") ? status.get("pendingSignalCount").getAsInt() : 0;
        String base = steps + (steps == 1 ? " step" : " steps");
        return pending > 0 ? base + " (+" + pending + " pending)" : base;
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
        // Prevents a stuck-active recording key if the panel closes mid-recording (#3591 item 3).
        ShaftRecordingActivity.stopped(recordingKey);
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
                        + "healer_run_failed_test per flaky test. Pair it with a weekly scheduled agent that runs "
                        + "the Doctor analysis and consolidates the report."),
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
