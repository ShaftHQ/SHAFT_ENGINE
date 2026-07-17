package com.shaft.intellij.ui;

import com.google.gson.JsonObject;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.project.Project;
import com.intellij.ui.components.JBCheckBox;
import com.intellij.ui.components.JBTextField;
import com.intellij.util.ui.JBUI;
import com.shaft.intellij.mcp.ShaftMcpInvocationService;
import com.shaft.intellij.mcp.ShaftMcpToolResult;
import com.shaft.intellij.settings.ShaftSettingsState;
import org.jetbrains.annotations.NotNull;

import javax.swing.Icon;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JPanel;
import java.awt.BorderLayout;
import java.awt.FlowLayout;
import java.awt.Font;
import java.awt.GridLayout;
import java.util.List;
import java.util.Set;

/**
 * Curated standalone Recorder tab (issue #3665 part B). Unlike the raw-JSON {@link
 * ShaftFeaturePanel} it used to be, this is a composite: a WebDriver-only "Quick Start" section
 * (mirroring {@link GuidedWorkflowPanel}'s capture-start argument shape and Swing idiom, since
 * Playwright/Mobile recording already has a fully curated home in the Guided tab) on top of an
 * unmodified, embedded {@link ShaftFeaturePanel} carrying every other Recorder-category tool
 * (target discovery, record-at-target, backend comparison, evidence packs, checkpoints, replay
 * generation, and {@code capture_status}/{@code capture_code_blocks} for power users), collapsed
 * by default behind an "Advanced" toggle. Zero changes to {@link ShaftFeaturePanel} itself: this
 * class only composes it, so the other four tabs that reuse that class are unaffected.
 *
 * <p>Guard pattern mirrors {@link ShaftFeaturePanel#run}: {@code settings.mcpReady()} then {@code
 * project != null}, both surfaced in the status label rather than crashing or silently no-op'ing
 * -- unlike {@link GuidedWorkflowPanel}, which falls back to a review-only prefill when there is no
 * live MCP connection, this panel always calls the MCP tool directly once both guards pass.</p>
 */
final class RecorderToolPanel extends JPanel {
    /** Matches {@code ToolTemplates.recorder()}'s {@code capture_start} template default. */
    static final String DEFAULT_OUTPUT_PATH = "recordings/intellij-capture.json";
    private static final Set<String> QUICK_START_TOOLS =
            Set.of("capture_start", "capture_stop", "capture_status", "capture_code_blocks");

    private final Project project;
    private final ShaftSettingsState.Settings settings;
    private final ShaftFeaturePanel featurePanel;
    private final JBTextField targetUrl;
    private final JComboBox<String> browser;
    private final JBCheckBox headless;
    private final JBTextField outputPath;
    private final JLabel status;
    private final JPanel advancedPanel;
    private final JCheckBox advancedToggle;

    RecorderToolPanel(Project project, @NotNull ShaftSettingsState.Settings settings) {
        super(new BorderLayout(6, 6));
        this.project = project;
        this.settings = settings;
        setBorder(JBUI.Borders.empty(8));

        targetUrl = field("Target URL", "");
        // Only Chrome and Edge are accepted server-side by capture_start's browser argument
        // (parsed via CaptureBrowser#parse) -- Firefox/Safari would be silently rejected, so they
        // are not offered here (same reasoning as GuidedWorkflowPanel#recorderBrowser, issue #3660).
        browser = new JComboBox<>(new String[]{"Chrome", "Edge"});
        browser.getAccessibleContext().setAccessibleName("Recorder browser");
        headless = new JBCheckBox("Headless browser", settings.recorderHeadless);
        headless.getAccessibleContext().setAccessibleName("Headless browser");
        headless.addItemListener(event -> settings.recorderHeadless = headless.isSelected());
        outputPath = field("Output path", DEFAULT_OUTPUT_PATH);
        status = new JLabel("Ready");
        status.getAccessibleContext().setAccessibleName("Recorder status");
        setStatus("Ready");
        status.setFont(status.getFont().deriveFont(Font.BOLD));

        JButton start = button("Start recording", "Start a WebDriver SHAFT recording",
                ShaftIcons.SEND, this::startRecording);
        JButton stop = button("Stop recording", "Stop the active SHAFT recording",
                ShaftIcons.CANCEL, this::stopRecording);
        JButton checkStatus = button("Check status", "Check the SHAFT recording status",
                ShaftIcons.CHECK, this::checkStatus);
        JButton review = button("Review code", "Generate reviewed SHAFT code blocks from the recording",
                ShaftIcons.CODE, this::reviewCode);

        JPanel quickStartFields = new JPanel(new GridLayout(0, 1, 4, 4));
        quickStartFields.add(row("Target URL", 'U', targetUrl));
        quickStartFields.add(row("Browser", 'B', browser));
        quickStartFields.add(row("Headless", 'H', headless));
        quickStartFields.add(row("Output path", 'O', outputPath));

        JPanel actions = new JPanel(new FlowLayout(FlowLayout.LEFT, 6, 0));
        actions.add(start);
        actions.add(stop);
        actions.add(checkStatus);
        actions.add(review);

        JPanel actionsAndStatus = new JPanel(new BorderLayout(4, 4));
        actionsAndStatus.add(actions, BorderLayout.NORTH);
        actionsAndStatus.add(row("Status", 'A', status), BorderLayout.SOUTH);

        JPanel quickStart = new JPanel(new BorderLayout(6, 6));
        quickStart.add(introLabel("Record a WebDriver flow with SHAFT MCP."), BorderLayout.NORTH);
        quickStart.add(quickStartFields, BorderLayout.CENTER);
        quickStart.add(actionsAndStatus, BorderLayout.SOUTH);

        featurePanel = new ShaftFeaturePanel(project, settings,
                List.of(new ToolCategory("Recorder", ToolTemplates.recorder())));

        advancedPanel = new JPanel(new BorderLayout());
        advancedPanel.add(featurePanel, BorderLayout.CENTER);
        advancedPanel.setBorder(JBUI.Borders.emptyTop(8));
        // Collapsed by default: the curated Quick Start controls above are what most users need
        // (mirrors GuidedWorkflowPanel's advancedPanel/advancedToggle pattern exactly).
        advancedPanel.setVisible(false);

        advancedToggle = new JCheckBox("Advanced: all Recorder tools (raw JSON)", false);
        advancedToggle.getAccessibleContext().setAccessibleName("Show advanced Recorder tools");
        advancedToggle.getAccessibleContext().setAccessibleDescription(
                "Show the full raw-JSON Recorder tool catalog: target discovery, record-at-target, "
                        + "backend comparison, evidence packs, checkpoints, and replay generation.");
        advancedToggle.addItemListener(event -> {
            advancedPanel.setVisible(advancedToggle.isSelected());
            revalidate();
            repaint();
        });

        JPanel advancedSection = new JPanel(new BorderLayout(4, 4));
        advancedSection.add(advancedToggle, BorderLayout.NORTH);
        advancedSection.add(advancedPanel, BorderLayout.CENTER);

        JPanel body = new JPanel(new BorderLayout(6, 6));
        body.add(quickStart, BorderLayout.NORTH);
        body.add(advancedSection, BorderLayout.CENTER);
        add(body, BorderLayout.CENTER);
    }

    /** Package-private test accessor: the embedded raw-JSON panel for every other Recorder tool. */
    ShaftFeaturePanel featurePanel() {
        return featurePanel;
    }

    /**
     * Delegates to the embedded {@link ShaftFeaturePanel}, then expands the Advanced section
     * whenever the prefilled tool is not one this tab's Quick Start section curates -- otherwise the
     * prefilled request would land in a collapsed section the user never sees (required for {@code
     * RecordShaftFlowHereAction}-style prefill routing: {@link ShaftToolWindowPanel#prefillTool}
     * calls this for every {@code capture_*} tool).
     *
     * @param toolName MCP tool name
     * @param arguments JSON arguments
     * @return {@code true} if a Recorder-category template owns {@code toolName}
     */
    boolean prefillTool(String toolName, JsonObject arguments) {
        boolean matched = featurePanel.prefillTool(toolName, arguments);
        if (matched && !QUICK_START_TOOLS.contains(toolName)) {
            advancedToggle.setSelected(true);
        }
        return matched;
    }

    private void startRecording() {
        if (!guardReady()) {
            return;
        }
        setStatus("Starting the recording...");
        ShaftMcpInvocationService.getInstance(project).startTool("capture_start", captureStartArguments())
                .future()
                .whenComplete((result, error) -> onEdt(() -> {
                    if (failed(result, error)) {
                        setStatus("Recording failed to start: " + failureText(result, error));
                        return;
                    }
                    setStatus("Recording started. Interact with the browser, then press Stop recording.");
                }));
    }

    private void stopRecording() {
        if (!guardReady()) {
            return;
        }
        setStatus("Stopping the recording...");
        ShaftMcpInvocationService.getInstance(project).startTool("capture_stop", captureStopArguments())
                .future()
                .whenComplete((result, error) -> onEdt(() -> {
                    if (failed(result, error)) {
                        setStatus("Recording failed to stop: " + failureText(result, error));
                        return;
                    }
                    setStatus("Recording stopped. Use Review code to generate the reviewed test.");
                }));
    }

    private void checkStatus() {
        if (!guardReady()) {
            return;
        }
        setStatus("Checking recording status...");
        ShaftMcpInvocationService.getInstance(project).startTool("capture_status", captureStatusArguments())
                .future()
                .whenComplete((result, error) -> onEdt(() -> {
                    if (failed(result, error)) {
                        setStatus("Recorder status unavailable: " + failureText(result, error));
                        return;
                    }
                    setStatus("Recorder status refreshed. Open Advanced options for the full status.");
                }));
    }

    private void reviewCode() {
        if (!guardReady()) {
            return;
        }
        setStatus("Generating reviewed code...");
        ShaftMcpInvocationService.getInstance(project).startTool("capture_code_blocks", captureCodeBlocksArguments())
                .future()
                .whenComplete((result, error) -> onEdt(() -> {
                    if (failed(result, error)) {
                        setStatus("Could not generate code: " + failureText(result, error));
                        return;
                    }
                    setStatus("Code generated. Open Advanced options to review the raw output.");
                }));
    }

    /**
     * Mirrors {@link ShaftFeaturePanel#run}'s guard order: MCP configuration first, then an open
     * project, each surfaced as plain-language status text rather than a crash or silent no-op.
     */
    private boolean guardReady() {
        if (!settings.mcpReady()) {
            setStatus("Configure SHAFT MCP in Settings before running Recorder tools.");
            return false;
        }
        if (project == null) {
            setStatus("Open an IntelliJ project before running SHAFT MCP tools.");
            return false;
        }
        return true;
    }

    /**
     * Builds {@code capture_start}'s arguments from the Quick Start fields, matching {@code
     * ToolTemplates.recorder()}'s template shape exactly (targetUrl, browser, outputPath, headless
     * -- no {@code sessionGoal}, since this standalone tab has no Guided-style "intent" concept and
     * the curated default template does not include one). Package-private for direct unit testing,
     * mirroring {@code GuidedWorkflowPanel#webdriverCaptureStartArguments()}.
     */
    JsonObject captureStartArguments() {
        JsonObject arguments = new JsonObject();
        arguments.addProperty("outputPath", outputPath.getText().trim());
        arguments.addProperty("targetUrl", targetUrl.getText().trim());
        arguments.addProperty("browser", (String) browser.getSelectedItem());
        arguments.addProperty("headless", headless.isSelected());
        return arguments;
    }

    static JsonObject captureStopArguments() {
        JsonObject arguments = new JsonObject();
        arguments.addProperty("discard", false);
        return arguments;
    }

    static JsonObject captureStatusArguments() {
        return new JsonObject();
    }

    /** Mirrors {@code ToolTemplates.recorder()}'s "Generate Code Blocks" template defaults. */
    JsonObject captureCodeBlocksArguments() {
        JsonObject arguments = new JsonObject();
        arguments.addProperty("sessionPath", outputPath.getText().trim());
        arguments.addProperty("outputDirectory", ".");
        arguments.addProperty("packageName", "tests.generated");
        arguments.addProperty("className", "RecordedFlowTest");
        arguments.addProperty("overwrite", false);
        arguments.addProperty("driverVariableName", "driver");
        return arguments;
    }

    private void setStatus(String text) {
        status.setText(text);
        status.setToolTipText(text);
        status.getAccessibleContext().setAccessibleDescription(text);
    }

    private static void onEdt(Runnable action) {
        var application = ApplicationManager.getApplication();
        if (application == null) {
            action.run();
        } else {
            application.invokeLater(action);
        }
    }

    private static boolean failed(ShaftMcpToolResult result, Throwable error) {
        return error != null || result == null || !result.success();
    }

    private static String failureText(ShaftMcpToolResult result, Throwable error) {
        if (error != null) {
            return String.valueOf(error.getMessage());
        }
        return result == null ? "no result" : result.output();
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
        JPanel row = new JPanel(new BorderLayout(4, 4));
        JLabel label = new JLabel(labelText);
        label.setDisplayedMnemonic(mnemonic);
        label.setLabelFor(component);
        row.add(label, BorderLayout.WEST);
        row.add(component, BorderLayout.CENTER);
        return row;
    }

    private static JButton button(String text, String description, Icon icon, Runnable action) {
        JButton button = new JButton();
        ShaftIconButtons.apply(button, description, text, icon);
        button.getAccessibleContext().setAccessibleDescription(description);
        button.addActionListener(event -> action.run());
        return button;
    }
}
