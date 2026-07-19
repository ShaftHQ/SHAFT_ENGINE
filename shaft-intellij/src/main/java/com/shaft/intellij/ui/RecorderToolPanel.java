package com.shaft.intellij.ui;

import com.google.gson.JsonObject;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.command.WriteCommandAction;
import com.intellij.openapi.editor.Document;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.fileEditor.FileDocumentManager;
import com.intellij.openapi.fileEditor.FileEditorManager;
import com.intellij.openapi.project.Project;
import com.intellij.psi.PsiDocumentManager;
import com.intellij.psi.PsiFile;
import com.intellij.ui.components.JBCheckBox;
import com.intellij.ui.components.JBTextField;
import com.intellij.util.ui.JBUI;
import com.shaft.intellij.java.JavaTargetContext;
import com.shaft.intellij.java.JavaTargetContextResolver;
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
import java.awt.Dimension;
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
 *
 * <p>{@link #startRecordingAtTarget} is the record-at-cursor entry point (issue #3661): {@code
 * RecordShaftFlowHereAction}'s advanced mode calls it directly instead of prefilling {@code
 * capture_record_at_target_code_blocks} for manual copy-paste, and stopping the resulting
 * recording routes straight into that tool plus an insert-at-caret, collapsing caret -> live
 * recording -> review/insert into the one caret action.</p>
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
    // Stable per-instance identity for the shared readiness strip's live-recording indicator
    // (issue #3661), mirroring GuidedWorkflowPanel#recordingKey so overlapping recordings across
    // tool-window surfaces don't collapse onto one process-wide flag (issue #3591 item 3).
    private final String recordingKey = "recorder#" + Integer.toHexString(System.identityHashCode(this));
    // Set by startRecordingAtTarget() (issue #3661): when non-null, the next successful
    // capture_stop routes straight into capture_record_at_target_code_blocks and inserts the
    // result at the caret instead of leaving "Review code" as a separate manual step -- the
    // "route straight into the existing review/insert flow when the user stops recording" the
    // issue asks for. Cleared once consumed so a later plain Stop click (no live target) falls
    // back to the generic message.
    private JavaTargetContext targetContext;

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

        JPanel targetUrlRow = row("Target URL", 'U', targetUrl);
        JPanel browserRow = row("Browser", 'B', browser);
        JPanel headlessRow = row("Headless", 'H', headless);
        JPanel outputPathRow = row("Output path", 'O', outputPath);
        // Issue #3771: each row() panel packs its own label at that label's natural width, so
        // without this the fields beside "Browser"/"Headless" land left of "Target URL"/"Output
        // path" -- a ragged left edge next to a real aligned form.
        alignLabelColumn(targetUrlRow, browserRow, headlessRow, outputPathRow);

        JPanel quickStartFields = new JPanel(new GridLayout(0, 1, 4, 4));
        quickStartFields.add(targetUrlRow);
        quickStartFields.add(browserRow);
        quickStartFields.add(headlessRow);
        quickStartFields.add(outputPathRow);

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

    /**
     * Starts a WebDriver recording anchored at a resolved Java caret target (issue #3661):
     * {@code RecordShaftFlowHereAction}'s advanced mode calls this directly instead of copying a
     * prefilled {@code capture_record_at_target_code_blocks} request to the clipboard for the user
     * to run manually after recording elsewhere. Stopping the resulting recording routes straight
     * into {@link #reviewAndInsertAtTarget}, collapsing caret -> live recording -> review/insert
     * into the one caret action.
     *
     * @param context resolved Java caret target the generated code will be anchored at
     */
    void startRecordingAtTarget(JavaTargetContext context) {
        this.targetContext = context;
        startRecording();
    }

    private void startRecording() {
        if (!guardReady()) {
            return;
        }
        JavaTargetContext target = targetContext;
        setStatus(target == null
                ? "Starting the recording..."
                : "Starting the recording anchored at " + target.methodName() + " in " + target.className() + "...");
        ShaftMcpInvocationService.getInstance(project).startTool("capture_start", captureStartArguments())
                .future()
                .whenComplete((result, error) -> onEdt(() -> {
                    if (failed(result, error)) {
                        setStatus("Recording failed to start: " + failureText(result, error));
                        return;
                    }
                    ShaftRecordingActivity.started(recordingKey);
                    setStatus(target == null
                            ? "Recording started. Interact with the browser, then press Stop recording."
                            : "Recording live - anchored at " + target.methodName() + " in " + target.className()
                                    + ". Interact with the browser, then press Stop recording to review and insert.");
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
                    ShaftRecordingActivity.stopped(recordingKey);
                    if (failed(result, error)) {
                        setStatus("Recording failed to stop: " + failureText(result, error));
                        return;
                    }
                    JavaTargetContext target = targetContext;
                    targetContext = null;
                    if (target != null) {
                        reviewAndInsertAtTarget(target);
                        return;
                    }
                    setStatus("Recording stopped. Use Review code to generate the reviewed test.");
                }));
    }

    /**
     * The "route straight into the existing review/insert flow when the user stops recording" half
     * of issue #3661: generates focused record-at-target code blocks anchored at {@code context},
     * then inserts the resulting method snippet at the caret -- the same {@code
     * FileDocumentManager.requestWriting} + {@code WriteCommandAction} seam {@code
     * GuidedWorkflowPanel#insertCodeAtCaret} already proved, not a new insertion mechanism.
     */
    private void reviewAndInsertAtTarget(JavaTargetContext context) {
        setStatus("Recording stopped. Generating code anchored at " + context.methodName()
                + " in " + context.className() + "...");
        ShaftMcpInvocationService.getInstance(project)
                .startTool("capture_record_at_target_code_blocks", captureRecordAtTargetArguments(context))
                .future()
                .whenComplete((result, error) -> onEdt(() -> applyRecordAtTargetResult(result, error, context)));
    }

    private void applyRecordAtTargetResult(ShaftMcpToolResult result, Throwable error, JavaTargetContext context) {
        if (failed(result, error)) {
            setStatus("Could not generate code anchored at " + context.methodName() + ": "
                    + failureText(result, error));
            return;
        }
        JsonObject raw = AssistantMarkdown.jsonObjectFromMcpOutput(result.output());
        String snippet = firstBlockByKind(raw, "TEST_METHOD");
        if (snippet.isBlank()) {
            setStatus("Recording stopped. The generated result has no insertable code block; "
                    + "open Advanced options to review it.");
            return;
        }
        Editor editor = selectedJavaEditor();
        if (editor == null) {
            setStatus("Recording stopped and code generated, but no Java file is open to insert into. "
                    + "Open " + context.className() + " and use Review code to insert manually.");
            return;
        }
        Document document = editor.getDocument();
        if (!FileDocumentManager.getInstance().requestWriting(document, project)) {
            setStatus("The current file could not be made writable.");
            return;
        }
        int offset = Math.max(0, Math.min(editor.getCaretModel().getOffset(), document.getTextLength()));
        WriteCommandAction.writeCommandAction(project)
                .withName("Insert SHAFT Recorded Code")
                .run(() -> document.insertString(offset, snippet));
        setStatus("Inserted generated code anchored at " + context.methodName() + " in " + context.className() + ".");
    }

    /**
     * Returns the editor selected in the editor tab strip, gated on it holding a resolvable Java
     * target (mirrors {@code GuidedWorkflowPanel#selectedJavaEditor}), or null when there is none --
     * callers treat null as "open a Java file first".
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

    /**
     * Returns the {@code code} of the first {@code codeBlocks[]} entry whose {@code kind} matches,
     * or {@code ""} when none match (mirrors {@code GuidedWorkflowPanel#firstBlockByKind}).
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

    // McpCodeBlock's source is in "code"; a few callers key on "content" instead, so both are
    // checked (mirrors GuidedWorkflowPanel#blockCode).
    private static String blockCode(JsonObject block) {
        String code = jsonString(block, "code");
        return code.isBlank() ? jsonString(block, "content") : code;
    }

    private static String jsonString(JsonObject object, String key) {
        return object.has(key) && object.get(key).isJsonPrimitive() ? object.get(key).getAsString() : "";
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
        // sessionGoal is only added for a record-at-target recording (issue #3661): the curated
        // default Quick Start template has no "intent" concept, matching the class javadoc's note
        // that GuidedWorkflowPanel owns that curated behavior, not this standalone tab.
        if (targetContext != null) {
            arguments.addProperty("sessionGoal",
                    "Record a SHAFT flow at " + targetContext.methodName() + " in " + targetContext.className());
        }
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

    /**
     * Builds {@code capture_record_at_target_code_blocks}'s arguments from {@code context}, matching
     * the shape {@code RecordShaftFlowHereAction} used to build for its clipboard prefill (issue
     * #3661). Package-private for direct unit testing, mirroring {@link #captureCodeBlocksArguments()}.
     */
    JsonObject captureRecordAtTargetArguments(JavaTargetContext context) {
        JsonObject arguments = new JsonObject();
        arguments.addProperty("sessionPath", outputPath.getText().trim());
        arguments.addProperty("outputDirectory", project == null || project.getBasePath() == null
                ? "." : project.getBasePath());
        arguments.addProperty("packageName", context.packageName());
        arguments.addProperty("className", context.className());
        arguments.addProperty("overwrite", false);
        arguments.addProperty("targetSourcePath", context.sourcePath());
        arguments.addProperty("insertAfter", context.methodName());
        arguments.addProperty("driverVariableName", "driver");
        return arguments;
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

    /**
     * Equalizes the label column of a set of {@link #row} panels (issue #3771): each row is its own
     * independent {@link BorderLayout}, so left to its own natural size a longer label (e.g.
     * "Target URL") pushes its field further right than a shorter sibling label (e.g. "Browser"),
     * producing a ragged left edge across the Quick Start section. Widening every label to the
     * group's widest preferred size lines every field up in one column.
     */
    private static void alignLabelColumn(JPanel... rows) {
        int columnWidth = 0;
        for (JPanel row : rows) {
            columnWidth = Math.max(columnWidth, rowLabel(row).getPreferredSize().width);
        }
        for (JPanel row : rows) {
            JLabel label = rowLabel(row);
            label.setPreferredSize(new Dimension(columnWidth, label.getPreferredSize().height));
        }
    }

    private static JLabel rowLabel(JPanel row) {
        return (JLabel) row.getComponent(0);
    }

    private static JButton button(String text, String description, Icon icon, Runnable action) {
        JButton button = new JButton();
        ShaftIconButtons.apply(button, description, text, icon);
        button.getAccessibleContext().setAccessibleDescription(description);
        button.addActionListener(event -> action.run());
        return button;
    }
}
