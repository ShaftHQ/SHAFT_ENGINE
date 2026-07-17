package com.shaft.intellij.ui;

import com.google.gson.JsonArray;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.intellij.openapi.Disposable;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.ide.CopyPasteManager;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.ui.DialogWrapper;
import com.intellij.ui.components.JBScrollPane;
import com.intellij.ui.components.JBTextArea;
import com.intellij.ui.components.JBTextField;
import com.intellij.ui.table.JBTable;
import com.intellij.util.Alarm;
import com.intellij.util.ui.JBUI;
import com.shaft.intellij.mcp.ShaftMcpInvocation;
import com.shaft.intellij.mcp.ShaftMcpInvocationService;
import com.shaft.intellij.mcp.ShaftMcpToolResult;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.RowFilter;
import javax.swing.table.AbstractTableModel;
import javax.swing.table.TableRowSorter;
import java.awt.BorderLayout;
import java.awt.FlowLayout;
import java.awt.Font;
import java.awt.datatransfer.StringSelection;
import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;

/**
 * Displays a live SHAFT API recording session: transactions are polled roughly every 2 seconds on
 * a background thread, and the {@link JBTable} model is only ever mutated on the EDT. Polling
 * stops on panel disposal or when the Stop button ends the session.
 *
 * <p>Backs both the browser-based recorder ({@link CaptureMode#WEB}, polling
 * {@code capture_api_transactions}/{@code capture_api_stop}) and the no-browser mobile API proxy
 * ({@link CaptureMode#PURE_API}, polling {@code mobile_api_record_transactions}/
 * {@code mobile_api_record_stop}) -- issue #3530 A2. {@code capture_api_generate} is shared by
 * both: it is already session-path-agnostic, taking only the persisted session JSON path.
 *
 * <p>Stop/Generate/Pin Fields/Copy CA Certificate keep visible text (unlike every other day-to-day
 * panel, which is icon-only via {@code ShaftIconButtons.apply}) and carry Alt-reachable mnemonics
 * (S/G/P/C respectively, non-conflicting within each button's visible row) per the setup panel's
 * convention (issue #3637).
 */
public final class ApiRecordingSessionPanel extends JPanel implements Disposable {
    private static final int POLL_INTERVAL_MILLIS = 2_000;
    private static final String GENERATE_TOOL_NAME = "capture_api_generate";
    private static final String RESPONSE_LEAVES_TOOL_NAME = "capture_api_response_leaves";

    /**
     * Selects which MCP tool pair a session polls. Both pairs return the same body-free
     * {@code NetworkTransaction} array shape, so {@link #parseTransactions} needs no mode
     * awareness of its own.
     */
    public enum CaptureMode {
        WEB("capture_api_transactions", "capture_api_stop"),
        PURE_API("mobile_api_record_transactions", "mobile_api_record_stop");

        private final String transactionsTool;
        private final String stopTool;

        CaptureMode(String transactionsTool, String stopTool) {
            this.transactionsTool = transactionsTool;
            this.stopTool = stopTool;
        }

        /** Package-private for {@code ApiRecordingSessionPanelTest}. */
        String transactionsTool() {
            return transactionsTool;
        }

        /** Package-private for {@code ApiRecordingSessionPanelTest}. */
        String stopTool() {
            return stopTool;
        }
    }

    private final Project project;
    private final CaptureMode mode;
    private final TransactionTableModel tableModel = new TransactionTableModel();
    private final JBTable table = new JBTable(tableModel);
    private final TableRowSorter<TransactionTableModel> rowSorter = new TableRowSorter<>(tableModel);
    private final JBTextField filterField = new JBTextField();
    private final JButton stopButton = new JButton("Stop");
    private final JButton generateButton = new JButton("Generate");
    private final JButton pinFieldsButton = new JButton("Pin Fields...");
    private final JLabel statusLabel = new JLabel("Recording...");
    private final Alarm pollAlarm;

    // Populated by the pin-fields dialog (issue #3530 negative-case: "pin this path as a business
    // assertion" panel control) and threaded into capture_api_generate's pinnedJsonPaths argument.
    private final Set<String> pinnedJsonPaths = new LinkedHashSet<>();

    // PURE_API only: the loopback proxy has no browser to drive, so the device/emulator needs the
    // proxy port plus a one-click copy of the CA certificate the MITM proxy signs traffic with
    // (issue #3530 A2 -- session panel/status parity for the pure-API path).
    private final JBTextArea pairingInfoArea = new JBTextArea("Waiting for the loopback proxy to start...");
    private final JButton copyCertificateButton = new JButton("Copy CA Certificate");
    private String caCertificatePem = "";

    private volatile boolean stopped;
    // Written on the pooled poll thread (pollOnce) and read on the EDT (stopRecording/dispose);
    // volatile so a Stop/close on the EDT always observes the in-flight invocation to cancel it,
    // and so the poll thread and EDT never disagree on the session identity/output path.
    private volatile String sessionId;
    private volatile String sessionOutputPath = "";
    private volatile ShaftMcpInvocation currentInvocation;

    /**
     * Creates a {@link CaptureMode#WEB} panel and immediately begins polling for transactions.
     *
     * @param project current IntelliJ project
     * @param targetUrl the URL the session was started for
     * @param sessionId the session identifier returned by {@code capture_api_start}, or null if unknown yet
     */
    public ApiRecordingSessionPanel(@NotNull Project project, @NotNull String targetUrl, @Nullable String sessionId) {
        this(project, CaptureMode.WEB, "Recording: " + targetUrl, sessionId);
    }

    /**
     * Creates the panel for the given {@link CaptureMode} and immediately begins polling for
     * transactions.
     *
     * @param project current IntelliJ project
     * @param mode which MCP tool pair to poll (issue #3530 A2)
     * @param headerText title shown above the transactions table
     * @param sessionId the session identifier returned by the mode's start tool, or null if unknown yet
     */
    public ApiRecordingSessionPanel(@NotNull Project project, @NotNull CaptureMode mode, @NotNull String headerText,
            @Nullable String sessionId) {
        super(new BorderLayout(6, 6));
        this.project = project;
        this.mode = mode;
        this.sessionId = sessionId;
        this.pollAlarm = new Alarm(Alarm.ThreadToUse.POOLED_THREAD, this);
        setBorder(JBUI.Borders.empty(8));

        table.setRowSorter(rowSorter);
        table.getColumnModel().getColumn(TransactionTableModel.INCLUDE_COLUMN).setMaxWidth(70);
        table.getAccessibleContext().setAccessibleName("API recording transactions");

        filterField.getAccessibleContext().setAccessibleName("Filter transactions");
        filterField.getEmptyText().setText("Filter by method, URL, or status...");
        filterField.getDocument().addDocumentListener(new javax.swing.event.DocumentListener() {
            @Override
            public void insertUpdate(javax.swing.event.DocumentEvent event) {
                applyFilter();
            }

            @Override
            public void removeUpdate(javax.swing.event.DocumentEvent event) {
                applyFilter();
            }

            @Override
            public void changedUpdate(javax.swing.event.DocumentEvent event) {
                applyFilter();
            }
        });

        stopButton.addActionListener(event -> stopRecording());
        generateButton.setEnabled(false);
        generateButton.getAccessibleContext().setAccessibleDescription(
                "Generate a SHAFT.API test from the included transactions");
        generateButton.addActionListener(event -> generateCode());
        pinFieldsButton.setEnabled(false);
        pinFieldsButton.getAccessibleContext().setAccessibleDescription(
                "Pick volatile response fields to force-assert as business assertions");
        pinFieldsButton.addActionListener(event -> openPinFieldsDialog());
        assignActionRowMnemonics(stopButton, pinFieldsButton, generateButton);

        JPanel header = new JPanel(new FlowLayout(FlowLayout.LEFT, 6, 2));
        JLabel titleLabel = new JLabel(headerText);
        titleLabel.setFont(titleLabel.getFont().deriveFont(Font.BOLD));
        header.add(titleLabel);

        JPanel actions = new JPanel(new FlowLayout(FlowLayout.RIGHT, 6, 0));
        actions.add(stopButton);
        actions.add(pinFieldsButton);
        actions.add(generateButton);

        JPanel toolbar = new JPanel(new BorderLayout(6, 0));
        toolbar.add(filterField, BorderLayout.CENTER);
        toolbar.add(actions, BorderLayout.EAST);

        JPanel north = new JPanel(new BorderLayout(4, 4));
        north.add(header, BorderLayout.NORTH);
        north.add(toolbar, BorderLayout.SOUTH);
        if (mode == CaptureMode.PURE_API) {
            north.add(buildPairingInfoPanel(), BorderLayout.CENTER);
        }

        add(north, BorderLayout.NORTH);
        add(new JBScrollPane(table), BorderLayout.CENTER);
        add(statusLabel, BorderLayout.SOUTH);

        startPolling();
    }

    /**
     * Builds the pure-API pairing panel: the loopback proxy has no browser to drive, so the
     * device/emulator needs the proxy host:port plus the CA certificate the proxy signs traffic
     * with, populated once the start tool call returns (see {@link #showPairingInfo}).
     */
    private JComponent buildPairingInfoPanel() {
        pairingInfoArea.setEditable(false);
        pairingInfoArea.setLineWrap(true);
        pairingInfoArea.setWrapStyleWord(true);
        pairingInfoArea.getAccessibleContext().setAccessibleName("Mobile API capture pairing instructions");

        copyCertificateButton.setEnabled(false);
        copyCertificateButton.getAccessibleContext().setAccessibleDescription(
                "Copy the loopback proxy's CA certificate to install on the device or emulator");
        copyCertificateButton.addActionListener(event -> {
            if (!caCertificatePem.isBlank()) {
                CopyPasteManager.getInstance().setContents(new StringSelection(caCertificatePem));
            }
        });
        // Own visible row (issue #3637): never shares a row with the actions above, so no
        // cross-row conflict is possible -- still gets a real mnemonic for Alt-reachability.
        copyCertificateButton.setMnemonic('C');

        JPanel pairingActions = new JPanel(new FlowLayout(FlowLayout.LEFT, 6, 2));
        pairingActions.add(copyCertificateButton);

        JPanel pairingPanel = new JPanel(new BorderLayout(4, 4));
        pairingPanel.setBorder(JBUI.Borders.empty(4, 0));
        pairingPanel.add(new JBScrollPane(pairingInfoArea), BorderLayout.CENTER);
        pairingPanel.add(pairingActions, BorderLayout.SOUTH);
        return pairingPanel;
    }

    /**
     * Populates the pure-API pairing panel once {@code mobile_api_record_start} (or a later
     * {@code mobile_api_record_status} poll) returns the proxy port, CA certificate, and any
     * pairing warnings. A no-op for a {@link CaptureMode#WEB} panel.
     *
     * @param proxyPort loopback port the MITM proxy is listening on
     * @param caCertificatePem PEM-encoded CA certificate for the device/emulator to trust
     * @param warnings pairing guidance and any capture warnings
     */
    public void showPairingInfo(int proxyPort, @NotNull String caCertificatePem, @NotNull List<String> warnings) {
        if (mode != CaptureMode.PURE_API) {
            return;
        }
        this.caCertificatePem = caCertificatePem;
        copyCertificateButton.setEnabled(!caCertificatePem.isBlank());
        StringBuilder text = new StringBuilder("Loopback proxy listening on 127.0.0.1:").append(proxyPort).append('.');
        for (String warning : warnings) {
            text.append('\n').append(warning);
        }
        pairingInfoArea.setText(text.toString());
    }

    /**
     * Returns the table component for tests and screenshot rendering.
     *
     * @return the transactions table
     */
    public JComponent transactionsTable() {
        return table;
    }

    /**
     * Returns the underlying table model for tests.
     *
     * @return table model
     */
    TransactionTableModel tableModel() {
        return tableModel;
    }

    /**
     * Assigns non-conflicting mnemonics to the {@code actions} row's three buttons (issue #3637):
     * extracted as a static, pure method (rather than inlined in the constructor) so
     * {@code ApiRecordingSessionPanelTest} can verify the assignment has no duplicate mnemonic
     * without needing a live {@link Project} to construct the full panel (this panel starts an
     * {@link Alarm} and MCP polling in its constructor, so it is not itself headlessly testable).
     *
     * @param stop the Stop button
     * @param pinFields the Pin Fields... button
     * @param generate the Generate button
     */
    static void assignActionRowMnemonics(JButton stop, JButton pinFields, JButton generate) {
        stop.setMnemonic('S');
        pinFields.setMnemonic('P');
        generate.setMnemonic('G');
    }

    JBTextField filterField() {
        return filterField;
    }

    JButton stopButton() {
        return stopButton;
    }

    JButton generateButton() {
        return generateButton;
    }

    JButton pinFieldsButton() {
        return pinFieldsButton;
    }

    JLabel statusLabel() {
        return statusLabel;
    }

    private void applyFilter() {
        String text = filterField.getText();
        if (text == null || text.isBlank()) {
            rowSorter.setRowFilter(null);
            return;
        }
        rowSorter.setRowFilter(RowFilter.regexFilter("(?i)" + java.util.regex.Pattern.quote(text.trim())));
    }

    private void startPolling() {
        scheduleNextPoll();
    }

    private void scheduleNextPoll() {
        if (stopped || pollAlarm.isDisposed()) {
            return;
        }
        pollAlarm.addRequest(this::pollOnce, POLL_INTERVAL_MILLIS);
    }

    /**
     * Runs a single poll cycle off the EDT (the Alarm is POOLED_THREAD) and
     * applies the resulting transactions to the table model on the EDT.
     */
    private void pollOnce() {
        if (stopped) {
            return;
        }
        JsonObject arguments = new JsonObject();
        if (sessionId != null) {
            arguments.addProperty("sessionId", sessionId);
        }
        currentInvocation = ShaftMcpInvocationService.getInstance(project)
                .startTool(mode.transactionsTool, arguments);
        currentInvocation.future().whenComplete((result, error) ->
                ApplicationManager.getApplication().invokeLater(() -> {
                    if (stopped) {
                        return;
                    }
                    applyPollResult(result, error);
                    scheduleNextPoll();
                }));
    }

    private void applyPollResult(ShaftMcpToolResult result, Throwable error) {
        if (error != null || result == null || !result.success()) {
            statusLabel.setText(error != null ? "Poll error: " + error.getMessage()
                    : result != null ? result.output() : "Poll failed");
            return;
        }
        List<TransactionRow> rows = parseTransactions(result.output());
        tableModel.setRows(rows);
        statusLabel.setText("Recording... " + rows.size() + " transaction(s)");
    }

    // Package-private (not private) for ApiRecordingSessionPanelTest: this parsing logic is pure
    // and worth covering directly without standing up the full panel (which needs a live Project).
    static List<TransactionRow> parseTransactions(String output) {
        List<TransactionRow> rows = new ArrayList<>();
        // capture_api_transactions returns a bare List<NetworkTransaction> -- a JSON array, not an
        // object -- so this must unwrap to an array, not look for an object with a "transactions"
        // key (that key never exists on the wire and previously left this table permanently empty).
        JsonArray transactions = AssistantMarkdown.jsonArrayFromMcpOutput(output);
        if (transactions == null) {
            return rows;
        }
        for (JsonElement element : transactions) {
            if (!element.isJsonObject()) {
                continue;
            }
            JsonObject transaction = element.getAsJsonObject();
            rows.add(new TransactionRow(
                    stringOf(transaction, "transactionId"),
                    stringOf(transaction, "method"),
                    stringOf(transaction, "url"),
                    intOf(transaction, "statusCode") == 0 ? "" : String.valueOf(intOf(transaction, "statusCode")),
                    stringOf(transaction, "resourceKind"),
                    responseSize(transaction),
                    true));
        }
        return rows;
    }

    private static String stringOf(JsonObject object, String key) {
        JsonElement element = object.get(key);
        return element == null || element.isJsonNull() ? "" : element.getAsString();
    }

    private static int intOf(JsonObject object, String key) {
        JsonElement element = object.get(key);
        return element == null || element.isJsonNull() || !element.isJsonPrimitive() ? 0 : element.getAsInt();
    }

    // NetworkTransaction#bodyRefMetadata nests a "response" entry with a "sizeBytes" field (see
    // CaptureSessionStore#putBodyRefMetadata); absent when the transaction has no response body.
    private static String responseSize(JsonObject transaction) {
        JsonElement metadata = transaction.get("bodyRefMetadata");
        if (metadata == null || !metadata.isJsonObject()) {
            return "";
        }
        JsonElement response = metadata.getAsJsonObject().get("response");
        if (response == null || !response.isJsonObject()) {
            return "";
        }
        JsonElement sizeBytes = response.getAsJsonObject().get("sizeBytes");
        return sizeBytes == null || sizeBytes.isJsonNull() ? "" : sizeBytes.getAsString();
    }

    /**
     * Stops the poller and invokes {@code capture_api_stop} to end the recording
     * session. The final table state remains visible.
     */
    public void stopRecording() {
        if (stopped) {
            return;
        }
        stopped = true;
        pollAlarm.cancelAllRequests();
        if (currentInvocation != null) {
            currentInvocation.cancel();
        }
        stopButton.setEnabled(false);
        filterField.setEnabled(true);
        statusLabel.setText("Stopping...");

        JsonObject arguments = new JsonObject();
        if (sessionId != null) {
            arguments.addProperty("sessionId", sessionId);
        }
        ShaftMcpInvocationService.getInstance(project)
                .startTool(mode.stopTool, arguments)
                .future()
                .whenComplete((result, error) -> ApplicationManager.getApplication().invokeLater(() -> {
                    if (error != null || result == null || !result.success()) {
                        statusLabel.setText("Stopped (with errors). " + tableModel.getRowCount() + " transaction(s).");
                        return;
                    }
                    // capture_api_stop returns CaptureStatus, whose outputPath is the persisted session
                    // JSON capture_api_generate needs -- captured here so Generate has a sessionPath.
                    JsonObject status = AssistantMarkdown.jsonObjectFromMcpOutput(result.output());
                    sessionOutputPath = status != null && status.has("outputPath")
                            ? status.get("outputPath").getAsString()
                            : "";
                    generateButton.setEnabled(!sessionOutputPath.isBlank());
                    pinFieldsButton.setEnabled(!sessionOutputPath.isBlank());
                    statusLabel.setText("Stopped. " + tableModel.getRowCount() + " transaction(s).");
                }));
    }

    /**
     * Fetches classified response leaves for the stopped session via
     * {@code capture_api_response_leaves} and opens a picker dialog over the volatile ones -- the
     * only leaves a pin can actually affect (stable leaves are already asserted; sensitive leaves
     * are never asserted regardless of pinning) -- issue #3530 negative-case.
     */
    private void openPinFieldsDialog() {
        pinFieldsButton.setEnabled(false);
        statusLabel.setText("Loading response fields...");

        JsonObject arguments = new JsonObject();
        arguments.addProperty("sessionPath", sessionOutputPath);
        JsonArray excludedTransactionIds = new JsonArray();
        tableModel.excludedTransactionIds().forEach(excludedTransactionIds::add);
        arguments.add("excludedTransactionIds", excludedTransactionIds);

        ShaftMcpInvocationService.getInstance(project)
                .startTool(RESPONSE_LEAVES_TOOL_NAME, arguments)
                .future()
                .whenComplete((result, error) -> ApplicationManager.getApplication().invokeLater(() -> {
                    pinFieldsButton.setEnabled(true);
                    if (error != null || result == null || !result.success()) {
                        statusLabel.setText("Could not load response fields: "
                                + (result != null ? result.output() : String.valueOf(error)));
                        return;
                    }
                    showPinFieldsDialog(parsePinnableLeaves(result.output()));
                }));
    }

    private void showPinFieldsDialog(List<PinnableLeafRow> rows) {
        if (rows.isEmpty()) {
            statusLabel.setText("No volatile response fields to pin -- stable fields are already asserted.");
            return;
        }
        PinnableLeafTableModel pickerModel = new PinnableLeafTableModel(rows, pinnedJsonPaths);
        new PinFieldsDialog(project, pickerModel, pinnedJsonPaths).show();
    }

    /**
     * Modal picker for volatile response fields to pin as business assertions (issue #3635).
     * A {@link DialogWrapper} instead of a hand-rolled {@code JDialog} so sizing goes through
     * {@link JBUI} and Esc/Enter bindings come for free from the platform's default Cancel/OK
     * actions, instead of raw pixel sizing with no keyboard shortcuts.
     */
    static final class PinFieldsDialog extends DialogWrapper {
        private final PinnableLeafTableModel pickerModel;
        private final Set<String> pinnedJsonPaths;
        private final JBTable picker;

        PinFieldsDialog(@Nullable Project project, PinnableLeafTableModel pickerModel, Set<String> pinnedJsonPaths) {
            super(project, false);
            this.pickerModel = pickerModel;
            this.pinnedJsonPaths = pinnedJsonPaths;
            this.picker = new JBTable(pickerModel);
            picker.getColumnModel().getColumn(PinnableLeafTableModel.PIN_COLUMN).setMaxWidth(50);
            picker.getAccessibleContext().setAccessibleName(
                    "Volatile response fields to pin as business assertions");
            setTitle("Pin Response Fields");
            init();
        }

        @Override
        protected JComponent createCenterPanel() {
            JBScrollPane scrollPane = new JBScrollPane(picker);
            scrollPane.setPreferredSize(JBUI.size(640, 360));
            return scrollPane;
        }

        // On OK, replace the panel's pinned set with the picker's checked JSON paths. On Cancel
        // (the default DialogWrapper action, bound to Esc for free) nothing here runs, so
        // pinnedJsonPaths is left untouched -- matching the original dialog.dispose() cancel path.
        @Override
        protected void doOKAction() {
            pinnedJsonPaths.clear();
            pinnedJsonPaths.addAll(pickerModel.pinnedJsonPaths());
            super.doOKAction();
        }
    }

    /**
     * Parses {@code capture_api_response_leaves}'s per-transaction leaf array into one row per
     * {@code VOLATILE} leaf -- the only classification a pin can affect. Package-private for
     * {@code ApiRecordingSessionPanelTest}.
     *
     * @param output raw MCP tool output ({@code List<ApiCaptureGenerator.TransactionLeaves>} JSON)
     * @return one row per volatile, non-blank leaf, in encounter order
     */
    static List<PinnableLeafRow> parsePinnableLeaves(String output) {
        List<PinnableLeafRow> rows = new ArrayList<>();
        JsonArray transactions = AssistantMarkdown.jsonArrayFromMcpOutput(output);
        if (transactions == null) {
            return rows;
        }
        for (JsonElement transactionElement : transactions) {
            if (!transactionElement.isJsonObject()) {
                continue;
            }
            JsonObject transaction = transactionElement.getAsJsonObject();
            String method = stringOf(transaction, "method");
            String url = stringOf(transaction, "url");
            JsonElement leavesElement = transaction.get("leaves");
            if (leavesElement == null || !leavesElement.isJsonArray()) {
                continue;
            }
            for (JsonElement leafElement : leavesElement.getAsJsonArray()) {
                if (!leafElement.isJsonObject()) {
                    continue;
                }
                JsonObject leaf = leafElement.getAsJsonObject();
                String value = stringOf(leaf, "value");
                if (!"VOLATILE".equals(stringOf(leaf, "classification")) || value.isBlank()) {
                    continue;
                }
                rows.add(new PinnableLeafRow(method, url, stringOf(leaf, "jsonPath"), value));
            }
        }
        return rows;
    }

    /**
     * One volatile, pinnable response leaf shown in the pin-fields picker.
     *
     * @param method HTTP method of the transaction the leaf came from
     * @param url request URL of the transaction the leaf came from
     * @param jsonPath JSONPath pointer this pin targets; {@code capture_api_generate}'s
     *                 {@code pinnedJsonPaths} matches by this path across every transaction
     * @param value recorded sample value, for the user's reference
     */
    record PinnableLeafRow(String method, String url, String jsonPath, String value) {
    }

    /**
     * Table model backing the pin-fields picker dialog.
     */
    static final class PinnableLeafTableModel extends AbstractTableModel {
        static final int PIN_COLUMN = 0;
        private static final String[] COLUMNS = {"Pin", "Transaction", "JSON Path", "Sample Value"};
        private final List<PinnableLeafRow> rows;
        private final Set<String> pinned = new LinkedHashSet<>();

        PinnableLeafTableModel(List<PinnableLeafRow> rows, Set<String> initiallyPinned) {
            this.rows = List.copyOf(rows);
            this.pinned.addAll(initiallyPinned);
        }

        /**
         * Returns the JSON paths currently checked, for {@code capture_api_generate}'s
         * {@code pinnedJsonPaths} argument.
         *
         * @return checked JSON paths
         */
        Set<String> pinnedJsonPaths() {
            return Set.copyOf(pinned);
        }

        @Override
        public int getRowCount() {
            return rows.size();
        }

        @Override
        public int getColumnCount() {
            return COLUMNS.length;
        }

        @Override
        public String getColumnName(int column) {
            return COLUMNS[column];
        }

        @Override
        public Class<?> getColumnClass(int columnIndex) {
            return columnIndex == PIN_COLUMN ? Boolean.class : String.class;
        }

        @Override
        public boolean isCellEditable(int rowIndex, int columnIndex) {
            return columnIndex == PIN_COLUMN;
        }

        @Override
        public Object getValueAt(int rowIndex, int columnIndex) {
            PinnableLeafRow row = rows.get(rowIndex);
            return switch (columnIndex) {
                case PIN_COLUMN -> pinned.contains(row.jsonPath());
                case 1 -> row.method() + " " + row.url();
                case 2 -> row.jsonPath();
                case 3 -> row.value();
                default -> "";
            };
        }

        @Override
        public void setValueAt(Object value, int rowIndex, int columnIndex) {
            if (columnIndex != PIN_COLUMN || !(value instanceof Boolean checked)) {
                return;
            }
            String jsonPath = rows.get(rowIndex).jsonPath();
            if (checked) {
                pinned.add(jsonPath);
            } else {
                pinned.remove(jsonPath);
            }
            fireTableCellUpdated(rowIndex, columnIndex);
        }
    }

    /**
     * Generates a {@code SHAFT.API} test from the stopped session via {@code capture_api_generate},
     * excluding any transaction whose Include checkbox is unchecked (issue #3548 item 3) and
     * force-asserting any response leaf pinned via {@link #openPinFieldsDialog}.
     */
    private void generateCode() {
        if (sessionOutputPath.isBlank()) {
            statusLabel.setText("Stop the recording before generating code.");
            return;
        }
        generateButton.setEnabled(false);
        statusLabel.setText("Generating SHAFT.API test...");

        JsonObject arguments = new JsonObject();
        arguments.addProperty("sessionPath", sessionOutputPath);
        arguments.addProperty("outputDirectory", "generated-tests");
        arguments.addProperty("packageName", "tests.generated");
        arguments.addProperty("className", "");
        arguments.addProperty("style", "SCENARIO");
        arguments.addProperty("validationDepth", "SCHEMA");
        arguments.addProperty("overwrite", false);
        arguments.addProperty("replay", false);
        arguments.addProperty("openApiSpecPath", "");
        JsonArray excludedTransactionIds = new JsonArray();
        tableModel.excludedTransactionIds().forEach(excludedTransactionIds::add);
        arguments.add("excludedTransactionIds", excludedTransactionIds);
        JsonArray pinnedJsonPathsArray = new JsonArray();
        pinnedJsonPaths.forEach(pinnedJsonPathsArray::add);
        arguments.add("pinnedJsonPaths", pinnedJsonPathsArray);

        ShaftMcpInvocationService.getInstance(project)
                .startTool(GENERATE_TOOL_NAME, arguments)
                .future()
                .whenComplete((result, error) -> ApplicationManager.getApplication().invokeLater(() -> {
                    generateButton.setEnabled(true);
                    if (error != null || result == null || !result.success()) {
                        statusLabel.setText("Generation failed: "
                                + (result != null ? result.output() : String.valueOf(error)));
                        return;
                    }
                    JsonObject payload = AssistantMarkdown.jsonObjectFromMcpOutput(result.output());
                    boolean successful = payload != null && payload.has("successful")
                            && payload.get("successful").getAsBoolean();
                    String sourcePath = payload != null && payload.has("sourcePath")
                            ? payload.get("sourcePath").getAsString()
                            : "";
                    statusLabel.setText(successful
                            ? "Generated " + sourcePath
                            : "Generation completed with errors -- see the SHAFT tool output.");
                }));
    }

    /**
     * Cancels polling. Called when the owning tab/panel is disposed.
     */
    @Override
    public void dispose() {
        stopped = true;
        if (!pollAlarm.isDisposed()) {
            pollAlarm.cancelAllRequests();
        }
        if (currentInvocation != null) {
            currentInvocation.cancel();
        }
    }

    /**
     * One row of the API recording transactions table.
     *
     * @param id recorded transaction ID ({@code NetworkTransaction#transactionId}); used to drive
     *           {@code capture_api_generate}'s {@code excludedTransactionIds} filter, not just to
     *           display the row
     * @param method HTTP method
     * @param url request URL
     * @param status HTTP status
     * @param resourceKind resource kind (e.g. xhr, fetch)
     * @param size response size
     * @param included whether the row is included in generated code (session-local state)
     */
    record TransactionRow(String id, String method, String url, String status, String resourceKind, String size,
                           boolean included) {
    }

    /**
     * Table model backing the live transactions table. Only mutated on the EDT.
     */
    static final class TransactionTableModel extends AbstractTableModel {
        static final int INCLUDE_COLUMN = 5;
        private static final String[] COLUMNS = {"Method", "URL", "Status", "Resource", "Size", "Include"};
        private List<TransactionRow> rows = new ArrayList<>();

        void setRows(List<TransactionRow> newRows) {
            // Preserve session-local include checkbox state by matching on the stable transaction ID.
            List<TransactionRow> merged = new ArrayList<>(newRows.size());
            for (TransactionRow row : newRows) {
                boolean included = rows.stream()
                        .filter(existing -> existing.id().equals(row.id()))
                        .findFirst()
                        .map(TransactionRow::included)
                        .orElse(true);
                merged.add(new TransactionRow(row.id(), row.method(), row.url(), row.status(), row.resourceKind(),
                        row.size(), included));
            }
            rows = merged;
            fireTableDataChanged();
        }

        List<TransactionRow> rows() {
            return rows;
        }

        /**
         * Returns the transaction IDs whose Include checkbox is unchecked, in table order, for
         * {@code capture_api_generate}'s {@code excludedTransactionIds} filter.
         *
         * @return excluded transaction IDs
         */
        List<String> excludedTransactionIds() {
            return rows.stream()
                    .filter(row -> !row.included())
                    .map(TransactionRow::id)
                    .filter(id -> !id.isBlank())
                    .toList();
        }

        @Override
        public int getRowCount() {
            return rows.size();
        }

        @Override
        public int getColumnCount() {
            return COLUMNS.length;
        }

        @Override
        public String getColumnName(int column) {
            return COLUMNS[column];
        }

        @Override
        public Class<?> getColumnClass(int columnIndex) {
            return columnIndex == INCLUDE_COLUMN ? Boolean.class : String.class;
        }

        @Override
        public boolean isCellEditable(int rowIndex, int columnIndex) {
            return columnIndex == INCLUDE_COLUMN;
        }

        @Override
        public Object getValueAt(int rowIndex, int columnIndex) {
            TransactionRow row = rows.get(rowIndex);
            return switch (columnIndex) {
                case 0 -> row.method();
                case 1 -> row.url();
                case 2 -> row.status();
                case 3 -> row.resourceKind();
                case 4 -> row.size();
                case INCLUDE_COLUMN -> row.included();
                default -> "";
            };
        }

        @Override
        public void setValueAt(Object value, int rowIndex, int columnIndex) {
            if (columnIndex != INCLUDE_COLUMN || !(value instanceof Boolean included)) {
                return;
            }
            TransactionRow row = rows.get(rowIndex);
            rows.set(rowIndex, new TransactionRow(row.id(), row.method(), row.url(), row.status(),
                    row.resourceKind(), row.size(), included));
            fireTableCellUpdated(rowIndex, columnIndex);
        }
    }
}
