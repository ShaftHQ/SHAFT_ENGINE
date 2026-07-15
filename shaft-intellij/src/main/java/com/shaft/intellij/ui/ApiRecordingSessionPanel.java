package com.shaft.intellij.ui;

import com.google.gson.JsonArray;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.intellij.openapi.Disposable;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.project.Project;
import com.intellij.ui.components.JBScrollPane;
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
import java.util.ArrayList;
import java.util.List;

/**
 * Displays a live SHAFT API recording session: transactions are polled from
 * {@code capture_api_transactions} roughly every 2 seconds on a background
 * thread, and the {@link JBTable} model is only ever mutated on the EDT.
 * Polling stops on panel disposal or when the Stop button ends the session.
 */
public final class ApiRecordingSessionPanel extends JPanel implements Disposable {
    private static final int POLL_INTERVAL_MILLIS = 2_000;
    private static final String TRANSACTIONS_TOOL_NAME = "capture_api_transactions";
    private static final String STOP_TOOL_NAME = "capture_api_stop";
    private static final String GENERATE_TOOL_NAME = "capture_api_generate";

    private final Project project;
    private final TransactionTableModel tableModel = new TransactionTableModel();
    private final JBTable table = new JBTable(tableModel);
    private final TableRowSorter<TransactionTableModel> rowSorter = new TableRowSorter<>(tableModel);
    private final JBTextField filterField = new JBTextField();
    private final JButton stopButton = new JButton("Stop");
    private final JButton generateButton = new JButton("Generate");
    private final JLabel statusLabel = new JLabel("Recording...");
    private final Alarm pollAlarm;

    private volatile boolean stopped;
    // Written on the pooled poll thread (pollOnce) and read on the EDT (stopRecording/dispose);
    // volatile so a Stop/close on the EDT always observes the in-flight invocation to cancel it,
    // and so the poll thread and EDT never disagree on the session identity/output path.
    private volatile String sessionId;
    private volatile String sessionOutputPath = "";
    private volatile ShaftMcpInvocation currentInvocation;

    /**
     * Creates the panel and immediately begins polling for transactions.
     *
     * @param project current IntelliJ project
     * @param targetUrl the URL the session was started for
     * @param sessionId the session identifier returned by {@code capture_api_start}, or null if unknown yet
     */
    public ApiRecordingSessionPanel(@NotNull Project project, @NotNull String targetUrl, @Nullable String sessionId) {
        super(new BorderLayout(6, 6));
        this.project = project;
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

        JPanel header = new JPanel(new FlowLayout(FlowLayout.LEFT, 6, 2));
        JLabel titleLabel = new JLabel("Recording: " + targetUrl);
        titleLabel.setFont(titleLabel.getFont().deriveFont(Font.BOLD));
        header.add(titleLabel);

        JPanel actions = new JPanel(new FlowLayout(FlowLayout.RIGHT, 6, 0));
        actions.add(stopButton);
        actions.add(generateButton);

        JPanel toolbar = new JPanel(new BorderLayout(6, 0));
        toolbar.add(filterField, BorderLayout.CENTER);
        toolbar.add(actions, BorderLayout.EAST);

        JPanel north = new JPanel(new BorderLayout(4, 4));
        north.add(header, BorderLayout.NORTH);
        north.add(toolbar, BorderLayout.SOUTH);

        add(north, BorderLayout.NORTH);
        add(new JBScrollPane(table), BorderLayout.CENTER);
        add(statusLabel, BorderLayout.SOUTH);

        startPolling();
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

    JBTextField filterField() {
        return filterField;
    }

    JButton stopButton() {
        return stopButton;
    }

    JButton generateButton() {
        return generateButton;
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
                .startTool(TRANSACTIONS_TOOL_NAME, arguments);
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
                .startTool(STOP_TOOL_NAME, arguments)
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
                    statusLabel.setText("Stopped. " + tableModel.getRowCount() + " transaction(s).");
                }));
    }

    /**
     * Generates a {@code SHAFT.API} test from the stopped session via {@code capture_api_generate},
     * excluding any transaction whose Include checkbox is unchecked (issue #3548 item 3).
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
