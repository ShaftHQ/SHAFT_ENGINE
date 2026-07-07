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

    private final Project project;
    private final TransactionTableModel tableModel = new TransactionTableModel();
    private final JBTable table = new JBTable(tableModel);
    private final TableRowSorter<TransactionTableModel> rowSorter = new TableRowSorter<>(tableModel);
    private final JBTextField filterField = new JBTextField();
    private final JButton stopButton = new JButton("Stop");
    private final JLabel statusLabel = new JLabel("Recording...");
    private final Alarm pollAlarm;

    private volatile boolean stopped;
    private String sessionId;
    private ShaftMcpInvocation currentInvocation;

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

        JPanel header = new JPanel(new FlowLayout(FlowLayout.LEFT, 6, 2));
        JLabel titleLabel = new JLabel("Recording: " + targetUrl);
        titleLabel.setFont(titleLabel.getFont().deriveFont(Font.BOLD));
        header.add(titleLabel);

        JPanel toolbar = new JPanel(new BorderLayout(6, 0));
        toolbar.add(filterField, BorderLayout.CENTER);
        toolbar.add(stopButton, BorderLayout.EAST);

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

    private static List<TransactionRow> parseTransactions(String output) {
        List<TransactionRow> rows = new ArrayList<>();
        JsonObject payload = AssistantMarkdown.jsonObjectFromMcpOutput(output);
        if (payload == null) {
            return rows;
        }
        JsonElement transactionsElement = payload.get("transactions");
        if (transactionsElement == null || !transactionsElement.isJsonArray()) {
            return rows;
        }
        JsonArray transactions = transactionsElement.getAsJsonArray();
        for (JsonElement element : transactions) {
            if (!element.isJsonObject()) {
                continue;
            }
            JsonObject transaction = element.getAsJsonObject();
            rows.add(new TransactionRow(
                    stringOf(transaction, "method"),
                    stringOf(transaction, "url"),
                    stringOf(transaction, "status"),
                    stringOf(transaction, "resourceKind"),
                    stringOf(transaction, "size"),
                    true));
        }
        return rows;
    }

    private static String stringOf(JsonObject object, String key) {
        JsonElement element = object.get(key);
        return element == null || element.isJsonNull() ? "" : element.getAsString();
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
                    } else {
                        statusLabel.setText("Stopped. " + tableModel.getRowCount() + " transaction(s).");
                    }
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
     * @param method HTTP method
     * @param url request URL
     * @param status HTTP status
     * @param resourceKind resource kind (e.g. xhr, fetch)
     * @param size response size
     * @param included whether the row is included in generated code (session-local state)
     */
    record TransactionRow(String method, String url, String status, String resourceKind, String size,
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
            // Preserve session-local include checkbox state by matching on URL+method.
            List<TransactionRow> merged = new ArrayList<>(newRows.size());
            for (TransactionRow row : newRows) {
                boolean included = rows.stream()
                        .filter(existing -> existing.method().equals(row.method()) && existing.url().equals(row.url()))
                        .findFirst()
                        .map(TransactionRow::included)
                        .orElse(true);
                merged.add(new TransactionRow(row.method(), row.url(), row.status(), row.resourceKind(),
                        row.size(), included));
            }
            rows = merged;
            fireTableDataChanged();
        }

        List<TransactionRow> rows() {
            return rows;
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
            rows.set(rowIndex, new TransactionRow(row.method(), row.url(), row.status(), row.resourceKind(),
                    row.size(), included));
            fireTableCellUpdated(rowIndex, columnIndex);
        }
    }
}
