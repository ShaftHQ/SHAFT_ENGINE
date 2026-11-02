package com.shaft.intellij.ui;

import com.google.gson.JsonArray;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.ide.CopyPasteManager;
import com.intellij.openapi.project.Project;
import com.intellij.ui.JBColor;
import com.intellij.ui.components.JBLabel;
import com.intellij.ui.components.JBScrollPane;
import com.intellij.ui.table.JBTable;
import com.intellij.util.ui.JBUI;
import com.shaft.intellij.mcp.ShaftMcpInvocationService;
import com.shaft.intellij.mcp.ShaftMcpToolResult;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import javax.swing.Icon;
import javax.swing.JButton;
import javax.swing.JPanel;
import javax.swing.ListSelectionModel;
import javax.swing.table.AbstractTableModel;
import java.awt.BorderLayout;
import java.awt.FlowLayout;
import java.awt.Font;
import java.awt.datatransfer.StringSelection;
import java.util.ArrayList;
import java.util.List;

/**
 * Automation canvas locator playground (issue #5959): shows ranked {@code capture_pick_locator}
 * candidates with uniqueness, prefers role over xpath in ranking (backend), and copies a SHAFT
 * locator expression — not a raw driver find.
 */
final class LocatorPlaygroundPanel extends JPanel {
    static final String ACCESSIBLE_NAME = "SHAFT locator playground";
    static final String TOOL_NAME = "capture_pick_locator";
    static final String CLOSED_BROWSER_STATUS =
            "No live pick yet. Start a capture session, switch the recorder to inspect mode, "
                    + "click an element in the managed browser, then refresh.";

    private final @Nullable Project project;
    private final CandidateTableModel model = new CandidateTableModel();
    private final JBTable table = new JBTable(model);
    private final JBLabel status = new JBLabel(" ");
    private final JBLabel detail = new JBLabel("Hover or select a ranked candidate.");
    private final JButton refreshButton;
    private final JButton copyButton;
    private String winningSnippet = "";

    LocatorPlaygroundPanel(@Nullable Project project) {
        super(new BorderLayout(0, JBUI.scale(8)));
        this.project = project;
        setBorder(JBUI.Borders.empty(8));
        getAccessibleContext().setAccessibleName(ACCESSIBLE_NAME);
        getAccessibleContext().setAccessibleDescription(
                "Ranked locator candidates with uniqueness; copy inserts a SHAFT locator expression");

        JBLabel title = new JBLabel("Locator picker");
        title.setFont(title.getFont().deriveFont(Font.BOLD));
        JBLabel hint = new JBLabel(
                "<html>Pick from the live page (inspect mode). Candidates are ranked role → label → "
                        + "testid → id → css → xpath. Non-unique rows are marked. Copy inserts a "
                        + "SHAFT.GUI.Locator expression.</html>");
        hint.setAllowAutoWrapping(true);

        refreshButton = button("Refresh pick", "Load ranked candidates from the last capture_pick_locator result",
                ShaftIcons.SEARCH, this::refreshFromCapture);
        copyButton = button("Copy SHAFT locator", "Copy the selected (or top) SHAFT locator expression",
                ShaftIcons.COPY, this::copySelectedSnippet);
        copyButton.setEnabled(false);

        JPanel actions = new JPanel(new FlowLayout(FlowLayout.LEFT, 6, 0));
        actions.add(refreshButton);
        actions.add(copyButton);

        JPanel north = new JPanel(new BorderLayout(0, JBUI.scale(4)));
        north.setOpaque(false);
        north.add(title, BorderLayout.NORTH);
        north.add(hint, BorderLayout.CENTER);
        north.add(actions, BorderLayout.SOUTH);

        table.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
        table.setRowSelectionAllowed(true);
        table.getAccessibleContext().setAccessibleName("Ranked locator candidates");
        table.getSelectionModel().addListSelectionListener(event -> {
            if (!event.getValueIsAdjusting()) {
                onSelectionChanged();
            }
        });
        table.addMouseMotionListener(new java.awt.event.MouseMotionAdapter() {
            @Override
            public void mouseMoved(java.awt.event.MouseEvent event) {
                int row = table.rowAtPoint(event.getPoint());
                if (row >= 0 && row < model.getRowCount()) {
                    showDetail(model.rowAt(row));
                }
            }
        });

        detail.setForeground(JBColor.namedColor("Label.disabledForeground", JBColor.GRAY));
        detail.getAccessibleContext().setAccessibleName("Locator candidate detail");
        status.getAccessibleContext().setAccessibleName("Locator playground status");

        JPanel south = new JPanel(new BorderLayout(0, JBUI.scale(4)));
        south.setOpaque(false);
        south.add(detail, BorderLayout.CENTER);
        south.add(status, BorderLayout.SOUTH);

        add(north, BorderLayout.NORTH);
        add(new JBScrollPane(table), BorderLayout.CENTER);
        add(south, BorderLayout.SOUTH);
    }

    JButton refreshButton() {
        return refreshButton;
    }

    JButton copyButton() {
        return copyButton;
    }

    JBLabel statusLabel() {
        return status;
    }

    CandidateTableModel model() {
        return model;
    }

    String winningSnippet() {
        return winningSnippet;
    }

    /**
     * Applies a parsed pick-locator payload (test seam and MCP success path).
     *
     * @param snippet winning SHAFT / By expression
     * @param ranked ranked candidates best-first
     */
    void applyPickResult(@Nullable String snippet, @NotNull List<RankedRow> ranked) {
        winningSnippet = snippet == null ? "" : snippet.trim();
        model.setRows(ranked);
        copyButton.setEnabled(!winningSnippet.isBlank() || !ranked.isEmpty());
        if (ranked.isEmpty()) {
            setStatus(CLOSED_BROWSER_STATUS);
            detail.setText("No ranked candidates.");
            return;
        }
        table.setRowSelectionInterval(0, 0);
        showDetail(ranked.get(0));
        String uniqueness = ranked.get(0).unique() ? "unique" : "not unique (" + ranked.get(0).uniquenessCount() + ")";
        setStatus("Top: " + ranked.get(0).strategy() + " · " + uniqueness
                + (winningSnippet.isBlank() ? "" : " · ready to copy"));
    }

    /**
     * Parses {@code capture_pick_locator} tool output (direct JSON or MCP content envelope).
     *
     * @param output raw tool output
     * @return true when a payload was applied
     */
    boolean applyToolOutput(@Nullable String output) {
        JsonObject payload = unwrapPayload(output);
        if (payload == null) {
            applyPickResult("", List.of());
            return false;
        }
        String snippet = stringField(payload, "snippet");
        List<RankedRow> rows = new ArrayList<>();
        JsonElement rankedElement = payload.get("ranked");
        if (rankedElement != null && rankedElement.isJsonArray()) {
            JsonArray ranked = rankedElement.getAsJsonArray();
            int index = 1;
            for (JsonElement entry : ranked) {
                if (!entry.isJsonObject()) {
                    continue;
                }
                JsonObject object = entry.getAsJsonObject();
                String strategy = stringField(object, "strategy");
                String expression = stringField(object, "expression");
                int uniqueness = intField(object, "uniquenessCount", -1);
                int score = intField(object, "score", 0);
                String rowSnippet = stringField(object, "snippet");
                if (strategy.isBlank() && expression.isBlank()) {
                    continue;
                }
                rows.add(new RankedRow(index++, strategy, expression, uniqueness, score, rowSnippet));
            }
        }
        applyPickResult(snippet, rows);
        return !rows.isEmpty() || (snippet != null && !snippet.isBlank());
    }

    private void refreshFromCapture() {
        ShaftMcpInvocationService service = invocationService();
        if (service == null) {
            setStatus("Open a SHAFT project to refresh from a live capture session.");
            return;
        }
        setStatus("Refreshing from capture_pick_locator…");
        service.startTool(TOOL_NAME, new JsonObject())
                .future()
                .whenComplete((result, error) -> onEdt(() -> handleRefreshResult(result, error)));
    }

    private void handleRefreshResult(ShaftMcpToolResult result, Throwable error) {
        if (error != null) {
            applyPickResult("", List.of());
            setStatus("Pick failed (browser closed or MCP error): " + error.getMessage());
            return;
        }
        if (result == null || !result.success()) {
            applyPickResult("", List.of());
            setStatus(result == null ? CLOSED_BROWSER_STATUS : "Pick failed: " + result.output());
            return;
        }
        if (!applyToolOutput(result.output())) {
            setStatus(CLOSED_BROWSER_STATUS);
        }
    }

    private void copySelectedSnippet() {
        String snippet = selectedSnippet();
        if (snippet.isBlank()) {
            setStatus("Nothing to copy yet.");
            return;
        }
        try {
            CopyPasteManager.getInstance().setContents(new StringSelection(snippet));
            setStatus("Copied SHAFT locator expression.");
        } catch (RuntimeException | Error unavailable) {
            // Headless unit tests have no CopyPasteManager; still expose last copy via winningSnippet.
            winningSnippet = snippet;
            setStatus("Copied SHAFT locator expression (clipboard unavailable in this JVM).");
        }
    }

    String selectedSnippet() {
        int row = table.getSelectedRow();
        if (row >= 0 && row < model.getRowCount()) {
            String snippet = model.rowAt(row).snippet();
            if (!snippet.isBlank()) {
                return snippet;
            }
        }
        return winningSnippet;
    }

    private void onSelectionChanged() {
        int row = table.getSelectedRow();
        if (row >= 0 && row < model.getRowCount()) {
            showDetail(model.rowAt(row));
            copyButton.setEnabled(true);
        }
    }

    private void showDetail(RankedRow row) {
        if (row == null) {
            detail.setText("Hover or select a ranked candidate.");
            return;
        }
        String uniqueness = row.unique()
                ? "unique"
                : (row.uniquenessCount() < 0 ? "uniqueness unknown" : "not unique (" + row.uniquenessCount() + " matches)");
        detail.setText(row.strategy() + " · " + uniqueness + " · score " + row.score()
                + " · " + (row.snippet().isBlank() ? row.expression() : row.snippet()));
    }

    private void setStatus(String text) {
        status.setText(text);
        status.setToolTipText(text);
        status.getAccessibleContext().setAccessibleDescription(text);
    }

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

    private static void onEdt(Runnable action) {
        var application = ApplicationManager.getApplication();
        if (application == null) {
            action.run();
        } else {
            application.invokeLater(action);
        }
    }

    private static JButton button(String text, String description, Icon icon, Runnable action) {
        JButton button = new JButton();
        ShaftIconButtons.apply(button, description, text, icon);
        button.getAccessibleContext().setAccessibleDescription(description);
        button.addActionListener(event -> action.run());
        return button;
    }

    static JsonObject unwrapPayload(@Nullable String output) {
        JsonObject payload = jsonObject(output);
        if (payload == null) {
            return null;
        }
        if (isPickPayload(payload)) {
            return payload;
        }
        return pickPayloadFromContentEnvelope(payload);
    }

    private static boolean isPickPayload(JsonObject payload) {
        return payload.has("snippet") || payload.has("ranked");
    }

    private static JsonObject pickPayloadFromContentEnvelope(JsonObject payload) {
        JsonElement content = payload.get("content");
        if (content == null || !content.isJsonArray()) {
            return null;
        }
        for (JsonElement entry : content.getAsJsonArray()) {
            JsonObject nested = nestedPickPayload(entry);
            if (nested != null) {
                return nested;
            }
        }
        return null;
    }

    private static JsonObject nestedPickPayload(JsonElement entry) {
        if (!entry.isJsonObject()) {
            return null;
        }
        JsonElement text = entry.getAsJsonObject().get("text");
        if (text == null || !text.isJsonPrimitive()) {
            return null;
        }
        JsonObject nested = jsonObject(text.getAsString());
        return nested != null && isPickPayload(nested) ? nested : null;
    }

    private static JsonObject jsonObject(String text) {
        if (text == null || text.isBlank()) {
            return null;
        }
        try {
            JsonElement parsed = JsonParser.parseString(text);
            return parsed.isJsonObject() ? parsed.getAsJsonObject() : null;
        } catch (RuntimeException malformed) {
            return null;
        }
    }

    private static String stringField(JsonObject object, String field) {
        JsonElement value = object.get(field);
        return value != null && value.isJsonPrimitive() ? value.getAsString() : "";
    }

    private static int intField(JsonObject object, String field, int fallback) {
        JsonElement value = object.get(field);
        if (value == null || !value.isJsonPrimitive()) {
            return fallback;
        }
        try {
            return value.getAsInt();
        } catch (RuntimeException ignored) {
            return fallback;
        }
    }

    /**
     * One ranked playground row.
     *
     * @param rank 1-based rank
     * @param strategy locator strategy
     * @param expression raw expression
     * @param uniquenessCount match count ({@code 1} unique; negative = unknown)
     * @param score deterministic score
     * @param snippet SHAFT / By copy expression
     */
    record RankedRow(
            int rank,
            String strategy,
            String expression,
            int uniquenessCount,
            int score,
            String snippet) {
        RankedRow {
            strategy = strategy == null ? "" : strategy;
            expression = expression == null ? "" : expression;
            snippet = snippet == null ? "" : snippet;
        }

        boolean unique() {
            return uniquenessCount == 1;
        }
    }

    static final class CandidateTableModel extends AbstractTableModel {
        private static final String[] COLUMNS = {
                "Rank", "Strategy", "Expression", "Uniqueness", "Score", "Snippet"
        };
        private List<RankedRow> rows = List.of();

        void setRows(List<RankedRow> next) {
            rows = next == null ? List.of() : List.copyOf(next);
            fireTableDataChanged();
        }

        RankedRow rowAt(int index) {
            return rows.get(index);
        }

        List<RankedRow> rows() {
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
        public Object getValueAt(int rowIndex, int columnIndex) {
            RankedRow row = rows.get(rowIndex);
            return switch (columnIndex) {
                case 0 -> row.rank();
                case 1 -> row.strategy();
                case 2 -> row.expression();
                case 3 -> row.uniquenessCount() < 0
                        ? "?"
                        : (row.unique() ? "unique" : "not unique (" + row.uniquenessCount() + ")");
                case 4 -> row.score();
                case 5 -> row.snippet();
                default -> "";
            };
        }
    }
}
