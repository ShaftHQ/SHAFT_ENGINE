package com.shaft.intellij.ui;

import com.google.gson.JsonObject;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.project.Project;
import com.intellij.ui.DocumentAdapter;
import com.intellij.ui.JBSplitter;
import com.intellij.ui.components.JBLabel;
import com.intellij.ui.components.JBScrollPane;
import com.intellij.ui.components.JBTextArea;
import com.intellij.util.ui.JBUI;
import com.shaft.intellij.mcp.ShaftMcpInvocationService;
import com.shaft.intellij.mcp.ShaftMcpToolResult;

import javax.swing.JButton;
import javax.swing.JPanel;
import javax.swing.JTable;
import javax.swing.event.DocumentEvent;
import javax.swing.table.DefaultTableModel;
import java.awt.BorderLayout;
import java.awt.FlowLayout;
import java.awt.Font;
import java.util.List;

/**
 * Analysis &amp; Design canvas: ingest a story, show the gap register, keep Gherkin gated (issue #5948).
 */
final class DesignStagePanel extends JPanel {
    static final String ACCESSIBLE_NAME = "SHAFT analysis and design";
    private static final String[] AC_COLUMNS = {"ID", "Criterion"};
    private static final String[] GAP_COLUMNS = {"ID", "Kind", "Severity", "Rank", "AC", "Question", "Accepted"};
    private static final String[] ORACLE_COLUMNS = {"AC", "Oracle", "Evidence"};

    private final Project project;
    private final JBTextArea story;
    private final JBLabel statusBadge;
    private final JBLabel packStrip;
    private final JTable acTable;
    private final JTable gapTable;
    private final JTable oracleTable;
    private final JButton ingest;
    private final JButton analyze;
    private final JButton acceptRisk;
    private final JButton gherkin;
    private DesignCanvasModel model = new DesignCanvasModel();
    private boolean busy;

    DesignStagePanel() {
        this(null);
    }

    DesignStagePanel(Project project) {
        super(new BorderLayout(0, JBUI.scale(8)));
        this.project = project;
        setBorder(JBUI.Borders.empty(8));
        getAccessibleContext().setAccessibleName(ACCESSIBLE_NAME);
        getAccessibleContext().setAccessibleDescription(
                "Paste a user story, analyze testability, then review Gherkin before fluent Java");

        JBLabel title = new JBLabel("Analysis & Design");
        title.setFont(title.getFont().deriveFont(Font.BOLD));
        JBLabel hint = new JBLabel(
                "<html>Story → analyze → Gherkin for review → SHAFT fluent Java. "
                        + "Do not commit <code>.feature</code> files as source of truth.</html>");
        hint.setAllowAutoWrapping(true);
        statusBadge = new JBLabel("Draft");
        statusBadge.getAccessibleContext().setAccessibleName("Design status");
        packStrip = new JBLabel("Actor: —  ·  Outcome: —  ·  AC 0  ·  blocking 0");
        packStrip.getAccessibleContext().setAccessibleName("Design pack summary");

        JPanel header = new JPanel(new BorderLayout(0, JBUI.scale(4)));
        header.setOpaque(false);
        header.add(title, BorderLayout.NORTH);
        header.add(hint, BorderLayout.CENTER);
        JPanel meta = new JPanel(new BorderLayout(0, JBUI.scale(2)));
        meta.setOpaque(false);
        meta.add(statusBadge, BorderLayout.NORTH);
        meta.add(packStrip, BorderLayout.CENTER);
        header.add(meta, BorderLayout.SOUTH);

        story = new JBTextArea(10, 40);
        story.setLineWrap(true);
        story.setWrapStyleWord(true);
        story.getAccessibleContext().setAccessibleName("User story or requirements");
        story.getDocument().addDocumentListener(new DocumentAdapter() {
            @Override
            protected void textChanged(DocumentEvent event) {
                refreshButtons();
            }
        });

        acTable = table("Acceptance criteria", AC_COLUMNS);
        gapTable = table("Gap register", GAP_COLUMNS);
        oracleTable = table("Evidence oracles", ORACLE_COLUMNS);

        ingest = action("Ingest story", this::ingestStory);
        analyze = action("Analyze", this::analyzeStory);
        acceptRisk = action("Accept residual risk", this::acceptResidualRisk);
        gherkin = action("Draft Gherkin", this::draftGherkin);
        gherkin.setEnabled(false);

        JPanel actions = new JPanel(new FlowLayout(FlowLayout.LEFT, JBUI.scale(8), 0));
        actions.setOpaque(false);
        actions.add(ingest);
        actions.add(analyze);
        actions.add(acceptRisk);
        actions.add(gherkin);

        JBSplitter tables = new JBSplitter(true, 0.45f);
        tables.setFirstComponent(new JBScrollPane(acTable));
        JPanel lower = new JPanel(new BorderLayout(0, JBUI.scale(6)));
        lower.add(new JBScrollPane(gapTable), BorderLayout.CENTER);
        lower.add(new JBScrollPane(oracleTable), BorderLayout.SOUTH);
        tables.setSecondComponent(lower);

        JBSplitter body = new JBSplitter(true, 0.38f);
        body.setFirstComponent(new JBScrollPane(story));
        body.setSecondComponent(tables);

        add(header, BorderLayout.NORTH);
        add(body, BorderLayout.CENTER);
        add(actions, BorderLayout.SOUTH);
        refreshButtons();
    }

    JBTextArea storyArea() {
        return story;
    }

    JButton ingestButton() {
        return ingest;
    }

    JButton analyzeButton() {
        return analyze;
    }

    JButton acceptRiskButton() {
        return acceptRisk;
    }

    JButton gherkinButton() {
        return gherkin;
    }

    JBLabel statusBadge() {
        return statusBadge;
    }

    JTable gapTable() {
        return gapTable;
    }

    void applyAnalysisJson(String json) {
        model = DesignCanvasModel.fromJson(json);
        fill(acTable, AC_COLUMNS, model.acRows());
        fill(gapTable, GAP_COLUMNS, model.gapRows());
        fill(oracleTable, ORACLE_COLUMNS, model.oracleRows());
        statusBadge.setText(model.badgeText());
        packStrip.setText(model.packStrip());
        refreshButtons();
    }

    private void ingestStory() {
        invoke("design_ingest", arguments(""), true);
    }

    private void analyzeStory() {
        invoke("design_analyze", arguments(""), false);
    }

    private void draftGherkin() {
        invoke("design_gherkin_draft", arguments(""), false);
    }

    private void acceptResidualRisk() {
        invoke("design_analyze", arguments(model.acceptedGapIdsArgument()), false);
    }

    private JsonObject arguments(String acceptedGapIds) {
        JsonObject arguments = new JsonObject();
        arguments.addProperty("text", story.getText());
        arguments.addProperty("filePath", "");
        arguments.addProperty("sourceUrl", "");
        arguments.addProperty("acceptedGapIds", acceptedGapIds);
        return arguments;
    }

    private void invoke(String tool, JsonObject arguments, boolean thenAnalyze) {
        if (project == null) {
            applyAnalysisJson("MCP is not available in this context.");
            return;
        }
        busy = true;
        refreshButtons();
        ShaftMcpInvocationService.getInstance(project)
                .invokeTool(tool, arguments)
                .whenComplete((result, error) -> ApplicationManager.getApplication().invokeLater(() -> {
                    busy = false;
                    applyToolResult(result, error, thenAnalyze);
                }));
    }

    private void applyToolResult(ShaftMcpToolResult result, Throwable error, boolean thenAnalyze) {
        if (error != null) {
            applyAnalysisJson(error.getMessage() == null ? error.toString() : error.getMessage());
            return;
        }
        if (result == null || !result.success()) {
            String output = result == null || result.output() == null ? "" : result.output();
            applyAnalysisJson(output.isBlank() ? "design tool failed." : output);
            return;
        }
        applyAnalysisJson(result.output());
        if (thenAnalyze && !"error".equals(model.status())) {
            analyzeStory();
        }
    }

    private void refreshButtons() {
        boolean ready = project != null && !busy && !story.getText().isBlank();
        ingest.setEnabled(ready);
        analyze.setEnabled(ready);
        acceptRisk.setEnabled(ready && model.acceptEnabled());
        gherkin.setEnabled(ready && model.gherkinGenerationAllowed());
    }

    private static JButton action(String name, Runnable runnable) {
        JButton button = new JButton(name);
        button.getAccessibleContext().setAccessibleName(name);
        button.addActionListener(event -> runnable.run());
        return button;
    }

    private static JTable table(String accessibleName, String[] columns) {
        JTable table = new JTable(new DefaultTableModel(columns, 0));
        table.getAccessibleContext().setAccessibleName(accessibleName);
        table.setAutoCreateRowSorter(true);
        return table;
    }

    private static void fill(JTable table, String[] columns, List<String[]> rows) {
        DefaultTableModel tableModel = new DefaultTableModel(columns, 0);
        for (String[] row : rows) {
            tableModel.addRow(row);
        }
        table.setModel(tableModel);
    }
}
