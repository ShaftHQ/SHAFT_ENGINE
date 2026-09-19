package com.shaft.intellij.ui;

import com.google.gson.JsonObject;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.project.Project;
import com.intellij.ui.components.JBLabel;
import com.intellij.ui.components.JBScrollPane;
import com.intellij.ui.components.JBTextArea;
import com.intellij.util.ui.JBUI;
import com.shaft.intellij.mcp.ShaftMcpInvocationService;
import com.shaft.intellij.mcp.ShaftMcpToolResult;

import javax.swing.JButton;
import javax.swing.JPanel;
import java.awt.BorderLayout;
import java.awt.FlowLayout;
import java.awt.Font;

/**
 * Analysis &amp; Design canvas: paste a story or acceptance criteria and ingest a pack (issue #5947).
 */
final class DesignStagePanel extends JPanel {
    static final String ACCESSIBLE_NAME = "SHAFT analysis and design";
    private final Project project;
    private final JBTextArea story;
    private final JBTextArea packPreview;
    private final JButton ingest;

    DesignStagePanel() {
        this(null);
    }

    DesignStagePanel(Project project) {
        super(new BorderLayout(0, JBUI.scale(8)));
        this.project = project;
        setBorder(JBUI.Borders.empty(8));
        getAccessibleContext().setAccessibleName(ACCESSIBLE_NAME);
        getAccessibleContext().setAccessibleDescription(
                "Paste a user story or acceptance criteria to start analysis and Gherkin design");

        JBLabel title = new JBLabel("Analysis & Design");
        title.setFont(title.getFont().deriveFont(Font.BOLD));
        JBLabel hint = new JBLabel(
                "<html>Paste a user story or acceptance criteria. SHAFT drafts Gherkin for review, "
                        + "then hands an approved pack to Automation as fluent Java — not as "
                        + "committed <code>.feature</code> files.</html>");
        hint.setAllowAutoWrapping(true);

        JPanel header = new JPanel(new BorderLayout(0, JBUI.scale(4)));
        header.setOpaque(false);
        header.add(title, BorderLayout.NORTH);
        header.add(hint, BorderLayout.CENTER);

        story = new JBTextArea(12, 40);
        story.setLineWrap(true);
        story.setWrapStyleWord(true);
        story.getAccessibleContext().setAccessibleName("User story or requirements");

        ingest = new JButton("Ingest story");
        ingest.getAccessibleContext().setAccessibleName("Ingest story");
        ingest.setEnabled(project != null);
        ingest.addActionListener(event -> ingestStory());

        JPanel actions = new JPanel(new FlowLayout(FlowLayout.LEFT, 0, 0));
        actions.setOpaque(false);
        actions.add(ingest);

        packPreview = new JBTextArea(8, 40);
        packPreview.setEditable(false);
        packPreview.setLineWrap(true);
        packPreview.setWrapStyleWord(true);
        packPreview.getAccessibleContext().setAccessibleName("Ingested design pack");

        JPanel south = new JPanel(new BorderLayout(0, JBUI.scale(6)));
        south.setOpaque(false);
        south.add(actions, BorderLayout.NORTH);
        south.add(new JBScrollPane(packPreview), BorderLayout.CENTER);

        add(header, BorderLayout.NORTH);
        add(new JBScrollPane(story), BorderLayout.CENTER);
        add(south, BorderLayout.SOUTH);
    }

    JBTextArea storyArea() {
        return story;
    }

    JButton ingestButton() {
        return ingest;
    }

    private void ingestStory() {
        if (project == null) {
            packPreview.setText("MCP is not available in this context.");
            return;
        }
        JsonObject arguments = new JsonObject();
        arguments.addProperty("text", story.getText());
        arguments.addProperty("filePath", "");
        arguments.addProperty("sourceUrl", "");
        ingest.setEnabled(false);
        ShaftMcpInvocationService.getInstance(project)
                .invokeTool("design_ingest", arguments)
                .whenComplete((result, error) -> ApplicationManager.getApplication().invokeLater(() -> {
                    ingest.setEnabled(true);
                    packPreview.setText(formatPack(result, error));
                }));
    }

    private static String formatPack(ShaftMcpToolResult result, Throwable error) {
        if (error != null) {
            return error.getMessage() == null ? error.toString() : error.getMessage();
        }
        if (result == null) {
            return "No result from design_ingest.";
        }
        String output = result.output() == null ? "" : result.output();
        if (!result.success()) {
            return output.isBlank() ? "design_ingest failed." : output;
        }
        return output;
    }
}
