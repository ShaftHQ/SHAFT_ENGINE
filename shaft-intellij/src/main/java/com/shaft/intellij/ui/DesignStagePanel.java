package com.shaft.intellij.ui;

import com.intellij.ui.components.JBLabel;
import com.intellij.ui.components.JBScrollPane;
import com.intellij.ui.components.JBTextArea;
import com.intellij.util.ui.JBUI;

import javax.swing.JPanel;
import java.awt.BorderLayout;
import java.awt.Font;

/**
 * Analysis &amp; Design canvas: paste a story or acceptance criteria. Gherkin draft/handoff
 * land in later Stage 1 issues; this panel is the empty-state host for issue #5942.
 */
final class DesignStagePanel extends JPanel {
    static final String ACCESSIBLE_NAME = "SHAFT analysis and design";
    private final JBTextArea story;

    DesignStagePanel() {
        super(new BorderLayout(0, JBUI.scale(8)));
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

        add(header, BorderLayout.NORTH);
        add(new JBScrollPane(story), BorderLayout.CENTER);
    }

    JBTextArea storyArea() {
        return story;
    }
}
