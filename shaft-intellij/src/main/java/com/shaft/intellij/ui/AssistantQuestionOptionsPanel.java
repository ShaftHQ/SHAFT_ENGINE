package com.shaft.intellij.ui;

import com.intellij.util.ui.JBUI;
import com.intellij.util.ui.WrapLayout;

import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JPanel;
import java.awt.FlowLayout;
import java.util.ArrayList;
import java.util.List;
import java.util.function.Consumer;

/**
 * Selectable-answer chip row shown under a detected {@link AssistantQuestion} (issue #3674).
 * Mirrors {@link ToolApprovalPromptPanel}'s bubble styling and its convention of reporting a
 * click through a callback, but -- unlike that one-shot approval decision -- a chip click never
 * disables the row: choosing a suggested answer is non-destructive (it only fills the composer,
 * matching {@code ShaftAssistantPanel#emptyStateChip}'s "review and send it yourself" idiom), so
 * the user may click a different chip or ignore all of them and type a free-text answer instead.
 * The always-available composer is the free-text fallback the issue calls for; this panel adds
 * nothing to suppress or replace it.
 */
final class AssistantQuestionOptionsPanel extends JPanel {
    private final List<JButton> optionButtons = new ArrayList<>();

    AssistantQuestionOptionsPanel(List<String> options, Consumer<String> onOptionChosen) {
        super(new WrapLayout(FlowLayout.LEFT, 6, 4));
        setOpaque(false);
        setBorder(BorderFactory.createCompoundBorder(
                BorderFactory.createEmptyBorder(4, 0, 0, 0),
                JBUI.Borders.empty(2)));
        getAccessibleContext().setAccessibleName("Suggested answers");
        for (String option : options == null ? List.<String>of() : options) {
            JButton button = new JButton(option);
            button.getAccessibleContext().setAccessibleName("Suggested answer: " + option);
            button.setToolTipText("Fills the message box with this answer; review and send it yourself.");
            button.addActionListener(event -> onOptionChosen.accept(option));
            optionButtons.add(button);
            add(button);
        }
    }

    /**
     * Returns the rendered option buttons, in display order. Used by tests to assert which options
     * were rendered and to drive clicks without relying on accessible-name lookups.
     *
     * @return immutable snapshot of the option buttons
     */
    List<JButton> optionButtonsForTest() {
        return List.copyOf(optionButtons);
    }
}
