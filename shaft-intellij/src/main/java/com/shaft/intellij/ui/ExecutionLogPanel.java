package com.shaft.intellij.ui;

import com.intellij.ui.components.JBLabel;
import com.intellij.ui.components.JBScrollPane;
import com.intellij.ui.components.JBTextArea;
import com.intellij.util.ui.JBUI;

import javax.swing.JPanel;
import java.awt.BorderLayout;

public final class ExecutionLogPanel extends JPanel {
    private final JBTextArea log = new JBTextArea();
    private final JBLabel state = new JBLabel("Idle");

    public ExecutionLogPanel() {
        super(new BorderLayout(0, JBUI.scale(6)));
        setBorder(JBUI.Borders.empty(8));
        log.setEditable(false);
        log.setLineWrap(true);
        log.getAccessibleContext().setAccessibleName("Execution log");
        state.getAccessibleContext().setAccessibleName("Execution state");
        add(state, BorderLayout.NORTH);
        add(new JBScrollPane(log), BorderLayout.CENTER);
    }

    public void note(String chunk) {
        log.append((chunk == null ? "" : chunk) + "\n");
        state.setText("Running");
    }

    public void showChunk(String chunk, boolean failure) {
        log.append((chunk == null ? "" : chunk) + "\n");
        state.setText(failure ? "Failed" : "Succeeded");
        state.setForeground(failure ? ShaftStatusPresentation.error() : ShaftStatusPresentation.success());
    }

    public String text() {
        return log.getText();
    }

    public String state() {
        return state.getText();
    }
}
