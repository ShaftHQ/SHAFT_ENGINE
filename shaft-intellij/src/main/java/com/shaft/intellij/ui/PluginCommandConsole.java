package com.shaft.intellij.ui;

import com.intellij.ui.components.JBTextArea;
import com.intellij.util.ui.JBUI;

import javax.swing.JPanel;
import java.awt.BorderLayout;

/**
 * In-plugin console that holds the exact command the workflow is about to run.
 * The IDE terminal is attempted as well; this component is the console the tool
 * window shows when that terminal plugin is not available.
 */
public final class PluginCommandConsole extends JPanel {
    private final JBTextArea console = new JBTextArea();

    public PluginCommandConsole() {
        super(new BorderLayout());
        setBorder(JBUI.Borders.empty(8, 8, 0, 8));
        console.setRows(2);
        console.setLineWrap(true);
        console.getAccessibleContext().setAccessibleName("In-plugin console");
        add(console, BorderLayout.CENTER);
    }

    public String place(String command) {
        String text = command == null ? "" : command;
        console.setText(text);
        return console.getText();
    }

    public String placed() {
        return console.getText();
    }
}
