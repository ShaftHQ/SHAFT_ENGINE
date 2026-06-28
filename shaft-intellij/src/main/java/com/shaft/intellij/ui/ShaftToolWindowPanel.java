package com.shaft.intellij.ui;

import com.intellij.openapi.project.Project;
import com.intellij.ui.components.JBTabbedPane;
import org.jetbrains.annotations.NotNull;

import javax.swing.JComponent;
import javax.swing.JPanel;
import java.awt.BorderLayout;

/**
 * Top-level SHAFT IntelliJ tool window content.
 */
public final class ShaftToolWindowPanel extends JPanel {
    private final JComponent preferredFocusComponent;

    public ShaftToolWindowPanel(@NotNull Project project) {
        super(new BorderLayout());
        JBTabbedPane tabs = new JBTabbedPane();
        ShaftAssistantPanel assistant = new ShaftAssistantPanel(project);
        preferredFocusComponent = assistant.preferredFocusComponent();
        tabs.addTab("Assistant", assistant);
        tabs.addTab("Tools", new ShaftFeaturePanel(project));
        add(tabs, BorderLayout.CENTER);
    }

    /**
     * Returns the default focus target.
     *
     * @return focus target
     */
    public JComponent preferredFocusComponent() {
        return preferredFocusComponent;
    }
}
