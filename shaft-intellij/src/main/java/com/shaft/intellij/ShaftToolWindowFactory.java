package com.shaft.intellij;

import com.intellij.openapi.project.DumbAware;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.wm.ToolWindow;
import com.intellij.openapi.wm.ToolWindowFactory;
import com.intellij.ui.content.Content;
import com.intellij.ui.content.ContentFactory;
import com.shaft.intellij.ui.ShaftToolWindowPanel;
import org.jetbrains.annotations.NotNull;

/**
 * Creates the SHAFT tool window lazily when the user opens it.
 */
public final class ShaftToolWindowFactory implements ToolWindowFactory, DumbAware {
    @Override
    public void createToolWindowContent(@NotNull Project project, @NotNull ToolWindow toolWindow) {
        ShaftToolWindowPanel panel = new ShaftToolWindowPanel(project);
        Content content = ContentFactory.getInstance().createContent(panel, "", false);
        content.setPreferredFocusableComponent(panel.preferredFocusComponent());
        // Ties the panel's lifecycle to this Content's teardown (project close, tool-window
        // content rebuild) so its Disposer#dispose() cascade actually runs -- previously the
        // panel was never registered anywhere, leaving child pollers like the Guided workflow's
        // status-poll Alarm to run forever after real teardown (issue #3619).
        content.setDisposer(panel);
        toolWindow.getContentManager().addContent(content);
    }
}
