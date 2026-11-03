package com.shaft.intellij.actions;

import com.intellij.openapi.actionSystem.ActionUpdateThread;
import com.intellij.openapi.actionSystem.AnAction;
import com.intellij.openapi.actionSystem.AnActionEvent;
import com.intellij.openapi.project.DumbAware;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.wm.ToolWindow;
import com.intellij.openapi.wm.ToolWindowManager;
import com.shaft.intellij.project.ShaftProjectDetector;
import com.shaft.intellij.ui.ShaftIcons;
import org.jetbrains.annotations.NotNull;

/**
 * Opens the SHAFT tool window.
 *
 * <p>W1.7: visible only in SHAFT projects so Tools → SHAFT → Open does not appear elsewhere.
 */
public final class OpenShaftToolWindowAction extends AnAction implements DumbAware {
    public OpenShaftToolWindowAction() {
        getTemplatePresentation().setIcon(ShaftIcons.TOOL_WINDOW);
    }

    @Override
    public @NotNull ActionUpdateThread getActionUpdateThread() {
        return ActionUpdateThread.BGT;
    }

    @Override
    public void update(@NotNull AnActionEvent event) {
        Project project = event.getProject();
        boolean visible = project != null && ShaftProjectDetector.isShaftProject(project);
        event.getPresentation().setEnabledAndVisible(visible);
    }

    @Override
    public void actionPerformed(@NotNull AnActionEvent event) {
        Project project = event.getProject();
        if (project == null || !ShaftProjectDetector.isShaftProject(project)) {
            return;
        }
        ToolWindowManager.getInstance(project).invokeLater(() -> {
            ToolWindow toolWindow = ToolWindowManager.getInstance(project).getToolWindow("SHAFT");
            if (toolWindow != null) {
                toolWindow.show();
            }
        });
    }
}
