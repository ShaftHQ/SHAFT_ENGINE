package com.shaft.intellij.notifications;

import com.google.gson.JsonObject;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.wm.ToolWindow;
import com.intellij.openapi.wm.ToolWindowManager;
import com.intellij.ui.content.Content;
import com.shaft.intellij.settings.ShaftSettingsState;
import com.shaft.intellij.ui.ShaftToolWindowPanel;

/**
 * Opens the SHAFT tool window and pre-fills an MCP tool request, or -- while advanced workflows
 * are off (the default) -- points the user at the Assistant instead of silently claiming a panel
 * opened. Shared by {@link FailedRunDoctorNotifier} and the "SHAFT Tests" tool-window tab
 * (issue #3467), both of which trigger the same Doctor/Healer prefill flow from different UI
 * surfaces.
 */
public final class ShaftToolWorkflowLauncher {
    private ShaftToolWorkflowLauncher() {
        throw new IllegalStateException("Utility class");
    }

    /**
     * Opens the SHAFT tool window and pre-fills {@code toolName} with {@code arguments}, or warns
     * and no-ops when advanced workflows are disabled.
     *
     * @param project current project
     * @param toolName MCP tool name to pre-fill
     * @param arguments MCP tool arguments
     */
    public static void open(Project project, String toolName, JsonObject arguments) {
        if (!ShaftSettingsState.getInstance().getState().advancedUiEnabled) {
            ShaftNotifier.warn(project, "SHAFT",
                    "Ask the SHAFT Assistant to \"diagnose my last failed test run\" to analyze the "
                            + "failure from chat.");
            return;
        }
        ToolWindowManager.getInstance(project).invokeLater(() -> {
            ToolWindow toolWindow = ToolWindowManager.getInstance(project).getToolWindow("SHAFT");
            if (toolWindow == null) {
                return;
            }
            toolWindow.show(() -> {
                Content content = toolWindow.getContentManager().getContent(0);
                if (content != null && content.getComponent() instanceof ShaftToolWindowPanel panel) {
                    panel.prefillTool(toolName, arguments);
                }
            });
        });
    }
}
