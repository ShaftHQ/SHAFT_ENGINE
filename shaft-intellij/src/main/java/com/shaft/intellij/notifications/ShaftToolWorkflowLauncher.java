package com.shaft.intellij.notifications;

import com.google.gson.JsonObject;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.wm.ToolWindow;
import com.intellij.openapi.wm.ToolWindowManager;
import com.intellij.ui.content.Content;
import com.shaft.intellij.settings.ShaftSettingsState;
import com.shaft.intellij.ui.ShaftToolWindowPanel;

/**
 * Opens the SHAFT tool window and pre-fills either an MCP tool request (advanced workflows on) or
 * the Assistant composer with an equivalent plain-language request (advanced workflows off, the
 * default) -- always acting, never a silent no-op or a dead-end warning. Shared by
 * {@link FailedRunDoctorNotifier} and the "SHAFT Tests" tool-window tab (issue #3467), both of
 * which trigger the same Doctor/Healer prefill flow from different UI surfaces.
 */
public final class ShaftToolWorkflowLauncher {
    private ShaftToolWorkflowLauncher() {
        throw new IllegalStateException("Utility class");
    }

    /**
     * Opens the SHAFT tool window and pre-fills {@code toolName} with {@code arguments} directly in
     * the raw Tools panel when advanced workflows are on; otherwise opens the Assistant tab and
     * pre-fills its composer with an equivalent plain-language request for the user to review and
     * send themselves (issue #3552).
     *
     * @param project current project
     * @param toolName MCP tool name to pre-fill
     * @param arguments MCP tool arguments
     */
    public static void open(Project project, String toolName, JsonObject arguments) {
        boolean advancedUiEnabled = ShaftSettingsState.getInstance().getState().advancedUiEnabled;
        ToolWindowManager.getInstance(project).invokeLater(() -> {
            ToolWindow toolWindow = ToolWindowManager.getInstance(project).getToolWindow("SHAFT");
            if (toolWindow == null) {
                return;
            }
            toolWindow.show(() -> {
                Content content = toolWindow.getContentManager().getContent(0);
                if (!(content != null && content.getComponent() instanceof ShaftToolWindowPanel panel)) {
                    return;
                }
                if (advancedUiEnabled) {
                    panel.prefillTool(toolName, arguments);
                } else {
                    panel.prefillAssistantPrompt(assistantPromptFor(toolName));
                }
            });
        });
    }

    /**
     * Plain-language Assistant equivalent of a Doctor/Healer MCP tool request, so default-mode users
     * (advanced workflows off) get a ready-to-send request instead of the raw tool call.
     * Package-private for {@code ShaftToolWorkflowLauncherTest}.
     */
    static String assistantPromptFor(String toolName) {
        return "healer_run_failed_test".equals(toolName)
                ? "Heal my last failed test run"
                : "Diagnose my last failed test run";
    }
}
