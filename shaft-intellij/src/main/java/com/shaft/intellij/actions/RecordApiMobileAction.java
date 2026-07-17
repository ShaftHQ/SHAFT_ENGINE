package com.shaft.intellij.actions;

import com.google.gson.JsonObject;
import com.intellij.openapi.actionSystem.AnAction;
import com.intellij.openapi.actionSystem.AnActionEvent;
import com.intellij.openapi.project.DumbAware;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.ui.Messages;
import com.intellij.openapi.wm.ToolWindow;
import com.intellij.openapi.wm.ToolWindowManager;
import com.intellij.ui.content.Content;
import com.shaft.intellij.notifications.ShaftNotifier;
import com.shaft.intellij.project.ShaftProjectDetector;
import com.shaft.intellij.settings.ShaftSettingsState;
import com.shaft.intellij.ui.ShaftToolWindowPanel;
import org.jetbrains.annotations.NotNull;

/**
 * Prepares an MCP {@code mobile_api_record_start} request and opens the API Recording tab so a
 * backend team can capture and generate a SHAFT.API test without launching a browser -- the
 * loopback MITM proxy captures native mobile API traffic directly (issue #3530 A2).
 */
public final class RecordApiMobileAction extends AnAction implements DumbAware {
    private static final String DEFAULT_PLATFORM = "Android";
    private static final String NOTIFICATION_TITLE = "API recording (mobile)";

    @Override
    public void actionPerformed(@NotNull AnActionEvent event) {
        Project project = event.getProject();
        if (project == null) {
            return;
        }

        String platform = Messages.showInputDialog(
                project,
                "Enter the target platform (Android or iOS):",
                "Record API (No Browser)",
                Messages.getQuestionIcon(),
                DEFAULT_PLATFORM,
                null);
        if (platform == null || platform.isBlank()) {
            return;
        }
        String trimmedPlatform = platform.trim();

        JsonObject arguments = new JsonObject();
        arguments.addProperty("platform", trimmedPlatform);
        arguments.addProperty("deviceLabel", "");
        arguments.addProperty("outputPath", "");

        if (!ShaftSettingsState.getInstance().getState().advancedUiEnabled) {
            // Default mode: route to the Assistant with the platform the user just typed, matching
            // RecordApiWebAction's equivalent plain-language routing (issue #3552) -- the raw API
            // Recording tab stays hidden here.
            openAssistantPrompt(project, recordApiMobilePrompt(trimmedPlatform));
            ShaftNotifier.info(project, NOTIFICATION_TITLE,
                    "Pure-API recording request ready in the Assistant for " + trimmedPlatform + ".");
            return;
        }
        openApiRecordingTab(project, trimmedPlatform, arguments);
        ShaftNotifier.info(project, NOTIFICATION_TITLE, "Pure-API recording prepared for " + trimmedPlatform + ".");
    }

    @Override
    public void update(@NotNull AnActionEvent event) {
        Project project = event.getProject();
        event.getPresentation().setEnabledAndVisible(project != null && ShaftProjectDetector.isShaftProject(project));
    }

    /**
     * Plain-language Assistant equivalent of the {@code mobile_api_record_start} MCP request.
     * Package-private for {@code RecordApiMobileActionTest}.
     */
    static String recordApiMobilePrompt(String platform) {
        return "Record API traffic without a browser on " + platform;
    }

    private static void openAssistantPrompt(Project project, String text) {
        ToolWindowManager.getInstance(project).invokeLater(() -> {
            ToolWindow toolWindow = ToolWindowManager.getInstance(project).getToolWindow("SHAFT");
            if (toolWindow == null) {
                return;
            }
            toolWindow.show(() -> {
                Content content = toolWindow.getContentManager().getContent(0);
                if (content != null && content.getComponent() instanceof ShaftToolWindowPanel panel) {
                    panel.prefillAssistantPrompt(text);
                }
            });
        });
    }

    private static void openApiRecordingTab(Project project, String platform, JsonObject arguments) {
        ToolWindowManager.getInstance(project).invokeLater(() -> {
            ToolWindow toolWindow = ToolWindowManager.getInstance(project).getToolWindow("SHAFT");
            if (toolWindow == null) {
                return;
            }
            toolWindow.show(() -> {
                Content content = toolWindow.getContentManager().getContent(0);
                if (content == null) {
                    return;
                }
                if (content.getComponent() instanceof ShaftToolWindowPanel panel) {
                    panel.showPureApiRecordingTab(platform, arguments);
                }
            });
        });
    }
}
