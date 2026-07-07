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
import com.shaft.intellij.ui.ShaftToolWindowPanel;
import org.jetbrains.annotations.NotNull;

/**
 * Prepares an MCP {@code capture_api_start} request for a target URL and opens the
 * API Recording tab so the user can review captured network transactions live.
 */
public final class RecordApiWebAction extends AnAction implements DumbAware {
    private static final String DEFAULT_TARGET_URL = "https://example.com";
    private static final String START_TOOL_NAME = "capture_api_start";

    @Override
    public void actionPerformed(@NotNull AnActionEvent event) {
        Project project = event.getProject();
        if (project == null) {
            return;
        }

        String targetUrl = Messages.showInputDialog(
                project,
                "Enter the URL to open and record API traffic for:",
                "Record API (Web)",
                Messages.getQuestionIcon(),
                DEFAULT_TARGET_URL,
                null);
        if (targetUrl == null || targetUrl.isBlank()) {
            return;
        }

        // Field names must match com.shaft.capture.runtime.NetworkCaptureOptions
        // exactly -- the MCP layer binds JSON keys to that class's fields, so a
        // mismatched shape here is silently ignored rather than rejected, and
        // request/response bodies (the entire point of API recording) default
        // to false unless captureRequestBodies/captureResponseBodies is set.
        JsonObject networkOptions = new JsonObject();
        networkOptions.addProperty("enabled", true);
        networkOptions.addProperty("excludeAssets", true);
        networkOptions.addProperty("captureRequestBodies", true);
        networkOptions.addProperty("captureResponseBodies", true);

        JsonObject arguments = new JsonObject();
        arguments.addProperty("targetUrl", targetUrl.trim());
        // Visible browser: the user drives it interactively to generate traffic,
        // matching AssistantCommand's analogous interactive capture_start flow.
        arguments.addProperty("headless", false);
        arguments.add("networkOptions", networkOptions);

        openApiRecordingTab(project, targetUrl.trim(), arguments);
        ShaftNotifier.info(project, "SHAFT", "API recording prepared for " + targetUrl.trim() + ".");
    }

    @Override
    public void update(@NotNull AnActionEvent event) {
        event.getPresentation().setEnabledAndVisible(event.getProject() != null);
    }

    private static void openApiRecordingTab(Project project, String targetUrl, JsonObject arguments) {
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
                    panel.showApiRecordingTab(targetUrl, arguments);
                }
            });
        });
    }

    /**
     * Returns the MCP tool name invoked to start an API recording session.
     *
     * @return tool name
     */
    public static String startToolName() {
        return START_TOOL_NAME;
    }
}
