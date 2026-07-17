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

import java.util.ArrayList;
import java.util.List;
import java.util.Set;

/**
 * Prepares an MCP {@code capture_api_start} request for a target URL and opens the
 * API Recording tab so the user can review captured network transactions live.
 */
public final class RecordApiWebAction extends AnAction implements DumbAware {
    private static final String DEFAULT_TARGET_URL = "https://example.com";
    private static final String START_TOOL_NAME = "capture_api_start";
    private static final String NOTIFICATION_TITLE = "API recording (web)";

    /**
     * Exact field names of {@code com.shaft.capture.runtime.NetworkCaptureOptions}, the class the
     * MCP layer binds {@code networkOptions} JSON keys to. A key outside this set is silently
     * ignored by the binder rather than rejected -- most dangerously, a typo'd
     * {@code captureRequestBodies}/{@code captureResponseBodies} leaves body capture off with no
     * error, which is the exact silent failure issue #3548 item 2 hardens against.
     */
    static final Set<String> NETWORK_CAPTURE_OPTIONS_FIELDS = Set.of(
            "enabled", "excludeAssets", "excludePattern", "includePattern",
            "captureResponseBodies", "captureRequestBodies");

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

        List<String> unknownKeys = unknownNetworkCaptureOptionKeys(networkOptions);
        if (!unknownKeys.isEmpty()) {
            // Warn instead of failing the request: an unrecognized key is dropped by the MCP binder,
            // not rejected, so the recording still starts -- just without the intended option applied.
            ShaftNotifier.warn(project, NOTIFICATION_TITLE, "networkOptions has unrecognized key(s) " + unknownKeys
                    + " -- they will be silently ignored and any body capture they were meant to control "
                    + "may default off. Valid keys: " + NETWORK_CAPTURE_OPTIONS_FIELDS);
        }

        JsonObject arguments = new JsonObject();
        arguments.addProperty("targetUrl", targetUrl.trim());
        // Visible browser: the user drives it interactively to generate traffic,
        // matching AssistantCommand's analogous interactive capture_start flow.
        arguments.addProperty("headless", false);
        arguments.add("networkOptions", networkOptions);

        if (!ShaftSettingsState.getInstance().getState().advancedUiEnabled) {
            // Default mode: route to the Assistant with the URL the user just typed, carried into an
            // equivalent plain-language request, instead of discarding it behind a dead-end warning
            // (issue #3552) -- the raw API Recording tab stays hidden here.
            openAssistantPrompt(project, recordApiPrompt(targetUrl.trim()));
            ShaftNotifier.info(project, NOTIFICATION_TITLE,
                    "API recording request ready in the Assistant for " + targetUrl.trim() + ".");
            return;
        }
        openApiRecordingTab(project, targetUrl.trim(), arguments);
        ShaftNotifier.info(project, NOTIFICATION_TITLE, "API recording prepared for " + targetUrl.trim() + ".");
    }

    @Override
    public void update(@NotNull AnActionEvent event) {
        Project project = event.getProject();
        event.getPresentation().setEnabledAndVisible(project != null && ShaftProjectDetector.isShaftProject(project));
    }

    /**
     * Plain-language Assistant equivalent of the {@code capture_api_start} MCP request. Package-
     * private for {@code RecordApiWebActionTest}.
     */
    static String recordApiPrompt(String targetUrl) {
        return "Record API traffic on " + targetUrl;
    }

    /**
     * Returns any {@code networkOptions} keys that do not match a
     * {@code com.shaft.capture.runtime.NetworkCaptureOptions} field, so a mistyped key can be
     * surfaced as a visible warning instead of silently doing nothing. Package-private for
     * {@code RecordApiWebActionTest}.
     *
     * @param networkOptions the {@code networkOptions} JSON object about to be sent
     * @return unknown key names, in encounter order; empty when every key is valid
     */
    static List<String> unknownNetworkCaptureOptionKeys(JsonObject networkOptions) {
        List<String> unknown = new ArrayList<>();
        for (String key : networkOptions.keySet()) {
            if (!NETWORK_CAPTURE_OPTIONS_FIELDS.contains(key)) {
                unknown.add(key);
            }
        }
        return unknown;
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
