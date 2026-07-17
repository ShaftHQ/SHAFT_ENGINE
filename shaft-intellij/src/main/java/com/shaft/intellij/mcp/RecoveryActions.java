package com.shaft.intellij.mcp;

import com.intellij.openapi.project.Project;
import com.intellij.openapi.wm.ToolWindow;
import com.intellij.openapi.wm.ToolWindowManager;

public final class RecoveryActions {
    private static final String EVENT_LOG_TOOL_WINDOW_ID = "Event Log";

    public enum Kind { RETRY, RESTART, VIEW_LOGS }

    private RecoveryActions() {
        throw new IllegalStateException("Utility class");
    }

    public static Kind forCategory(McpInvocationError category) {
        return switch (category) {
            case TIMEOUT, MALFORMED_RESPONSE -> Kind.RETRY;
            case PROCESS_EXITED, CONNECTION_LOST -> Kind.RESTART;
            case TOOL_ERROR -> Kind.VIEW_LOGS;
        };
    }

    public static void activateEventLog(Project project) {
        if (project == null) {
            return;
        }
        ToolWindow toolWindow = ToolWindowManager.getInstance(project).getToolWindow(EVENT_LOG_TOOL_WINDOW_ID);
        if (toolWindow != null) {
            toolWindow.activate(null);
        }
    }
}
