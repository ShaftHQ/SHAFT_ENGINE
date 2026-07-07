package com.shaft.intellij.settings;

import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.project.ProjectManager;
import com.intellij.openapi.wm.ToolWindow;
import com.intellij.openapi.wm.ToolWindowManager;
import com.intellij.ui.content.Content;
import com.shaft.intellij.approval.ToolApprovalService;
import com.shaft.intellij.ui.ShaftAssistantChatState;
import com.shaft.intellij.ui.ShaftToolWindowPanel;

import java.util.ArrayList;
import java.util.List;
import java.util.function.Supplier;

/**
 * Factory-resets every plugin-local data store: settings, stored provider credentials, tool
 * approvals, and the per-project Assistant chat history. Every open SHAFT tool window is then
 * re-rendered back to the setup view on the EDT.
 */
public final class ShaftPluginResetService {
    private static final String TOOL_WINDOW_ID = "SHAFT";

    private final Runnable settingsReset;
    private final Runnable credentialsReset;
    private final Runnable approvalsReset;
    private final Supplier<List<ShaftAssistantChatState>> chatStatesSupplier;
    private final Runnable toolWindowRerenderer;

    /**
     * Returns the application-level plugin reset service.
     *
     * @return plugin reset service
     */
    public static ShaftPluginResetService getInstance() {
        return ApplicationManager.getApplication().getService(ShaftPluginResetService.class);
    }

    public ShaftPluginResetService() {
        this(
                () -> resetSettings(ShaftSettingsState.getInstance()),
                () -> ShaftCredentialService.getInstance().clearAll(),
                () -> ToolApprovalService.getInstance().reset(),
                ShaftPluginResetService::openProjectChatStates,
                ShaftPluginResetService::rerenderOpenToolWindows);
    }

    ShaftPluginResetService(Runnable settingsReset,
                             Runnable credentialsReset,
                             Runnable approvalsReset,
                             Supplier<List<ShaftAssistantChatState>> chatStatesSupplier,
                             Runnable toolWindowRerenderer) {
        this.settingsReset = settingsReset;
        this.credentialsReset = credentialsReset;
        this.approvalsReset = approvalsReset;
        this.chatStatesSupplier = chatStatesSupplier;
        this.toolWindowRerenderer = toolWindowRerenderer;
    }

    /**
     * Factory-resets every plugin-local data store and re-renders open SHAFT tool windows back to
     * the setup view.
     */
    public void resetEverything() {
        settingsReset.run();
        credentialsReset.run();
        approvalsReset.run();
        for (ShaftAssistantChatState chatState : chatStatesSupplier.get()) {
            chatState.clearAll();
        }
        toolWindowRerenderer.run();
    }

    /**
     * Resets settings to the documented factory defaults (see
     * {@link ShaftSettingsState#factoryDefaults()}), explicitly forcing the not-set-up state so the
     * fresh-install setup view renders despite the bean's own {@code mcpSetupComplete} default.
     *
     * @param settingsState the settings state to reset
     */
    static void resetSettings(ShaftSettingsState settingsState) {
        settingsState.loadState(ShaftSettingsState.factoryDefaults());
    }

    private static List<ShaftAssistantChatState> openProjectChatStates() {
        List<ShaftAssistantChatState> states = new ArrayList<>();
        for (Project project : ProjectManager.getInstance().getOpenProjects()) {
            states.add(ShaftAssistantChatState.getInstance(project));
        }
        return states;
    }

    private static void rerenderOpenToolWindows() {
        ApplicationManager.getApplication().invokeLater(() -> {
            for (Project project : ProjectManager.getInstance().getOpenProjects()) {
                if (project.isDisposed()) {
                    continue;
                }
                ToolWindow toolWindow = ToolWindowManager.getInstance(project).getToolWindow(TOOL_WINDOW_ID);
                if (toolWindow == null) {
                    continue;
                }
                for (Content content : toolWindow.getContentManager().getContents()) {
                    if (content.getComponent() instanceof ShaftToolWindowPanel panel) {
                        panel.resetToSetupView();
                    }
                }
            }
        });
    }
}
