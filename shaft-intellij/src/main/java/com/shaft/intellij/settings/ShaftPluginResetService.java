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
import java.util.concurrent.CompletableFuture;
import java.util.function.Supplier;

/**
 * Factory-resets every plugin-local data store: settings, stored provider credentials, tool
 * approvals, and the per-project Assistant chat history. Every open SHAFT tool window is then
 * re-rendered. A completed first-run wizard stays on the main view; every other reset returns to setup.
 */
public final class ShaftPluginResetService {
    private static final String TOOL_WINDOW_ID = "SHAFT";

    private final Runnable settingsReset;
    private final Runnable upgradeSettingsReset;
    private final Supplier<CompletableFuture<Void>> credentialsReset;
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
                () -> ShaftCredentialService.getInstance().clearAllAsync(),
                ShaftPluginResetService::resetOpenProjectApprovals,
                ShaftPluginResetService::openProjectChatStates,
                ShaftPluginResetService::rerenderOpenToolWindows,
                () -> resetSettingsPreservingWizardComplete(ShaftSettingsState.getInstance()));
    }

    ShaftPluginResetService(Runnable settingsReset,
                             Supplier<CompletableFuture<Void>> credentialsReset,
                             Runnable approvalsReset,
                             Supplier<List<ShaftAssistantChatState>> chatStatesSupplier,
                             Runnable toolWindowRerenderer) {
        this(settingsReset, credentialsReset, approvalsReset, chatStatesSupplier, toolWindowRerenderer, settingsReset);
    }

    ShaftPluginResetService(Runnable settingsReset,
                             Supplier<CompletableFuture<Void>> credentialsReset,
                             Runnable approvalsReset,
                             Supplier<List<ShaftAssistantChatState>> chatStatesSupplier,
                             Runnable toolWindowRerenderer,
                             Runnable upgradeSettingsReset) {
        this.settingsReset = settingsReset;
        this.upgradeSettingsReset = upgradeSettingsReset;
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
        resetState(true);
    }

    /**
     * Drops a stale {@code mcpCommand} and the rest of the factory setup state on upgrade, keeps
     * {@code firstRunWizardCompleted} when it was already true, and preserves Assistant chat.
     * A completed wizard is not sent back to the setup view.
     */
    public void resetForUpgrade() {
        resetState(false);
    }

    private void resetState(boolean clearChat) {
        if (clearChat) {
            settingsReset.run();
        } else {
            upgradeSettingsReset.run();
        }
        approvalsReset.run();
        if (clearChat) {
            for (ShaftAssistantChatState chatState : chatStatesSupplier.get()) {
                chatState.clearAll();
            }
        }
        credentialsReset.get().whenComplete((ignoredResult, ignoredError) -> toolWindowRerenderer.run());
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

    /**
     * Upgrade reset: drop a stale MCP command. A user who already finished the wizard, or whose
     * MCP install was verified before that flag existed, stays off the wizard.
     */
    static void resetSettingsPreservingWizardComplete(ShaftSettingsState settingsState) {
        ShaftSettingsState.Settings live = settingsState.getState();
        boolean completed = live.firstRunWizardCompleted || live.mcpReady();
        String notice = live.lastUpgradeNoticeVersion == null ? "" : live.lastUpgradeNoticeVersion;
        resetSettings(settingsState);
        ShaftSettingsState.Settings restored = settingsState.getState();
        restored.firstRunWizardCompleted = completed;
        restored.lastUpgradeNoticeVersion = notice;
        restored.mcpCommand = "";
        restored.mcpSetupComplete = false;
    }

    static boolean showSetupAfterUpgrade(ShaftSettingsState.Settings settings) {
        return settings == null || !settings.firstRunWizardCompleted;
    }

    private static void resetOpenProjectApprovals() {
        for (Project project : ProjectManager.getInstance().getOpenProjects()) {
            ToolApprovalService.getInstance(project).reset();
        }
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
                    if (content.getComponent() instanceof ShaftToolWindowPanel panel
                            && panel.returnToSetupAfterUpgrade()) {
                        panel.resetToSetupView();
                    }
                }
            }
        });
    }
}
