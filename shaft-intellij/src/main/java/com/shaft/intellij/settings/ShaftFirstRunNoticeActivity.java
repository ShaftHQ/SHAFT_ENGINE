package com.shaft.intellij.settings;

import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.startup.ProjectActivity;
import com.intellij.openapi.wm.ToolWindow;
import com.intellij.openapi.wm.ToolWindowManager;
import com.shaft.intellij.notifications.ShaftNotifier;
import com.shaft.intellij.ui.firstrun.WizardMessages;
import kotlin.Unit;
import kotlin.coroutines.Continuation;
import org.jetbrains.annotations.NotNull;

import java.util.concurrent.atomic.AtomicBoolean;

/**
 * One startup reminder when setup is still incomplete. Schedules off the caller thread and
 * never builds the setup UI.
 */
public final class ShaftFirstRunNoticeActivity implements ProjectActivity {
    private static final AtomicBoolean POSTED = new AtomicBoolean();

    @Override
    public Object execute(@NotNull Project project, @NotNull Continuation<? super Unit> continuation) {
        schedule(() -> maybeNotify(ShaftSettingsState.getInstance().getState(), () -> post(project)));
        return Unit.INSTANCE;
    }

    static java.util.concurrent.Future<?> schedule(@NotNull Runnable check) {
        return ApplicationManager.getApplication().executeOnPooledThread(check);
    }

    public static void maybeNotify(ShaftSettingsState.Settings settings, Runnable poster) {
        if (settings == null || poster == null || settings.firstRunWizardCompleted) {
            return;
        }
        if (settings.mcpReady()) {
            settings.firstRunWizardCompleted = true;
            return;
        }
        poster.run();
    }

    private static void post(Project project) {
        if (!POSTED.compareAndSet(false, true)) {
            return;
        }
        ShaftNotifier.infoWithAction(project,
                WizardMessages.get("wizard.startup.title"),
                WizardMessages.get("wizard.startup.body"),
                WizardMessages.get("wizard.startup.action"),
                () -> showToolWindow(project));
    }

    private static void showToolWindow(Project project) {
        if (project == null) {
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
