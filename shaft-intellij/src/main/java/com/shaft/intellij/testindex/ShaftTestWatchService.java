package com.shaft.intellij.testindex;

import com.intellij.execution.RunnerAndConfigurationSettings;
import com.intellij.execution.executors.DefaultRunExecutor;
import com.intellij.execution.runners.ExecutionUtil;
import com.intellij.notification.Notification;
import com.intellij.notification.NotificationGroupManager;
import com.intellij.notification.NotificationType;
import com.intellij.openapi.Disposable;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.vfs.VirtualFileManager;
import com.intellij.openapi.vfs.newvfs.BulkFileListener;
import com.intellij.openapi.vfs.newvfs.events.VFileEvent;
import com.intellij.util.Alarm;
import com.intellij.util.messages.MessageBusConnection;
import com.shaft.intellij.settings.ShaftSettingsState;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.List;

/**
 * Bounded, opt-in "watch mode": when {@link ShaftSettingsState.Settings#watchModeEnabled} is on,
 * saving a file under {@code src/test/} debounces then reruns the last SHAFT test run
 * configuration (captured by {@link ShaftTestIndexListener}).
 * <p>
 * Bounded scheduling (a 750ms debounce plus {@link WatchRerunThrottle}'s rolling-window cap)
 * exists because unbounded Maven-fork reruns on rapid saves are a known repo risk on Windows: each
 * rerun forks a fresh Surefire JVM, and an unthrottled save-triggered loop can pile up forked-JVM
 * starts faster than they finish. Default OFF; the setting lives in {@code ShaftSettingsState}.
 */
public final class ShaftTestWatchService implements Disposable {
    private static final long DEBOUNCE_MILLIS = 750L;
    private static final String NOTIFICATION_GROUP_ID = "SHAFT Notifications";

    private final Project project;
    private final WatchRerunThrottle throttle;
    private final Alarm debounceAlarm;
    private volatile RunnerAndConfigurationSettings lastRunSettings;

    public ShaftTestWatchService(@NotNull Project project) {
        this(project, new WatchRerunThrottle());
    }

    ShaftTestWatchService(@NotNull Project project, @NotNull WatchRerunThrottle throttle) {
        this.project = project;
        this.throttle = throttle;
        this.debounceAlarm = new Alarm(Alarm.ThreadToUse.POOLED_THREAD, this);
        MessageBusConnection connection = project.getMessageBus().connect(this);
        connection.subscribe(VirtualFileManager.VFS_CHANGES, new BulkFileListener() {
            @Override
            public void after(@NotNull List<? extends VFileEvent> events) {
                onEvents(events);
            }
        });
    }

    /**
     * Returns the project-level watch service.
     *
     * @param project the project to scope watch mode to
     * @return watch service
     */
    public static ShaftTestWatchService getInstance(@NotNull Project project) {
        return project.getService(ShaftTestWatchService.class);
    }

    /**
     * Records the settings of the most recently started test run, so a later watch-mode trigger has
     * something to replay. Called by {@link ShaftTestIndexListener}.
     *
     * @param settings the run configuration settings that were just started
     */
    void recordLastRunSettings(@NotNull RunnerAndConfigurationSettings settings) {
        this.lastRunSettings = settings;
    }

    private void onEvents(List<? extends VFileEvent> events) {
        if (project.isDisposed() || !isWatchModeEnabled()) {
            return;
        }
        String basePath = project.getBasePath();
        if (basePath == null) {
            return;
        }
        boolean relevant = events.stream().anyMatch(event -> isTestSourceChange(event.getPath(), basePath));
        if (!relevant) {
            return;
        }
        debounceAlarm.cancelAllRequests();
        debounceAlarm.addRequest(this::onDebounceFired, (int) DEBOUNCE_MILLIS);
    }

    private void onDebounceFired() {
        if (project.isDisposed() || !isWatchModeEnabled()) {
            return;
        }
        long now = System.currentTimeMillis();
        if (throttle.tryAcquire(now)) {
            rerunLastTest();
        } else if (throttle.shouldNotifyThrottled(now)) {
            notifyThrottled();
        }
    }

    private void rerunLastTest() {
        RunnerAndConfigurationSettings settings = lastRunSettings;
        if (settings == null) {
            // Nothing has run in this project session yet: no-op, per design (there is nothing
            // meaningful to replay).
            return;
        }
        ApplicationManager.getApplication().invokeLater(() -> {
            if (!project.isDisposed()) {
                ExecutionUtil.runConfiguration(settings, DefaultRunExecutor.getRunExecutorInstance());
            }
        });
    }

    private void notifyThrottled() {
        Notification notification = NotificationGroupManager.getInstance()
                .getNotificationGroup(NOTIFICATION_GROUP_ID)
                .createNotification("SHAFT watch mode paused",
                        "SHAFT watch mode paused — rerun limit reached", NotificationType.WARNING);
        notification.notify(project);
    }

    private boolean isWatchModeEnabled() {
        return ShaftSettingsState.getInstance().getState().watchModeEnabled;
    }

    /**
     * Returns whether a changed file path is under a {@code src/test/} directory within the given
     * project base path. Pure string logic (normalizes {@code \} to {@code /} for Windows paths),
     * kept separate from the VFS glue above so it is directly unit testable.
     *
     * @param eventPath changed file's absolute path, or {@code null}
     * @param projectBasePath project base path, or {@code null}
     * @return {@code true} when the path is a test-source change within the project
     */
    static boolean isTestSourceChange(@Nullable String eventPath, @Nullable String projectBasePath) {
        if (eventPath == null || projectBasePath == null) {
            return false;
        }
        String normalizedEvent = eventPath.replace('\\', '/');
        String normalizedBase = projectBasePath.replace('\\', '/');
        if (!normalizedEvent.startsWith(normalizedBase)) {
            return false;
        }
        return normalizedEvent.contains("/src/test/");
    }

    @Override
    public void dispose() {
        // debounceAlarm and the message bus connection are children of this Disposable and are
        // cancelled/disconnected automatically.
    }
}
