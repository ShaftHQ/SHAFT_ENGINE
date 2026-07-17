package com.shaft.intellij.testindex;

import com.intellij.execution.RunnerAndConfigurationSettings;
import com.intellij.execution.configurations.RunConfiguration;
import com.intellij.execution.executors.DefaultRunExecutor;
import com.intellij.execution.junit.JUnitConfiguration;
import com.intellij.execution.runners.ExecutionUtil;
import com.intellij.notification.Notification;
import com.intellij.notification.NotificationAction;
import com.intellij.notification.NotificationGroupManager;
import com.intellij.notification.NotificationType;
import com.intellij.openapi.Disposable;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.application.ReadAction;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.openapi.vfs.VirtualFileManager;
import com.intellij.openapi.vfs.newvfs.BulkFileListener;
import com.intellij.openapi.vfs.newvfs.events.VFileEvent;
import com.intellij.psi.PsiClass;
import com.intellij.psi.PsiJavaFile;
import com.intellij.psi.PsiManager;
import com.intellij.util.Alarm;
import com.intellij.util.messages.MessageBusConnection;
import com.shaft.intellij.settings.ShaftSettingsState;
import com.theoryinpractice.testng.configuration.TestNGConfiguration;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.List;

/**
 * Bounded, opt-in "watch mode": when {@link ShaftSettingsState.Settings#watchModeEnabled} is on,
 * saving a file under {@code src/test/} debounces then reruns the last SHAFT test run
 * configuration (captured by {@link ShaftTestIndexListener}) -- but only when that changed file's
 * class actually matches what the last run targeted ({@link #correlate}). A save to an unrelated
 * test class instead surfaces a notification offering to run the class that was actually changed
 * (via {@link ShaftRunConfigurationResolver#run}), rather than silently replaying a stale result.
 * When either side of that comparison can't be determined (a non-Java file, or an unrecognized
 * run-configuration type), watch mode falls back to today's unconditional rerun rather than
 * blocking on an inability to introspect.
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
    private volatile VFileEvent pendingCorrelationEvent;

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
        VFileEvent relevantEvent = null;
        for (VFileEvent event : events) {
            if (isTestSourceChange(event.getPath(), basePath)) {
                // Last relevant event in the batch wins: correlation only needs "the file that
                // changed" for the common single-save case this feature targets (see class javadoc).
                relevantEvent = event;
            }
        }
        if (relevantEvent == null) {
            return;
        }
        pendingCorrelationEvent = relevantEvent;
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
        String changedClassFqn = resolveChangedClassFqn(pendingCorrelationEvent);
        String lastRunClassName = extractTargetClassName(settings.getConfiguration());
        if (correlate(changedClassFqn, lastRunClassName) == CorrelationResult.MISMATCH) {
            // Both sides are known and clearly different classes: rerunning lastRunSettings here
            // would silently replay a test unrelated to what was just saved (see class javadoc).
            notifyScopeMismatch(changedClassFqn, lastRunClassName);
            return;
        }
        ApplicationManager.getApplication().invokeLater(() -> {
            if (!project.isDisposed()) {
                ExecutionUtil.runConfiguration(settings, DefaultRunExecutor.getRunExecutorInstance());
            }
        });
    }

    /**
     * Resolves the top-level class FQN of the file a {@link VFileEvent} touched, so
     * {@link #rerunLastTest()} can correlate it against the last run's target class.
     * Runs the PSI lookup in a read action, per platform threading rules.
     *
     * @param event the triggering VFS event, or {@code null}
     * @return the changed file's top-level class FQN, or {@code null} when the event, its
     *         {@link VirtualFile}, or a resolvable {@link PsiJavaFile} top-level class is unavailable
     *         (e.g. a test resource save) -- meaning there is nothing to correlate against
     */
    @Nullable
    private String resolveChangedClassFqn(@Nullable VFileEvent event) {
        VirtualFile file = event == null ? null : event.getFile();
        if (file == null) {
            return null;
        }
        return ReadAction.compute(() -> {
            if (project.isDisposed() || !file.isValid()) {
                return null;
            }
            if (!(PsiManager.getInstance(project).findFile(file) instanceof PsiJavaFile javaFile)) {
                return null;
            }
            PsiClass[] classes = javaFile.getClasses();
            return classes.length > 0 ? classes[0].getQualifiedName() : null;
        });
    }

    /**
     * Extracts the class a run configuration targets, for the two configuration types
     * {@link ShaftRunConfigurationResolver} creates (see that class's javadoc).
     * <p>
     * Confirmed against the decompiled platform classes: {@code TestNGConfiguration.getPersistantData()}
     * (sic -- the platform's own spelling) and {@code JUnitConfiguration.getPersistentData()} each
     * return a data holder whose {@code getMainClassName()} never returns {@code null}, only
     * {@code ""} when unset.
     *
     * @param configuration the last run's configuration
     * @return the target class name ({@code null} or blank when unset or an unrecognized configuration
     *         type -- both treated as "cannot determine" by {@link #correlate}
     */
    @Nullable
    static String extractTargetClassName(RunConfiguration configuration) {
        if (configuration instanceof TestNGConfiguration testNg) {
            return testNg.getPersistantData().getMainClassName();
        }
        if (configuration instanceof JUnitConfiguration jUnit) {
            return jUnit.getPersistentData().getMainClassName();
        }
        return null;
    }

    /**
     * Compares the changed file's class against the last run's target class.
     *
     * @param changedClassFqn  the changed file's resolved class FQN, or {@code null}/blank when
     *                         unresolvable (non-Java file, no top-level class)
     * @param lastRunClassName the last run's target class name, or {@code null}/blank when it could
     *                         not be determined
     * @return {@link CorrelationResult#INDETERMINATE} when either side is unknown -- falls back to
     *         today's unconditional rerun rather than blocking on an inability to introspect;
     *         otherwise {@link CorrelationResult#MATCH} (exact FQN match, else simple-name match, per
     *         {@link ShaftRunConfigurationResolver}'s own short-name-ambiguity precedent) or
     *         {@link CorrelationResult#MISMATCH}
     */
    static CorrelationResult correlate(@Nullable String changedClassFqn, @Nullable String lastRunClassName) {
        if (changedClassFqn == null || changedClassFqn.isBlank()
                || lastRunClassName == null || lastRunClassName.isBlank()) {
            return CorrelationResult.INDETERMINATE;
        }
        if (changedClassFqn.equals(lastRunClassName)) {
            return CorrelationResult.MATCH;
        }
        return simpleName(changedClassFqn).equals(simpleName(lastRunClassName))
                ? CorrelationResult.MATCH : CorrelationResult.MISMATCH;
    }

    /**
     * Returns the simple (unqualified) class name from a possibly fully-qualified name.
     *
     * @param className a simple or fully-qualified class name
     * @return the part after the last {@code .}, or the whole string if there is none
     */
    static String simpleName(String className) {
        int lastDot = className.lastIndexOf('.');
        return lastDot >= 0 ? className.substring(lastDot + 1) : className;
    }

    /** Outcome of {@link #correlate(String, String)}. */
    enum CorrelationResult {
        MATCH, MISMATCH, INDETERMINATE
    }

    private void notifyScopeMismatch(String changedClassFqn, String lastRunClassName) {
        String changedSimpleName = simpleName(changedClassFqn);
        String lastRunSimpleName = simpleName(lastRunClassName);
        Notification notification = NotificationGroupManager.getInstance()
                .getNotificationGroup(NOTIFICATION_GROUP_ID)
                .createNotification("SHAFT watch mode skipped a stale rerun",
                        "Changed `" + changedSimpleName + "` is not covered by the last run (`"
                                + lastRunSimpleName + "`)", NotificationType.INFORMATION);
        notification.addAction(NotificationAction.createSimple("Run " + changedSimpleName,
                () -> ShaftRunConfigurationResolver.run(project, changedClassFqn)));
        notification.notify(project);
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
