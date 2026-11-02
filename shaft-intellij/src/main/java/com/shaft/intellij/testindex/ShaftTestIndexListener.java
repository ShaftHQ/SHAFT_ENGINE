package com.shaft.intellij.testindex;

import com.intellij.execution.ExecutionListener;
import com.intellij.execution.RunnerAndConfigurationSettings;
import com.intellij.execution.process.ProcessHandler;
import com.intellij.execution.runners.ExecutionEnvironment;
import com.intellij.openapi.project.Project;
import org.jetbrains.annotations.NotNull;

import java.util.Locale;

/**
 * Feeds {@link ShaftTestIndex} (pass/fail rows for the "SHAFT Tests" tool-window tab) and
 * {@link ShaftTestWatchService} (the "last run configuration" watch-mode replays) from the same
 * IDE execution events, mirroring {@code FailedRunDoctorNotifier}'s run-profile heuristic.
 */
public final class ShaftTestIndexListener implements ExecutionListener {
    @Override
    public void processStarted(@NotNull String executorId,
                               @NotNull ExecutionEnvironment environment,
                               @NotNull ProcessHandler handler) {
        Project project = environment.getProject();
        if (project.isDisposed() || !looksLikeTestRun(environment)) {
            return;
        }
        RunnerAndConfigurationSettings settings = environment.getRunnerAndConfigurationSettings();
        if (settings != null) {
            // Also forces ShaftTestWatchService's lazy project-service instantiation (and with it,
            // its VFS_CHANGES subscription) the first time any test runs in this project session --
            // watch-mode reruns are meaningless before a "last run" exists anyway, so there is
            // nothing to eagerly wire up any earlier than this.
            ShaftTestWatchService.getInstance(project).recordLastRunSettings(settings);
        }
    }

    @Override
    public void processTerminated(@NotNull String executorId,
                                  @NotNull ExecutionEnvironment environment,
                                  @NotNull ProcessHandler handler,
                                  int exitCode) {
        Project project = environment.getProject();
        if (project.isDisposed() || !looksLikeTestRun(environment)) {
            return;
        }
        String profileName = environment.getRunProfile().getName();
        ShaftTestIndex.getInstance(project).recordRun(profileName, exitCode, System.currentTimeMillis());
    }

    private static boolean looksLikeTestRun(ExecutionEnvironment environment) {
        String profileName = environment.getRunProfile().getName();
        String profileClass = environment.getRunProfile().getClass().getName().toLowerCase(Locale.ROOT);
        String name = profileName == null ? "" : profileName.toLowerCase(Locale.ROOT);
        return profileClass.contains("test") || name.contains("test") || name.contains("suite");
    }
}
