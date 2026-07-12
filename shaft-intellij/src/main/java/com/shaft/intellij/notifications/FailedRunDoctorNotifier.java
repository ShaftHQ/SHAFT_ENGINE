package com.shaft.intellij.notifications;

import com.google.gson.JsonArray;
import com.google.gson.JsonObject;
import com.intellij.execution.ExecutionListener;
import com.intellij.execution.process.ProcessHandler;
import com.intellij.execution.runners.ExecutionEnvironment;
import com.intellij.notification.Notification;
import com.intellij.notification.NotificationAction;
import com.intellij.notification.NotificationGroupManager;
import com.intellij.notification.NotificationType;
import com.intellij.openapi.actionSystem.AnActionEvent;
import com.intellij.openapi.project.Project;
import org.jetbrains.annotations.NotNull;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.stream.Stream;

/**
 * Offers one-click SHAFT Doctor triage when a test run fails and Allure evidence exists.
 *
 * <p>The deterministic Doctor analysis turns failed Allure results into a root-cause report in
 * seconds, but it used to hide behind a workflow template the user had to know about. This
 * listener raises a notification right at the failed run, one click away from the prefilled
 * {@code doctor_analyze_failed_allure} request.</p>
 */
public final class FailedRunDoctorNotifier implements ExecutionListener {
    private static final String GROUP_ID = "SHAFT Notifications";
    private static final long THROTTLE_MILLIS = 60_000;
    private static final Map<String, Long> LAST_NOTIFIED_BY_PROJECT = new ConcurrentHashMap<>();

    @Override
    public void processTerminated(@NotNull String executorId,
                                  @NotNull ExecutionEnvironment environment,
                                  @NotNull ProcessHandler handler,
                                  int exitCode) {
        Project project = environment.getProject();
        if (exitCode == 0 || project.isDisposed()) {
            return;
        }
        if (!looksLikeTestRun(environment)) {
            return;
        }
        String basePath = project.getBasePath();
        if (basePath == null || !hasAllureResults(Path.of(basePath))) {
            return;
        }
        if (throttled(basePath)) {
            return;
        }
        notifyDoctorAvailable(project);
    }

    private static boolean looksLikeTestRun(ExecutionEnvironment environment) {
        String profileName = environment.getRunProfile().getName();
        String profileClass = environment.getRunProfile().getClass().getName().toLowerCase(java.util.Locale.ROOT);
        String name = profileName == null ? "" : profileName.toLowerCase(java.util.Locale.ROOT);
        return profileClass.contains("test") || name.contains("test") || name.contains("suite");
    }

    private static boolean hasAllureResults(Path projectRoot) {
        Path allureResults = projectRoot.resolve("allure-results");
        if (!Files.isDirectory(allureResults)) {
            allureResults = projectRoot.resolve("target").resolve("allure-results");
        }
        if (!Files.isDirectory(allureResults)) {
            return false;
        }
        try (Stream<Path> entries = Files.list(allureResults)) {
            return entries.anyMatch(entry -> entry.getFileName().toString().endsWith("-result.json")
                    || entry.getFileName().toString().endsWith("result.json"));
        } catch (IOException unreadable) {
            return false;
        }
    }

    private static boolean throttled(String basePath) {
        long now = System.currentTimeMillis();
        Long previous = LAST_NOTIFIED_BY_PROJECT.put(basePath, now);
        return previous != null && now - previous < THROTTLE_MILLIS;
    }

    private static void notifyDoctorAvailable(Project project) {
        Notification notification = NotificationGroupManager.getInstance()
                .getNotificationGroup(GROUP_ID)
                .createNotification(
                        "Test run failed",
                        "Allure evidence found. SHAFT Doctor can triage the failure deterministically.",
                        NotificationType.WARNING);
        notification.addAction(new NotificationAction("Diagnose with SHAFT Doctor") {
            @Override
            public void actionPerformed(@NotNull AnActionEvent event, @NotNull Notification current) {
                current.expire();
                openDoctorWorkflow(project);
            }
        });
        notification.addAction(new NotificationAction("Heal failed test") {
            @Override
            public void actionPerformed(@NotNull AnActionEvent event, @NotNull Notification current) {
                current.expire();
                openHealerWorkflow(project);
            }
        });
        notification.notify(project);
    }

    private static void openDoctorWorkflow(Project project) {
        ShaftToolWorkflowLauncher.open(project, "doctor_analyze_failed_allure", doctorArguments());
    }

    private static void openHealerWorkflow(Project project) {
        ShaftToolWorkflowLauncher.open(project, "healer_run_failed_test", healerArguments());
    }

    /**
     * Builds the {@code doctor_analyze_failed_allure} MCP arguments for the last failed run. Public
     * so {@code ShaftTestsPanel} (issue #3467 "SHAFT Tests" tab) can reuse the same one-click
     * Doctor prefill this notifier offers.
     *
     * @return MCP tool arguments
     */
    public static JsonObject doctorArguments() {
        JsonObject arguments = new JsonObject();
        JsonArray allurePaths = new JsonArray();
        allurePaths.add("allure-results");
        arguments.add("allureResultPaths", allurePaths);
        arguments.add("historicalBundlePaths", new JsonArray());
        arguments.addProperty("outputDirectory", "target/shaft-doctor");
        arguments.addProperty("includeScreenshots", true);
        arguments.addProperty("includePageSnapshots", true);
        arguments.addProperty("minimumAllureResults", 1);
        arguments.addProperty("repositoryRoot", ".");
        arguments.add("allowedSourcePaths", new JsonArray());
        arguments.addProperty("useAi", false);
        arguments.addProperty("allowLocalAi", false);
        arguments.addProperty("allowRemoteAi", false);
        arguments.addProperty("driverVariableName", "driver");
        return arguments;
    }

    /**
     * Builds the {@code healer_run_failed_test} MCP arguments for the last failed run. Public for
     * the same reason as {@link #doctorArguments()}.
     *
     * @return MCP tool arguments
     */
    public static JsonObject healerArguments() {
        JsonObject arguments = new JsonObject();
        JsonArray testCommand = new JsonArray();
        testCommand.add("mvn");
        testCommand.add("-q");
        testCommand.add("-Dtest=ExampleTest");
        testCommand.add("test");
        arguments.add("testCommand", testCommand);
        arguments.addProperty("repositoryRoot", ".");
        arguments.addProperty("outputDirectory", "target/shaft-healer");
        arguments.addProperty("maxAttempts", 1);
        arguments.addProperty("includeScreenshots", true);
        arguments.addProperty("includePageSnapshots", true);
        arguments.add("allowedSourcePaths", new JsonArray());
        arguments.addProperty("networkValidationApproved", false);
        arguments.addProperty("useConfiguredAi", false);
        arguments.addProperty("allowLocalAi", false);
        arguments.addProperty("allowRemoteAi", false);
        arguments.addProperty("driverVariableName", "driver");
        return arguments;
    }
}
