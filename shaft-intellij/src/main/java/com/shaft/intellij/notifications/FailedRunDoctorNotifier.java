package com.shaft.intellij.notifications;

import com.google.gson.JsonArray;
import com.google.gson.JsonObject;
import com.intellij.execution.ExecutionListener;
import com.intellij.execution.process.ProcessEvent;
import com.intellij.execution.process.ProcessHandler;
import com.intellij.execution.process.ProcessListener;
import com.intellij.execution.runners.ExecutionEnvironment;
import com.intellij.notification.Notification;
import com.intellij.notification.NotificationAction;
import com.intellij.notification.NotificationGroupManager;
import com.intellij.notification.NotificationType;
import com.intellij.openapi.actionSystem.AnActionEvent;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.Key;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Instant;
import java.util.concurrent.atomic.AtomicLong;
import java.util.stream.Stream;

/**
 * Offers automatic and one-click SHAFT Doctor triage when a test run fails and evidence exists.
 *
 * <p>On a failed run with Allure (or, failing that, SHAFT trace) evidence, this listener both
 * (a) automatically runs the read-only {@code doctor_analyze_failed_allure}/{@code
 * doctor_analyze_trace} diagnosis and renders a human-readable root-cause card into the Assistant
 * transcript within one round trip, and (b) raises a notification offering to re-run Doctor or run
 * Healer, so the user has a persistent way back to the same diagnosis even if they were not
 * watching the Assistant tab when it appeared (issue #3547).</p>
 */
public final class FailedRunDoctorNotifier implements ExecutionListener {
    private static final String GROUP_ID = "SHAFT Notifications";
    private static final long THROTTLE_MILLIS = 60_000;

    /**
     * Records, per {@link ProcessHandler} instance, whether that specific run was forcibly
     * destroyed (the user pressed Stop, or the IDE force-killed it) rather than exiting on its
     * own. {@code ProcessHandler} already extends {@code UserDataHolderBase}, so storing the flag
     * here scopes it to exactly one run and lets it die with the handler itself -- no static
     * collection to leak or to key by anything (issue #3591).
     */
    private static final Key<Boolean> USER_CANCELLED = Key.create("SHAFT_RUN_USER_CANCELLED");

    /**
     * Throttle timestamp for this listener's project. {@code <projectListeners>} registration in
     * plugin.xml gives every project its own {@code FailedRunDoctorNotifier} instance for the life
     * of that project, so a plain instance field already scopes the throttle correctly -- the
     * previous {@code static Map<basePath, Long>} could only ever grow, never shrink, across every
     * project ever opened in the IDE session (issue #3591).
     */
    private final AtomicLong lastNotifiedAt = new AtomicLong();

    @Override
    public void processStarted(@NotNull String executorId,
                               @NotNull ExecutionEnvironment environment,
                               @NotNull ProcessHandler handler) {
        if (!looksLikeTestRun(environment)) {
            return;
        }
        // Attached per-run on the real ProcessHandler so processTerminated can tell a user Stop
        // apart from the process simply exiting with a failing test's non-zero code. There is no
        // single "was this cancelled" flag on ExecutionListener itself; processWillTerminate's
        // willBeDestroyed parameter is true only on the destroyProcess() path (Stop / force-kill)
        // and false on detachProcess(), and is never invoked at all for a process that just exits
        // on its own -- verified against the platform's ProcessHandler bytecode (issue #3591).
        handler.addProcessListener(new ProcessListener() {
            @Override
            public void processWillTerminate(@NotNull ProcessEvent event, boolean willBeDestroyed) {
                if (willBeDestroyed) {
                    handler.putUserData(USER_CANCELLED, Boolean.TRUE);
                }
            }
        });
    }

    @Override
    public void processTerminated(@NotNull String executorId,
                                  @NotNull ExecutionEnvironment environment,
                                  @NotNull ProcessHandler handler,
                                  int exitCode) {
        Project project = environment.getProject();
        if (exitCode == 0 || project.isDisposed()) {
            return;
        }
        if (isUserCancelledRun(handler.getUserData(USER_CANCELLED))) {
            // The user pressed Stop (or the IDE force-killed the run): a non-zero exit here is
            // expected and intentional, not a failure to triage. Running Doctor anyway would burn
            // MCP time on every deliberate cancel.
            return;
        }
        if (!looksLikeTestRun(environment)) {
            return;
        }
        String basePath = project.getBasePath();
        if (basePath == null) {
            return;
        }
        Path projectRoot = Path.of(basePath);
        String allureResultsPath = resolveAllureResultsPath(projectRoot);
        String tracePath = allureResultsPath == null ? resolveNewestTraceDirectory(projectRoot) : null;
        if (allureResultsPath == null && tracePath == null) {
            // No evidence at all yet (for example a timeout-killed replay that never flushed
            // Allure results): nothing to diagnose, and a notification with a Diagnose button that
            // would immediately fail is worse than no notification.
            return;
        }
        if (throttled()) {
            return;
        }
        String testIdentity = environment.getRunProfile().getName();
        notifyDoctorAvailable(project, testIdentity, allureResultsPath, tracePath);
        openDoctorWorkflow(project, allureResultsPath, tracePath);
    }

    /**
     * Pure decision extracted from {@link #processTerminated} so it is unit-testable without any
     * platform types (mirrors {@code ShaftTestMethodAnnotations}'s PSI-free predicate style): a
     * run counts as user-cancelled only when {@code processWillTerminate} actually recorded
     * {@code willBeDestroyed == true} on that process. A {@code null} value -- the common case,
     * since most terminations never call {@code destroyProcess()} at all -- must NOT be treated as
     * cancelled, or every genuinely failed run would silently stop notifying.
     *
     * @param recordedWillBeDestroyed the value stored by {@link #processStarted}'s listener, or
     *                                {@code null} when the process was never force-destroyed
     * @return {@code true} only when the run was an explicit user Stop / forced kill
     */
    static boolean isUserCancelledRun(@Nullable Boolean recordedWillBeDestroyed) {
        return Boolean.TRUE.equals(recordedWillBeDestroyed);
    }

    private static boolean looksLikeTestRun(ExecutionEnvironment environment) {
        String profileName = environment.getRunProfile().getName();
        String profileClass = environment.getRunProfile().getClass().getName().toLowerCase(java.util.Locale.ROOT);
        String name = profileName == null ? "" : profileName.toLowerCase(java.util.Locale.ROOT);
        return profileClass.contains("test") || name.contains("test") || name.contains("suite");
    }

    /**
     * Resolves the Allure results directory actually present on disk, relative to the project root
     * ({@code allure-results} or the more common Maven default {@code target/allure-results}), or
     * {@code null} when neither has any result files yet. Threading the real resolved path into
     * {@link #doctorArguments(String)} (rather than always hardcoding {@code allure-results}) fixes
     * a real bug: the previous hardcoded value silently failed on any project using the Maven
     * default layout, because the MCP workspace policy rejects a path that does not exist on disk
     * rather than falling through to auto-discovery.
     *
     * @param projectRoot project base directory
     * @return project-relative path to the Allure results directory, or {@code null}
     */
    @Nullable
    static String resolveAllureResultsPath(Path projectRoot) {
        Path candidate = projectRoot.resolve("allure-results");
        if (!hasResultFiles(candidate)) {
            candidate = projectRoot.resolve("target").resolve("allure-results");
        }
        return hasResultFiles(candidate) ? relative(projectRoot, candidate) : null;
    }

    private static boolean hasResultFiles(Path allureResults) {
        if (!Files.isDirectory(allureResults)) {
            return false;
        }
        try (Stream<Path> entries = Files.list(allureResults)) {
            return entries.anyMatch(entry -> entry.getFileName().toString().endsWith("result.json"));
        } catch (IOException unreadable) {
            return false;
        }
    }

    /**
     * Resolves the most recently generated SHAFT trace directory under {@code
     * target/shaft-traces} (an {@code index.json} up to two levels deep), used as the
     * {@code doctor_analyze_trace} fallback when no Allure evidence exists at all -- for example a
     * replay killed by timeout before Allure results were flushed, which may still have left a
     * trace. Best-effort: any unreadable entry is skipped rather than failing the whole scan.
     *
     * @param projectRoot project base directory
     * @return project-relative path to the newest trace directory, or {@code null} when none exists
     */
    @Nullable
    static String resolveNewestTraceDirectory(Path projectRoot) {
        Path traceRoot = projectRoot.resolve("target").resolve("shaft-traces");
        if (!Files.isDirectory(traceRoot)) {
            return null;
        }
        Path newestDirectory = null;
        Instant newestModified = null;
        try (Stream<Path> entries = Files.walk(traceRoot, 2)) {
            for (Path indexPath : entries.filter(path -> "index.json".equals(path.getFileName().toString())).toList()) {
                Instant modified;
                try {
                    modified = Files.getLastModifiedTime(indexPath).toInstant();
                } catch (IOException unreadable) {
                    continue;
                }
                if (newestModified == null || modified.isAfter(newestModified)) {
                    newestModified = modified;
                    newestDirectory = indexPath.getParent();
                }
            }
        } catch (IOException unreadable) {
            return null;
        }
        return newestDirectory == null ? null : relative(projectRoot, newestDirectory);
    }

    private static String relative(Path projectRoot, Path candidate) {
        return projectRoot.relativize(candidate).toString().replace('\\', '/');
    }

    private boolean throttled() {
        long now = System.currentTimeMillis();
        long previous = lastNotifiedAt.getAndSet(now);
        return previous != 0 && now - previous < THROTTLE_MILLIS;
    }

    private static void notifyDoctorAvailable(
            Project project, String testIdentity, @Nullable String allureResultsPath, @Nullable String tracePath) {
        Notification notification = NotificationGroupManager.getInstance()
                .getNotificationGroup(GROUP_ID)
                .createNotification(
                        "Test run failed",
                        "SHAFT Doctor is diagnosing the failure now -- open the SHAFT Assistant tab for the "
                                + "root cause, or re-run it below.",
                        NotificationType.WARNING);
        notification.addAction(new NotificationAction("Diagnose with SHAFT Doctor") {
            @Override
            public void actionPerformed(@NotNull AnActionEvent event, @NotNull Notification current) {
                current.expire();
                openDoctorWorkflow(project, allureResultsPath, tracePath);
            }
        });
        notification.addAction(new NotificationAction("Heal failed test") {
            @Override
            public void actionPerformed(@NotNull AnActionEvent event, @NotNull Notification current) {
                current.expire();
                openHealerWorkflow(project, testIdentity);
            }
        });
        notification.notify(project);
    }

    /**
     * Runs the Doctor analysis (Allure evidence when available, otherwise the newest trace) and
     * renders it into the Assistant transcript -- used both automatically on failure and by the
     * notification's "Diagnose" button, so both paths always produce an actual diagnosis in default
     * mode rather than only prefilling the composer (issue #3547).
     */
    private static void openDoctorWorkflow(
            Project project, @Nullable String allureResultsPath, @Nullable String tracePath) {
        if (allureResultsPath != null) {
            ShaftToolWorkflowLauncher.runAndRender(
                    project, "doctor_analyze_failed_allure", doctorArguments(allureResultsPath));
        } else if (tracePath != null) {
            ShaftToolWorkflowLauncher.runAndRender(project, "doctor_analyze_trace", traceArguments(tracePath));
        }
    }

    /**
     * Runs the Healer analysis for {@code testIdentity} and renders it into the Assistant transcript
     * -- used by the notification's "Heal" button, which must actually run in default mode rather
     * than only prefilling the composer (issue #3547).
     */
    private static void openHealerWorkflow(Project project, String testIdentity) {
        ShaftToolWorkflowLauncher.runAndRender(project, "healer_run_failed_test", healerArguments(testIdentity));
    }

    /**
     * Builds the {@code doctor_analyze_failed_allure} MCP arguments. Public so {@code
     * ShaftTestsPanel} (issue #3467 "SHAFT Tests" tab) can reuse the same one-click Doctor prefill
     * this notifier offers.
     *
     * @param allureResultsPath project-relative Allure results directory to analyze, or
     *                          {@code null}/blank to let the server auto-discover the newest
     *                          evidence in the workspace
     * @return MCP tool arguments
     */
    public static JsonObject doctorArguments(@Nullable String allureResultsPath) {
        JsonObject arguments = new JsonObject();
        JsonArray allurePaths = new JsonArray();
        if (allureResultsPath != null && !allureResultsPath.isBlank()) {
            allurePaths.add(allureResultsPath);
        }
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
     * Builds the {@code doctor_analyze_trace} MCP arguments for the trace-only fallback (no Allure
     * evidence found).
     *
     * @param tracePath project-relative SHAFT trace directory
     * @return MCP tool arguments
     */
    private static JsonObject traceArguments(String tracePath) {
        JsonObject arguments = new JsonObject();
        arguments.addProperty("tracePath", tracePath);
        arguments.addProperty("backend", "webdriver");
        return arguments;
    }

    /**
     * Builds the {@code healer_run_failed_test} MCP arguments for {@code testIdentity} (the real
     * failing run-configuration name -- see {@link #resolveTestSelector}), replacing the previous
     * hardcoded {@code -Dtest=ExampleTest} placeholder (issue #3547). Public for the same reason as
     * {@link #doctorArguments(String)}.
     *
     * @param testIdentity the failing run's identity, typically {@code
     *                     ExecutionEnvironment.getRunProfile().getName()} or a {@code
     *                     ShaftTestIndex} row's {@code testId}
     * @return MCP tool arguments
     */
    public static JsonObject healerArguments(@Nullable String testIdentity) {
        JsonObject arguments = new JsonObject();
        JsonArray testCommand = new JsonArray();
        testCommand.add("mvn");
        testCommand.add("-q");
        testCommand.add("-Dtest=" + resolveTestSelector(testIdentity));
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

    /**
     * Best-effort Maven {@code -Dtest} selector for {@code runConfigurationName}. Falls back to a
     * generic {@code *Test} wildcard only when the identity is entirely blank/unknown -- never
     * crashes, and never silently reintroduces a nonexistent hardcoded class name.
     *
     * @param runConfigurationName the run profile display name, or {@code null}
     * @return a Maven {@code -Dtest} selector value (without the {@code -Dtest=} prefix)
     */
    static String resolveTestSelector(@Nullable String runConfigurationName) {
        String normalized = normalizeTestIdentity(runConfigurationName);
        return normalized.isBlank() ? "*Test" : normalized;
    }

    /**
     * Best-effort normalization from an IntelliJ run-configuration display name to a Maven Surefire
     * test selector. Handles the common IDE shapes:
     * <ul>
     *     <li>{@code "CheckoutTest"} -&gt; {@code "CheckoutTest"} (unchanged)</li>
     *     <li>{@code "CheckoutTest.testLogin"} -&gt; {@code "CheckoutTest#testLogin"} (Maven's
     *     class-and-method syntax)</li>
     *     <li>{@code "CheckoutTest (1)"} -&gt; {@code "CheckoutTest"} (IDE run-counter suffix
     *     stripped)</li>
     * </ul>
     * Anything that does not match a known shape (a fully custom run-configuration name, a suite
     * name with spaces, etc.) is returned unchanged as the best available identity -- the profile
     * display name may not equal the Maven class name, and this is a best effort, not a guarantee.
     *
     * @param runConfigurationName the run profile display name, or {@code null}
     * @return normalized selector, or {@code ""} when {@code runConfigurationName} is blank
     */
    static String normalizeTestIdentity(@Nullable String runConfigurationName) {
        if (runConfigurationName == null) {
            return "";
        }
        String trimmed = runConfigurationName.trim();
        if (trimmed.isEmpty()) {
            return "";
        }
        String withoutRunCounter = trimmed.replaceAll("\\s*\\([^()]*\\)\\s*$", "").trim();
        String candidate = withoutRunCounter.isEmpty() ? trimmed : withoutRunCounter;
        int dot = candidate.indexOf('.');
        if (dot > 0 && dot == candidate.lastIndexOf('.') && dot + 1 < candidate.length()) {
            String classPart = candidate.substring(0, dot);
            String methodPart = candidate.substring(dot + 1);
            if (isJavaIdentifier(classPart) && isJavaIdentifier(methodPart)) {
                return classPart + "#" + methodPart;
            }
        }
        return candidate;
    }

    private static boolean isJavaIdentifier(String value) {
        return !value.isBlank()
                && Character.isJavaIdentifierStart(value.charAt(0))
                && value.chars().allMatch(Character::isJavaIdentifierPart);
    }
}
