package com.shaft.listeners.internal;

/**
 * Cross-framework state holder for the test execution context.
 * <p>
 * Decouples {@code ReportManagerHelper} from concrete listener classes
 * ({@code CucumberFeatureListener}, {@code JunitListener}) so that the
 * reporting infrastructure in shaft-core does not carry a compile-time
 * dependency on the listeners that live in shaft-web.
 * <p>
 * Listeners in shaft-web populate this context on lifecycle events;
 * shaft-core utilities read from it.
 */
public class FrameworkContext {

    private static final ThreadLocal<String> cucumberScenarioName = new ThreadLocal<>();
    private static final ThreadLocal<Boolean> lastCucumberStepOK = new ThreadLocal<>();
    private static final ThreadLocal<Boolean> lastJunitTestOK = new ThreadLocal<>();
    private static final ThreadLocal<String> junitTestName = new ThreadLocal<>();

    private FrameworkContext() {
    }

    // ── Cucumber ──────────────────────────────────────────────────────────────

    /** Called by {@code CucumberFeatureListener} when a scenario starts. */
    public static void setCucumberScenarioName(String name) {
        cucumberScenarioName.set(name);
    }

    /** Returns the name of the last started Cucumber scenario, or {@code null}. */
    public static String getCucumberScenarioName() {
        return cucumberScenarioName.get();
    }

    /**
     * Called by {@code CucumberFeatureListener} after each step completes.
     *
     * @param ok {@code true} if the step passed, {@code false} if it failed/skipped.
     */
    public static void setLastCucumberStepOK(Boolean ok) {
        lastCucumberStepOK.set(ok);
    }

    /** Returns the pass/fail result of the last finished Cucumber step, or {@code null}. */
    public static Boolean getLastCucumberStepOK() {
        return lastCucumberStepOK.get();
    }

    // ── JUnit ─────────────────────────────────────────────────────────────────

    /** Called by {@code JunitListener} when a test starts. */
    public static void setJunitTestName(String name) {
        junitTestName.set(name);
    }

    /** Returns the name of the currently running JUnit test on this thread, or {@code null}. */
    public static String getJunitTestName() {
        return junitTestName.get();
    }

    /**
     * Called by {@code JunitListener} after a test finishes.
     *
     * @param ok {@code true} if the test passed, {@code false} if it failed.
     */
    public static void setLastJunitTestOK(Boolean ok) {
        lastJunitTestOK.set(ok);
    }

    /** Returns the pass/fail result of the last finished JUnit test, or {@code null}. */
    public static Boolean getLastJunitTestOK() {
        return lastJunitTestOK.get();
    }

    // ── Cleanup ───────────────────────────────────────────────────────────────

    /** Clears all thread-local state for the current thread. Call at the end of each test. */
    public static void clear() {
        cucumberScenarioName.remove();
        lastCucumberStepOK.remove();
        lastJunitTestOK.remove();
        junitTestName.remove();
    }
}
