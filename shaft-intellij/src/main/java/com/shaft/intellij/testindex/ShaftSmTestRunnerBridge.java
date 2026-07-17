package com.shaft.intellij.testindex;

import com.intellij.execution.testframework.sm.runner.SMTRunnerEventsListener;
import com.intellij.execution.testframework.sm.runner.SMTestProxy;
import com.intellij.openapi.project.Project;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

/**
 * Bridges IntelliJ's SM Test Runner protocol to {@link ShaftTestIndex}, so the "SHAFT Tests"
 * tool-window tab ({@code ShaftTestsPanel}) can show grey/running -> pass/fail progression while a
 * run is in flight, and per-{@code @Test}-method rows, instead of only a single class-level result
 * once the whole process terminates (issue #3688).
 * <p>
 * <b>Verified platform API shape (2024.3, IntelliJ IDEA Ultimate -- this build's pinned
 * {@code platformVersion}):</b> {@code SMTRunnerEventsListener}/{@code SMTestProxy} compiled and
 * ran with <em>zero</em> new Gradle dependency -- their classes ship inside {@code lib/app-client.jar},
 * which {@code product-info.json} lists as part of the base {@code com.intellij} plugin's own
 * classpath (already pulled in transitively by {@code intellijPlatform { intellijIdea(...) } }),
 * not as a separately-declarable {@code productModuleV2}. A live probe of
 * {@code bundledModule("intellij.platform.execution.impl")} (the module name that ships these
 * classes upstream) failed fast with "Specified bundledModule ... doesn't exist" against this
 * product's layout -- exhaustively confirmed against all 56 {@code productModuleV2} entries in
 * {@code product-info.json}, none of which cover the generic SM test-framework UI. So, for this
 * platform version, there is no separate "smRunner module" dependency to add: the classes are
 * already reachable through this module's existing {@code bundledPlugin("com.intellij.java")} +
 * {@code intellijIdea(...)} declarations.
 * <p>
 * <b>Registration:</b> declared as a {@code <projectListeners>} entry in {@code plugin.xml} against
 * topic {@code SMTRunnerEventsListener.TEST_STATUS} (a project message-bus {@code Topic}, confirmed
 * via {@code javap} on the bundled class), mirroring {@code ShaftTestIndexListener}'s existing
 * {@code ExecutionListener} registration in this same module. This is a deliberate deviation from
 * "attach directly to the run's process handler": the message-bus topic already broadcasts every SM
 * test run project-wide, so one declarative subscription (platform-managed lifecycle, no manual
 * {@code Disposable} bookkeeping) covers every SHAFT run without the fragility of reaching into a
 * specific {@code RunContentDescriptor}'s console view to find where its SM listener list lives.
 * <p>
 * <b>Node identity:</b> {@link #parseIndexKey} decodes {@link SMTestProxy#getLocationUrl()} using
 * the same {@code java:suite://qualifiedClassName} / {@code java:test://qualifiedClassName/methodName}
 * URL scheme {@code com.intellij.execution.testframework.JavaTestLocator} produces for JUnit/TestNG
 * (confirmed by disassembling that class's {@code createLocationUrl} methods in the bundled
 * {@code java-impl.jar} -- its string-concatenation templates are {@code protocol + "://" + class}
 * and {@code protocol + "://" + class + "/" + method}). A method-level key uses
 * {@link #METHOD_KEY_SEPARATOR} to match {@code ShaftTestsPanel}'s per-method row lookup. Any other
 * protocol (synthetic root suites, non-Java frameworks) parses to {@code null} and is silently
 * ignored rather than misfiled under a bogus key.
 * <p>
 * <b>Untestable in this headless Gradle test JVM:</b> the {@link SMTRunnerEventsListener} overrides
 * below need a real {@link SMTestProxy} instance to exercise end-to-end, and constructing one that
 * behaves realistically (parent/child wiring, {@code isDefect()} state machine) needs the live SM
 * protocol driving it -- not available without a full platform test fixture. All decision logic is
 * therefore factored into {@link #parseIndexKey} and {@link #finishedStatus}, pure static methods
 * taking plain strings/booleans, so the node-identity mapping and status-transition logic are fully
 * unit tested without ever constructing an {@code SMTestProxy}; only the thin one-line delegation in
 * each override is unverified by this test suite.
 */
public final class ShaftSmTestRunnerBridge implements SMTRunnerEventsListener {
    private static final String SUITE_PREFIX = "java:suite://";
    private static final String TEST_PREFIX = "java:test://";
    /** Separator between a qualified class name and method name in a per-method index key;
     * must match {@code ShaftTestsPanel}'s method-row lookup. */
    static final char METHOD_KEY_SEPARATOR = '#';

    private final Project project;

    public ShaftSmTestRunnerBridge(Project project) {
        this.project = project;
    }

    @Override
    public void onSuiteStarted(@NotNull SMTestProxy suite) {
        record(suite.getLocationUrl(), ShaftTestIndex.Status.RUNNING);
    }

    @Override
    public void onSuiteFinished(@NotNull SMTestProxy suite) {
        record(suite.getLocationUrl(), finishedStatus(suite.isDefect()));
    }

    @Override
    public void onTestStarted(@NotNull SMTestProxy test) {
        record(test.getLocationUrl(), ShaftTestIndex.Status.RUNNING);
    }

    @Override
    public void onTestFinished(@NotNull SMTestProxy test) {
        record(test.getLocationUrl(), finishedStatus(test.isDefect()));
    }

    @Override
    public void onTestFailed(@NotNull SMTestProxy test) {
        // Also reported via onTestFinished's isDefect() check once the SM protocol closes the node;
        // recording here too means a long-running failed test shows FAIL as soon as the framework
        // reports the failure, not only once the node fully finishes.
        record(test.getLocationUrl(), ShaftTestIndex.Status.FAIL);
    }

    // Ignored tests get no distinct index status (issue #3688 scope: grey/running -> pass/fail
    // progression; a third "ignored" visual state is out of scope and would need a new
    // ShaftTestIndex.Status value plus ShaftTestsPanel rendering -- left for a future iteration
    // rather than silently misreporting an ignored test as PASS or FAIL).
    @Override
    public void onTestIgnored(@NotNull SMTestProxy test) {
        // Intentional no-op; see class javadoc scope note above.
    }

    @Override
    public void onTestingStarted(SMTestProxy.@NotNull SMRootTestProxy testsRoot) {
        // The synthetic run root has no java:suite/java:test location to key a row on.
    }

    @Override
    public void onTestingFinished(SMTestProxy.@NotNull SMRootTestProxy testsRoot) {
        // No-op; per-suite/per-test events above already recorded every identifiable node.
    }

    @Override
    public void onTestsCountInSuite(int count) {
        // No-op; not needed for status tracking.
    }

    @Override
    public void onCustomProgressTestsCategory(@Nullable String categoryName, int testCount) {
        // No-op; SHAFT only tracks JUnit/TestNG pass-fail-running, not custom progress categories.
    }

    @Override
    public void onCustomProgressTestStarted() {
        // No-op; see onCustomProgressTestsCategory.
    }

    @Override
    public void onCustomProgressTestFailed() {
        // No-op; see onCustomProgressTestsCategory.
    }

    @Override
    public void onCustomProgressTestFinished() {
        // No-op; see onCustomProgressTestsCategory.
    }

    @Override
    public void onSuiteTreeNodeAdded(@NotNull SMTestProxy testProxy) {
        // No-op; this fires while IntelliJ builds a static "planned tree" preview, before the run
        // actually starts executing that node -- recording a status here would show a premature
        // RUNNING/PASS/FAIL for a test that has not run yet.
    }

    @Override
    public void onSuiteTreeStarted(@NotNull SMTestProxy suite) {
        // No-op; see onSuiteTreeNodeAdded.
    }

    private void record(@Nullable String locationUrl, ShaftTestIndex.Status status) {
        String key = parseIndexKey(locationUrl);
        if (key != null) {
            ShaftTestIndex.getInstance(project).recordStatus(key, status, System.currentTimeMillis());
        }
    }

    /**
     * Derives the finished status of a node from {@link SMTestProxy#isDefect()} -- pure so the
     * onFinished/onSuiteFinished mapping is unit-testable without a live {@link SMTestProxy}.
     *
     * @param isDefect the node's {@code isDefect()} value at the moment it finished
     * @return {@link ShaftTestIndex.Status#FAIL} when the node is a defect, otherwise
     *         {@link ShaftTestIndex.Status#PASS}
     */
    static ShaftTestIndex.Status finishedStatus(boolean isDefect) {
        return isDefect ? ShaftTestIndex.Status.FAIL : ShaftTestIndex.Status.PASS;
    }

    /**
     * Parses an SM Test Runner {@link SMTestProxy#getLocationUrl()} into the {@link ShaftTestIndex}
     * row key it corresponds to (see class javadoc for the verified URL scheme). Pure and
     * unit-testable without a live {@link SMTestProxy}.
     *
     * @param locationUrl the proxy's location URL, or {@code null}
     * @return {@code qualifiedClassName} for a suite node, {@code qualifiedClassName#methodName}
     *         for a test node, or {@code null} when the URL is null, blank, or not a recognized
     *         {@code java:suite://}/{@code java:test://} URL
     */
    static @Nullable String parseIndexKey(@Nullable String locationUrl) {
        if (locationUrl == null) {
            return null;
        }
        if (locationUrl.startsWith(SUITE_PREFIX)) {
            String qualifiedName = locationUrl.substring(SUITE_PREFIX.length());
            return qualifiedName.isBlank() ? null : qualifiedName;
        }
        if (locationUrl.startsWith(TEST_PREFIX)) {
            String rest = locationUrl.substring(TEST_PREFIX.length());
            int slash = rest.lastIndexOf('/');
            if (slash <= 0 || slash == rest.length() - 1) {
                return null;
            }
            return rest.substring(0, slash) + METHOD_KEY_SEPARATOR + rest.substring(slash + 1);
        }
        return null;
    }
}
