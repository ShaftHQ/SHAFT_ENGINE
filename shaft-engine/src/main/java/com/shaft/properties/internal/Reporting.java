package com.shaft.properties.internal;

import org.aeonbits.owner.Config.Sources;
import org.aeonbits.owner.ConfigFactory;

/**
 * Configuration properties interface for reporting behavior in the SHAFT framework.
 * Controls report generation options, verbosity, and output format settings.
 *
 * <p>Use {@link #set()} to override values programmatically:
 * <pre>{@code
 * SHAFT.Properties.reporting.set().cleanAllureResultsDirectoryBeforeExecution(true);
 * }</pre>
 */
@SuppressWarnings("unused")
@Sources({"system:properties", "file:src/main/resources/properties/Reporting.properties", "file:src/main/resources/properties/default/Reporting.properties", "classpath:Reporting.properties"})
public interface Reporting extends EngineProperties<Reporting> {
    private static void setProperty(String key, String value) {
        ThreadLocalPropertiesManager.setProperty(key, value);
        Properties.reportingOverride.set(ConfigFactory.create(Reporting.class, ThreadLocalPropertiesManager.getOverrides()));
        if (!key.equals("disableLogging"))
            EngineProperties.logPropertyUpdate(key, value);
    }

    /**
     * Capture element name in reports for better readability.
     *
     * <p>Default: {@code true}. Possible values: true, false.
     *
     * @return the configured value of {@code captureElementName}
     */
    @Key("captureElementName")
    @DefaultValue("true")
    boolean captureElementName();

    /**
     * Capture WebDriver logs for debugging purposes.
     *
     * <p>Default: {@code false}. Possible values: true, false.
     *
     * @return the configured value of {@code captureWebDriverLogs}
     */
    @Key("captureWebDriverLogs")
    @DefaultValue("false")
    boolean captureWebDriverLogs();

    /**
     * Always log discreetly without detailed output.
     *
     * <p>Default: {@code false}. Possible values: true, false.
     *
     * @return the configured value of {@code alwaysLogDiscreetly}
     */
    @Key("alwaysLogDiscreetly")
    @DefaultValue("false")
    boolean alwaysLogDiscreetly();

    /**
     * Enable debug mode for verbose logging.
     *
     * <p>Default: {@code false}. Possible values: true, false.
     *
     * @return the configured value of {@code debugMode}
     */
    @Key("debugMode")
    @DefaultValue("false")
    boolean debugMode();

    /**
     * Open Lighthouse performance report during execution.
     *
     * <p>Default: {@code false}. Possible values: true, false.
     *
     * @return the configured value of {@code openLighthouseReportWhileExecution}
     */
    @Key("openLighthouseReportWhileExecution")
    @DefaultValue("false")
    boolean openLighthouseReportWhileExecution();

    /**
     * Clean summary reports directory before execution.
     *
     * <p>Default: {@code true}. Possible values: true, false.
     *
     * @return the configured value of {@code cleanSummaryReportsDirectoryBeforeExecution}
     */
    @Key("cleanSummaryReportsDirectoryBeforeExecution")
    @DefaultValue("true")
    boolean cleanSummaryReportsDirectoryBeforeExecution();

    /**
     * Open execution summary report after test execution.
     *
     * <p>Default: {@code false}. Possible values: true, false.
     *
     * @return the configured value of {@code openExecutionSummaryReportAfterExecution}
     */
    @Key("openExecutionSummaryReportAfterExecution")
    @DefaultValue("false")
    boolean openExecutionSummaryReportAfterExecution();

    /**
     * Disable all logging output.
     *
     * <p>Default: {@code false}. Possible values: true, false.
     *
     * @return the configured value of {@code disableLogging}
     */
    @Key("disableLogging")
    @DefaultValue("false")
    boolean disableLogging();

    /**
     * Attach a streamed, deduplicated snapshot of the full engine log to the Allure report after test
     * execution without deleting the live log file.
     *
     * <p>Default: {@code false}. Possible values: true, false.
     *
     * @return the configured value of {@code attachFullLog}
     */
    @Key("attachFullLog")
    @DefaultValue("false")
    boolean attachFullLog();

    /**
     * Attach one redacted "Failure evidence" JSON (at most 256 KB) when an element action fails: locator,
     * match counts per frame, nearest candidates, URL, title, and screenshot reference.
     *
     * <p>Default: {@code true}. Possible values: true, false.
     *
     * @return the configured value of {@code attachFailureEvidence}
     */
    @Key("attachFailureEvidence")
    @DefaultValue("true")
    boolean attachFailureEvidence();

    /**
     * Select the evidence profile. Profile values except CUSTOM override granular screenshot, page-
     * source, GIF, video, WebDriver-log, full-log, diagnostics, and trace controls.
     *
     * <p>Default: {@code FAILURE_ONLY}. Possible values: FAILURE_ONLY, BALANCED, FAST, FULL, CUSTOM.
     *
     * @return the configured value of {@code evidenceLevel}
     */
    @Key("evidenceLevel")
    @DefaultValue("FAILURE_ONLY")
    String evidenceLevel();

    /**
     * Generate end-of-run HTML/JSON locator health reports and attach them to Allure.
     *
     * <p>Default: {@code false}. Possible values: true, false.
     *
     * @return the configured value of {@code locatorHealthReportEnabled}
     */
    @Key("locatorHealthReportEnabled")
    @DefaultValue("false")
    boolean locatorHealthReportEnabled();

    /**
     * Mark locator lookups at or above this duration as slow warnings.
     *
     * <p>Default: {@code 750}. Possible values: Integer milliseconds.
     *
     * @return the configured value of {@code slowLocatorThresholdMillis}
     */
    @Key("slowLocatorThresholdMillis")
    @DefaultValue("750")
    int slowLocatorThresholdMillis();

    /**
     * Fail the run when locator health warnings are present.
     *
     * <p>Default: {@code false}. Possible values: true, false.
     *
     * @return the configured value of {@code failOnLocatorHealthWarnings}
     */
    @Key("failOnLocatorHealthWarnings")
    @DefaultValue("false")
    boolean failOnLocatorHealthWarnings();

    /**
     * Generate end-of-run locator health reports.
     *
     * <p>Default: {@code false}. Possible values: true, false.
     *
     * @return the configured value of {@code shaft.locatorHealth.enabled}
     */
    @Key("shaft.locatorHealth.enabled")
    @DefaultValue("false")
    boolean locatorHealthEnabled();

    /**
     * Mark locators below this health score as risky in the dashboard.
     *
     * <p>Default: {@code 70}. Possible values: 0 to 100.
     *
     * @return the configured value of {@code shaft.locatorHealth.warnBelowScore}
     */
    @Key("shaft.locatorHealth.warnBelowScore")
    @DefaultValue("70")
    int locatorHealthWarnBelowScore();

    /**
     * Attach the HTML locator health dashboard to Allure.
     *
     * <p>Default: {@code true}. Possible values: true, false.
     *
     * @return the configured value of {@code shaft.locatorHealth.attachDashboard}
     */
    @Key("shaft.locatorHealth.attachDashboard")
    @DefaultValue("true")
    boolean locatorHealthAttachDashboard();

    /**
     * Fail the run when any locator score is below this threshold; -1 disables score-based failure.
     *
     * <p>Default: {@code -1}. Possible values: -1 or 0 to 100.
     *
     * @return the configured value of {@code shaft.locatorHealth.failBelowScore}
     */
    @Key("shaft.locatorHealth.failBelowScore")
    @DefaultValue("-1")
    int locatorHealthFailBelowScore();

    /**
     * Attach shaft-diagnostics.zip with sanitized diagnostics.json for failed and broken tests.
     *
     * <p>Default: {@code true}. Possible values: true, false.
     *
     * @return the configured value of {@code shaft.diagnostics.enabled}
     */
    @Key("shaft.diagnostics.enabled")
    @DefaultValue("true")
    boolean diagnosticsBundleEnabled();

    /**
     * Maximum size for a single diagnostics ZIP entry.
     *
     * <p>Default: {@code 50}. Possible values: Integer megabytes.
     *
     * @return the configured value of {@code shaft.diagnostics.maxArtifactMb}
     */
    @Key("shaft.diagnostics.maxArtifactMb")
    @DefaultValue("50")
    int diagnosticsMaxArtifactMb();

    /**
     * Attach the SHAFT failure trace viewer archive to Allure.
     *
     * <p>Default: {@code true}. Possible values: true, false.
     *
     * @return the configured value of {@code shaft.trace.enabled}
     */
    @Key("shaft.trace.enabled")
    @DefaultValue("true")
    boolean traceEnabled();

    /**
     * Control trace generation: auto is retry-aware (resolves to retry when
     * retryMaximumNumberOfAttempts &gt; 0, otherwise failure).
     *
     * <p>Default: {@code auto}. Possible values: auto, failure, retry, always.
     *
     * @return the configured value of {@code shaft.trace.mode}
     */
    @Key("shaft.trace.mode")
    @DefaultValue("auto")
    String traceMode();

    /**
     * Retain failed-attempt trace archives under attempt-indexed names when retries are enabled.
     *
     * <p>Default: {@code true}. Possible values: true, false.
     *
     * @return the configured value of {@code shaft.trace.retainFailedAttempts}
     */
    @Key("shaft.trace.retainFailedAttempts")
    @DefaultValue("true")
    boolean traceRetainFailedAttempts();

    /**
     * Include the best matching project source-code frame and snippet.
     *
     * <p>Default: {@code true}. Possible values: true, false.
     *
     * @return the configured value of {@code shaft.trace.includeCodeContext}
     */
    @Key("shaft.trace.includeCodeContext")
    @DefaultValue("true")
    boolean traceIncludeCodeContext();

    /**
     * Include web page snapshots when available.
     *
     * <p>Default: {@code true}. Possible values: true, false.
     *
     * @return the configured value of {@code shaft.trace.includeFullPageSnapshots}
     */
    @Key("shaft.trace.includeFullPageSnapshots")
    @DefaultValue("true")
    boolean traceIncludeFullPageSnapshots();

    /**
     * Include DOM snapshots in the SHAFT failure trace bundle when available.
     *
     * <p>Default: {@code true}. Possible values: true, false.
     *
     * @return the configured value of {@code shaft.trace.includeDomSnapshots}
     */
    @Key("shaft.trace.includeDomSnapshots")
    @DefaultValue("true")
    boolean traceIncludeDomSnapshots();

    /**
     * Include screenshots in the SHAFT failure trace bundle when available.
     *
     * <p>Default: {@code true}. Possible values: true, false.
     *
     * @return the configured value of {@code shaft.trace.includeScreenshots}
     */
    @Key("shaft.trace.includeScreenshots")
    @DefaultValue("true")
    boolean traceIncludeScreenshots();

    /**
     * Include native mobile page source when available.
     *
     * <p>Default: {@code true}. Possible values: true, false.
     *
     * @return the configured value of {@code shaft.trace.includeNativePageSource}
     */
    @Key("shaft.trace.includeNativePageSource")
    @DefaultValue("true")
    boolean traceIncludeNativePageSource();

    /**
     * Reserve network evidence in the trace metadata.
     *
     * <p>Default: {@code true}. Possible values: true, false.
     *
     * @return the configured value of {@code shaft.trace.includeNetwork}
     */
    @Key("shaft.trace.includeNetwork")
    @DefaultValue("true")
    boolean traceIncludeNetwork();

    /**
     * Reserve console evidence in the trace metadata.
     *
     * <p>Default: {@code true}. Possible values: true, false.
     *
     * @return the configured value of {@code shaft.trace.includeConsole}
     */
    @Key("shaft.trace.includeConsole")
    @DefaultValue("true")
    boolean traceIncludeConsole();

    /**
     * Maximum size for a single trace bundle entry.
     *
     * <p>Default: {@code 50}. Possible values: Integer megabytes.
     *
     * @return the configured value of {@code shaft.trace.maxArtifactMb}
     */
    @Key("shaft.trace.maxArtifactMb")
    @DefaultValue("50")
    int traceMaxArtifactMb();

    /**
     * Maximum total size of the trace bundle for one test session.
     *
     * <p>Default: {@code 512}. Possible values: Integer megabytes.
     *
     * @return the configured value of {@code shaft.trace.maxSessionMb}
     */
    @Key("shaft.trace.maxSessionMb")
    @DefaultValue("512")
    int traceMaxSessionMb();

    /**
     * Attach opt-in flake and auto-wait timing profiles to Allure.
     *
     * <p>Default: {@code false}. Possible values: true, false.
     *
     * @return the configured value of {@code shaft.flakeProfiler.enabled}
     */
    @Key("shaft.flakeProfiler.enabled")
    @DefaultValue("false")
    boolean flakeProfilerEnabled();

    /**
     * Attach each test's JSON/HTML profile when profiler signals exist.
     *
     * <p>Default: {@code true}. Possible values: true, false.
     *
     * @return the configured value of {@code shaft.flakeProfiler.attachPerTest}
     */
    @Key("shaft.flakeProfiler.attachPerTest")
    @DefaultValue("true")
    boolean flakeProfilerAttachPerTest();

    /**
     * Fail the run when severe flake-risk actions are found.
     *
     * <p>Default: {@code false}. Possible values: true, false.
     *
     * @return the configured value of {@code shaft.flakeProfiler.failOnSevereFlakeRisk}
     */
    @Key("shaft.flakeProfiler.failOnSevereFlakeRisk")
    @DefaultValue("false")
    boolean flakeProfilerFailOnSevereFlakeRisk();

    /**
     * Duration threshold used to flag slow and severe-risk actions.
     *
     * <p>Default: {@code 2000}. Possible values: Integer milliseconds.
     *
     * @return the configured value of {@code shaft.flakeProfiler.slowActionThresholdMs}
     */
    @Key("shaft.flakeProfiler.slowActionThresholdMs")
    @DefaultValue("2000")
    int flakeProfilerSlowActionThresholdMs();

    /**
     * Starts a fluent, thread-local override of these properties for the current test thread.
     *
     * @return a new {@link SetProperty} builder
     */
    default SetProperty set() {
        return new SetProperty();
    }

    class SetProperty implements EngineProperties.SetProperty {

        /**
         * Overrides the {@code captureElementName} property at runtime. Capture element name in reports
         * for better readability.
         *
         * @param value the new value of {@code captureElementName}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty captureElementName(boolean value) {
            setProperty("captureElementName", String.valueOf(value));
            return this;
        }

        /**
         * Overrides the {@code forceCheckForElementVisibility} property at runtime. Legacy configuration
         * property with no current runtime consumer.
         *
         * @param value the new value of {@code forceCheckForElementVisibility}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty forceCheckForElementVisibility(boolean value) {
            setProperty("forceCheckForElementVisibility", String.valueOf(value));
            return this;
        }

        /**
         * Overrides the {@code captureWebDriverLogs} property at runtime. Capture WebDriver logs for
         * debugging purposes.
         *
         * @param value the new value of {@code captureWebDriverLogs}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty captureWebDriverLogs(boolean value) {
            setProperty("captureWebDriverLogs", String.valueOf(value));
            return this;
        }

        /**
         * Overrides the {@code alwaysLogDiscreetly} property at runtime. Always log discreetly without
         * detailed output.
         *
         * @param value the new value of {@code alwaysLogDiscreetly}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty alwaysLogDiscreetly(boolean value) {
            setProperty("alwaysLogDiscreetly", String.valueOf(value));
            return this;
        }

        /**
         * Overrides the {@code debugMode} property at runtime. Enable debug mode for verbose logging.
         *
         * @param value the new value of {@code debugMode}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty debugMode(boolean value) {
            setProperty("debugMode", String.valueOf(value));
            return this;
        }

        /**
         * Overrides the {@code openLighthouseReportWhileExecution} property at runtime. Open Lighthouse
         * performance report during execution.
         *
         * @param value the new value of {@code openLighthouseReportWhileExecution}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty openLighthouseReportWhileExecution(boolean value) {
            setProperty("openLighthouseReportWhileExecution", String.valueOf(value));
            return this;
        }

        /**
         * Overrides the {@code openExecutionSummaryReportAfterExecution} property at runtime. Open
         * execution summary report after test execution.
         *
         * @param value the new value of {@code openExecutionSummaryReportAfterExecution}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty openExecutionSummaryReportAfterExecution(boolean value) {
            setProperty("openExecutionSummaryReportAfterExecution", String.valueOf(value));
            return this;
        }

        /**
         * Overrides the {@code disableLogging} property at runtime. Disable all logging output.
         *
         * @param value the new value of {@code disableLogging}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty disableLogging(boolean value) {
            setProperty("disableLogging", String.valueOf(value));
            return this;
        }

        /**
         * Overrides the {@code attachFullLog} property at runtime. Attach a streamed, deduplicated
         * snapshot of the full engine log to the Allure report after test execution without deleting the
         * live log file.
         *
         * @param value the new value of {@code attachFullLog}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty attachFullLog(boolean value) {
            setProperty("attachFullLog", String.valueOf(value));
            return this;
        }

        /**
         * Overrides the {@code attachFailureEvidence} property at runtime. Attach one redacted failure
         * evidence JSON when an element action fails.
         *
         * @param value the new value of {@code attachFailureEvidence}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty attachFailureEvidence(boolean value) {
            setProperty("attachFailureEvidence", String.valueOf(value));
            return this;
        }

        /**
         * Overrides the {@code evidenceLevel} property at runtime. Select the evidence profile.
         *
         * @param value the new value of {@code evidenceLevel}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty evidenceLevel(String value) {
            setProperty("evidenceLevel", value);
            PropertiesHelper.overridePropertiesForEvidenceLevel();
            return this;
        }

        /**
         * Overrides the {@code locatorHealthReportEnabled} property at runtime. Generate end-of-run
         * HTML/JSON locator health reports and attach them to Allure.
         *
         * @param value the new value of {@code locatorHealthReportEnabled}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty locatorHealthReportEnabled(boolean value) {
            setProperty("locatorHealthReportEnabled", String.valueOf(value));
            return this;
        }

        /**
         * Overrides the {@code slowLocatorThresholdMillis} property at runtime. Mark locator lookups at or
         * above this duration as slow warnings.
         *
         * @param value the new value of {@code slowLocatorThresholdMillis}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty slowLocatorThresholdMillis(int value) {
            setProperty("slowLocatorThresholdMillis", String.valueOf(value));
            return this;
        }

        /**
         * Overrides the {@code failOnLocatorHealthWarnings} property at runtime. Fail the run when locator
         * health warnings are present.
         *
         * @param value the new value of {@code failOnLocatorHealthWarnings}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty failOnLocatorHealthWarnings(boolean value) {
            setProperty("failOnLocatorHealthWarnings", String.valueOf(value));
            return this;
        }

        /**
         * Overrides the {@code shaft.locatorHealth.enabled} property at runtime. Generate end-of-run
         * locator health reports.
         *
         * @param value the new value of {@code shaft.locatorHealth.enabled}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty locatorHealthEnabled(boolean value) {
            setProperty("shaft.locatorHealth.enabled", String.valueOf(value));
            return this;
        }

        /**
         * Overrides the {@code shaft.locatorHealth.warnBelowScore} property at runtime. Mark locators
         * below this health score as risky in the dashboard.
         *
         * @param value the new value of {@code shaft.locatorHealth.warnBelowScore}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty locatorHealthWarnBelowScore(int value) {
            setProperty("shaft.locatorHealth.warnBelowScore", String.valueOf(value));
            return this;
        }

        /**
         * Overrides the {@code shaft.locatorHealth.attachDashboard} property at runtime. Attach the HTML
         * locator health dashboard to Allure.
         *
         * @param value the new value of {@code shaft.locatorHealth.attachDashboard}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty locatorHealthAttachDashboard(boolean value) {
            setProperty("shaft.locatorHealth.attachDashboard", String.valueOf(value));
            return this;
        }

        /**
         * Overrides the {@code shaft.locatorHealth.failBelowScore} property at runtime. Fail the run when
         * any locator score is below this threshold; -1 disables score-based failure.
         *
         * @param value the new value of {@code shaft.locatorHealth.failBelowScore}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty locatorHealthFailBelowScore(int value) {
            setProperty("shaft.locatorHealth.failBelowScore", String.valueOf(value));
            return this;
        }

        /**
         * Overrides the {@code shaft.diagnostics.enabled} property at runtime. Attach shaft-
         * diagnostics.zip with sanitized diagnostics.json for failed and broken tests.
         *
         * @param value the new value of {@code shaft.diagnostics.enabled}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty diagnosticsBundleEnabled(boolean value) {
            setProperty("shaft.diagnostics.enabled", String.valueOf(value));
            return this;
        }

        /**
         * Overrides the {@code shaft.diagnostics.maxArtifactMb} property at runtime. Maximum size for a
         * single diagnostics ZIP entry.
         *
         * @param value the new value of {@code shaft.diagnostics.maxArtifactMb}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty diagnosticsMaxArtifactMb(int value) {
            setProperty("shaft.diagnostics.maxArtifactMb", String.valueOf(value));
            return this;
        }

        /**
         * Overrides the {@code shaft.trace.enabled} property at runtime. Attach the SHAFT failure trace
         * viewer archive to Allure.
         *
         * @param value the new value of {@code shaft.trace.enabled}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty traceEnabled(boolean value) {
            setProperty("shaft.trace.enabled", String.valueOf(value));
            return this;
        }

        /**
         * Overrides the {@code shaft.trace.mode} property at runtime. Control trace generation: auto is
         * retry-aware (resolves to retry when retryMaximumNumberOfAttempts &gt; 0, otherwise failure).
         *
         * @param value the new value of {@code shaft.trace.mode}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty traceMode(String value) {
            setProperty("shaft.trace.mode", value);
            return this;
        }

        /**
         * Overrides the {@code shaft.trace.retainFailedAttempts} property at runtime. Retain failed-
         * attempt trace archives under attempt-indexed names when retries are enabled.
         *
         * @param value the new value of {@code shaft.trace.retainFailedAttempts}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty traceRetainFailedAttempts(boolean value) {
            setProperty("shaft.trace.retainFailedAttempts", String.valueOf(value));
            return this;
        }

        /**
         * Overrides the {@code shaft.trace.includeCodeContext} property at runtime. Include the best
         * matching project source-code frame and snippet.
         *
         * @param value the new value of {@code shaft.trace.includeCodeContext}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty traceIncludeCodeContext(boolean value) {
            setProperty("shaft.trace.includeCodeContext", String.valueOf(value));
            return this;
        }

        /**
         * Overrides the {@code shaft.trace.includeFullPageSnapshots} property at runtime. Include web page
         * snapshots when available.
         *
         * @param value the new value of {@code shaft.trace.includeFullPageSnapshots}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty traceIncludeFullPageSnapshots(boolean value) {
            setProperty("shaft.trace.includeFullPageSnapshots", String.valueOf(value));
            return this;
        }

        /**
         * Overrides the {@code shaft.trace.includeDomSnapshots} property at runtime. Include DOM snapshots
         * in the SHAFT failure trace bundle when available.
         *
         * @param value the new value of {@code shaft.trace.includeDomSnapshots}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty traceIncludeDomSnapshots(boolean value) {
            setProperty("shaft.trace.includeDomSnapshots", String.valueOf(value));
            return this;
        }

        /**
         * Overrides the {@code shaft.trace.includeScreenshots} property at runtime. Include screenshots in
         * the SHAFT failure trace bundle when available.
         *
         * @param value the new value of {@code shaft.trace.includeScreenshots}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty traceIncludeScreenshots(boolean value) {
            setProperty("shaft.trace.includeScreenshots", String.valueOf(value));
            return this;
        }

        /**
         * Overrides the {@code shaft.trace.includeNativePageSource} property at runtime. Include native
         * mobile page source when available.
         *
         * @param value the new value of {@code shaft.trace.includeNativePageSource}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty traceIncludeNativePageSource(boolean value) {
            setProperty("shaft.trace.includeNativePageSource", String.valueOf(value));
            return this;
        }

        /**
         * Overrides the {@code shaft.trace.includeNetwork} property at runtime. Reserve network evidence
         * in the trace metadata.
         *
         * @param value the new value of {@code shaft.trace.includeNetwork}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty traceIncludeNetwork(boolean value) {
            setProperty("shaft.trace.includeNetwork", String.valueOf(value));
            return this;
        }

        /**
         * Overrides the {@code shaft.trace.includeConsole} property at runtime. Reserve console evidence
         * in the trace metadata.
         *
         * @param value the new value of {@code shaft.trace.includeConsole}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty traceIncludeConsole(boolean value) {
            setProperty("shaft.trace.includeConsole", String.valueOf(value));
            return this;
        }

        /**
         * Overrides the {@code shaft.trace.maxArtifactMb} property at runtime. Maximum size for a single
         * trace bundle entry.
         *
         * @param value the new value of {@code shaft.trace.maxArtifactMb}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty traceMaxArtifactMb(int value) {
            setProperty("shaft.trace.maxArtifactMb", String.valueOf(value));
            return this;
        }

        /**
         * Overrides the {@code shaft.trace.maxSessionMb} property at runtime. Maximum total size of the
         * trace bundle for one test session.
         *
         * @param value the new value of {@code shaft.trace.maxSessionMb}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty traceMaxSessionMb(int value) {
            setProperty("shaft.trace.maxSessionMb", String.valueOf(value));
            return this;
        }

        /**
         * Overrides the {@code shaft.flakeProfiler.enabled} property at runtime. Attach opt-in flake and
         * auto-wait timing profiles to Allure.
         *
         * @param value the new value of {@code shaft.flakeProfiler.enabled}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty flakeProfilerEnabled(boolean value) {
            setProperty("shaft.flakeProfiler.enabled", String.valueOf(value));
            return this;
        }

        /**
         * Overrides the {@code shaft.flakeProfiler.attachPerTest} property at runtime. Attach each test's
         * JSON/HTML profile when profiler signals exist.
         *
         * @param value the new value of {@code shaft.flakeProfiler.attachPerTest}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty flakeProfilerAttachPerTest(boolean value) {
            setProperty("shaft.flakeProfiler.attachPerTest", String.valueOf(value));
            return this;
        }

        /**
         * Overrides the {@code shaft.flakeProfiler.failOnSevereFlakeRisk} property at runtime. Fail the
         * run when severe flake-risk actions are found.
         *
         * @param value the new value of {@code shaft.flakeProfiler.failOnSevereFlakeRisk}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty flakeProfilerFailOnSevereFlakeRisk(boolean value) {
            setProperty("shaft.flakeProfiler.failOnSevereFlakeRisk", String.valueOf(value));
            return this;
        }

        /**
         * Overrides the {@code shaft.flakeProfiler.slowActionThresholdMs} property at runtime. Duration
         * threshold used to flag slow and severe-risk actions.
         *
         * @param value the new value of {@code shaft.flakeProfiler.slowActionThresholdMs}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty flakeProfilerSlowActionThresholdMs(int value) {
            setProperty("shaft.flakeProfiler.slowActionThresholdMs", String.valueOf(value));
            return this;
        }

    }

}
