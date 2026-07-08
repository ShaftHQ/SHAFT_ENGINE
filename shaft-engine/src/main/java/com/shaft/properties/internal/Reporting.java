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

    @Key("captureElementName")
    @DefaultValue("true")
    boolean captureElementName();

    @Key("captureWebDriverLogs")
    @DefaultValue("false")
    boolean captureWebDriverLogs();

    @Key("alwaysLogDiscreetly")
    @DefaultValue("false")
    boolean alwaysLogDiscreetly();

    @Key("debugMode")
    @DefaultValue("false")
    boolean debugMode();

    @Key("openLighthouseReportWhileExecution")
    @DefaultValue("false")
    boolean openLighthouseReportWhileExecution();

    @Key("cleanSummaryReportsDirectoryBeforeExecution")
    @DefaultValue("true")
    boolean cleanSummaryReportsDirectoryBeforeExecution();

    @Key("openExecutionSummaryReportAfterExecution")
    @DefaultValue("false")
    boolean openExecutionSummaryReportAfterExecution();

    @Key("disableLogging")
    @DefaultValue("false")
    boolean disableLogging();

    @Key("attachFullLog")
    @DefaultValue("false")
    boolean attachFullLog();

    @Key("evidenceLevel")
    @DefaultValue("FAILURE_ONLY")
    String evidenceLevel();

    @Key("locatorHealthReportEnabled")
    @DefaultValue("false")
    boolean locatorHealthReportEnabled();

    @Key("slowLocatorThresholdMillis")
    @DefaultValue("750")
    int slowLocatorThresholdMillis();

    @Key("failOnLocatorHealthWarnings")
    @DefaultValue("false")
    boolean failOnLocatorHealthWarnings();

    @Key("shaft.locatorHealth.enabled")
    @DefaultValue("false")
    boolean locatorHealthEnabled();

    @Key("shaft.locatorHealth.warnBelowScore")
    @DefaultValue("70")
    int locatorHealthWarnBelowScore();

    @Key("shaft.locatorHealth.attachDashboard")
    @DefaultValue("true")
    boolean locatorHealthAttachDashboard();

    @Key("shaft.locatorHealth.failBelowScore")
    @DefaultValue("-1")
    int locatorHealthFailBelowScore();

    @Key("shaft.diagnostics.enabled")
    @DefaultValue("true")
    boolean diagnosticsBundleEnabled();

    @Key("shaft.diagnostics.maxArtifactMb")
    @DefaultValue("50")
    int diagnosticsMaxArtifactMb();

    @Key("shaft.trace.enabled")
    @DefaultValue("true")
    boolean traceEnabled();

    @Key("shaft.trace.mode")
    @DefaultValue("failure")
    String traceMode();

    @Key("shaft.trace.includeCodeContext")
    @DefaultValue("true")
    boolean traceIncludeCodeContext();

    @Key("shaft.trace.includeFullPageSnapshots")
    @DefaultValue("true")
    boolean traceIncludeFullPageSnapshots();

    @Key("shaft.trace.includeDomSnapshots")
    @DefaultValue("true")
    boolean traceIncludeDomSnapshots();

    @Key("shaft.trace.includeScreenshots")
    @DefaultValue("true")
    boolean traceIncludeScreenshots();

    @Key("shaft.trace.includeNativePageSource")
    @DefaultValue("true")
    boolean traceIncludeNativePageSource();

    @Key("shaft.trace.includeNetwork")
    @DefaultValue("true")
    boolean traceIncludeNetwork();

    @Key("shaft.trace.includeConsole")
    @DefaultValue("true")
    boolean traceIncludeConsole();

    @Key("shaft.trace.maxArtifactMb")
    @DefaultValue("50")
    int traceMaxArtifactMb();

    @Key("shaft.flakeProfiler.enabled")
    @DefaultValue("false")
    boolean flakeProfilerEnabled();

    @Key("shaft.flakeProfiler.attachPerTest")
    @DefaultValue("true")
    boolean flakeProfilerAttachPerTest();

    @Key("shaft.flakeProfiler.failOnSevereFlakeRisk")
    @DefaultValue("false")
    boolean flakeProfilerFailOnSevereFlakeRisk();

    @Key("shaft.flakeProfiler.slowActionThresholdMs")
    @DefaultValue("2000")
    int flakeProfilerSlowActionThresholdMs();

    default SetProperty set() {
        return new SetProperty();
    }

    class SetProperty implements EngineProperties.SetProperty {

        public SetProperty captureElementName(boolean value) {
            setProperty("captureElementName", String.valueOf(value));
            return this;
        }

        public SetProperty forceCheckForElementVisibility(boolean value) {
            setProperty("forceCheckForElementVisibility", String.valueOf(value));
            return this;
        }

        public SetProperty captureWebDriverLogs(boolean value) {
            setProperty("captureWebDriverLogs", String.valueOf(value));
            return this;
        }

        public SetProperty alwaysLogDiscreetly(boolean value) {
            setProperty("alwaysLogDiscreetly", String.valueOf(value));
            return this;
        }

        public SetProperty debugMode(boolean value) {
            setProperty("debugMode", String.valueOf(value));
            return this;
        }

        public SetProperty openLighthouseReportWhileExecution(boolean value) {
            setProperty("openLighthouseReportWhileExecution", String.valueOf(value));
            return this;
        }

        public SetProperty openExecutionSummaryReportAfterExecution(boolean value) {
            setProperty("openExecutionSummaryReportAfterExecution", String.valueOf(value));
            return this;
        }

        public SetProperty disableLogging(boolean value) {
            setProperty("disableLogging", String.valueOf(value));
            return this;
        }

        public SetProperty attachFullLog(boolean value) {
            setProperty("attachFullLog", String.valueOf(value));
            return this;
        }

        public SetProperty evidenceLevel(String value) {
            setProperty("evidenceLevel", value);
            PropertiesHelper.overridePropertiesForEvidenceLevel();
            return this;
        }

        public SetProperty locatorHealthReportEnabled(boolean value) {
            setProperty("locatorHealthReportEnabled", String.valueOf(value));
            return this;
        }

        public SetProperty slowLocatorThresholdMillis(int value) {
            setProperty("slowLocatorThresholdMillis", String.valueOf(value));
            return this;
        }

        public SetProperty failOnLocatorHealthWarnings(boolean value) {
            setProperty("failOnLocatorHealthWarnings", String.valueOf(value));
            return this;
        }

        public SetProperty locatorHealthEnabled(boolean value) {
            setProperty("shaft.locatorHealth.enabled", String.valueOf(value));
            return this;
        }

        public SetProperty locatorHealthWarnBelowScore(int value) {
            setProperty("shaft.locatorHealth.warnBelowScore", String.valueOf(value));
            return this;
        }

        public SetProperty locatorHealthAttachDashboard(boolean value) {
            setProperty("shaft.locatorHealth.attachDashboard", String.valueOf(value));
            return this;
        }

        public SetProperty locatorHealthFailBelowScore(int value) {
            setProperty("shaft.locatorHealth.failBelowScore", String.valueOf(value));
            return this;
        }

        public SetProperty diagnosticsBundleEnabled(boolean value) {
            setProperty("shaft.diagnostics.enabled", String.valueOf(value));
            return this;
        }

        public SetProperty diagnosticsMaxArtifactMb(int value) {
            setProperty("shaft.diagnostics.maxArtifactMb", String.valueOf(value));
            return this;
        }

        public SetProperty traceEnabled(boolean value) {
            setProperty("shaft.trace.enabled", String.valueOf(value));
            return this;
        }

        public SetProperty traceMode(String value) {
            setProperty("shaft.trace.mode", value);
            return this;
        }

        public SetProperty traceIncludeCodeContext(boolean value) {
            setProperty("shaft.trace.includeCodeContext", String.valueOf(value));
            return this;
        }

        public SetProperty traceIncludeFullPageSnapshots(boolean value) {
            setProperty("shaft.trace.includeFullPageSnapshots", String.valueOf(value));
            return this;
        }

        public SetProperty traceIncludeDomSnapshots(boolean value) {
            setProperty("shaft.trace.includeDomSnapshots", String.valueOf(value));
            return this;
        }

        public SetProperty traceIncludeScreenshots(boolean value) {
            setProperty("shaft.trace.includeScreenshots", String.valueOf(value));
            return this;
        }

        public SetProperty traceIncludeNativePageSource(boolean value) {
            setProperty("shaft.trace.includeNativePageSource", String.valueOf(value));
            return this;
        }

        public SetProperty traceIncludeNetwork(boolean value) {
            setProperty("shaft.trace.includeNetwork", String.valueOf(value));
            return this;
        }

        public SetProperty traceIncludeConsole(boolean value) {
            setProperty("shaft.trace.includeConsole", String.valueOf(value));
            return this;
        }

        public SetProperty traceMaxArtifactMb(int value) {
            setProperty("shaft.trace.maxArtifactMb", String.valueOf(value));
            return this;
        }

        public SetProperty flakeProfilerEnabled(boolean value) {
            setProperty("shaft.flakeProfiler.enabled", String.valueOf(value));
            return this;
        }

        public SetProperty flakeProfilerAttachPerTest(boolean value) {
            setProperty("shaft.flakeProfiler.attachPerTest", String.valueOf(value));
            return this;
        }

        public SetProperty flakeProfilerFailOnSevereFlakeRisk(boolean value) {
            setProperty("shaft.flakeProfiler.failOnSevereFlakeRisk", String.valueOf(value));
            return this;
        }

        public SetProperty flakeProfilerSlowActionThresholdMs(int value) {
            setProperty("shaft.flakeProfiler.slowActionThresholdMs", String.valueOf(value));
            return this;
        }

    }

}
