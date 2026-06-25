package testPackage.properties;

import com.shaft.driver.SHAFT;
import org.testng.annotations.BeforeClass;
import org.testng.annotations.Test;

public class ReportingTests {
    boolean captureElementName;
    boolean captureWebDriverLogs;
    boolean alwaysLogDiscreetly;
    boolean debugMode;
    boolean openLighthouseReportWhileExecution;
    boolean openExecutionSummaryReportAfterExecution;
    boolean disableLogging;
    boolean attachFullLog;
    boolean diagnosticsBundleEnabled;
    int diagnosticsMaxArtifactMb;
    boolean traceEnabled;
    String traceMode;
    boolean traceIncludeCodeContext;
    boolean traceIncludeFullPageSnapshots;
    boolean traceIncludeNativePageSource;
    boolean traceIncludeNetwork;
    boolean traceIncludeConsole;
    int traceMaxArtifactMb;

    @BeforeClass
    public void beforeClass() {
        captureElementName = SHAFT.Properties.reporting.captureElementName();
        captureWebDriverLogs = SHAFT.Properties.reporting.captureWebDriverLogs();
        alwaysLogDiscreetly = SHAFT.Properties.reporting.alwaysLogDiscreetly();
        debugMode = SHAFT.Properties.reporting.debugMode();
        openLighthouseReportWhileExecution = SHAFT.Properties.reporting.openLighthouseReportWhileExecution();
        openExecutionSummaryReportAfterExecution = SHAFT.Properties.reporting.openExecutionSummaryReportAfterExecution();
        disableLogging = SHAFT.Properties.reporting.disableLogging();
        attachFullLog = SHAFT.Properties.reporting.attachFullLog();
        diagnosticsBundleEnabled = SHAFT.Properties.reporting.diagnosticsBundleEnabled();
        diagnosticsMaxArtifactMb = SHAFT.Properties.reporting.diagnosticsMaxArtifactMb();
        traceEnabled = SHAFT.Properties.reporting.traceEnabled();
        traceMode = SHAFT.Properties.reporting.traceMode();
        traceIncludeCodeContext = SHAFT.Properties.reporting.traceIncludeCodeContext();
        traceIncludeFullPageSnapshots = SHAFT.Properties.reporting.traceIncludeFullPageSnapshots();
        traceIncludeNativePageSource = SHAFT.Properties.reporting.traceIncludeNativePageSource();
        traceIncludeNetwork = SHAFT.Properties.reporting.traceIncludeNetwork();
        traceIncludeConsole = SHAFT.Properties.reporting.traceIncludeConsole();
        traceMaxArtifactMb = SHAFT.Properties.reporting.traceMaxArtifactMb();

    }

    @Test
    public void test() {
        SHAFT.Properties.reporting.set().captureElementName(captureElementName);
        SHAFT.Properties.reporting.set().captureWebDriverLogs(captureWebDriverLogs);
        SHAFT.Properties.reporting.set().alwaysLogDiscreetly(alwaysLogDiscreetly);
        SHAFT.Properties.reporting.set().debugMode(debugMode);
        SHAFT.Properties.reporting.set().openLighthouseReportWhileExecution(openLighthouseReportWhileExecution);
        SHAFT.Properties.reporting.set().openExecutionSummaryReportAfterExecution(openExecutionSummaryReportAfterExecution);
        SHAFT.Properties.reporting.set().disableLogging(disableLogging);
        SHAFT.Properties.reporting.set().attachFullLog(attachFullLog);
        SHAFT.Properties.reporting.set().diagnosticsBundleEnabled(diagnosticsBundleEnabled);
        SHAFT.Properties.reporting.set().diagnosticsMaxArtifactMb(diagnosticsMaxArtifactMb);
        SHAFT.Properties.reporting.set().traceEnabled(traceEnabled);
        SHAFT.Properties.reporting.set().traceMode(traceMode);
        SHAFT.Properties.reporting.set().traceIncludeCodeContext(traceIncludeCodeContext);
        SHAFT.Properties.reporting.set().traceIncludeFullPageSnapshots(traceIncludeFullPageSnapshots);
        SHAFT.Properties.reporting.set().traceIncludeNativePageSource(traceIncludeNativePageSource);
        SHAFT.Properties.reporting.set().traceIncludeNetwork(traceIncludeNetwork);
        SHAFT.Properties.reporting.set().traceIncludeConsole(traceIncludeConsole);
        SHAFT.Properties.reporting.set().traceMaxArtifactMb(traceMaxArtifactMb);
    }
}
