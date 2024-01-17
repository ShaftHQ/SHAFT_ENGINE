package testPackage.properties;

import com.shaft.driver.SHAFT;
import org.testng.annotations.BeforeClass;
import org.testng.annotations.Test;

public class ReportingTests {
    boolean captureElementName;
    boolean captureWebDriverLogs;
    boolean alwaysLogDiscreetly;
    boolean debugMode;
    boolean cleanAllureResultsDirectoryBeforeExecution;
    boolean generateAllureReportArchive;
    boolean openAllureReportAfterExecution;
    boolean openLighthouseReportWhileExecution;
    boolean openExecutionSummaryReportAfterExecution;
    boolean disableLogging;

    @BeforeClass
    public void beforeClass() {
        captureElementName = SHAFT.Properties.reporting.captureElementName();
        captureWebDriverLogs = SHAFT.Properties.reporting.captureWebDriverLogs();
        alwaysLogDiscreetly = SHAFT.Properties.reporting.alwaysLogDiscreetly();
        debugMode = SHAFT.Properties.reporting.debugMode();
        cleanAllureResultsDirectoryBeforeExecution = SHAFT.Properties.reporting.cleanAllureResultsDirectoryBeforeExecution();
        generateAllureReportArchive = SHAFT.Properties.reporting.generateAllureReportArchive();
        openAllureReportAfterExecution = SHAFT.Properties.reporting.openAllureReportAfterExecution();
        openLighthouseReportWhileExecution = SHAFT.Properties.reporting.openLighthouseReportWhileExecution();
        openExecutionSummaryReportAfterExecution = SHAFT.Properties.reporting.openExecutionSummaryReportAfterExecution();
        disableLogging = SHAFT.Properties.reporting.disableLogging();

    }

    @Test
    public void test() {
        SHAFT.Properties.reporting.set().captureElementName(captureElementName);
        SHAFT.Properties.reporting.set().captureWebDriverLogs(captureWebDriverLogs);
        SHAFT.Properties.reporting.set().alwaysLogDiscreetly(alwaysLogDiscreetly);
        SHAFT.Properties.reporting.set().debugMode(debugMode);
        SHAFT.Properties.reporting.set().generateAllureReportArchive(generateAllureReportArchive);
        SHAFT.Properties.reporting.set().openAllureReportAfterExecution(openAllureReportAfterExecution);
        SHAFT.Properties.reporting.set().openLighthouseReportWhileExecution(openLighthouseReportWhileExecution);
        SHAFT.Properties.reporting.set().openExecutionSummaryReportAfterExecution(openExecutionSummaryReportAfterExecution);
        SHAFT.Properties.reporting.set().disableLogging(disableLogging);

    }
}
