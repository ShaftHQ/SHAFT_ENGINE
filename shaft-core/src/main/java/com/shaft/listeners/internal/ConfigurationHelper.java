package com.shaft.listeners.internal;

import com.shaft.tools.io.internal.CheckpointCounter;
import com.shaft.tools.io.internal.ReportHelper;
import io.qameta.allure.Step;
import org.testng.ITestContext;
import org.testng.annotations.AfterTest;
import org.testng.annotations.BeforeTest;

/**
 * TestNG listener helper that provides {@code @BeforeTest} / {@code @AfterTest} lifecycle hooks
 * for attaching SHAFT property files and generating end-of-suite reports.
 *
 * <p>This class is intended to be listed as a TestNG listener in {@code testng.xml} or
 * via the {@code @Listeners} annotation and is not designed for direct instantiation in test code.
 */
public class ConfigurationHelper {

    /**
     * Constructs a new {@code ConfigurationHelper} instance.
     */
    public ConfigurationHelper() {
    }

    /**
     * TestNG before-test hook that attaches SHAFT property files to the Allure report
     * so they are available for inspection after the run.
     *
     * @param testContext the TestNG {@link ITestContext} for the current {@code <test>} element
     */
    @BeforeTest(description = "Engine Setup", alwaysRun = true)
    public void suiteSetup(ITestContext testContext) {
        ReportHelper.attachPropertyFiles();
    }

    /**
     * TestNG after-test hook that attaches the engine log, Cucumber report, checkpoint summary,
     * and issues log to the Allure report.
     */
    @AfterTest(description = "Engine Tear down", alwaysRun = true)
    public void engineTearDown() {
        attachLogsAndReports();
    }

    @Step("Attaching Reports")
    private void attachLogsAndReports() {
        ReportHelper.attachEngineLog();
        ReportHelper.attachCucumberReport();
        CheckpointCounter.attach();
        ReportHelper.attachIssuesLog();
    }
}
