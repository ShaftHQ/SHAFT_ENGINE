package com.shaft.listeners.internal;

import com.shaft.driver.DriverFactory;
import com.shaft.driver.SHAFT;
import com.shaft.tools.io.internal.CheckpointCounter;
import com.shaft.tools.io.internal.ReportHelper;
import io.qameta.allure.Step;
import org.testng.ITestContext;
import org.testng.annotations.AfterSuite;
import org.testng.annotations.AfterTest;
import org.testng.annotations.BeforeSuite;
import org.testng.annotations.BeforeTest;

public class ConfigurationHelper {

    @BeforeTest(description = "Engine Setup", alwaysRun = true)
    public void suiteSetup(ITestContext testContext) {
        ReportHelper.attachImportantLinks();
        ReportHelper.attachPropertyFiles();
    }

    @AfterTest(description = "Engine Teardown", alwaysRun = true)
    public void engineTeardown() {
        attachLogsAndReports();
        if (SHAFT.Properties.flags.autoCloseDriverInstance())
            cleaningOrphanedDriverInstances();
    }

    @Step("Attaching Reports")
    private void attachLogsAndReports() {
        ReportHelper.attachEngineLog();
        ReportHelper.attachExtentReport();
        ReportHelper.attachCucumberReport();
        CheckpointCounter.attach();
        ReportHelper.attachIssuesLog();
    }

    @Step("Closing Orphaned Drivers")
    private void cleaningOrphanedDriverInstances() {
        DriverFactory.closeAllDrivers();
    }
}
