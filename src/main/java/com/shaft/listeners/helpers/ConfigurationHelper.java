package com.shaft.listeners.helpers;

import com.shaft.driver.DriverFactory;
import com.shaft.tools.io.helpers.CheckpointCounter;
import com.shaft.tools.io.helpers.ReportHelper;
import org.testng.ITestContext;
import org.testng.annotations.AfterClass;
import org.testng.annotations.AfterSuite;
import org.testng.annotations.BeforeSuite;

public class ConfigurationHelper {

    @BeforeSuite(description = "Initializing Engine", alwaysRun = true)
    public void suiteSetup(ITestContext testContext) {
        ReportHelper.attachImportantLinks();
        ReportHelper.attachPropertyFiles();
    }

    @AfterClass(description = "Cleaning up orphaned drivers", alwaysRun = true)
    public void classTeardown() {
        DriverFactory.closeAllDrivers();
        ReportHelper.attachEngineLog();
    }

    @AfterSuite(description = "Attaching Reports", alwaysRun = true)
    public void suiteTeardown() {
        ReportHelper.attachExtentReport();
        ReportHelper.attachCucumberReport();
        CheckpointCounter.attach();
        ReportHelper.attachIssuesLog();
    }
}
