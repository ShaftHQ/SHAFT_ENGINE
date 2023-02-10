package io.github.shafthq.shaft.listeners.helpers;

import com.shaft.driver.DriverFactory;
import io.github.shafthq.shaft.driver.helpers.AppiumSelfManagementHelper;
import io.github.shafthq.shaft.tools.io.helpers.CheckpointCounter;
import io.github.shafthq.shaft.tools.io.helpers.ReportHelper;
import org.testng.ITestContext;
import org.testng.annotations.AfterClass;
import org.testng.annotations.AfterSuite;
import org.testng.annotations.BeforeSuite;

public class ConfigurationHelper {

    @BeforeSuite(description = "Initializing Engine", alwaysRun = true)
    public void suiteSetup(ITestContext testContext) {
        ReportHelper.attachImportantLinks();
        ReportHelper.attachPropertyFiles();
        if (AppiumSelfManagementHelper.isAppiumSelfManagedExecution()) {
            AppiumSelfManagementHelper.setupAppiumSelfManagedExecutionPrerequisites();
        }
        if (AppiumSelfManagementHelper.isAppiumDockerizedExecution()) {
            AppiumSelfManagementHelper.downloadAndroidEmulatorFiles();
        }
    }

    //TODO: this method is not being executed in case there's another after class method
    @AfterClass(description = "Cleaning up orphaned drivers", alwaysRun = true)
    public void classTeardown() {
        DriverFactory.closeAllDrivers();
    }

    @AfterSuite(description = "Attaching Reports", alwaysRun = true)
    public void suiteTeardown() {
        ReportHelper.attachEngineLog();
        ReportHelper.attachExtentReport();
        ReportHelper.attachCucumberReport();
        CheckpointCounter.attach();
        ReportHelper.attachIssuesLog();
    }
}
