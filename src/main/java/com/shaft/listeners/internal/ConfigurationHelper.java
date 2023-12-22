package com.shaft.listeners.internal;

import com.shaft.tools.io.internal.CheckpointCounter;
import com.shaft.tools.io.internal.ReportHelper;
import io.qameta.allure.Step;
import org.testng.ITestContext;
import org.testng.annotations.AfterTest;
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
    }

    @Step("Attaching Reports")
    private void attachLogsAndReports() {
        ReportHelper.attachEngineLog();
        ReportHelper.attachExtentReport();
        ReportHelper.attachCucumberReport();
        CheckpointCounter.attach();
        ReportHelper.attachIssuesLog();
    }
}
