package com.shaft.tools.io;

import com.shaft.cli.FileActions;
import com.shaft.gui.browser.BrowserFactory;
import org.testng.annotations.AfterSuite;

import java.text.SimpleDateFormat;
import java.util.Date;

public class LogsReporter {
    //TODO: migrate invokedMethodListener to annotations here?
    @AfterSuite
    public void closureActivities() {
        initializeClosureActivities();
        attachBrowserLogs();
        attachFullLogs();
        attachCucumberReport();
        attachExtentReport();
        ReportManager.setDiscreteLogging(true);
        ReportManager.generateAllureReportArchive();
        ReportManager.openAllureReportAfterExecution();
    }

    public void attachFullLogs() {
        String executionEndTimestamp = new SimpleDateFormat("yyyyMMdd-HHmmss").format(new Date());
        ReportManager.attachIssuesLog(executionEndTimestamp);
        ReportManager.attachFullLog(executionEndTimestamp);
    }

    public void attachBrowserLogs() {
        if (Boolean.FALSE.equals(BrowserFactory.isBrowsersListEmpty())) {
            BrowserFactory.closeAllDrivers();
        } else {
            ReportManager.logDiscrete("There were no Web Browsers used for this test run.");
        }
    }

    private void attachCucumberReport() {
        if (FileActions.doesFileExist("allure-results/cucumberReport.html")) {
            ReportManager.attach("HTML", "Cucumber Execution Report", FileActions.readFromFile("allure-results/cucumberReport.html"));
        }
    }

    private void attachExtentReport() {
        ReportManager.extentReportsFlush();
        if (FileActions.doesFileExist(ReportManager.getExtentReportFileName())) {
            ReportManager.attach("HTML", "Extent Emailable Execution Report", FileActions.readFromFile(ReportManager.getExtentReportFileName()));
        }
    }

    private void initializeClosureActivities() {
        ReportManager.logClosureActivitiesInitialization();
    }
}