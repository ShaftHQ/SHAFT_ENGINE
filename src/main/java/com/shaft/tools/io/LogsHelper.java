package com.shaft.tools.io;

import com.shaft.cli.FileActions;
import com.shaft.driver.DriverFactoryHelper;
import com.shaft.gui.browser.BrowserFactory;
import org.testng.annotations.AfterSuite;

import java.text.SimpleDateFormat;
import java.util.Date;

public class LogsHelper {
    //TODO: migrate invokedMethodListener to annotations here?
    @AfterSuite
    public void closureActivities() {
        initializeClosureActivities();
        attachBrowserLogs();
        attachFullLogs();
        attachCucumberReport();
        attachExtentReport();
        ReportManagerHelper.setDiscreteLogging(true);
        ReportManagerHelper.generateAllureReportArchive();
        ReportManagerHelper.openAllureReportAfterExecution();
    }

    public void attachFullLogs() {
        String executionEndTimestamp = new SimpleDateFormat("yyyyMMdd-HHmmss").format(new Date());
        ReportManagerHelper.attachIssuesLog(executionEndTimestamp);
        ReportManagerHelper.attachFullLog(executionEndTimestamp);
    }

    public void attachBrowserLogs() {
        if (Boolean.FALSE.equals(DriverFactoryHelper.isDriversListEmpty())) {
            BrowserFactory.closeAllBrowsers();
        } else {
            ReportManager.logDiscrete("There were no Web Browsers used for this test run.");
        }
    }

    private void attachCucumberReport() {
        if (FileActions.doesFileExist("allure-results/cucumberReport.html")) {
            ReportManagerHelper.attach("HTML", "Cucumber Execution Report", FileActions.readFromFile("allure-results/cucumberReport.html"));
        }
    }

    private void attachExtentReport() {
        ReportManagerHelper.extentReportsFlush();
        if (Boolean.parseBoolean(System.getProperty("generateExtentReports").trim()) && FileActions.doesFileExist(ReportManagerHelper.getExtentReportFileName())) {
            ReportManagerHelper.attach("HTML", "Extent Emailable Execution Report", FileActions.readFromFile(ReportManagerHelper.getExtentReportFileName()));
        }
    }

    private void initializeClosureActivities() {
        ReportManagerHelper.logClosureActivitiesInitialization();
    }
}