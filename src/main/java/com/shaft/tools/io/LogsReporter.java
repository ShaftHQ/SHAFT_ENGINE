package com.shaft.tools.io;

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
    }

    private void initializeClosureActivities() {
        ReportManager.logClosureActivitiesInitialization();
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
}