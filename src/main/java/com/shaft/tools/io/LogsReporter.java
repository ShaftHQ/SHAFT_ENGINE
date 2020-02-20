package com.shaft.tools.io;

import com.shaft.gui.browser.BrowserFactory;
import com.shaft.gui.video.RecordManager;
import org.testng.annotations.AfterSuite;

import java.text.SimpleDateFormat;
import java.util.Date;

public class LogsReporter {
    @AfterSuite
    public void closureActivities() {
        initializeClosureActivities();
        attachExecutionVideoRecording();
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

    public void attachExecutionVideoRecording() {
        if (Boolean.TRUE.equals(RecordManager.getRecordVideo())) {
            RecordManager.stopRecording();
            RecordManager.attachRecording();
        } else {
            ReportManager.logDiscrete(
                    "Video Recording has been disabled for this test run. Please use the relevant property in the execution.properties file to enable video recording for future test runs.");
        }
    }
}