package com.shaft.tools.io;

import org.testng.annotations.AfterSuite;

import com.shaft.gui.browser.BrowserFactory;
import com.shaft.gui.video.RecordManager;

public class LogsReporter {
    @AfterSuite
    public void closureActivities() {
	initializeClosureActivities();
	attachExecutionVideoRecording();
	attachBrowserLogs();
	attachFullLogs();
    }

    private void initializeClosureActivities() {
	ReportManager.createImportantReportEntry("Test Closure Activities");
    }

    public void attachFullLogs() {
	ReportManager.attachIssuesLog();
	ReportManager.attachFullLog();
    }

    public void attachBrowserLogs() {
	if (Boolean.FALSE.equals(BrowserFactory.isBrowsersListEmpty())) {
	    BrowserFactory.attachBrowserLogs();
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