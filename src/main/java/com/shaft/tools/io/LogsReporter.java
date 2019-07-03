package com.shaft.tools.io;

import org.testng.annotations.AfterSuite;

import com.shaft.gui.browser.BrowserFactory;
import com.shaft.gui.video.RecordManager;

public class LogsReporter {
    @AfterSuite
    public void closureActivities() {
	attachFullLogs();
	attachBrowserLogs();
	attachExecutionVideoRecording();
    }

    public void attachFullLogs() {
	ReportManager.attachFullLog();
	ReportManager.attachIssuesLog();
    }

    public void attachBrowserLogs() {
	if (!BrowserFactory.isBrowsersListEmpty()) {
	    BrowserFactory.attachBrowserLogs();
	    BrowserFactory.closeAllDrivers();
	} else {
	    ReportManager.logDiscrete("There were no Web Browsers used for this test run.");
	}
    }

    public void attachExecutionVideoRecording() {
	if (RecordManager.getRecordVideo()) {
	    RecordManager.stopRecording();
	    RecordManager.attachRecording();
	} else {
	    ReportManager.logDiscrete(
		    "Video Recording has been disabled for this test run. Please use the relevant property in the execution.properties file to enable video recording for future test runs.");
	}
    }
}