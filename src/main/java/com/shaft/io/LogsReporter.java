package com.shaft.io;

import org.testng.annotations.AfterSuite;

import com.shaft.browser.BrowserFactory;
import com.shaft.video.RecordManager;

public class LogsReporter {
    @AfterSuite
    public void closureActivities() {
	attachFullLogs();
	attachBrowserLogs();
	attachExecutionVideoRecording();
    }

    public void attachFullLogs() {
	ReportManager.attachFullLog();
	ReportManager.attachSystemProperties();
    }

    public void attachBrowserLogs() {
	if (!BrowserFactory.isBrowsersListEmpty()) {
	    BrowserFactory.attachBrowserLogs();
	    BrowserFactory.closeAllDrivers();
	} else {
	    ReportManager.log("There were no Web Browsers used for this test run.");
	}
    }

    public void attachExecutionVideoRecording() {
	if (RecordManager.getRecordVideo()) {
	    RecordManager.stopRecording();
	    RecordManager.attachRecording();
	} else {
	    ReportManager.log(
		    "Video Recording has been disabled for this test run. Please use the relevant property in the execution.properties file to enable video recording for future test runs.");
	}
    }
}