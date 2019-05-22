package com.shaft.io;

import org.testng.annotations.Test;

import com.shaft.browser.BrowserFactory;
import com.shaft.video.RecordManager;

import io.qameta.allure.Epic;

@Epic("Closure Activities")
public class LogsReporter {
    @Test(priority = 1, alwaysRun = true, description = "SHAFT Engine Logs")
    public void attachFullLog() {
	ReportManager.attachSystemProperties();
	ReportManager.attachFullLog();
    }

    @Test(priority = 2, alwaysRun = true, description = "Selenium WebDriver Logs")
    public void attachBrowserLogs() {
	if (!BrowserFactory.isBrowsersListEmpty()) {
	    BrowserFactory.attachBrowserLogs();
	    BrowserFactory.closeAllDrivers();
	} else {
	    ReportManager.log("There were no Web Browsers used for this test run.");
	}
    }

    @Test(priority = 3, alwaysRun = true, description = "Video Recording")
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
