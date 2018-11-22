package com.shaft.listeners;

import org.testng.ISuite;
import org.testng.ISuiteListener;

import com.shaft.browser.BrowserFactory;
import com.shaft.io.ReportManager;

public class SuiteListener implements ISuiteListener {

    @Override
    public void onStart(ISuite suite) {
	ReportManager.populateEnvironmentData();
	ReportManager.logEngineVersion(true);
	// confirm that no browser sessions were leaked due to an unexpected failure in
	// the previous test suite *discreetly*
	ReportManager.setDiscreetLogging(true);
	BrowserFactory.closeAllDrivers();
	ReportManager.setDiscreetLogging(false);
    }

    @Override
    public void onFinish(ISuite suite) {
	BrowserFactory.closeAllDrivers();
	ReportManager.generateAllureReportArchive();
    }
}