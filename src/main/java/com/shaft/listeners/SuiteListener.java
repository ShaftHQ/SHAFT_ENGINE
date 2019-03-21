package com.shaft.listeners;

import org.testng.ISuite;
import org.testng.ISuiteListener;

import com.shaft.browser.BrowserFactory;
import com.shaft.io.PropertiesFileManager;
import com.shaft.io.ReportManager;

public class SuiteListener implements ISuiteListener {

    @Override
    public void onStart(ISuite suite) {
	PropertiesFileManager.readPropertyFiles();
	ReportManager.populateEnvironmentData();
	ReportManager.logEngineVersion(true);
	ReportManager.setTotalNumberOfTests(suite.getAllMethods().size());
	ReportManager.setDiscreteLogging(Boolean.valueOf(System.getProperty("alwaysLogDiscreetly")));
	ReportManager.setDebugMode(Boolean.valueOf(System.getProperty("debugMode")));
    }

    @Override
    public void onFinish(ISuite suite) {
	Boolean discreetLoggingState = ReportManager.isDiscreteLogging();
	ReportManager.setDiscreteLogging(true);
	BrowserFactory.closeAllDrivers();
	ReportManager.setDiscreteLogging(discreetLoggingState);
	ReportManager.generateAllureReportArchive();
    }
}