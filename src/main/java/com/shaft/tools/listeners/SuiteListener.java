package com.shaft.tools.listeners;

import org.testng.ISuite;
import org.testng.ISuiteListener;

import com.shaft.tools.io.PropertiesFileManager;
import com.shaft.tools.io.ReportManager;

public class SuiteListener implements ISuiteListener {

    @Override
    public void onStart(ISuite suite) {
	PropertiesFileManager.readPropertyFiles();
	ReportManager.prepareAllureReportingEnvironment();
	ReportManager.logEngineVersion();
	ReportManager.setTotalNumberOfTests(suite.getAllMethods().size());
	ReportManager.setDiscreteLogging(Boolean.valueOf(System.getProperty("alwaysLogDiscreetly")));
	ReportManager.setDebugMode(Boolean.valueOf(System.getProperty("debugMode")));
    }

    @Override
    public void onFinish(ISuite suite) {
	ReportManager.setDiscreteLogging(true);
	ReportManager.generateAllureReportArchive();
    }
}