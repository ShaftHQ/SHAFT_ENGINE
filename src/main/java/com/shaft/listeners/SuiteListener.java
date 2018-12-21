package com.shaft.listeners;

import org.testng.ISuite;
import org.testng.ISuiteListener;
import org.testng.ITestNGMethod;

import java.util.List;
import org.testng.IInvokedMethod;

import com.shaft.browser.BrowserFactory;
import com.shaft.io.ReportManager;

public class SuiteListener implements ISuiteListener {
    int totalNumberOfTests = 0;

    @Override
    public void onStart(ISuite suite) {
	ReportManager.populateEnvironmentData();
	ReportManager.logEngineVersion(true);
	// confirm that no browser sessions were leaked due to an unexpected failure in
	// the previous test suite *discreetly*

	// the below block needs debugging
	List<IInvokedMethod> invokedMethods = suite.getAllInvokedMethods();
	invokedMethods.forEach(invokedMethod -> {
	    if (!invokedMethod.isConfigurationMethod()) {
		ITestNGMethod testMethod = invokedMethod.getTestMethod();
		if (testMethod.isTest()) {
		    totalNumberOfTests++;
		}
	    }
	});
	ReportManager.setTotalNumberOfTests(totalNumberOfTests);
    }

    @Override
    public void onFinish(ISuite suite) {
	ReportManager.setDiscreetLogging(true);
	BrowserFactory.closeAllDrivers();
	ReportManager.generateAllureReportArchive();
	ReportManager.setDiscreetLogging(false);
    }
}