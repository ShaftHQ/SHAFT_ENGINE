package com.shaft.listeners;

import org.testng.IInvokedMethod;
import org.testng.IInvokedMethodListener;
import org.testng.ITestNGMethod;
import org.testng.ITestResult;

import com.shaft.browser.BrowserFactory;
import com.shaft.io.RecordManager;
import com.shaft.io.ReportManager;

public class InvokedMethodListener implements IInvokedMethodListener {

    private Boolean isFirstTest = true;
    private int invokedTestsCounter = 0;

    @Override
    public void beforeInvocation(IInvokedMethod method, ITestResult testResult) {
	if (method.isTestMethod()) {
	    ITestNGMethod testMethod = method.getTestMethod();
	    if (testMethod.isTest()) {
		// ReportManager.log("BeforeInvocation: Test Method.");
		if (testMethod.getDescription() != null) {
		    ReportManager.logTestInformation(testResult.getTestClass().getName(), testMethod.getDescription());
		} else {
		    ReportManager.logTestInformation(testResult.getTestClass().getName(), testMethod.getMethodName());
		}
		BrowserFactory.startAnimatedGif();
		if (isFirstTest) {
		    RecordManager.startRecording();
		}
	    }
	}
    }

    @Override
    public void afterInvocation(IInvokedMethod method, ITestResult testResult) {
	if (method.isTestMethod()) {
	    ITestNGMethod testMethod = method.getTestMethod();
	    if (testMethod.isTest()) {
		// ReportManager.log("AfterInvocation: Test Method.");
		BrowserFactory.attachAnimatedGif();
		ReportManager.attachTestLog();

		int testSuiteSize = testResult.getTestContext().getAllTestMethods().length;

		if (invokedTestsCounter == testSuiteSize - 1) {
		    // is last test in the class
		    RecordManager.stopRecording();
		    RecordManager.attachRecording();
		    BrowserFactory.attachBrowserLogs();
		    ReportManager.logEngineVersion(false);
		    ReportManager.attachFullLog();
		}
		invokedTestsCounter++;
	    }
	}
    }
}
