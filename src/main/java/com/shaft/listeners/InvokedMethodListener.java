package com.shaft.listeners;

import org.testng.IInvokedMethod;
import org.testng.IInvokedMethodListener;
import org.testng.ITestNGMethod;
import org.testng.ITestResult;

import com.shaft.browser.BrowserFactory;
import com.shaft.element.ElementActions;
import com.shaft.io.RecordManager;
import com.shaft.io.ReportManager;

public class InvokedMethodListener implements IInvokedMethodListener {
    private int invokedTestsCounter = 0;
    private int testSize = 0;

    @Override
    public void beforeInvocation(IInvokedMethod method, ITestResult testResult) {
//	ElementActions.switchToDefaultContent();
	if (!method.isConfigurationMethod()) {
	    try {
		// testSize where the structure is testSuite > test > testClasses > testMethods
		testSize = testResult.getTestContext().getAllTestMethods().length;
	    } catch (NullPointerException e) {
		// this is thrown if there is no test context for some reason...
		ReportManager.log(e);
	    }
	    ITestNGMethod testMethod = method.getTestMethod();
	    if (testMethod.isTest()) {
		// ReportManager.log("BeforeInvocation: Test Method.");
		if (testMethod.getDescription() != null) {
		    ReportManager.logTestInformation(testMethod.getTestClass().getName(), testMethod.getMethodName(),
			    testMethod.getDescription(), invokedTestsCounter + 1, testSize);
		} else {
		    ReportManager.logTestInformation(testMethod.getTestClass().getName(), testMethod.getMethodName(),
			    "", invokedTestsCounter + 1, testSize);
		}
		BrowserFactory.startAnimatedGif();
		if (invokedTestsCounter == 0) {
		    RecordManager.startRecording();
		}
	    }
	}
    }

    @Override
    public void afterInvocation(IInvokedMethod method, ITestResult testResult) {
	if (!method.isConfigurationMethod()) {
	    ITestNGMethod testMethod = method.getTestMethod();
	    if (testMethod.isTest()) {
		// ReportManager.log("AfterInvocation: Test Method.");
		ElementActions.switchToDefaultContent();
		BrowserFactory.attachAnimatedGif();
		ReportManager.attachTestLog();
//		try {
//		    // testSize where the structure is testSuite > test > testClasses > testMethods
//		    int testSize = testResult.getTestContext().getAllTestMethods().length;

		if (invokedTestsCounter == testSize - 1) {
		    // is last test in the class
		    RecordManager.stopRecording();
		    RecordManager.attachRecording();
		    BrowserFactory.attachBrowserLogs();
		    ReportManager.logEngineVersion(false);
		    ReportManager.attachFullLog();
		    invokedTestsCounter = 0;
		} else {
		    invokedTestsCounter++;
		}
//		} catch (NullPointerException e) {
//		    // this is thrown if there is no test context for some reason...
//		    ReportManager.log(e);
//		}
	    }
	}
    }
}
