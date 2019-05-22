package com.shaft.listeners;

import org.testng.Assert;
import org.testng.IInvokedMethod;
import org.testng.IInvokedMethodListener;
import org.testng.ITestNGMethod;
import org.testng.ITestResult;
import org.testng.SkipException;

import com.shaft.browser.BrowserFactory;
import com.shaft.element.ElementActions;
import com.shaft.io.ReportManager;
import com.shaft.video.RecordManager;

public class InvokedMethodListener implements IInvokedMethodListener {
    private int invokedTestsCounter = 0;
    private int testSize = 0;

    @Override
    public void beforeInvocation(IInvokedMethod method, ITestResult testResult) {
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
		if (testMethod.getDescription() != null) {
		    ReportManager.logTestInformation(testMethod.getTestClass().getName(), testMethod.getMethodName(),
			    testMethod.getDescription());
		} else {
		    ReportManager.logTestInformation(testMethod.getTestClass().getName(), testMethod.getMethodName(),
			    "");
		}

		if (invokedTestsCounter == 0) {
		    RecordManager.startRecording();
		}
	    }

	    // implementing the new kill switch at the start of every test method
	    if (BrowserFactory.isKillSwitch()) {
		throw new SkipException("Skipping Test: " + testResult.getName());
	    }
	}
    }

    @Override
    public void afterInvocation(IInvokedMethod method, ITestResult testResult) {
	// attaching log and gif
	BrowserFactory.attachAnimatedGif();
	ReportManager.attachTestLog();

	if (!method.isConfigurationMethod()) {
	    ITestNGMethod testMethod = method.getTestMethod();
	    if (testMethod.isTest()) {
		updateTestStatusInCaseOfVerificationFailure(testResult);
		if (invokedTestsCounter == testSize - 1) {
		    // is last test in the last class of the test suite
		    ReportManager.logEngineVersion(false);
//		    ReportManager.triggerClosureActivitiesLogs();
//		    ReportManager.setDiscreteLogging(true);
//		    BrowserFactory.closeAllDrivers();
//		    ReportManager.setDiscreteLogging(false);
		    invokedTestsCounter = 0;
		} else {
		    invokedTestsCounter++;
		}
	    }
	}
	// resetting scope and config
	ElementActions.switchToDefaultContent();
	ReportManager.setDiscreteLogging(Boolean.valueOf(System.getProperty("alwaysLogDiscreetly")));
    }

    private void updateTestStatusInCaseOfVerificationFailure(ITestResult testResult) {
	if (testResult != null && testResult.getStatus() == ITestResult.FAILURE && testResult.getThrowable() != null) {
	    String failureMessage = testResult.getThrowable().getMessage();

	    if ((failureMessage != null) && failureMessage.contains("Verification")) {
		Assert.fail(testResult.getThrowable().getMessage());
	    }
	}
    }
}
