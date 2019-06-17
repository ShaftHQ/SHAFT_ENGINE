package com.shaft.listeners;

import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.Set;

import org.testng.Assert;
import org.testng.IInvokedMethod;
import org.testng.IInvokedMethodListener;
import org.testng.ITestNGMethod;
import org.testng.ITestResult;
import org.testng.SkipException;
import org.testng.internal.ConstructorOrMethod;

import com.shaft.browser.BrowserFactory;
import com.shaft.element.ElementActions;
import com.shaft.io.ReportManager;
import com.shaft.video.RecordManager;

import io.qameta.allure.model.Link;
import io.qameta.allure.util.AnnotationUtils;

public class InvokedMethodListener implements IInvokedMethodListener {
    private int invokedTestsCounter = 0;
    private int testSize = 0;
    private List<List<String>> listOfOpenIssues = new ArrayList<>();
    private int openIssuesForFailedTestsCounter = 0;
    private int openIssuesForPassedTestsCounter = 0;
    private int failedTestsWithoutOpenIssuesCounter = 0;
//    private int flakyTestsCounter = 0;

    @Override
    public void beforeInvocation(IInvokedMethod method, ITestResult testResult) {
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
		ReportManager.logTestInformation(testMethod.getTestClass().getName(), testMethod.getMethodName(), "");
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

    @Override
    public void afterInvocation(IInvokedMethod method, ITestResult testResult) {
	if (!method.getTestMethod().getQualifiedName().contains("closureActivities")) {
	    // attaching log and gif for test methods only
	    BrowserFactory.attachAnimatedGif();
	    ReportManager.attachTestLog();
	}

	// resetting scope and config
	ElementActions.switchToDefaultContent();
	ReportManager.setDiscreteLogging(Boolean.valueOf(System.getProperty("alwaysLogDiscreetly")));
	ITestNGMethod testMethod = method.getTestMethod();
	if (testMethod.isTest()) {
	    updateTestStatusInCaseOfVerificationFailure(testResult);
	    updateIssuesLog(testResult, testMethod);
	    if (invokedTestsCounter == testSize - 1) {
		// is last test in the last class of the test suite
		ReportManager.logEngineVersion(false);
		invokedTestsCounter = 0;
		ReportManager.logIssuesSummary(openIssuesForFailedTestsCounter, openIssuesForPassedTestsCounter,
			failedTestsWithoutOpenIssuesCounter);
	    } else {
		invokedTestsCounter++;
	    }
	}
    }

    private void updateTestStatusInCaseOfVerificationFailure(ITestResult testResult) {
	if (testResult != null && testResult.getStatus() == ITestResult.FAILURE && testResult.getThrowable() != null) {
	    String failureMessage = testResult.getThrowable().getMessage();
	    if ((failureMessage != null) && failureMessage.contains("Verification")) {
		Assert.fail(testResult.getThrowable().getMessage());
	    }
	}
    }

    private void reportOpenIssueStatus(ITestResult testResult, ITestNGMethod testMethod, Boolean executionStatus) {
	Optional<Method> method = Optional.ofNullable(testMethod).map(ITestNGMethod::getConstructorOrMethod)
		.map(ConstructorOrMethod::getMethod);
	if (method.isPresent()) {
	    Set<Link> links = method.map(AnnotationUtils::getLinks).get();
	    int previouslyOpenedIssues = listOfOpenIssues.size();
	    links.forEach(link -> {
		if (link.getType().equals("issue")) {
		    List<String> newIssue = new ArrayList<String>();
		    newIssue.add(link.getName());
		    newIssue.add(link.getUrl());
		    listOfOpenIssues.add(newIssue);
		}
	    });
	    // log issue
	    String className = testMethod.getTestClass().getName();
	    String methodName = testMethod.getMethodName();
	    if (previouslyOpenedIssues < listOfOpenIssues.size()) {
		if (executionStatus) {
		    openIssuesForPassedTestsCounter++;
		    // flag already opened issue for closure
		    ReportManager.logIssue("Test Method \"" + className + "." + methodName
			    + "\" passed. Please validate and close this open issue \""
			    + listOfOpenIssues.get(listOfOpenIssues.size() - 1).get(0) + "\": \""
			    + listOfOpenIssues.get(listOfOpenIssues.size() - 1).get(1) + "\".\n");
		} else {
		    openIssuesForFailedTestsCounter++;
		    // confirm already opened issue
		    ReportManager.logIssue("Test Method \"" + className + "." + methodName
			    + "\" failed with open issue \"" + listOfOpenIssues.get(listOfOpenIssues.size() - 1).get(0)
			    + "\": \"" + listOfOpenIssues.get(listOfOpenIssues.size() - 1).get(1) + "\".\n");
		}
	    } else {
		if (!executionStatus) {
		    // log new issue
		    failedTestsWithoutOpenIssuesCounter++;
		    ReportManager.logIssue("Test Method \"" + className + "." + methodName
			    + "\" failed. Please investigate and open a new Issue if needed.\n");
		}
	    }
	}
    }

    private void updateIssuesLog(ITestResult testResult, ITestNGMethod testMethod) {
	if (testResult != null && testResult.getStatus() == ITestResult.SUCCESS) {
	    // if test passed
	    reportOpenIssueStatus(testResult, testMethod, true);
	} else if (testResult != null && testResult.getStatus() == ITestResult.FAILURE) {
	    // if test failed
	    reportOpenIssueStatus(testResult, testMethod, false);
	}
    }

//    private void updateFlakyTestsLog(ITestResult testResult, ITestNGMethod testMethod) {
//	if (testResult != null && testResult.getStatus() == ITestResult.FAILURE) {
//	    // if test failed
//	    Optional<Method> method = Optional.ofNullable(testMethod).map(ITestNGMethod::getConstructorOrMethod)
//		    .map(ConstructorOrMethod::getMethod);
//	    if (method.isPresent()) {
//		if (method.get().isAnnotationPresent(Flaky.class)) {
//		    flakyTestsCounter++;
//		}
//	    }
//	}
//    }
}