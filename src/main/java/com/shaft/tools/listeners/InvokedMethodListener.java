package com.shaft.tools.listeners;

import com.shaft.gui.browser.BrowserFactory;
import com.shaft.gui.element.ElementActions;
import com.shaft.gui.image.ScreenshotManager;
import com.shaft.gui.video.RecordManager;
import com.shaft.tools.io.ReportManager;
import com.shaft.validation.Verifications;
import io.qameta.allure.model.Link;
import io.qameta.allure.util.AnnotationUtils;
import org.testng.*;
import org.testng.internal.ConfigurationMethod;
import org.testng.internal.ConstructorOrMethod;

import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.Set;

public class InvokedMethodListener implements IInvokedMethodListener {
    private final List<List<String>> listOfOpenIssues = new ArrayList<>();
    private final List<List<String>> listOfOpenIssuesForFailedTests = new ArrayList<>();
    // class name, method name, link name, link url
    private final List<List<String>> listOfOpenIssuesForPassedTests = new ArrayList<>();
    // class name, method name, link name, link url
    private final List<List<String>> listOfNewIssuesForFailedTests = new ArrayList<>();
    private int invokedTestsCounter = 0; // TODO: remove this variable
    private int testSize = 0;
    // link name, link url
    private int openIssuesForFailedTestsCounter = 0;
    private int openIssuesForPassedTestsCounter = 0;
    private int newIssuesForFailedTestsCounter = 0;
    // class name, method name

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
        } else if (testMethod instanceof ConfigurationMethod) {
            // org.testng.internal.ConfigurationMethod
            // ReportManager.logDiscrete("Current TestNG Method Name: " +
            // testMethod.getClass().getName());
            // configuration method information is not added to any logger (TestNG.Reporter)
            ReportManager.logConfigurationMethodInformation(testMethod.getTestClass().getName(),
                    testMethod.getMethodName());
        }
        // implementing the new kill switch at the start of every test method
        if (BrowserFactory.isKillSwitch()) {
            throw new SkipException("Skipping Test: " + testResult.getName());
        }
    }

    @Override
    public void afterInvocation(IInvokedMethod method, ITestResult testResult) {
        if (!method.getTestMethod().getQualifiedName().contains("closureActivities")) {
            RecordManager.attachVideoRecording();
            ScreenshotManager.attachAnimatedGif();
            // configuration method attachment is not added to the report (Allure ->
            // threadContext.getCurrent(); -> empty)
            ReportManager.attachTestLog(testResult.getMethod().getMethodName(),
                    createTestLog(Reporter.getOutput(testResult)));
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
                invokedTestsCounter = 0;
            } else {
                invokedTestsCounter++;
            }
        }
    }

    private void updateTestStatusInCaseOfVerificationFailure(ITestResult testResult) {
        if (testResult != null && Verifications.getVerificationErrorToForceFail() != null) {
            testResult.setStatus(ITestResult.FAILURE);
            testResult.setThrowable(Verifications.getVerificationErrorToForceFail());
            Verifications.resetVerificationStateAfterFailing();
        }
    }

    private void updateIssuesLog(ITestResult testResult, ITestNGMethod testMethod) {
        if (testResult != null && testResult.getStatus() == ITestResult.SUCCESS) {
            // if test passed
            reportOpenIssueStatus(testMethod, true);
        } else if (testResult != null && testResult.getStatus() == ITestResult.FAILURE) {
            // if test failed
            reportOpenIssueStatus(testMethod, false);
        }
    }

    private void reportOpenIssueStatus(ITestNGMethod testMethod, Boolean executionStatus) {
        Optional<Method> method = Optional.ofNullable(testMethod).map(ITestNGMethod::getConstructorOrMethod)
                .map(ConstructorOrMethod::getMethod);
        if (method.isPresent()) {
            Set<Link> links = method.map(AnnotationUtils::getLinks).orElse(null);
            int previouslyOpenedIssues = listOfOpenIssues.size();
            if (links != null) {
                links.forEach(link -> {
                    if (link.getType().equals("issue")) {
                        List<String> newIssue = new ArrayList<>();
                        newIssue.add(link.getName());
                        newIssue.add(link.getUrl());
                        listOfOpenIssues.add(newIssue);
                    }
                });
            }
            // log issue
            logIssue(testMethod, previouslyOpenedIssues, executionStatus);
        }
    }

    private void logIssue(ITestNGMethod testMethod, int previouslyOpenedIssues, Boolean executionStatus) {
        // log issue
        String className = testMethod.getTestClass().getName();
        String methodName = testMethod.getMethodName();
        if (previouslyOpenedIssues < listOfOpenIssues.size()) {
            if (Boolean.TRUE.equals(executionStatus)) {
                // flag already opened issue for closure
                openIssuesForPassedTestsCounter++;
                ReportManager.setOpenIssuesForPassedTestsCounter(openIssuesForPassedTestsCounter);
                List<String> newIssue = new ArrayList<>();
                newIssue.add(className);
                newIssue.add(methodName);
                newIssue.add(listOfOpenIssues.get(listOfOpenIssues.size() - 1).get(0));
                newIssue.add(listOfOpenIssues.get(listOfOpenIssues.size() - 1).get(1));
                listOfOpenIssuesForPassedTests.add(newIssue);
                ReportManager.setListOfOpenIssuesForPassedTests(listOfOpenIssuesForPassedTests);
            } else {
                // confirm already opened issue
                openIssuesForFailedTestsCounter++;
                ReportManager.setOpenIssuesForFailedTestsCounter(openIssuesForFailedTestsCounter);
                List<String> newIssue = new ArrayList<>();
                newIssue.add(className);
                newIssue.add(methodName);
                newIssue.add(listOfOpenIssues.get(listOfOpenIssues.size() - 1).get(0));
                newIssue.add(listOfOpenIssues.get(listOfOpenIssues.size() - 1).get(1));
                listOfOpenIssuesForFailedTests.add(newIssue);
                ReportManager.setListOfOpenIssuesForFailedTests(listOfOpenIssuesForFailedTests);
            }
        } else {
            if (Boolean.FALSE.equals(executionStatus)) {
                // log new issue
                newIssuesForFailedTestsCounter++;
                ReportManager.setFailedTestsWithoutOpenIssuesCounter(newIssuesForFailedTestsCounter);
                List<String> newIssue = new ArrayList<>();
                newIssue.add(className);
                newIssue.add(methodName);
                listOfNewIssuesForFailedTests.add(newIssue);
                ReportManager.setListOfNewIssuesForFailedTests(listOfNewIssuesForFailedTests);
            }
        }
    }

    private String createTestLog(List<String> output) {
        StringBuilder builder = new StringBuilder();
        for (String each : output) {
            builder.append(each).append(System.lineSeparator());
        }
        String testLog = builder.toString();
        if (testLog.length() >= 2) {
            // Removing the last ","
            return testLog.substring(0, builder.length() - 2);
        } else {
            return testLog;
        }

    }
}