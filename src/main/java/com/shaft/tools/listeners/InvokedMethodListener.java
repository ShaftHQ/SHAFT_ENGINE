package com.shaft.tools.listeners;

import com.shaft.gui.browser.BrowserFactory;
import com.shaft.gui.element.ElementActions;
import com.shaft.gui.image.ScreenshotManager;
import com.shaft.gui.video.RecordManager;
import com.shaft.tools.io.ReportManager;
import com.shaft.validation.Verifications;
import io.qameta.allure.Issue;
import io.qameta.allure.Issues;
import io.qameta.allure.model.Link;
import io.qameta.allure.util.AnnotationUtils;
import org.testng.*;
import org.testng.internal.ConfigurationMethod;
import org.testng.internal.ConstructorOrMethod;

import java.lang.reflect.Method;
import java.util.*;

public class InvokedMethodListener implements IInvokedMethodListener {
    private final List<List<String>> listOfOpenIssues = new ArrayList<>();
    private final List<List<String>> listOfOpenIssuesForFailedTests = new ArrayList<>();
    private final List<List<String>> listOfOpenIssuesForPassedTests = new ArrayList<>();
    private final List<List<String>> listOfNewIssuesForFailedTests = new ArrayList<>();
    private int invokedTestsCounter = 0; // TODO: remove this variable
    private int testSize = 0;
    private int openIssuesForFailedTestsCounter = 0;
    private int openIssuesForPassedTestsCounter = 0;
    private int newIssuesForFailedTestsCounter = 0;

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
        if (!testMethod.getQualifiedName().contains("AbstractTestNGCucumberTests")) {
            if (testMethod.isTest()) {
                if (testMethod.getDescription() != null) {
                    ReportManager.logTestInformation(ReportManager.getTestClassName(), ReportManager.getTestMethodName(),
                            testMethod.getDescription());
                } else {
                    ReportManager.logTestInformation(ReportManager.getTestClassName(), ReportManager.getTestMethodName(), "");
                }
            } else if (testMethod instanceof ConfigurationMethod) {
                // org.testng.internal.ConfigurationMethod
                // ReportManager.logDiscrete("Current TestNG Method Name: " +
                // testMethod.getClass().getName());
                // configuration method information is not added to any logger (TestNG.Reporter)
                ReportManager.logConfigurationMethodInformation(testMethod.getTestClass().getName(),
                        testMethod.getMethodName());
            }
        }
        // implementing the new kill switch at the start of every test method
        if (BrowserFactory.isKillSwitch()) {
            SkipException ex = new SkipException("Skipping Test: " + testResult.getName());
            ReportManager.log(ex);
            throw ex;
        }

        if (Boolean.TRUE.equals(Boolean.valueOf(System.getProperty("skipTestsWithLinkedIssues")))) {
            Issue issue = testResult.getMethod().getConstructorOrMethod().getMethod().getAnnotation(Issue.class);
            if (issue != null) {
                SkipException ex = new SkipException("Skipping Test as it's expected to fail due to open issue: [" + issue.value() + "]");
                ReportManager.log(ex);
                throw ex;
            }
            Issues issues = testResult.getMethod().getConstructorOrMethod().getMethod().getAnnotation(Issues.class);
            if (issues != null) {
                StringBuilder issueNames = new StringBuilder();
                Arrays.stream(issues.value()).iterator().forEachRemaining(issueI -> issueNames.append(issueI.value()).append(" ,"));
                SkipException ex = new SkipException("Skipping Test as it's expected to fail due to open issues: [" + issueNames.substring(0, issueNames.length() - 2) + "]");
                ReportManager.log(ex);
                throw ex;
            }
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
        ReportManager.setDiscreteLogging(Boolean.parseBoolean(System.getProperty("alwaysLogDiscreetly")));
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
                addNewIssue(className, methodName, listOfOpenIssuesForPassedTests);
                ReportManager.setListOfOpenIssuesForPassedTests(listOfOpenIssuesForPassedTests);
            } else {
                // confirm already opened issue
                openIssuesForFailedTestsCounter++;
                ReportManager.setOpenIssuesForFailedTestsCounter(openIssuesForFailedTestsCounter);
                addNewIssue(className, methodName, listOfOpenIssuesForFailedTests);
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

    private void addNewIssue(String className, String methodName, List<List<String>> listOfOpenIssuesForPassedTests) {
        List<String> newIssue = new ArrayList<>();
        newIssue.add(className);
        newIssue.add(methodName);
        newIssue.add(listOfOpenIssues.get(listOfOpenIssues.size() - 1).get(0));
        newIssue.add(listOfOpenIssues.get(listOfOpenIssues.size() - 1).get(1));
        listOfOpenIssuesForPassedTests.add(newIssue);
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