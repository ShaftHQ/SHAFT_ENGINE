package com.shaft.tools.io.internal;

import com.shaft.validation.internal.executor.Helper;
import io.qameta.allure.model.Link;
import io.qameta.allure.util.AnnotationUtils;
import org.testng.ITestNGMethod;
import org.testng.ITestResult;
import org.testng.internal.ConstructorOrMethod;

import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.Set;

public class IssueReporter {
    private static final List<List<String>> listOfOpenIssues = new ArrayList<>();
    private static final List<List<String>> listOfOpenIssuesForFailedTests = new ArrayList<>();
    private static final List<List<String>> listOfOpenIssuesForPassedTests = new ArrayList<>();
    private static final List<List<String>> listOfNewIssuesForFailedTests = new ArrayList<>();
    private static int openIssuesForFailedTestsCounter = 0;
    private static int openIssuesForPassedTestsCounter = 0;
    private static int newIssuesForFailedTestsCounter = 0;

    public static void updateTestStatusInCaseOfVerificationFailure(ITestResult testResult) {
        if (testResult != null && Helper.getVerificationErrorToForceFail() != null) {
            testResult.setStatus(ITestResult.FAILURE);
            testResult.setThrowable(Helper.getVerificationErrorToForceFail());
            Helper.resetVerificationStateAfterFailing();
        }
    }

    public static void updateIssuesLog(ITestResult testResult) {
        if (testResult != null) {
            ITestNGMethod testMethod = testResult.getMethod();
            if (testResult.getStatus() == ITestResult.SUCCESS) {
                // if test passed
                reportOpenIssueStatus(testMethod, true);
                ReportManagerHelper.extentReportsPass("Test Passed.");
            } else if (testResult.getStatus() == ITestResult.FAILURE) {
                // if test failed
                reportOpenIssueStatus(testMethod, false);
                if (testResult.getThrowable() != null) {
                    ReportManagerHelper.extentReportsFail(testResult.getThrowable());
                } else {
                    ReportManagerHelper.extentReportsFail("Test Failed.");
                }
            } else if (testResult.getStatus() == ITestResult.SKIP) {
                // if test skipped
                if (testResult.getThrowable() != null) {
                    ReportManagerHelper.extentReportsSkip(testResult.getThrowable());
                } else {
                    ReportManagerHelper.extentReportsSkip("Test Skipped as it depends on unsuccessfully executed methods.");
                }
            }
        }
    }

    private static void reportOpenIssueStatus(ITestNGMethod testMethod, Boolean executionStatus) {
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

    private static void logIssue(ITestNGMethod testMethod, int previouslyOpenedIssues, Boolean executionStatus) {
        // log issue
        String className = testMethod.getTestClass().getName();
        String methodName = testMethod.getMethodName();
        if (previouslyOpenedIssues < listOfOpenIssues.size()) {
            if (Boolean.TRUE.equals(executionStatus)) {
                // flag already opened issue for closure
                openIssuesForPassedTestsCounter++;
                ReportManagerHelper.setOpenIssuesForPassedTestsCounter(openIssuesForPassedTestsCounter);
                addNewIssue(className, methodName, listOfOpenIssuesForPassedTests);
                ReportManagerHelper.setListOfOpenIssuesForPassedTests(listOfOpenIssuesForPassedTests);
            } else {
                // confirm already opened issue
                openIssuesForFailedTestsCounter++;
                ReportManagerHelper.setOpenIssuesForFailedTestsCounter(openIssuesForFailedTestsCounter);
                addNewIssue(className, methodName, listOfOpenIssuesForFailedTests);
                ReportManagerHelper.setListOfOpenIssuesForFailedTests(listOfOpenIssuesForFailedTests);
            }
        } else {
            if (Boolean.FALSE.equals(executionStatus)) {
                // log new issue
                newIssuesForFailedTestsCounter++;
                ReportManagerHelper.setFailedTestsWithoutOpenIssuesCounter(newIssuesForFailedTestsCounter);
                List<String> newIssue = new ArrayList<>();
                newIssue.add(className);
                newIssue.add(methodName);
                listOfNewIssuesForFailedTests.add(newIssue);
                ReportManagerHelper.setListOfNewIssuesForFailedTests(listOfNewIssuesForFailedTests);
            }
        }
    }

    private static void addNewIssue(String className, String methodName, List<List<String>> listOfOpenIssuesForPassedTests) {
        List<String> newIssue = new ArrayList<>();
        newIssue.add(className);
        newIssue.add(methodName);
        newIssue.add(listOfOpenIssues.get(listOfOpenIssues.size() - 1).get(0));
        newIssue.add(listOfOpenIssues.get(listOfOpenIssues.size() - 1).get(1));
        listOfOpenIssuesForPassedTests.add(newIssue);
    }
}
