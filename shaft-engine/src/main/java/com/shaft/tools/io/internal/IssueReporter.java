package com.shaft.tools.io.internal;

import com.shaft.validation.internal.ValidationsHelper;
import com.shaft.listeners.internal.TestExecutionInfo;
import io.qameta.allure.model.Link;
import io.qameta.allure.util.AnnotationUtils;
import org.testng.ITestNGMethod;
import org.testng.ITestResult;
import org.testng.internal.ConstructorOrMethod;

import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.concurrent.atomic.AtomicInteger;

public class IssueReporter {
    private static final List<List<String>> listOfOpenIssues = Collections.synchronizedList(new ArrayList<>());
    private static final List<List<String>> listOfOpenIssuesForFailedTests = Collections.synchronizedList(new ArrayList<>());
    private static final List<List<String>> listOfOpenIssuesForPassedTests = Collections.synchronizedList(new ArrayList<>());
    private static final List<List<String>> listOfNewIssuesForFailedTests = Collections.synchronizedList(new ArrayList<>());
    private static final AtomicInteger openIssuesForFailedTestsCounter = new AtomicInteger(0);
    private static final AtomicInteger openIssuesForPassedTestsCounter = new AtomicInteger(0);
    private static final AtomicInteger newIssuesForFailedTestsCounter = new AtomicInteger(0);

    public static void updateTestStatusInCaseOfVerificationFailure(ITestResult testResult) {
        if (testResult != null && ValidationsHelper.getVerificationErrorToForceFail() != null) {
            testResult.setStatus(ITestResult.FAILURE);
            testResult.setThrowable(ValidationsHelper.getVerificationErrorToForceFail());
            ValidationsHelper.resetVerificationStateAfterFailing();
        }
    }

    public static void updateIssuesLog(ITestResult testResult) {
        if (testResult != null && !testResult.wasRetried()) {
            ITestNGMethod testMethod = testResult.getMethod();
            if (testResult.getStatus() == ITestResult.SUCCESS) {
                // if test passed
                reportOpenIssueStatus(testMethod, true);
            } else if (testResult.getStatus() == ITestResult.FAILURE) {
                // if test failed
                reportOpenIssueStatus(testMethod, false);
            }
        }
    }

    public static void updateIssuesLog(TestExecutionInfo info, boolean executionStatus) {
        if (info != null && !info.retried()) {
            reportOpenIssueStatus(info.method(), info.className(), info.methodName(), executionStatus);
        }
    }

    private static void reportOpenIssueStatus(ITestNGMethod testMethod, Boolean executionStatus) {
        Optional<Method> method = Optional.ofNullable(testMethod).map(ITestNGMethod::getConstructorOrMethod)
                .map(ConstructorOrMethod::getMethod);
        String className = testMethod == null || testMethod.getTestClass() == null ? "" : testMethod.getTestClass().getName();
        String methodName = testMethod == null ? "" : testMethod.getMethodName();
        method.ifPresent(value -> reportOpenIssueStatus(value, className, methodName, executionStatus));
    }

    private static void reportOpenIssueStatus(Method method, String className, String methodName, Boolean executionStatus) {
        Optional<Method> optionalMethod = Optional.ofNullable(method);
        if (optionalMethod.isPresent()) {
            Set<Link> links = optionalMethod.map(AnnotationUtils::getLinks).orElse(null);
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
            logIssue(className, methodName, previouslyOpenedIssues, executionStatus);
        }
    }

    private static void logIssue(String className, String methodName, int previouslyOpenedIssues, Boolean executionStatus) {
        // log issue
        if (previouslyOpenedIssues < listOfOpenIssues.size()) {
            if (Boolean.TRUE.equals(executionStatus)) {
                // flag already opened issue for closure
                ReportManagerHelper.setOpenIssuesForPassedTestsCounter(openIssuesForPassedTestsCounter.incrementAndGet());
                addNewIssue(className, methodName, listOfOpenIssuesForPassedTests);
                ReportManagerHelper.setListOfOpenIssuesForPassedTests(listOfOpenIssuesForPassedTests);
            } else {
                // confirm already opened issue
                ReportManagerHelper.setOpenIssuesForFailedTestsCounter(openIssuesForFailedTestsCounter.incrementAndGet());
                addNewIssue(className, methodName, listOfOpenIssuesForFailedTests);
                ReportManagerHelper.setListOfOpenIssuesForFailedTests(listOfOpenIssuesForFailedTests);
            }
        } else {
            if (Boolean.FALSE.equals(executionStatus)) {
                // log new issue
                ReportManagerHelper.setFailedTestsWithoutOpenIssuesCounter(newIssuesForFailedTestsCounter.incrementAndGet());
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
        synchronized (listOfOpenIssues) {
            List<String> lastIssue = listOfOpenIssues.get(listOfOpenIssues.size() - 1);
            newIssue.add(lastIssue.get(0));
            newIssue.add(lastIssue.get(1));
        }
        listOfOpenIssuesForPassedTests.add(newIssue);
    }
}
