package com.shaft.tools.listeners;

import com.shaft.driver.DriverFactoryHelper;
import com.shaft.gui.element.ElementActions;
import com.shaft.gui.image.ScreenshotManager;
import com.shaft.gui.video.RecordManager;
import com.shaft.tools.io.ReportManagerHelper;
import com.shaft.validation.ValidationsHelper;
import io.qameta.allure.*;
import io.qameta.allure.model.Link;
import io.qameta.allure.util.AnnotationUtils;
import org.testng.*;
import org.testng.internal.ConfigurationMethod;
import org.testng.internal.ConstructorOrMethod;
import java.lang.reflect.Method;
import java.util.*;
import static com.shaft.tools.tms.XrayIntegrationHelper.*;

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
            ReportManagerHelper.log(e);
        }
        ITestNGMethod testMethod = method.getTestMethod();

        String className;
        String methodName;
        String methodDescription = "";

        if (!testMethod.getQualifiedName().contains("AbstractTestNGCucumberTests")) {
            if (testMethod.isTest()) {
                className = ReportManagerHelper.getTestClassName();
                methodName = ReportManagerHelper.getTestMethodName();
                if (testMethod.getDescription() != null) {
                    methodDescription = testMethod.getDescription();
                }

                ReportManagerHelper.logTestInformation(className, methodName, methodDescription);
                ReportManagerHelper.extentReportsCreateTest(className + "." + methodName, methodDescription);
            } else if (testMethod instanceof ConfigurationMethod) {
                // org.testng.internal.ConfigurationMethod
                // ReportManager.logDiscrete("Current TestNG Method Name: " +
                // testMethod.getClass().getName());
                // configuration method information is not added to any logger (TestNG.Reporter)
                className = testMethod.getTestClass().getName();
                methodName = testMethod.getMethodName();

                ReportManagerHelper.logConfigurationMethodInformation(className, methodName);
                ReportManagerHelper.extentReportsReset();
            }
        }
        // implementing the new kill switch at the start of every test method
        if (DriverFactoryHelper.isKillSwitch()) {
            SkipException ex = new SkipException("Skipping Test: " + testResult.getName());
            ReportManagerHelper.log(ex);
            throw ex;
        }

        if (Boolean.TRUE.equals(Boolean.valueOf(System.getProperty("skipTestsWithLinkedIssues")))) {
            Issue issue = testResult.getMethod().getConstructorOrMethod().getMethod().getAnnotation(Issue.class);
            if (issue != null) {
                SkipException ex = new SkipException("Skipping Test as it's expected to fail due to open issue: [" + issue.value() + "]");
                ReportManagerHelper.log(ex);
                throw ex;
            }
            Issues issues = testResult.getMethod().getConstructorOrMethod().getMethod().getAnnotation(Issues.class);
            if (issues != null) {
                StringBuilder issueNames = new StringBuilder();
                Arrays.stream(issues.value()).iterator().forEachRemaining(issueI -> issueNames.append(issueI.value()).append(" ,"));
                SkipException ex = new SkipException("Skipping Test as it's expected to fail due to open issues: [" + issueNames.substring(0, issueNames.length() - 2) + "]");
                ReportManagerHelper.log(ex);
                throw ex;
            }
        }
    }

    @Override
    public void afterInvocation(IInvokedMethod method, ITestResult testResult) {

        if (!method.getTestMethod().getQualifiedName().contains("setupActivities")
        && !method.getTestMethod().getQualifiedName().contains("teardownActivities")) {
            List<String> attachments = new ArrayList<>();
            String attachment;
            if (System.getProperty("videoParams_scope").trim().equals("TestMethod")) {
                attachment=RecordManager.attachVideoRecording();
                if(!attachment.equals(""))
                attachments.add(attachment);
            }
            attachment= ScreenshotManager.attachAnimatedGif();
            if(!attachment.equals(""))
                attachments.add(attachment);
            // configuration method attachment is not added to the report (Allure ->
            // threadContext.getCurrent(); -> empty)
            String logText= createTestLog(Reporter.getOutput(testResult));
            ReportManagerHelper.attachTestLog(testResult.getMethod().getMethodName(),
                    logText);
            reportBugsToJIRA(attachments,logText,method,testResult);
        }

        // resetting scope and config
        if (!DriverFactoryHelper.isMobileNativeExecution()) {
            ElementActions.switchToDefaultContent();
        }
        ReportManagerHelper.setDiscreteLogging(Boolean.parseBoolean(System.getProperty("alwaysLogDiscreetly")));
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
        if (testResult != null && ValidationsHelper.getVerificationErrorToForceFail() != null) {
            testResult.setStatus(ITestResult.FAILURE);
            testResult.setThrowable(ValidationsHelper.getVerificationErrorToForceFail());
            ValidationsHelper.resetVerificationStateAfterFailing();
        }
    }

    private void updateIssuesLog(ITestResult testResult, ITestNGMethod testMethod) {
        if (testResult != null && testResult.getStatus() == ITestResult.SUCCESS) {
            // if test passed
            reportOpenIssueStatus(testMethod, true);
            ReportManagerHelper.extentReportsPass("Test Passed.");
        } else if (testResult != null && testResult.getStatus() == ITestResult.FAILURE) {
            // if test failed
            reportOpenIssueStatus(testMethod, false);
            if (testResult.getThrowable() != null) {
                ReportManagerHelper.extentReportsFail(testResult.getThrowable());
            } else {
                ReportManagerHelper.extentReportsFail("Test Failed.");
            }
        } else if (testResult != null && testResult.getStatus() == ITestResult.SKIP) {
            // if test skipped
            if (testResult.getThrowable() != null) {
                ReportManagerHelper.extentReportsSkip(testResult.getThrowable());
            } else {
                ReportManagerHelper.extentReportsSkip("Test Skipped as it depends on unsuccessfully executed methods.");
            }
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

    private void addNewIssue(String className, String methodName, List<List<String>> listOfOpenIssuesForPassedTests) {
        List<String> newIssue = new ArrayList<>();
        newIssue.add(className);
        newIssue.add(methodName);
        newIssue.add(listOfOpenIssues.get(listOfOpenIssues.size() - 1).get(0));
        newIssue.add(listOfOpenIssues.get(listOfOpenIssues.size() - 1).get(1));
        listOfOpenIssuesForPassedTests.add(newIssue);
    }

    public static String createTestLog(List<String> output) {
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
    /**
     * is called in afterInvocation() to report bugs in case of failure and if the integration is enabled
     * */
    private void reportBugsToJIRA(List<String> attachments,String logText,IInvokedMethod method, ITestResult testResult)
    {
        if(!testResult.isSuccess()
                &&System.getProperty("jiraInteraction").trim().equals("true")
                &&System.getProperty("ReportBugs").trim().equals("true"))
        {
            String bugID= createIssue(attachments, ReportManagerHelper.getTestMethodName(),logText);
            if(bugID!=null
                    &&method.isTestMethod()&& method.getTestMethod().getConstructorOrMethod().getMethod().isAnnotationPresent(TmsLink.class))
                link2Tickets(bugID,method.getTestMethod().getConstructorOrMethod().getMethod().getAnnotation(TmsLink.class).value());
        }
    }
}