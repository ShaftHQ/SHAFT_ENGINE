package com.shaft.listeners.internal;

import com.shaft.driver.SHAFT;
import com.shaft.tools.internal.tms.XrayIntegrationHelper;
import com.shaft.tools.io.internal.ReportManagerHelper;
import io.qameta.allure.*;
import org.testng.IInvokedMethod;
import org.testng.ITestNGMethod;
import org.testng.ITestResult;

import java.lang.annotation.Annotation;
import java.util.List;

import static com.shaft.tools.internal.tms.XrayIntegrationHelper.createIssue;
import static com.shaft.tools.internal.tms.XrayIntegrationHelper.link2Tickets;

public class JiraHelper {
    public static void reportExecutionStatusToJira() {
        if (SHAFT.Properties.jira.isEnabled() && SHAFT.Properties.jira.reportTestCasesExecution()) {
            try {
                var reportPath = SHAFT.Properties.jira.reportPath();
                if (reportPath.contains("testng-results.xml")) {
                    XrayIntegrationHelper.importTestNGResults(reportPath);
                } else if (reportPath.contains("cucumber.json")) {
                    XrayIntegrationHelper.importCucumberResults(reportPath);
                }

                XrayIntegrationHelper.renameTestExecutionSuit(SHAFT.Properties.jira.executionName(),
                        SHAFT.Properties.jira.executionDescription());

            } catch (Exception e) {
                ReportManagerHelper.logDiscrete(e);
            }
        }
    }

    /**
     * The method is to update testng-results.xml with the required data to integrate with xray plugin
     * The Method uses Epic, Feature, Story and Test allure annotations' values.
     *
     * @see Allure
     */
    public static void prepareTestResultAttributes(IInvokedMethod method, ITestResult testResult) {
        if (method.isTestMethod()) {
            if (annotationPresent(method, Feature.class))
                testResult.setAttribute("requirement", method.getTestMethod().getConstructorOrMethod().getMethod().getAnnotation(Feature.class).value());
            if (annotationPresent(method, Epic.class))
                testResult.setAttribute("requirement", method.getTestMethod().getConstructorOrMethod().getMethod().getAnnotation(Epic.class).value());
            if (annotationPresent(method, Story.class))
                testResult.setAttribute("requirement", method.getTestMethod().getConstructorOrMethod().getMethod().getAnnotation(Story.class).value());
            if (annotationPresent(method, TmsLink.class))
                testResult.setAttribute("test", method.getTestMethod().getConstructorOrMethod().getMethod().getAnnotation(TmsLink.class).value());
            //TO-DO: testResult.setAttribute("labels", method.getTestMethod().getConstructorOrMethod().getMethod().getAnnotation().labels());
        }
    }

    @SuppressWarnings("unchecked")
    private static boolean annotationPresent(IInvokedMethod method, Class<?> clazz) {
        return method.getTestMethod().getConstructorOrMethod().getMethod().isAnnotationPresent((Class<? extends Annotation>) clazz);
    }

    /**
     * is called in afterInvocation() to report bugs in case of failure and if the integration is enabled
     */
    public static void reportBugsToJIRA(List<String> attachments, String logText, ITestResult iTestResult, ITestNGMethod iTestNGMethod) {
        if (!iTestResult.isSuccess()
                && SHAFT.Properties.jira.isEnabled()
                && SHAFT.Properties.jira.reportBugs()) {
            String bugID = createIssue(attachments, ReportManagerHelper.getTestMethodName(), logText);
            if (bugID != null
                    && iTestNGMethod.isTest() && iTestNGMethod.getConstructorOrMethod().getMethod().isAnnotationPresent(TmsLink.class))
                link2Tickets(bugID, iTestNGMethod.getConstructorOrMethod().getMethod().getAnnotation(TmsLink.class).value());
        }
    }
}
