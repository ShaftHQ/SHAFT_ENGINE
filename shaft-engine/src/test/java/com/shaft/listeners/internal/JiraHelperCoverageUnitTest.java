package com.shaft.listeners.internal;

import com.shaft.driver.SHAFT;
import com.shaft.tools.internal.tms.XrayIntegrationHelper;
import io.qameta.allure.Epic;
import io.qameta.allure.Feature;
import io.qameta.allure.Story;
import io.qameta.allure.TmsLink;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import org.testng.IInvokedMethod;
import org.testng.ITestNGMethod;
import org.testng.ITestResult;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;
import org.testng.internal.ConstructorOrMethod;

import java.lang.reflect.Method;
import java.util.List;

/**
 * Unit coverage for {@link JiraHelper} (issue #7077). No live Jira.
 */
@Test(singleThreaded = true)
public class JiraHelperCoverageUnitTest {
    private boolean savedEnabled;
    private boolean savedReportCases;
    private boolean savedReportBugs;
    private String savedReportPath;
    private String savedExecutionName;
    private String savedExecutionDescription;

    @BeforeMethod
    public void saveJiraProps() {
        savedEnabled = SHAFT.Properties.jira.isEnabled();
        savedReportCases = SHAFT.Properties.jira.reportTestCasesExecution();
        savedReportBugs = SHAFT.Properties.jira.reportBugs();
        savedReportPath = SHAFT.Properties.jira.reportPath();
        savedExecutionName = SHAFT.Properties.jira.executionName();
        savedExecutionDescription = SHAFT.Properties.jira.executionDescription();
    }

    @AfterMethod(alwaysRun = true)
    public void restoreJiraProps() {
        SHAFT.Properties.jira.set()
                .jiraInteraction(savedEnabled)
                .reportTestCasesExecution(savedReportCases)
                .reportBugs(savedReportBugs)
                .reportPath(savedReportPath)
                .executionName(savedExecutionName)
                .executionDescription(savedExecutionDescription);
    }

    @Test
    public void reportExecutionStatusShouldNoOpWhenDisabled() {
        SHAFT.Properties.jira.set().jiraInteraction(false).reportTestCasesExecution(true);
        try (MockedStatic<XrayIntegrationHelper> xray = Mockito.mockStatic(XrayIntegrationHelper.class)) {
            JiraHelper.reportExecutionStatusToJira();
            xray.verifyNoInteractions();
        }
    }

    @Test
    public void reportExecutionStatusShouldImportTestNgResults() {
        SHAFT.Properties.jira.set()
                .jiraInteraction(true)
                .reportTestCasesExecution(true)
                .reportPath("/tmp/testng-results.xml")
                .executionName("exec")
                .executionDescription("desc");
        try (MockedStatic<XrayIntegrationHelper> xray = Mockito.mockStatic(XrayIntegrationHelper.class)) {
            JiraHelper.reportExecutionStatusToJira();
            xray.verify(() -> XrayIntegrationHelper.importTestNGResults("/tmp/testng-results.xml"));
            xray.verify(() -> XrayIntegrationHelper.renameTestExecutionSuit("exec", "desc"));
        }
    }

    @Test
    public void reportExecutionStatusShouldImportCucumberAndJunitPaths() {
        SHAFT.Properties.jira.set().jiraInteraction(true).reportTestCasesExecution(true);
        try (MockedStatic<XrayIntegrationHelper> xray = Mockito.mockStatic(XrayIntegrationHelper.class)) {
            SHAFT.Properties.jira.set().reportPath("/tmp/cucumber.json");
            JiraHelper.reportExecutionStatusToJira();
            xray.verify(() -> XrayIntegrationHelper.importCucumberResults("/tmp/cucumber.json"));

            SHAFT.Properties.jira.set().reportPath("/tmp/junit-report.xml");
            JiraHelper.reportExecutionStatusToJira();
            xray.verify(() -> XrayIntegrationHelper.importJunitResults("/tmp/junit-report.xml"));
        }
    }

    @Test
    public void reportExecutionStatusShouldSwallowHelperFailures() {
        SHAFT.Properties.jira.set()
                .jiraInteraction(true)
                .reportTestCasesExecution(true)
                .reportPath("/tmp/testng-results.xml");
        try (MockedStatic<XrayIntegrationHelper> xray = Mockito.mockStatic(XrayIntegrationHelper.class)) {
            xray.when(() -> XrayIntegrationHelper.importTestNGResults(Mockito.anyString()))
                    .thenThrow(new RuntimeException("xray down"));
            JiraHelper.reportExecutionStatusToJira();
        }
    }

    @Test
    public void prepareTestResultAttributesShouldCopyAllureAnnotations() throws Exception {
        Method annotated = AnnotatedFixture.class.getDeclaredMethod("annotatedCase");
        IInvokedMethod method = Mockito.mock(IInvokedMethod.class);
        ITestNGMethod testNgMethod = Mockito.mock(ITestNGMethod.class);
        ConstructorOrMethod constructorOrMethod = Mockito.mock(ConstructorOrMethod.class);
        ITestResult result = Mockito.mock(ITestResult.class);

        Mockito.when(method.isTestMethod()).thenReturn(true);
        Mockito.when(method.getTestMethod()).thenReturn(testNgMethod);
        Mockito.when(testNgMethod.getConstructorOrMethod()).thenReturn(constructorOrMethod);
        Mockito.when(constructorOrMethod.getMethod()).thenReturn(annotated);

        JiraHelper.prepareTestResultAttributes(method, result);

        Mockito.verify(result, Mockito.atLeastOnce()).setAttribute(Mockito.eq("requirement"), Mockito.anyString());
        Mockito.verify(result).setAttribute("test", "TMS-9");
    }

    @Test
    public void prepareTestResultAttributesShouldSkipNonTestMethods() {
        IInvokedMethod method = Mockito.mock(IInvokedMethod.class);
        ITestResult result = Mockito.mock(ITestResult.class);
        Mockito.when(method.isTestMethod()).thenReturn(false);
        JiraHelper.prepareTestResultAttributes(method, result);
        Mockito.verifyNoInteractions(result);
    }

    @Test
    public void reportBugsToJiraShouldCreateAndLinkWhenConfigured() throws Exception {
        Method annotated = AnnotatedFixture.class.getDeclaredMethod("annotatedCase");
        ITestResult result = Mockito.mock(ITestResult.class);
        ITestNGMethod testNgMethod = Mockito.mock(ITestNGMethod.class);
        ConstructorOrMethod constructorOrMethod = Mockito.mock(ConstructorOrMethod.class);
        Mockito.when(result.isSuccess()).thenReturn(false);
        Mockito.when(testNgMethod.isTest()).thenReturn(true);
        Mockito.when(testNgMethod.getConstructorOrMethod()).thenReturn(constructorOrMethod);
        Mockito.when(constructorOrMethod.getMethod()).thenReturn(annotated);

        SHAFT.Properties.jira.set().jiraInteraction(true).reportBugs(true);
        try (MockedStatic<XrayIntegrationHelper> xray = Mockito.mockStatic(XrayIntegrationHelper.class)) {
            xray.when(() -> XrayIntegrationHelper.createIssue(Mockito.anyList(), Mockito.anyString(), Mockito.anyString()))
                    .thenReturn("BUG-1");
            JiraHelper.reportBugsToJIRA(List.of("a.png"), "failure log", result, testNgMethod);
            xray.verify(() -> XrayIntegrationHelper.link2Tickets("BUG-1", "TMS-9"));
        }
    }

    @Test
    public void reportBugsToJiraShouldNoOpOnSuccessOrWhenDisabled() {
        ITestResult result = Mockito.mock(ITestResult.class);
        ITestNGMethod testNgMethod = Mockito.mock(ITestNGMethod.class);
        Mockito.when(result.isSuccess()).thenReturn(true);
        SHAFT.Properties.jira.set().jiraInteraction(true).reportBugs(true);
        try (MockedStatic<XrayIntegrationHelper> xray = Mockito.mockStatic(XrayIntegrationHelper.class)) {
            JiraHelper.reportBugsToJIRA(List.of(), "log", result, testNgMethod);
            xray.verifyNoInteractions();
        }

        Mockito.when(result.isSuccess()).thenReturn(false);
        SHAFT.Properties.jira.set().jiraInteraction(false).reportBugs(true);
        try (MockedStatic<XrayIntegrationHelper> xray = Mockito.mockStatic(XrayIntegrationHelper.class)) {
            JiraHelper.reportBugsToJIRA(List.of(), "log", result, testNgMethod);
            xray.verifyNoInteractions();
        }
    }

    @Test
    public void reportBugsNeutralInfoShouldCreateIssueWhenThrowablePresent() throws Exception {
        Method annotated = AnnotatedFixture.class.getDeclaredMethod("annotatedCase");
        TestExecutionInfo info = new TestExecutionInfo("id", getClass().getName(), "method", "title", "title",
                annotated, new AssertionError("boom"), false);
        SHAFT.Properties.jira.set().jiraInteraction(true).reportBugs(true);
        try (MockedStatic<XrayIntegrationHelper> xray = Mockito.mockStatic(XrayIntegrationHelper.class)) {
            xray.when(() -> XrayIntegrationHelper.createIssue(Mockito.anyList(), Mockito.anyString(), Mockito.anyString()))
                    .thenReturn("BUG-2");
            JiraHelper.reportBugsToJIRA(List.of(), "neutral log", info);
            xray.verify(() -> XrayIntegrationHelper.link2Tickets("BUG-2", "TMS-9"));
        }
    }

    @Test
    public void reportBugsNeutralInfoShouldSkipWhenNoThrowable() {
        TestExecutionInfo info = new TestExecutionInfo("id", getClass().getName(), "method", "title", "title",
                null, null, false);
        SHAFT.Properties.jira.set().jiraInteraction(true).reportBugs(true);
        try (MockedStatic<XrayIntegrationHelper> xray = Mockito.mockStatic(XrayIntegrationHelper.class)) {
            JiraHelper.reportBugsToJIRA(List.of(), "log", info);
            xray.verifyNoInteractions();
        }
    }

    static class AnnotatedFixture {
        @Feature("coverage-feature")
        @Epic("coverage-epic")
        @Story("coverage-story")
        @TmsLink("TMS-9")
        /** Annotation carrier for prepareTestResultAttributes / reportBugs paths. */
        public void annotatedCase() {
            // no-op fixture method — annotations only
        }
    }
}
