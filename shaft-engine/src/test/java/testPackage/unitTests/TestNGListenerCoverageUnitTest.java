package testPackage.unitTests;

import com.epam.reportportal.listeners.ItemStatus;
import com.epam.reportportal.testng.ITestNGService;
import com.shaft.listeners.TestNGListener;
import com.shaft.listeners.internal.JiraHelper;
import com.shaft.listeners.internal.RetryAnalyzer;
import com.shaft.listeners.internal.TestNGListenerHelper;
import com.shaft.properties.internal.ThreadLocalPropertiesManager;
import com.shaft.tools.io.internal.IssueReporter;
import com.shaft.tools.io.internal.ReportManagerHelper;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import org.testng.IInvokedMethod;
import org.testng.ITestContext;
import org.testng.ITestClass;
import org.testng.ITestNGMethod;
import org.testng.ITestResult;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.Test;

import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.util.List;

import static org.testng.Assert.assertEquals;
import static org.testng.Assert.assertNull;

@Test(singleThreaded = true)
public class TestNGListenerCoverageUnitTest {

    @AfterMethod(alwaysRun = true)
    public void afterMethod() throws Exception {
        setReportPortalEnabled(false);
        TestNGListenerHelper.setPendingConfigFailure(null);
        com.shaft.properties.internal.Properties.clearForCurrentThread();
    }

    @Test
    public void onConfigurationFailureShouldStorePendingThrowableWhenThrowableExists() throws Exception {
        TestNGListener listener = new TestNGListener();
        ITestResult testResult = Mockito.mock(ITestResult.class);
        RuntimeException throwable = new RuntimeException("config failure");
        Mockito.when(testResult.getThrowable()).thenReturn(throwable);

        setReportPortalEnabled(false);
        listener.onConfigurationFailure(testResult);

        Throwable pendingFailure = TestNGListenerHelper.getAndClearPendingConfigFailure();
        assertEquals(pendingFailure, throwable);
    }

    @Test
    public void onExecutionStartShouldNotInitializeReportPortalWhenDisabled() throws Exception {
        ThreadLocalPropertiesManager.setProperty("rp.enable", "false");
        TestNGListener listener = new TestNGListener();

        Method initializeReportPortal = TestNGListener.class.getDeclaredMethod("initializeReportPortalIfEnabled");
        initializeReportPortal.setAccessible(true);
        initializeReportPortal.invoke(listener);

        assertNull(getReportPortalService(listener));
    }

    @Test
    public void onConfigurationFailureShouldReportToReportPortalWhenEnabled() throws Exception {
        TestNGListener listener = new TestNGListener();
        ITestResult testResult = Mockito.mock(ITestResult.class);
        Mockito.when(testResult.getThrowable()).thenReturn(null);

        ITestNGService reportPortalService = Mockito.mock(ITestNGService.class);
        setReportPortalService(listener, reportPortalService);
        setReportPortalEnabled(listener, true);
        try {
            listener.onConfigurationFailure(testResult);
            Mockito.verify(reportPortalService).sendReportPortalMsg(testResult);
            Mockito.verify(reportPortalService).finishTestMethod(ItemStatus.FAILED, testResult);
        } finally {
            setReportPortalEnabled(false);
        }
    }

    @Test
    public void onConfigurationSuccessAndSkipShouldReportToReportPortalWhenEnabled() throws Exception {
        TestNGListener listener = new TestNGListener();
        ITestResult testResult = Mockito.mock(ITestResult.class);

        ITestNGService reportPortalService = Mockito.mock(ITestNGService.class);
        setReportPortalService(listener, reportPortalService);
        setReportPortalEnabled(listener, true);
        try {
            listener.onConfigurationSuccess(testResult);
            listener.onConfigurationSkip(testResult);

            Mockito.verify(reportPortalService).finishTestMethod(ItemStatus.PASSED, testResult);
            Mockito.verify(reportPortalService).startConfiguration(testResult);
            Mockito.verify(reportPortalService).finishTestMethod(ItemStatus.SKIPPED, testResult);
        } finally {
            setReportPortalEnabled(false);
        }
    }

    @Test
    public void onTestFailedButWithinSuccessPercentageShouldReportFailureWhenEnabled() throws Exception {
        TestNGListener listener = new TestNGListener();
        ITestResult testResult = Mockito.mock(ITestResult.class);

        ITestNGService reportPortalService = Mockito.mock(ITestNGService.class);
        setReportPortalService(listener, reportPortalService);
        setReportPortalEnabled(listener, true);
        try {
            listener.onTestFailedButWithinSuccessPercentage(testResult);
            Mockito.verify(reportPortalService).finishTestMethod(ItemStatus.FAILED, testResult);
            assertNull(TestNGListenerHelper.getAndClearPendingConfigFailure());
        } finally {
            setReportPortalEnabled(false);
        }
    }

    @Test
    public void onTestFailureShouldReportToReportPortalWhenEnabled() throws Exception {
        TestNGListener listener = new TestNGListener();
        ITestResult testResult = createTestResult("failingTest", new RuntimeException("failed"));

        ITestNGService reportPortalService = Mockito.mock(ITestNGService.class);
        setReportPortalService(listener, reportPortalService);
        setReportPortalEnabled(listener, true);
        try {
            listener.onTestFailure(testResult);
            Mockito.verify(reportPortalService).sendReportPortalMsg(testResult);
            Mockito.verify(reportPortalService).finishTestMethod(ItemStatus.FAILED, testResult);
        } finally {
            removeTrackedMethod("failedTests", testResult.getMethod());
            setReportPortalEnabled(false);
        }
    }

    @Test
    public void onTestSkippedShouldHandleNullThrowableAndReportToReportPortalWhenEnabled() throws Exception {
        TestNGListener listener = new TestNGListener();
        ITestResult testResult = createTestResult("skippedTest", null);

        ITestNGService reportPortalService = Mockito.mock(ITestNGService.class);
        setReportPortalService(listener, reportPortalService);
        setReportPortalEnabled(listener, true);
        try {
            listener.onTestSkipped(testResult);
            Mockito.verify(reportPortalService).finishTestMethod(ItemStatus.SKIPPED, testResult);
        } finally {
            removeTrackedMethod("skippedTests", testResult.getMethod());
            setReportPortalEnabled(false);
        }
    }

    @Test
    public void deduplicatedExecutionCountsShouldKeepFinalFailuresAlignedWithIssueSummary() throws Exception {
        ITestNGMethod passedOnly = createTestMethod("passedOnly");
        ITestNGMethod flakyThenPassed = createTestMethod("flakyThenPassed");
        ITestNGMethod failedOnly = createTestMethod("failedOnly");
        ITestNGMethod skippedOnly = createTestMethod("skippedOnly");

        Method getDeduplicatedTestExecutionCounts = TestNGListener.class.getDeclaredMethod(
                "getDeduplicatedTestExecutionCounts", List.class, List.class, List.class);
        getDeduplicatedTestExecutionCounts.setAccessible(true);
        Object counts = getDeduplicatedTestExecutionCounts.invoke(null,
                List.of(passedOnly, flakyThenPassed),
                List.of(flakyThenPassed, failedOnly, failedOnly),
                List.of(skippedOnly, failedOnly));

        assertEquals(invokeCount(counts, "passed"), 1);
        assertEquals(invokeCount(counts, "failed"), 1);
        assertEquals(invokeCount(counts, "skipped"), 1);
        assertEquals(invokeCount(counts, "flaky"), 1);
        assertEquals(invokeCount(counts, "finalPassed"), 2);
    }

    @Test
    public void invocationHooksShouldActivateAndRestoreRetryEvidenceForTestMethods() throws Exception {
        TestNGListener listener = new TestNGListener();
        IInvokedMethod invokedMethod = Mockito.mock(IInvokedMethod.class);
        ITestNGMethod testMethod = createTestMethod("retryEvidenceTest");
        ITestResult testResult = createTestResult("retryEvidenceTest", null);
        ITestContext testContext = Mockito.mock(ITestContext.class);
        Mockito.when(invokedMethod.isTestMethod()).thenReturn(true);
        Mockito.when(invokedMethod.isConfigurationMethod()).thenReturn(false);
        Mockito.when(invokedMethod.getTestMethod()).thenReturn(testMethod);
        setExecutionStartTime(System.currentTimeMillis());

        try (MockedStatic<RetryAnalyzer> retryAnalyzer = Mockito.mockStatic(RetryAnalyzer.class);
             MockedStatic<JiraHelper> ignoredJiraHelper = Mockito.mockStatic(JiraHelper.class);
             MockedStatic<TestNGListenerHelper> ignoredListenerHelper = Mockito.mockStatic(TestNGListenerHelper.class);
             MockedStatic<IssueReporter> ignoredIssueReporter = Mockito.mockStatic(IssueReporter.class);
             MockedStatic<ReportManagerHelper> ignoredReportManagerHelper = Mockito.mockStatic(ReportManagerHelper.class)) {
            listener.beforeInvocation(invokedMethod, testResult, testContext);
            listener.afterInvocation(invokedMethod, testResult, testContext);

            retryAnalyzer.verify(RetryAnalyzer::activateSupportingEvidenceCaptureForRetryAttempt);
            retryAnalyzer.verify(RetryAnalyzer::restoreSupportingEvidenceCaptureForRetryAttempt);
        }
    }

    @SuppressWarnings("unchecked")
    private static void removeTrackedMethod(String fieldName, ITestNGMethod testMethod) throws Exception {
        getTrackedMethods(fieldName).remove(testMethod);
    }

    @SuppressWarnings("unchecked")
    private static List<ITestNGMethod> getTrackedMethods(String fieldName) throws Exception {
        Field trackedMethodsField = TestNGListener.class.getDeclaredField(fieldName);
        trackedMethodsField.setAccessible(true);
        return (List<ITestNGMethod>) trackedMethodsField.get(null);
    }

    private static ITestResult createTestResult(String methodName, Throwable throwable) {
        ITestResult testResult = Mockito.mock(ITestResult.class);
        ITestNGMethod testMethod = createTestMethod(methodName);
        ITestClass testClass = Mockito.mock(ITestClass.class);

        Mockito.when(testResult.getMethod()).thenReturn(testMethod);
        Mockito.when(testResult.getTestClass()).thenReturn(testClass);
        Mockito.when(testClass.getName()).thenReturn("testPackage.unitTests.TestNGListenerCoverageUnitTest");
        Mockito.when(testResult.getThrowable()).thenReturn(throwable);

        return testResult;
    }

    private static ITestNGMethod createTestMethod(String methodName) {
        ITestNGMethod testMethod = Mockito.mock(ITestNGMethod.class);
        Mockito.when(testMethod.getMethodName()).thenReturn(methodName);
        Mockito.when(testMethod.getQualifiedName()).thenReturn("testPackage.unitTests.TestNGListenerCoverageUnitTest." + methodName);
        Mockito.when(testMethod.getDescription()).thenReturn("description");
        return testMethod;
    }

    private static int invokeCount(Object counts, String methodName) throws Exception {
        Method method = counts.getClass().getDeclaredMethod(methodName);
        method.setAccessible(true);
        return (int) method.invoke(counts);
    }

    private static void setReportPortalEnabled(boolean value) throws Exception {
        Field reportPortalEnabledField = TestNGListener.class.getDeclaredField("isReportPortalEnabled");
        reportPortalEnabledField.setAccessible(true);
        reportPortalEnabledField.set(null, value);
    }

    private static void setExecutionStartTime(long value) throws Exception {
        Field executionStartTimeField = TestNGListener.class.getDeclaredField("executionStartTime");
        executionStartTimeField.setAccessible(true);
        executionStartTimeField.set(null, value);
    }

    private static void setReportPortalEnabled(TestNGListener listener, boolean value) throws Exception {
        Field reportPortalEnabledField = TestNGListener.class.getDeclaredField("isReportPortalEnabledForListener");
        reportPortalEnabledField.setAccessible(true);
        reportPortalEnabledField.set(listener, value);
    }

    private static void setReportPortalService(TestNGListener listener, ITestNGService reportPortalService) throws Exception {
        Field reportPortalServiceField = TestNGListener.class.getDeclaredField("reportPortalTestNGService");
        reportPortalServiceField.setAccessible(true);
        reportPortalServiceField.set(listener, reportPortalService);
    }

    private static ITestNGService getReportPortalService(TestNGListener listener) throws Exception {
        Field reportPortalServiceField = TestNGListener.class.getDeclaredField("reportPortalTestNGService");
        reportPortalServiceField.setAccessible(true);
        return (ITestNGService) reportPortalServiceField.get(listener);
    }
}
