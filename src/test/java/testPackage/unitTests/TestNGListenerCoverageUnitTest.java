package testPackage.unitTests;

import com.epam.reportportal.listeners.ItemStatus;
import com.epam.reportportal.testng.ITestNGService;
import com.shaft.listeners.TestNGListener;
import com.shaft.listeners.internal.ConfigurationHelper;
import com.shaft.listeners.internal.TestNGListenerHelper;
import com.shaft.properties.internal.Properties;
import org.mockito.Mockito;
import org.testng.ITestClass;
import org.testng.ITestNGMethod;
import org.testng.ITestResult;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.Test;
import org.testng.xml.XmlClass;
import org.testng.xml.XmlSuite;
import org.testng.xml.XmlTest;

import java.lang.reflect.Field;
import java.util.List;

import static org.testng.Assert.assertEquals;
import static org.testng.Assert.assertNotNull;
import static org.testng.Assert.assertNull;
import static org.testng.Assert.assertTrue;

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

    @SuppressWarnings("unchecked")
    private static void removeTrackedMethod(String fieldName, ITestNGMethod testMethod) throws Exception {
        Field trackedMethodsField = TestNGListener.class.getDeclaredField(fieldName);
        trackedMethodsField.setAccessible(true);
        ((List<ITestNGMethod>) trackedMethodsField.get(null)).remove(testMethod);
    }

    private static ITestResult createTestResult(String methodName, Throwable throwable) {
        ITestResult testResult = Mockito.mock(ITestResult.class);
        ITestNGMethod testMethod = Mockito.mock(ITestNGMethod.class);
        ITestClass testClass = Mockito.mock(ITestClass.class);

        Mockito.when(testMethod.getMethodName()).thenReturn(methodName);
        Mockito.when(testMethod.getQualifiedName()).thenReturn("testPackage.unitTests.TestNGListenerCoverageUnitTest." + methodName);
        Mockito.when(testMethod.getDescription()).thenReturn("description");
        Mockito.when(testResult.getMethod()).thenReturn(testMethod);
        Mockito.when(testResult.getTestClass()).thenReturn(testClass);
        Mockito.when(testClass.getName()).thenReturn("testPackage.unitTests.TestNGListenerCoverageUnitTest");
        Mockito.when(testResult.getThrowable()).thenReturn(throwable);

        return testResult;
    }

    private static void setReportPortalEnabled(boolean value) throws Exception {
        Field reportPortalEnabledField = TestNGListener.class.getDeclaredField("isReportPortalEnabled");
        reportPortalEnabledField.setAccessible(true);
        reportPortalEnabledField.set(null, value);
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

    // Minimal concrete Cucumber runner used only for class-name detection in alter() tests.
    // It does not need Cucumber annotations — alter() checks isAssignableFrom(), not annotations.
    @SuppressWarnings("unused")
    private static class MockCucumberRunner extends io.cucumber.testng.AbstractTestNGCucumberTests {
    }

    @Test
    public void alterShouldRouteCucumberSuiteToCucumberHelperWithoutThrowing() {
        // Suite whose only test class is a Cucumber runner (extends AbstractTestNGCucumberTests)
        XmlSuite suite = new XmlSuite();
        suite.setName("cucumber-suite");
        XmlTest test = new XmlTest(suite);
        test.setName("cucumber-test");
        test.getClasses().add(new XmlClass(MockCucumberRunner.class.getName()));

        // alter() must not throw even though Properties.testNG may not be loaded yet
        new TestNGListener().alter(List.of(suite));

        // ConfigurationHelper is always appended by attachConfigurationHelperClass()
        boolean hasConfigHelper = test.getClasses().stream()
                .anyMatch(c -> c.getName().equals(ConfigurationHelper.class.getName()));
        assertTrue(hasConfigHelper,
                "alter() must attach ConfigurationHelper even for Cucumber suites");
    }

    @Test
    public void alterShouldRouteTestNGSuiteToConfigureTestNGPropertiesWithoutThrowing() {
        // Suite whose test class is a plain TestNG test (not a Cucumber runner)
        XmlSuite suite = new XmlSuite();
        suite.setName("testng-suite");
        XmlTest test = new XmlTest(suite);
        test.setName("testng-test");
        test.getClasses().add(new XmlClass(TestNGListenerCoverageUnitTest.class.getName()));

        // Force Properties.testNG to null to exercise the lazy-init path
        com.shaft.properties.internal.TestNG original = Properties.testNG;
        Properties.testNG = null;
        try {
            new TestNGListener().alter(List.of(suite));
            // Lazy init must have populated Properties.testNG
            assertNotNull(Properties.testNG,
                    "configureTestNGProperties() must lazily initialize Properties.testNG");
        } finally {
            Properties.testNG = original;
        }
    }

    @Test
    public void configureTestNGPropertiesShouldLazilyInitPropertiesTestNGWhenNull() {
        com.shaft.properties.internal.TestNG original = Properties.testNG;
        Properties.testNG = null;
        XmlSuite suite = new XmlSuite();
        suite.setName("lazy-init-suite");
        XmlTest test = new XmlTest(suite);
        test.setName("lazy-init-test");
        try {
            TestNGListenerHelper.configureTestNGProperties(List.of(suite));
            assertNotNull(Properties.testNG,
                    "configureTestNGProperties() must lazily initialize Properties.testNG when called before loadProperties()");
        } finally {
            Properties.testNG = original;
        }
    }
}
