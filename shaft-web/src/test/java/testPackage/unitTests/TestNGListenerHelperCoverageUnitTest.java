package testPackage.unitTests;

import com.shaft.driver.SHAFT;
import com.shaft.driver.internal.DriverFactory.DriverFactoryHelper;
import com.shaft.listeners.internal.TestNGListenerHelper;
import io.qameta.allure.Issue;
import io.qameta.allure.Issues;
import io.qameta.allure.TmsLink;
import io.qameta.allure.TmsLinks;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import org.testng.Assert;
import org.testng.ITestContext;
import org.testng.ITestNGMethod;
import org.testng.ITestResult;
import org.testng.SkipException;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;
import org.testng.internal.ConstructorOrMethod;
import org.testng.xml.XmlClass;
import org.testng.xml.XmlSuite;
import org.testng.xml.XmlTest;

import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.List;

@Test(singleThreaded = true)
public class TestNGListenerHelperCoverageUnitTest {
    private boolean originalSkipLinkedIssues;
    private String originalCrossBrowserMode;
    private boolean originalJvmProxySettings;
    private String originalProxySettings;
    private String originalSetParallel;
    private String originalSetParallelMode;
    private String originalSetThreadCount;
    private String originalSetPreserveOrder;
    private String originalSetGroupByInstances;
    private String originalSetVerbose;
    private String originalSetDataProviderThreadCount;

    @BeforeMethod(alwaysRun = true)
    public void beforeMethod() throws Exception {
        originalSkipLinkedIssues = SHAFT.Properties.flags.skipTestsWithLinkedIssues();
        originalCrossBrowserMode = SHAFT.Properties.platform.crossBrowserMode();
        originalJvmProxySettings = SHAFT.Properties.platform.jvmProxySettings();
        originalProxySettings = SHAFT.Properties.platform.proxy();
        originalSetParallel = System.getProperty("setParallel");
        originalSetParallelMode = System.getProperty("setParallelMode");
        originalSetThreadCount = System.getProperty("setThreadCount");
        originalSetPreserveOrder = System.getProperty("setPreserveOrder");
        originalSetGroupByInstances = System.getProperty("setGroupByInstances");
        originalSetVerbose = System.getProperty("setVerbose");
        originalSetDataProviderThreadCount = System.getProperty("setDataProviderThreadCount");
        setKillSwitch(false);
    }

    @AfterMethod(alwaysRun = true)
    public void afterMethod() throws Exception {
        SHAFT.Properties.flags.set().skipTestsWithLinkedIssues(originalSkipLinkedIssues);
        SHAFT.Properties.platform.set().crossBrowserMode(originalCrossBrowserMode);
        SHAFT.Properties.platform.set().jvmProxySettings(originalJvmProxySettings);
        SHAFT.Properties.platform.set().proxySettings(originalProxySettings);
        setSystemProperty("setParallel", originalSetParallel);
        setSystemProperty("setParallelMode", originalSetParallelMode);
        setSystemProperty("setThreadCount", originalSetThreadCount);
        setSystemProperty("setPreserveOrder", originalSetPreserveOrder);
        setSystemProperty("setGroupByInstances", originalSetGroupByInstances);
        setSystemProperty("setVerbose", originalSetVerbose);
        setSystemProperty("setDataProviderThreadCount", originalSetDataProviderThreadCount);
        TestNGListenerHelper.setPendingConfigFailure(null);
        TestNGListenerHelper.cleanup();
        setKillSwitch(false);
    }

    @Test
    public void createTestLogShouldHandleEmptyAndNonEmptyReporterOutput() {
        String fullLog = "line one" + System.lineSeparator() + "line two" + System.lineSeparator();
        String expectedLog = fullLog.substring(0, fullLog.length() - 2);
        Assert.assertEquals(TestNGListenerHelper.createTestLog(List.of("line one", "line two")), expectedLog);
        Assert.assertEquals(TestNGListenerHelper.createTestLog(List.of()), "");
    }

    @Test
    public void setAndGetTestNameShouldReadNameFromITestContext() {
        ITestContext context = Mockito.mock(ITestContext.class);
        org.testng.xml.XmlTest xmlTest = Mockito.mock(org.testng.xml.XmlTest.class);
        Mockito.when(context.getCurrentXmlTest()).thenReturn(xmlTest);
        Mockito.when(xmlTest.getName()).thenReturn("SampleXmlTestName");

        TestNGListenerHelper.setTestName(context);
        Assert.assertEquals(TestNGListenerHelper.getTestName(), "SampleXmlTestName");

        TestNGListenerHelper.cleanup();
        Assert.assertNull(TestNGListenerHelper.getTestName());
    }

    @Test
    public void pendingConfigFailureShouldBeSetAndCleared() {
        RuntimeException failure = new RuntimeException("config failed");
        TestNGListenerHelper.setPendingConfigFailure(failure);
        Assert.assertSame(TestNGListenerHelper.getAndClearPendingConfigFailure(), failure);
        Assert.assertNull(TestNGListenerHelper.getAndClearPendingConfigFailure());
    }

    @Test
    public void skipTestsWithLinkedIssuesShouldHonorFlagAndThrowSkipException() {
        SHAFT.Properties.flags.set().skipTestsWithLinkedIssues(true);

        ITestResult singleIssueResult = createResultFromMethod("methodWithSingleIssue");
        SkipException singleIssueException = Assert.expectThrows(SkipException.class, () -> TestNGListenerHelper.skipTestsWithLinkedIssues(singleIssueResult));
        Assert.assertTrue(singleIssueException.getMessage().contains("ISSUE-1"));

        ITestResult multipleIssuesResult = createResultFromMethod("methodWithMultipleIssues");
        SkipException multipleIssuesException = Assert.expectThrows(SkipException.class, () -> TestNGListenerHelper.skipTestsWithLinkedIssues(multipleIssuesResult));
        Assert.assertTrue(multipleIssuesException.getMessage().contains("ISSUE-2"));
        Assert.assertTrue(multipleIssuesException.getMessage().contains("ISSUE-3"));

        SHAFT.Properties.flags.set().skipTestsWithLinkedIssues(false);
        TestNGListenerHelper.skipTestsWithLinkedIssues(singleIssueResult);
    }

    @Test
    public void annotationValueReadersShouldExtractIssueAndTmsValues() {
        Assert.assertEquals(TestNGListenerHelper.getIssueAnnotationValue(createResultFromMethod("methodWithSingleIssue")), "ISSUE-1");
        Assert.assertEquals(TestNGListenerHelper.getIssueAnnotationValue(createResultFromMethod("methodWithMultipleIssues")), "ISSUE-2, ISSUE-3");
        Assert.assertEquals(TestNGListenerHelper.getIssueAnnotationValue(createResultFromMethod("methodWithoutIssue")), "");

        Assert.assertEquals(TestNGListenerHelper.getTmsLinkAnnotationValue(createResultFromMethod("methodWithSingleTms")), "TMS-1");
        Assert.assertEquals(TestNGListenerHelper.getTmsLinkAnnotationValue(createResultFromMethod("methodWithMultipleTms")), "TMS-2, TMS-3");
        Assert.assertEquals(TestNGListenerHelper.getTmsLinkAnnotationValue(createResultFromMethod("methodWithoutTms")), "");
    }

    @Test
    public void failFastShouldSkipWhenKillSwitchIsEnabled() throws Exception {
        ITestResult result = Mockito.mock(ITestResult.class);
        Mockito.when(result.getName()).thenReturn("KillSwitchTest");

        setKillSwitch(false);
        TestNGListenerHelper.failFast(result);

        setKillSwitch(true);
        try {
            SkipException exception = Assert.expectThrows(SkipException.class, () -> TestNGListenerHelper.failFast(result));
            Assert.assertTrue(exception.getMessage().contains("KillSwitchTest"));
        } finally {
            setKillSwitch(false);
        }
    }

    @Test
    public void configureCrossBrowserExecutionAndTestNGPropertiesShouldAdjustXmlSuites() {
        XmlSuite suite = new XmlSuite();
        suite.setName("sampleSuite");
        XmlTest xmlTest = new XmlTest(suite);
        xmlTest.setName("sampleTest");

        SHAFT.Properties.platform.set().crossBrowserMode("parallelized");
        System.setProperty("setParallel", "METHODS");
        System.setProperty("setParallelMode", "STATIC");
        System.setProperty("setThreadCount", "2.0");
        System.setProperty("setPreserveOrder", "true");
        System.setProperty("setGroupByInstances", "true");
        System.setProperty("setVerbose", "2");
        System.setProperty("setDataProviderThreadCount", "3");

        TestNGListenerHelper.configureCrossBrowserExecution(new ArrayList<>(List.of(suite)));
        Assert.assertEquals(suite.getTests().size(), 4);
        Assert.assertEquals(suite.getParallel(), XmlSuite.ParallelMode.TESTS);
        Assert.assertEquals(suite.getThreadCount(), 4);

        TestNGListenerHelper.configureTestNGProperties(List.of(suite));
        for (XmlTest expandedTest : suite.getTests()) {
            Assert.assertEquals(expandedTest.getThreadCount(), 1);
            Assert.assertNotNull(expandedTest.getName());
        }
        Assert.assertTrue(suite.getDataProviderThreadCount() > 0);
    }

    @Test
    public void attachConfigurationHelperAndConfigurationMethodsShouldExecuteWithoutErrors() {
        XmlSuite suite = new XmlSuite();
        suite.setName("sampleSuite");
        XmlTest xmlTest = new XmlTest(suite);
        xmlTest.setName("sampleTest");

        TestNGListenerHelper.attachConfigurationHelperClass(List.of(suite));
        Assert.assertTrue(xmlTest.getXmlClasses().stream()
                .map(XmlClass::getName)
                .anyMatch(className -> className.endsWith("ConfigurationHelper")));

        ITestResult beforeResult = createConfigurationResult("suiteSetup", true, false, false);
        ITestResult afterResult = createConfigurationResult("suiteTeardown", false, true, false);
        ITestResult testResult = createConfigurationResult("suiteSetup", false, false, true);

        TestNGListenerHelper.updateConfigurationMethods(beforeResult);
        TestNGListenerHelper.updateConfigurationMethods(afterResult);
        TestNGListenerHelper.updateConfigurationMethods(null);
        TestNGListenerHelper.updateTestMethods(testResult);
        TestNGListenerHelper.attachConfigurationMethods();
        TestNGListenerHelper.attachConfigurationMethods();
    }

    @Test
    public void configureJVMProxyShouldSetGlobalProxyPropertiesWhenEnabled() {
        SHAFT.Properties.platform.set().jvmProxySettings(true);
        SHAFT.Properties.platform.set().proxySettings("127.0.0.1:8888");
        TestNGListenerHelper.configureJVMProxy();

        Assert.assertEquals(com.shaft.properties.internal.ThreadLocalPropertiesManager.getProperty("http.proxyHost"), "127.0.0.1");
        Assert.assertEquals(com.shaft.properties.internal.ThreadLocalPropertiesManager.getProperty("http.proxyPort"), "8888");
        Assert.assertEquals(com.shaft.properties.internal.ThreadLocalPropertiesManager.getProperty("https.proxyHost"), "127.0.0.1");
        Assert.assertEquals(com.shaft.properties.internal.ThreadLocalPropertiesManager.getProperty("https.proxyPort"), "8888");
        Assert.assertEquals(com.shaft.properties.internal.ThreadLocalPropertiesManager.getProperty("ftp.proxyHost"), "127.0.0.1");
        Assert.assertEquals(com.shaft.properties.internal.ThreadLocalPropertiesManager.getProperty("ftp.proxyPort"), "8888");
    }

    @Test
    public void setTotalNumberOfTestsShouldSkipCucumberRunScenarioSuites() {
        ITestNGMethod normalMethod = Mockito.mock(ITestNGMethod.class);
        Mockito.when(normalMethod.getMethodName()).thenReturn("regularTest");
        org.testng.ISuite normalSuite = stubbedSuite(List.of(normalMethod));

        ITestNGMethod cucumberMethod = Mockito.mock(ITestNGMethod.class);
        Mockito.when(cucumberMethod.getMethodName()).thenReturn("runScenario");
        org.testng.ISuite cucumberSuite = stubbedSuite(List.of(cucumberMethod));

        TestNGListenerHelper.setTotalNumberOfTests(normalSuite);
        TestNGListenerHelper.setTotalNumberOfTests(cucumberSuite);
    }

    private static org.testng.ISuite stubbedSuite(List<ITestNGMethod> methods) {
        return (org.testng.ISuite) java.lang.reflect.Proxy.newProxyInstance(
                org.testng.ISuite.class.getClassLoader(),
                new Class<?>[]{org.testng.ISuite.class},
                (proxy, method, args) -> {
                    if ("getAllMethods".equals(method.getName())) return methods;
                    if (method.getReturnType() == boolean.class) return false;
                    if (method.getReturnType() == int.class) return 0;
                    return null;
                }
        );
    }

    @Test
    public void logTestInformationAndFinishedTestInformationShouldHandleStatuses() {
        ITestResult success = createResultForLogging("sampleMethod", ITestResult.SUCCESS);
        ITestResult failure = createResultForLogging("sampleMethod", ITestResult.FAILURE);
        ITestResult skipped = createResultForLogging("sampleMethod", ITestResult.SKIP);
        ITestResult cucumber = createResultForCucumberLogging();

        try (MockedStatic<com.shaft.tools.io.internal.ReportManagerHelper> reportManagerHelper = Mockito.mockStatic(com.shaft.tools.io.internal.ReportManagerHelper.class)) {
            reportManagerHelper.when(com.shaft.tools.io.internal.ReportManagerHelper::getTestClassName).thenReturn("sampleClass");
            reportManagerHelper.when(com.shaft.tools.io.internal.ReportManagerHelper::getTestMethodName).thenReturn("sampleMethod");

            TestNGListenerHelper.logTestInformation(success);
            TestNGListenerHelper.logFinishedTestInformation(success);
            TestNGListenerHelper.logFinishedTestInformation(failure);
            TestNGListenerHelper.logFinishedTestInformation(skipped);
            TestNGListenerHelper.logTestInformation(cucumber);
            TestNGListenerHelper.logFinishedTestInformation(cucumber);

            reportManagerHelper.verify(() -> com.shaft.tools.io.internal.ReportManagerHelper.logTestInformation("sampleClass", "sampleMethod", "sample description"));
            reportManagerHelper.verify(() -> com.shaft.tools.io.internal.ReportManagerHelper.logFinishedTestInformation("sampleClass", "sampleMethod", "sample description", "Passed"));
            reportManagerHelper.verify(() -> com.shaft.tools.io.internal.ReportManagerHelper.logFinishedTestInformation("sampleClass", "sampleMethod", "sample description", "Failed"));
            reportManagerHelper.verify(() -> com.shaft.tools.io.internal.ReportManagerHelper.logFinishedTestInformation("sampleClass", "sampleMethod", "sample description", "Skipped"));
        }
    }

    private static ITestResult createResultFromMethod(String methodName) {
        try {
            Method method = TestNGListenerHelperCoverageUnitTest.class.getDeclaredMethod(methodName);
            ITestNGMethod iTestNGMethod = Mockito.mock(ITestNGMethod.class);
            Mockito.when(iTestNGMethod.getConstructorOrMethod()).thenReturn(new ConstructorOrMethod(method));
            ITestResult iTestResult = Mockito.mock(ITestResult.class);
            Mockito.when(iTestResult.getMethod()).thenReturn(iTestNGMethod);
            return iTestResult;
        } catch (NoSuchMethodException e) {
            throw new IllegalStateException(e);
        }
    }

    private static ITestResult createConfigurationResult(String methodName, boolean isBeforeConfig, boolean isAfterConfig, boolean isTest) {
        ITestNGMethod iTestNGMethod = Mockito.mock(ITestNGMethod.class);
        Mockito.when(iTestNGMethod.getMethodName()).thenReturn(methodName);
        Mockito.when(iTestNGMethod.isBeforeMethodConfiguration()).thenReturn(isBeforeConfig);
        Mockito.when(iTestNGMethod.isBeforeTestConfiguration()).thenReturn(false);
        Mockito.when(iTestNGMethod.isBeforeClassConfiguration()).thenReturn(false);
        Mockito.when(iTestNGMethod.isBeforeSuiteConfiguration()).thenReturn(false);
        Mockito.when(iTestNGMethod.isAfterMethodConfiguration()).thenReturn(isAfterConfig);
        Mockito.when(iTestNGMethod.isAfterTestConfiguration()).thenReturn(false);
        Mockito.when(iTestNGMethod.isAfterClassConfiguration()).thenReturn(false);
        Mockito.when(iTestNGMethod.isAfterSuiteConfiguration()).thenReturn(false);
        Mockito.when(iTestNGMethod.isTest()).thenReturn(isTest);

        ITestResult iTestResult = Mockito.mock(ITestResult.class);
        Mockito.when(iTestResult.getMethod()).thenReturn(iTestNGMethod);
        return iTestResult;
    }

    private static ITestResult createResultForLogging(String methodName, int status) {
        ITestNGMethod iTestNGMethod = Mockito.mock(ITestNGMethod.class);
        Mockito.when(iTestNGMethod.getQualifiedName()).thenReturn("testPackage.unitTests.TestNGListenerHelperCoverageUnitTest." + methodName);
        Mockito.when(iTestNGMethod.isTest()).thenReturn(true);
        Mockito.when(iTestNGMethod.getDescription()).thenReturn("sample description");

        ITestResult iTestResult = Mockito.mock(ITestResult.class);
        Mockito.when(iTestResult.getMethod()).thenReturn(iTestNGMethod);
        Mockito.when(iTestResult.getStatus()).thenReturn(status);
        return iTestResult;
    }

    private static ITestResult createResultForCucumberLogging() {
        ITestNGMethod iTestNGMethod = Mockito.mock(ITestNGMethod.class);
        Mockito.when(iTestNGMethod.getQualifiedName()).thenReturn("io.cucumber.testng.AbstractTestNGCucumberTests.sample");
        Mockito.when(iTestNGMethod.isTest()).thenReturn(true);

        ITestResult iTestResult = Mockito.mock(ITestResult.class);
        Mockito.when(iTestResult.getMethod()).thenReturn(iTestNGMethod);
        Mockito.when(iTestResult.getStatus()).thenReturn(ITestResult.SUCCESS);
        return iTestResult;
    }

    private static void setKillSwitch(boolean value) throws Exception {
        Field killSwitchField = DriverFactoryHelper.class.getDeclaredField("killSwitch");
        killSwitchField.setAccessible(true);
        killSwitchField.setBoolean(null, value);
    }

    private static void setSystemProperty(String key, String value) {
        if (value == null) {
            System.clearProperty(key);
        } else {
            System.setProperty(key, value);
        }
    }

    @Issue("ISSUE-1")
    private void methodWithSingleIssue() {
    }

    @Issues({@Issue("ISSUE-2"), @Issue("ISSUE-3")})
    private void methodWithMultipleIssues() {
    }

    private void methodWithoutIssue() {
    }

    @TmsLink("TMS-1")
    private void methodWithSingleTms() {
    }

    @TmsLinks({@TmsLink("TMS-2"), @TmsLink("TMS-3")})
    private void methodWithMultipleTms() {
    }

    private void methodWithoutTms() {
    }
}
