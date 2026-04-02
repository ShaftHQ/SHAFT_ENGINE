package com.shaft.listeners.internal;

import com.shaft.driver.SHAFT;
import com.shaft.driver.internal.DriverFactory.DriverFactoryHelper;
import com.shaft.enums.internal.Screenshots;
import com.shaft.gui.internal.image.AnimatedGifManager;
import com.shaft.gui.internal.locator.LocatorBuilder;
import com.shaft.gui.internal.video.RecordManager;
import com.shaft.tools.io.internal.ReportManagerHelper;
import io.qameta.allure.Issue;
import io.qameta.allure.Issues;
import io.qameta.allure.TmsLink;
import io.qameta.allure.TmsLinks;
import org.openqa.selenium.Platform;
import org.openqa.selenium.remote.Browser;
import org.testng.*;
import org.testng.internal.ConfigurationMethod;
import org.testng.xml.XmlClass;
import org.testng.xml.XmlSuite;
import org.testng.xml.XmlTest;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Map;

/**
 * Provides TestNG lifecycle helper operations for logging, artifact attachment, and suite shaping.
 */
public class TestNGListenerHelper {

    private static final List<ITestResult> beforeMethods = Collections.synchronizedList(new ArrayList<>());
    private static final List<ITestResult> afterMethods = Collections.synchronizedList(new ArrayList<>());
    private static final ThreadLocal<String> testName = new ThreadLocal<>();
    /**
     * Stores the throwable from the most recent configuration method failure on this thread.
     * Used by {@link com.shaft.listeners.AllureListener} to convert a SKIPPED allure test result
     * into BROKEN when the skip was caused by a configuration method failure rather than an
     * intentional skip (e.g. {@link org.testng.SkipException}).
     */
    private static final ThreadLocal<Throwable> pendingConfigFailure = new ThreadLocal<>();

    /**
     * Stores the total number of tests in the current suite when applicable.
     *
     * @param testSuite current TestNG suite
     */
    public static void setTotalNumberOfTests(ISuite testSuite) {
        // This condition checks to confirm that this is not a cucumber test runner instance
        // If this condition is removed the total number of tests will be zero because the cucumber
        // test runner doesn't have any test methods
        if (!(testSuite.getAllMethods().size() == 1 && testSuite.getAllMethods().getFirst().getMethodName().equals("runScenario"))) {
            ReportManagerHelper.setTotalNumberOfTests(testSuite.getAllMethods().size());
        }
    }

    /**
     * Attaches any pending before/after configuration artifacts.
     */
    public static void attachConfigurationMethods() {
        attachBeforeConfigurationMethods();
        attachAfterConfigurationMethods();
    }

    /**
     * Updates cached configuration results for later artifact attachment.
     *
     * @param iTestResult current TestNG result
     */
    public static void updateConfigurationMethods(ITestResult iTestResult) {
        updateBeforeConfigurationMethods(iTestResult);
        updateAfterConfigurationMethods(iTestResult);
    }

    private static void attachBeforeConfigurationMethods() {
        synchronized (beforeMethods) {
            if (!beforeMethods.isEmpty()) {
                TestNGListenerHelper.attachTestArtifacts(beforeMethods.get(beforeMethods.size() > 1 ? beforeMethods.size() - 1 : 0));
                beforeMethods.clear();
            }
        }
    }

    private static void updateBeforeConfigurationMethods(ITestResult iTestResult) {
        if (iTestResult != null) {
            if (iTestResult.getMethod().isBeforeMethodConfiguration() || iTestResult.getMethod().isBeforeTestConfiguration()
                    || iTestResult.getMethod().isBeforeClassConfiguration() || iTestResult.getMethod().isBeforeSuiteConfiguration()) {
                // get test result and store it for later processing
                beforeMethods.add(iTestResult);
            }
        }
    }

    private static void attachAfterConfigurationMethods() {
        synchronized (afterMethods) {
            if (!afterMethods.isEmpty()) {
                TestNGListenerHelper.attachTestArtifacts(afterMethods.get(afterMethods.size() > 1 ? afterMethods.size() - 1 : 0));
                afterMethods.clear();
            }
        }
    }

    private static void updateAfterConfigurationMethods(ITestResult iTestResult) {
        if (iTestResult != null) {
            if (iTestResult.getMethod().isAfterMethodConfiguration() || iTestResult.getMethod().isAfterTestConfiguration()
                    || iTestResult.getMethod().isAfterClassConfiguration() || iTestResult.getMethod().isAfterSuiteConfiguration()) {
                // get test result and store it for later processing
                afterMethods.add(iTestResult);
            }
        }
    }

    public static void updateTestMethods(ITestResult iTestResult) {
        if (iTestResult.getMethod().isTest()) {
            // get test result and store it for later processing
            TestNGListenerHelper.attachTestArtifacts(iTestResult);
        }
    }

    /**
     * Gets the current logical test name for the active thread.
     *
     * @return the current test name, or {@code null} if not set
     */
    public static String getTestName() {
        return testName.get();
    }

    /**
     * Stores the throwable from the most recent configuration method (e.g. {@code @BeforeMethod})
     * failure on this thread, so that the Allure lifecycle listener can later promote a SKIPPED
     * test result to BROKEN when the skip was caused by this failure.
     *
     * @param throwable the exception thrown by the failing configuration method, or {@code null}
     *                  to clear any previously stored failure
     */
    public static void setPendingConfigFailure(Throwable throwable) {
        if (throwable != null) {
            pendingConfigFailure.set(throwable);
        } else {
            pendingConfigFailure.remove();
        }
    }

    /**
     * Returns and clears the configuration-method failure throwable stored for the current thread.
     *
     * @return the throwable set by the most recent {@link #setPendingConfigFailure} call, or
     *         {@code null} if none is pending
     */
    public static Throwable getAndClearPendingConfigFailure() {
        Throwable t = pendingConfigFailure.get();
        pendingConfigFailure.remove();
        return t;
    }

    /**
     * Stores the current XML test name for the active thread.
     *
     * @param iTestContext current TestNG context
     */
    public static void setTestName(ITestContext iTestContext) {
        testName.set(iTestContext.getCurrentXmlTest().getName());
    }

    /**
     * Removes thread-local state held by this helper, preventing memory leaks
     * in pooled thread environments.
     */
    public static void cleanup() {
        testName.remove();
        LocatorBuilder.cleanup();
    }

    /**
     * Expands suites for cross-browser execution according to configured mode.
     *
     * @param suites mutable list of TestNG suites
     */
    public static void configureCrossBrowserExecution(List<XmlSuite> suites) {
        String crossBrowserMode = SHAFT.Properties.platform.crossBrowserMode();
        List<Browser> supportedBrowsers = Arrays.asList(Browser.CHROME, Browser.SAFARI, Browser.FIREFOX, Browser.EDGE);
        if (!"off".equals(crossBrowserMode)) {
            suites.forEach(suite -> {
                supportedBrowsers.forEach(supportedBrowser -> createTestSuite((XmlTest) suite.getTests().getFirst().clone(), supportedBrowser));
                // removing first test which is now a duplicate
                suite.getTests().removeFirst();

                if (crossBrowserMode.equals("parallelized")) {
                    suite.setParallel(XmlSuite.ParallelMode.TESTS);
                    suite.setThreadCount(supportedBrowsers.size());
                    SHAFT.Properties.visuals.set().videoParamsRecordVideo(true);
                    SHAFT.Properties.visuals.set().screenshotParamsScreenshotType(String.valueOf(Screenshots.VIEWPORT));
                }
            });
        }
    }

    private static void createTestSuite(XmlTest xmlTest, Browser browser) {
        xmlTest.setParameters(Map.of(
                "executionAddress", "dockerized",
                "targetOperatingSystem", Platform.LINUX.name(),
                "targetBrowserName", browser.browserName()
        ));
        double threadCount = SHAFT.Properties.testNG.threadCount();
        if ("DYNAMIC".equals(SHAFT.Properties.testNG.parallelMode())) {
            threadCount = threadCount * Runtime.getRuntime().availableProcessors();
            xmlTest.setThreadCount((int) Math.floor(threadCount));
        } else {
            xmlTest.setThreadCount((int) Math.floor(threadCount));
        }
        xmlTest.setParallel(XmlSuite.ParallelMode.valueOf(SHAFT.Properties.testNG.parallel()));
        xmlTest.setName(xmlTest.getName() + " - " + browser);
    }

    public static void configureTestNGProperties(List<XmlSuite> suites) {
        suites.forEach(suite -> {
            suite.setDataProviderThreadCount(SHAFT.Properties.testNG.dataProviderThreadCount());
            suite.getTests().forEach(xmlTest -> {
                xmlTest.setPreserveOrder(SHAFT.Properties.testNG.preserveOrder());
                xmlTest.setGroupByInstances(SHAFT.Properties.testNG.groupByInstances());
                xmlTest.setVerbose(SHAFT.Properties.testNG.verbose());
                double threadCount = SHAFT.Properties.testNG.threadCount();
                if ("DYNAMIC".equals(SHAFT.Properties.testNG.parallelMode())) {
                    threadCount = threadCount * Runtime.getRuntime().availableProcessors();
                    xmlTest.setThreadCount((int) Math.floor(threadCount));
                } else {
                    xmlTest.setThreadCount((int) Math.floor(threadCount));
                }
                xmlTest.setParallel(XmlSuite.ParallelMode.valueOf(SHAFT.Properties.testNG.parallel()));
            });
        });
    }

    /**
     * Appends configuration helper classes to all XML tests in the provided suites.
     *
     * @param suites mutable list of TestNG suites
     */
    public static void attachConfigurationHelperClass(List<XmlSuite> suites) {
        suites.forEach(xmlSuite ->
                xmlSuite.getTests().forEach(xmlTest ->
                        xmlTest.getClasses().add(new XmlClass(ConfigurationHelper.class.getName()))));
    }

    /**
     * Configures JVM-level proxy settings when enabled.
     */
    public static void configureJVMProxy() {
        String PROXY_SERVER_SETTINGS = SHAFT.Properties.platform.proxy();
        if (SHAFT.Properties.platform.jvmProxySettings() && !PROXY_SERVER_SETTINGS.isEmpty()) {
            String[] proxyHostPort = PROXY_SERVER_SETTINGS.split(":");
            System.setProperty("http.proxyHost", proxyHostPort[0]);
            System.setProperty("http.proxyPort", proxyHostPort[1]);
            System.setProperty("https.proxyHost", proxyHostPort[0]);
            System.setProperty("https.proxyPort", proxyHostPort[1]);
            System.setProperty("ftp.proxyHost", proxyHostPort[0]);
            System.setProperty("ftp.proxyPort", proxyHostPort[1]);
        }
    }

    /**
     * Attaches generated artifacts and logs for a test result.
     *
     * @param iTestResult current TestNG result
     */
    public static void attachTestArtifacts(ITestResult iTestResult) {
        ITestNGMethod iTestNGMethod = iTestResult.getMethod();

        if (!Arrays.asList("suiteSetup", "suiteTeardown", "classTeardown").contains(iTestNGMethod.getMethodName())) {
            List<String> attachments = new ArrayList<>();
            String attachment;
            if (SHAFT.Properties.visuals.videoParamsScope().equals("TestMethod")) {
                RecordManager.attachVideoRecording();
                attachment = RecordManager.getVideoRecordingFilePath();
                if (!attachment.isEmpty())
                    attachments.add(attachment);
            }
            attachment = AnimatedGifManager.attachAnimatedGif();
            if (!attachment.isEmpty())
                attachments.add(attachment);

            String logText = TestNGListenerHelper.createTestLog(Reporter.getOutput(iTestResult));
            ReportManagerHelper.attachTestLog(iTestNGMethod.getMethodName(), logText);
            JiraHelper.reportBugsToJIRA(attachments, logText, iTestResult, iTestNGMethod);
        }
    }

    /**
     * Creates a single test-log string from TestNG reporter output entries.
     *
     * @param output reporter output lines
     * @return aggregated log text
     */
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
     * Skips tests that are linked to known issues when that flag is enabled.
     *
     * @param iTestResult current TestNG result
     */
    public static void skipTestsWithLinkedIssues(ITestResult iTestResult) {
        if (SHAFT.Properties.flags.skipTestsWithLinkedIssues()) {
            var method = iTestResult.getMethod().getConstructorOrMethod().getMethod();
            Issue issue = method.getAnnotation(Issue.class);
            if (issue != null) {
                SkipException ex = new SkipException("Skipping Test as it's expected to fail due to open issue: [" + issue.value() + "]");
                ReportManagerHelper.logDiscrete(ex);
                throw ex;
            }
            Issues issues = method.getAnnotation(Issues.class);
            if (issues != null) {
                StringBuilder issueNames = new StringBuilder();
                Arrays.stream(issues.value()).iterator().forEachRemaining(issueI -> issueNames.append(issueI.value()).append(" ,"));
                SkipException ex = new SkipException("Skipping Test as it's expected to fail due to open issues: [" + issueNames.substring(0, issueNames.length() - 2) + "]");
                ReportManagerHelper.logDiscrete(ex);
                throw ex;
            }
        }
    }

    /**
     * Extracts issue annotation values from a test method.
     *
     * @param iTestResult current TestNG result
     * @return issue value(s), or empty string if none exist
     */
    public static String getIssueAnnotationValue(ITestResult iTestResult) {
        var method = iTestResult.getMethod().getConstructorOrMethod().getMethod();
        Issue issue = method.getAnnotation(Issue.class);
        Issues issues = method.getAnnotation(Issues.class);
        if (issues != null) {
            return Arrays.toString(issues.value())
                    .replace("[@io.qameta.allure.Issue(\"", "")
                    .replace("@io.qameta.allure.Issue(\"", "")
                    .replace("\")]", "")
                    .replace("\"),", ",");
        } else if (issue != null) {
            return issue.value();
        } else {
            return "";
        }
    }

    /**
     * Extracts TMS link annotation values from a test method.
     *
     * @param iTestResult current TestNG result
     * @return TMS link value(s), or empty string if none exist
     */
    public static String getTmsLinkAnnotationValue(ITestResult iTestResult) {
        var method = iTestResult.getMethod().getConstructorOrMethod().getMethod();
        TmsLink tmsLink = method.getAnnotation(TmsLink.class);
        TmsLinks tmsLinks = method.getAnnotation(TmsLinks.class);
        if (tmsLinks != null) {
            return Arrays.toString(tmsLinks.value())
                    .replace("[@io.qameta.allure.TmsLink(\"", "")
                    .replace("@io.qameta.allure.TmsLink(\"", "")
                    .replace("\")]", "")
                    .replace("\"),", ",");
        } else if (tmsLink != null) {
            return tmsLink.value();
        } else {
            return "";
        }
    }

    public static void failFast(ITestResult iTestResult) {
        // implementing the new kill switch at the start of every test method
        if (DriverFactoryHelper.isKillSwitch()) {
            SkipException ex = new SkipException("Skipping Test: " + iTestResult.getName());
            ReportManagerHelper.logDiscrete(ex);
            throw ex;
        }
    }

    public static void logTestInformation(ITestResult iTestResult) {
        ITestNGMethod iTestNGMethod = iTestResult.getMethod();
        String className;
        String methodName;
        String methodDescription = "";

        if (!iTestNGMethod.getQualifiedName().contains("AbstractTestNGCucumberTests")) {
            if (iTestNGMethod.isTest()) {
                className = ReportManagerHelper.getTestClassName();
                methodName = ReportManagerHelper.getTestMethodName();
                if (iTestNGMethod.getDescription() != null) {
                    methodDescription = iTestNGMethod.getDescription();
                }
                ReportManagerHelper.logTestInformation(className, methodName, methodDescription);
            } else if (iTestNGMethod instanceof ConfigurationMethod configurationMethod) {
                className = configurationMethod.getTestClass().getName();
                methodName = configurationMethod.getMethodName();
                var configurationMethodType = configurationMethod.getMethodName();

                ReportManagerHelper.logConfigurationMethodInformation(className, methodName, configurationMethodType);
            }
        }
    }

    public static void logFinishedTestInformation(ITestResult iTestResult) {
        ITestNGMethod iTestNGMethod = iTestResult.getMethod();
        String className;
        String methodName;
        String methodDescription = "";
        String methodStatus = "";

        if (!iTestNGMethod.getQualifiedName().contains("AbstractTestNGCucumberTests")) {
            if (iTestNGMethod.isTest()) {
                className = ReportManagerHelper.getTestClassName();
                methodName = ReportManagerHelper.getTestMethodName();
                if (iTestNGMethod.getDescription() != null) {
                    methodDescription = iTestNGMethod.getDescription();
                }
                if (iTestResult.getStatus() == ITestResult.SUCCESS) {
                    methodStatus = "Passed";
                } else if (iTestResult.getStatus() == ITestResult.FAILURE) {
                    methodStatus = "Failed";
                } else if (iTestResult.getStatus() == ITestResult.SKIP) {
                    methodStatus = "Skipped";
                }
                ReportManagerHelper.logFinishedTestInformation(className, methodName, methodDescription, methodStatus);
            }
        }
    }
}
