package com.shaft.listeners.internal;

import com.shaft.properties.internal.Properties;
import com.shaft.properties.internal.ThreadLocalPropertiesManager;
import com.shaft.tools.io.internal.ReportManagerHelper;
import io.qameta.allure.Issue;
import io.qameta.allure.Issues;
import io.qameta.allure.TmsLink;
import io.qameta.allure.TmsLinks;
import org.testng.*;
import org.testng.internal.ConfigurationMethod;
import org.testng.xml.XmlClass;
import org.testng.xml.XmlSuite;
import org.testng.xml.XmlTest;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

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
        invokeVoidIfAvailable("com.shaft.gui.internal.locator.LocatorBuilder", "cleanup");
    }

    /**
     * Expands suites for cross-browser execution according to configured mode.
     *
     * @param suites mutable list of TestNG suites
     */
    public static void configureCrossBrowserExecution(List<XmlSuite> suites) {
        try {
            Class<?> cls = Class.forName("com.shaft.listeners.internal.CrossBrowserHelper",
                    false, Thread.currentThread().getContextClassLoader());
            cls.getMethod("configure", List.class).invoke(null, suites);
        } catch (ClassNotFoundException ignored) {
        } catch (Exception e) {
            ReportManagerHelper.logDiscrete(e);
        }
    }

    public static void configureTestNGProperties(List<XmlSuite> suites) {
        suites.forEach(suite -> {
            suite.setDataProviderThreadCount(Properties.testNG.dataProviderThreadCount());
            suite.getTests().forEach(xmlTest -> {
                xmlTest.setPreserveOrder(Properties.testNG.preserveOrder());
                xmlTest.setGroupByInstances(Properties.testNG.groupByInstances());
                xmlTest.setVerbose(Properties.testNG.verbose());
                double threadCount = Properties.testNG.threadCount();
                if ("DYNAMIC".equals(Properties.testNG.parallelMode())) {
                    threadCount = threadCount * Runtime.getRuntime().availableProcessors();
                    xmlTest.setThreadCount((int) Math.floor(threadCount));
                } else {
                    xmlTest.setThreadCount((int) Math.floor(threadCount));
                }
                xmlTest.setParallel(XmlSuite.ParallelMode.valueOf(Properties.testNG.parallel()));
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
        String PROXY_SERVER_SETTINGS = Properties.platform.proxy();
        if (Properties.platform.jvmProxySettings() && !PROXY_SERVER_SETTINGS.isEmpty()) {
            String[] proxyHostPort = PROXY_SERVER_SETTINGS.split(":");
            ThreadLocalPropertiesManager.setGlobalProperty("http.proxyHost", proxyHostPort[0]);
            ThreadLocalPropertiesManager.setGlobalProperty("http.proxyPort", proxyHostPort[1]);
            ThreadLocalPropertiesManager.setGlobalProperty("https.proxyHost", proxyHostPort[0]);
            ThreadLocalPropertiesManager.setGlobalProperty("https.proxyPort", proxyHostPort[1]);
            ThreadLocalPropertiesManager.setGlobalProperty("ftp.proxyHost", proxyHostPort[0]);
            ThreadLocalPropertiesManager.setGlobalProperty("ftp.proxyPort", proxyHostPort[1]);
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
            if (Properties.visuals.videoParamsScope().equals("TestMethod")) {
                invokeVoidIfAvailable("com.shaft.gui.internal.video.RecordManager", "attachVideoRecording");
                attachment = invokeStringIfAvailable("com.shaft.gui.internal.video.RecordManager", "getVideoRecordingFilePath");
                if (!attachment.isEmpty())
                    attachments.add(attachment);
            }
            attachment = invokeStringIfAvailable("com.shaft.gui.internal.image.AnimatedGifManager", "attachAnimatedGif");
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
        if (Properties.flags.skipTestsWithLinkedIssues()) {
            var constructorOrMethod = iTestResult.getMethod().getConstructorOrMethod();
            if (constructorOrMethod == null) return;
            var method = constructorOrMethod.getMethod();
            if (method == null) return;
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
        var constructorOrMethod = iTestResult.getMethod().getConstructorOrMethod();
        if (constructorOrMethod == null) return "";
        var method = constructorOrMethod.getMethod();
        if (method == null) return "";
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
        var constructorOrMethod = iTestResult.getMethod().getConstructorOrMethod();
        if (constructorOrMethod == null) return "";
        var method = constructorOrMethod.getMethod();
        if (method == null) return "";
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
        if (invokeIsKillSwitch()) {
            SkipException ex = new SkipException("Skipping Test: " + iTestResult.getName());
            ReportManagerHelper.logDiscrete(ex);
            throw ex;
        }
    }

    private static boolean invokeIsKillSwitch() {
        try {
            var cls = Class.forName("com.shaft.driver.internal.DriverFactory.DriverFactoryHelper",
                    false, Thread.currentThread().getContextClassLoader());
            return Boolean.TRUE.equals(cls.getMethod("isKillSwitch").invoke(null));
        } catch (ClassNotFoundException ignored) {
            return false;
        } catch (Exception e) {
            ReportManagerHelper.logDiscrete(e);
            return false;
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

    private static void invokeVoidIfAvailable(String className, String methodName) {
        try {
            var cls = Class.forName(className, false, Thread.currentThread().getContextClassLoader());
            cls.getMethod(methodName).invoke(null);
        } catch (ClassNotFoundException ignored) {
        } catch (Exception e) {
            ReportManagerHelper.logDiscrete(e);
        }
    }

    private static String invokeStringIfAvailable(String className, String methodName) {
        try {
            var cls = Class.forName(className, false, Thread.currentThread().getContextClassLoader());
            return (String) cls.getMethod(methodName).invoke(null);
        } catch (ClassNotFoundException ignored) {
            return "";
        } catch (Exception e) {
            ReportManagerHelper.logDiscrete(e);
            return "";
        }
    }
}
