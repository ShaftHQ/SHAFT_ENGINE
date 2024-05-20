package com.shaft.listeners.internal;

import com.shaft.driver.SHAFT;
import com.shaft.driver.internal.DriverFactory.DriverFactoryHelper;
import com.shaft.enums.internal.Screenshots;
import com.shaft.gui.internal.image.AnimatedGifManager;
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
import java.util.List;
import java.util.Map;

public class TestNGListenerHelper {

    private static final ArrayList<ITestResult> beforeMethods = new ArrayList<>();
    private static final ArrayList<ITestResult> afterMethods = new ArrayList<>();
    private static final ThreadLocal<String> testName = new ThreadLocal<>();

    public static void setTotalNumberOfTests(ISuite testSuite) {
        // This condition checks to confirm that this is not a cucumber test runner instance
        // If this condition is removed the total number of tests will be zero because the cucumber
        // test runner doesn't have any test methods
        if (!(testSuite.getAllMethods().size() == 1 && testSuite.getAllMethods().getFirst().getMethodName().equals("runScenario"))) {
            ReportManagerHelper.setTotalNumberOfTests(testSuite.getAllMethods().size());
        }
    }

    public static void attachConfigurationMethods() {
        attachBeforeConfigurationMethods();
        attachAfterConfigurationMethods();
    }

    public static void updateConfigurationMethods(ITestResult iTestResult) {
        updateBeforeConfigurationMethods(iTestResult);
        updateAfterConfigurationMethods(iTestResult);
    }

    private static void attachBeforeConfigurationMethods() {
        if (!beforeMethods.isEmpty()) {
            if (beforeMethods.size() > 1) {
                TestNGListenerHelper.attachTestArtifacts(beforeMethods.getLast());
            } else {
                TestNGListenerHelper.attachTestArtifacts(beforeMethods.getFirst());
            }
            beforeMethods.clear();
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
        if (!afterMethods.isEmpty()) {
            if (afterMethods.size() > 1) {
                TestNGListenerHelper.attachTestArtifacts(afterMethods.getLast());
            } else {
                TestNGListenerHelper.attachTestArtifacts(afterMethods.getFirst());
            }
            afterMethods.clear();
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

    public static String getTestName() {
        return testName.get();
    }

    public static void setTestName(ITestContext iTestContext) {
        testName.set(iTestContext.getCurrentXmlTest().getName());
    }

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

    public static void attachConfigurationHelperClass(List<XmlSuite> suites) {
        suites.forEach(xmlSuite ->
                xmlSuite.getTests().forEach(xmlTest ->
                        xmlTest.getClasses().add(new XmlClass(ConfigurationHelper.class.getName()))));
    }

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
