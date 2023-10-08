package com.shaft.listeners.internal;

import com.shaft.driver.SHAFT;
import com.shaft.driver.internal.DriverFactoryHelper;
import com.shaft.gui.internal.image.ScreenshotManager;
import com.shaft.gui.internal.video.RecordManager;
import com.shaft.tools.io.ReportManager;
import com.shaft.tools.io.internal.ReportManagerHelper;
import io.qameta.allure.Issue;
import io.qameta.allure.Issues;
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
        if (!(testSuite.getAllMethods().size() == 1 && testSuite.getAllMethods().get(0).getMethodName().equals("runScenario"))) {
            ReportManagerHelper.setTotalNumberOfTests(testSuite.getAllMethods().size());
        }
    }

    public static void updateConfigurationMethodLogs(ITestResult iTestResult) {
        if (iTestResult.getMethod().isTest()) {
            //attach before configuration logs to the current test
            beforeMethods.forEach(TestNGListenerHelper::attachTestArtifacts);
            beforeMethods.clear();

            //attach current test logs to the current test
            TestNGListenerHelper.attachTestArtifacts(iTestResult);

            //point Allure UUID to the current test method
//            TestNGListenerHelper.writeAllureUUID();
        } else {
            if (iTestResult.getMethod().isBeforeMethodConfiguration()) {
                // get test result and store it for later processing
                beforeMethods.add(iTestResult);
            } else if (iTestResult.getMethod().isAfterMethodConfiguration()) {
                // get test result and store it for later processing
                afterMethods.add(iTestResult);
            }
        }

        /*
         * unable to attach afterMethod logs to the relevant test method
         * TODO: find a fix
         */
        if (!afterMethods.isEmpty()) {
            //point Allure UUID to the last executed test method
//            TestNGListenerHelper.readAllureUUID();

            //attach after configuration logs to the previous test
            afterMethods.forEach(TestNGListenerHelper::attachTestArtifacts);
            afterMethods.clear();
        }
    }

    public static String getTestName() {
        return testName.get();
    }

    public static void setTestName(ITestContext iTestContext) {
        testName.set(iTestContext.getCurrentXmlTest().getName());
    }

    public static void configureCrossBrowserExecution(List<XmlSuite> suites) {
        if (!"off".equals(SHAFT.Properties.platform.crossBrowserMode())) {
            suites.forEach(suite -> {
                var firefox_test = (XmlTest) suite.getTests().get(0).clone();
                firefox_test.setParameters(Map.of(
                        "executionAddress", "dockerized",
                        "targetOperatingSystem", Platform.LINUX.name(),
                        "targetBrowserName", Browser.FIREFOX.browserName()
                ));
                firefox_test.setThreadCount(SHAFT.Properties.testNG.threadCount());
                firefox_test.setParallel(XmlSuite.ParallelMode.valueOf(SHAFT.Properties.testNG.parallel()));
                firefox_test.setName(firefox_test.getName() + " - FireFox");

                var safari_test = (XmlTest) suite.getTests().get(0).clone();
                safari_test.setParameters(Map.of(
                        "executionAddress", "dockerized",
                        "targetOperatingSystem", Platform.LINUX.name(),
                        "targetBrowserName", Browser.SAFARI.browserName()
                ));
                safari_test.setThreadCount(SHAFT.Properties.testNG.threadCount());
                safari_test.setParallel(XmlSuite.ParallelMode.valueOf(SHAFT.Properties.testNG.parallel()));
                safari_test.setName(safari_test.getName() + " - Safari");

                var chrome_test = (XmlTest) suite.getTests().get(0);
                chrome_test.setParameters(Map.of(
                        "executionAddress", "dockerized",
                        "targetOperatingSystem", Platform.LINUX.name(),
                        "targetBrowserName", Browser.CHROME.browserName()
                ));
                chrome_test.setThreadCount(SHAFT.Properties.testNG.threadCount());
                chrome_test.setParallel(XmlSuite.ParallelMode.valueOf(SHAFT.Properties.testNG.parallel()));
                chrome_test.setName(chrome_test.getName() + " - Chrome");

                if (SHAFT.Properties.platform.crossBrowserMode().equals("parallelized")) {
                    suite.setParallel(XmlSuite.ParallelMode.TESTS);
                    suite.setThreadCount(3);
                    SHAFT.Properties.visuals.set().videoParamsRecordVideo(true);
                    SHAFT.Properties.visuals.set().screenshotParamsScreenshotType("Regular");
                }
            });
//        } else {
            // IMPORTANT: the below code block introduced the following issue:

            //if not cross browser mode, still add some properties as parameters to the test suites for better reporting
//            var parameters = new java.util.HashMap<>(Map.of(
//                    "executionAddress", Properties.platform.executionAddress(),
//                    "targetOperatingSystem", Properties.platform.targetOperatingSystem()
//            ));
//
//            if (DriverFactoryHelper.isWebExecution()) {
//                parameters.put("targetBrowserName", Properties.web.targetBrowserName());
//            } else {
//                parameters.put("automationName", Properties.mobile.automationName());
//            }
//            suites.forEach(suite -> {
//                var params = suite.getParameters();
//                params.putAll(parameters);
//                suite.setParameters(params);
//            });
        }
    }

    public static void configureTestNGProperties(List<XmlSuite> suites) {
        suites.forEach(suite -> {
            suite.setPreserveOrder(SHAFT.Properties.testNG.preserveOrder());
            suite.setGroupByInstances(SHAFT.Properties.testNG.groupByInstances());
            suite.setVerbose(SHAFT.Properties.testNG.verbose());
            suite.setParallel(XmlSuite.ParallelMode.valueOf(SHAFT.Properties.testNG.parallel()));
            suite.setThreadCount(SHAFT.Properties.testNG.threadCount());
            suite.setDataProviderThreadCount(SHAFT.Properties.testNG.dataProviderThreadCount());

            if (SHAFT.Properties.reporting.debugMode()) {
                ReportManager.log("getPreserveOrder: " + suite.getPreserveOrder());
                ReportManager.log("getDataProviderThreadCount: " + suite.getDataProviderThreadCount());
                ReportManager.log("getThreadCount: " + suite.getThreadCount());
                ReportManager.log("getVerbose: " + suite.getVerbose());
                ReportManager.log("getGroupByInstances: " + suite.getGroupByInstances());
                ReportManager.log("getParallel: " + suite.getParallel());
            }
        });
    }

    public static void updateDefaultSuiteAndTestNames(List<XmlSuite> suites) {
//        var prefix = "SHAFT: ";
        var prefix = "";
        // rename default suite and test
        suites.forEach(suite -> {
            if (suite.getName().trim().equalsIgnoreCase("default suite")
                    || suite.getName().trim().equalsIgnoreCase("surefire suite")) {
                suite.setName(prefix + "Suite");
            } else {
                suite.setName(prefix + suite.getName());
            }
            suite.getTests().forEach(test -> {
                if (test.getName().trim().equalsIgnoreCase("default test")
                        || test.getName().trim().equalsIgnoreCase("surefire test") || test.getName().trim().equalsIgnoreCase("SHAFT_ENGINE")) {
                    test.setName(prefix + "Test");
                } else {
                    test.setName(prefix + test.getName());
                }
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
            attachment = ScreenshotManager.attachAnimatedGif();
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

    public static Boolean testHasIssueAnnotation(ITestResult iTestResult) {
        var method = iTestResult.getMethod().getConstructorOrMethod().getMethod();
        Issue issue = method.getAnnotation(Issue.class);
        return issue != null;
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
                ReportManagerHelper.extentReportsCreateTest(className + "." + methodName, methodDescription);
            } else if (iTestNGMethod instanceof ConfigurationMethod configurationMethod) {
                className = configurationMethod.getTestClass().getName();
                methodName = configurationMethod.getMethodName();
                var configurationMethodType = configurationMethod.getMethodName();

                ReportManagerHelper.logConfigurationMethodInformation(className, methodName, configurationMethodType);
                ReportManagerHelper.extentReportsReset();
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
