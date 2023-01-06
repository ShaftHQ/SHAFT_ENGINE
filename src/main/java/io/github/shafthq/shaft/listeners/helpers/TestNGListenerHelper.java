package io.github.shafthq.shaft.listeners.helpers;

import com.shaft.tools.io.ReportManager;
import io.github.shafthq.shaft.driver.DriverFactoryHelper;
import io.github.shafthq.shaft.gui.image.ScreenshotManager;
import io.github.shafthq.shaft.gui.video.RecordManager;
import io.github.shafthq.shaft.tools.io.helpers.ReportManagerHelper;
import io.qameta.allure.Issue;
import io.qameta.allure.Issues;
import org.testng.ITestNGMethod;
import org.testng.ITestResult;
import org.testng.Reporter;
import org.testng.SkipException;
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

    public static void configureCrossBrowserExecution(List<XmlSuite> suites) {
        if (System.getProperty("SHAFT.CrossBrowserMode") != null
                && !"off".equals(System.getProperty("SHAFT.CrossBrowserMode"))) {
            suites.forEach(suite -> {
                var firefox_test = (XmlTest) suite.getTests().get(0).clone();
                firefox_test.setParameters(Map.of(
                        "executionAddress", "dockerized",
                        "targetOperatingSystem", "Linux",
                        "targetBrowserName", "MozillaFirefox"
                ));
                firefox_test.setThreadCount(1);
                firefox_test.setParallel(XmlSuite.ParallelMode.NONE);
                firefox_test.setName(firefox_test.getName() + " - FireFox");

                var safari_test = (XmlTest) suite.getTests().get(0).clone();
                safari_test.setParameters(Map.of(
                        "executionAddress", "dockerized",
                        "targetOperatingSystem", "Linux",
                        "targetBrowserName", "Safari"
                ));
                safari_test.setThreadCount(1);
                safari_test.setParallel(XmlSuite.ParallelMode.NONE);
                safari_test.setName(safari_test.getName() + " - Safari");

                var chrome_test = (XmlTest) suite.getTests().get(0);
                chrome_test.setParameters(Map.of(
                        "executionAddress", "dockerized",
                        "targetOperatingSystem", "Linux",
                        "targetBrowserName", "GoogleChrome"
                ));
                chrome_test.setThreadCount(1);
                chrome_test.setParallel(XmlSuite.ParallelMode.NONE);
                chrome_test.setName(chrome_test.getName() + " - Chrome");

                if ("parallelized".equals(System.getProperty("SHAFT.CrossBrowserMode"))) {
                    suite.setParallel(XmlSuite.ParallelMode.TESTS);
                    suite.setThreadCount(3);
                    System.setProperty("screenshotParams_screenshotType", "Regular");
                }
            });
        }
    }

    public static void configureTestNGProperties(List<XmlSuite> suites) {
        suites.forEach(suite -> {
            suite.setPreserveOrder(Boolean.valueOf(System.getProperty("setPreserveOrder")));
            suite.setGroupByInstances(Boolean.parseBoolean(System.getProperty("setGroupByInstances")));
            suite.setVerbose(Integer.parseInt(System.getProperty("setVerbose")));
            suite.setParallel(XmlSuite.ParallelMode.valueOf(System.getProperty("setParallel")));
            suite.setThreadCount(Integer.parseInt(System.getProperty("setThreadCount")));
            suite.setDataProviderThreadCount(Integer.parseInt(System.getProperty("setDataProviderThreadCount")));

            if (Boolean.TRUE.equals(Boolean.valueOf(System.getProperty("debugMode")))) {
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

    public static void attachReportHelperClass(List<XmlSuite> suites) {
        suites.forEach(xmlSuite -> xmlSuite.getTests().forEach(xmlTest -> xmlTest.getClasses().add(new XmlClass(ConfigurationHelper.class.getName()))));
    }

    public static void configureJVMProxy() {
        String PROXY_SERVER_SETTINGS = System.getProperty("com.SHAFT.proxySettings");
        if (!PROXY_SERVER_SETTINGS.equals("")) {
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
            if (System.getProperty("videoParams_scope").trim().equals("TestMethod")) {
                RecordManager.attachVideoRecording();
                attachment = RecordManager.getVideoRecordingFilePath();
                if (!attachment.equals(""))
                    attachments.add(attachment);
            }
            attachment = ScreenshotManager.attachAnimatedGif();
            if (!attachment.equals(""))
                attachments.add(attachment);

            String logText = TestNGListenerHelper.createTestLog(Reporter.getOutput(iTestResult));
            ReportManagerHelper.attachTestLog(iTestNGMethod.getMethodName(),
                    logText);
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
        if (Boolean.TRUE.equals(Boolean.valueOf(System.getProperty("skipTestsWithLinkedIssues")))) {
            var method = iTestResult.getMethod().getConstructorOrMethod().getMethod();
            Issue issue = method.getAnnotation(Issue.class);
            if (issue != null) {
                SkipException ex = new SkipException("Skipping Test as it's expected to fail due to open issue: [" + issue.value() + "]");
                ReportManagerHelper.log(ex);
                throw ex;
            }
            Issues issues = method.getAnnotation(Issues.class);
            if (issues != null) {
                StringBuilder issueNames = new StringBuilder();
                Arrays.stream(issues.value()).iterator().forEachRemaining(issueI -> issueNames.append(issueI.value()).append(" ,"));
                SkipException ex = new SkipException("Skipping Test as it's expected to fail due to open issues: [" + issueNames.substring(0, issueNames.length() - 2) + "]");
                ReportManagerHelper.log(ex);
                throw ex;
            }
        }
    }

    public static void failFast(ITestResult iTestResult) {
        // implementing the new kill switch at the start of every test method
        if (DriverFactoryHelper.isKillSwitch()) {
            SkipException ex = new SkipException("Skipping Test: " + iTestResult.getName());
            ReportManagerHelper.log(ex);
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
            } else if (iTestNGMethod instanceof ConfigurationMethod) {
                className = iTestNGMethod.getTestClass().getName();
                methodName = iTestNGMethod.getMethodName();

                ReportManagerHelper.logConfigurationMethodInformation(className, methodName);
                ReportManagerHelper.extentReportsReset();
            }
        }
    }
}
