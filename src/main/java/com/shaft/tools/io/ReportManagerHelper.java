package com.shaft.tools.io;

import com.aventstack.extentreports.ExtentReports;
import com.aventstack.extentreports.ExtentTest;
import com.aventstack.extentreports.MediaEntityBuilder;
import com.aventstack.extentreports.markuputils.CodeLanguage;
import com.aventstack.extentreports.markuputils.MarkupHelper;
import com.aventstack.extentreports.reporter.ExtentSparkReporter;
import com.aventstack.extentreports.reporter.configuration.Theme;
import com.aventstack.extentreports.reporter.configuration.ViewName;
import com.shaft.api.RestActions;
import com.shaft.cli.FileActions;
import com.shaft.cli.TerminalActions;
import com.shaft.tools.listeners.CucumberFeatureListener;
import com.shaft.tools.support.JavaActions;
import io.qameta.allure.Allure;
import io.qameta.allure.Step;
import lombok.Getter;
import org.apache.commons.io.IOUtils;
import org.apache.commons.lang3.SystemUtils;
import org.apache.logging.log4j.Level;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.apache.logging.log4j.core.config.Configurator;
import org.testng.Reporter;

import java.io.*;
import java.net.URL;
import java.nio.charset.StandardCharsets;
import java.text.SimpleDateFormat;
import java.util.*;
import java.util.stream.Collectors;

public class ReportManagerHelper {
    private static final String TIMESTAMP_FORMAT = "dd-MM-yyyy HH:mm:ss.SSSS aaa";
    @Getter
    private static final ExtentReports extentReport = new ExtentReports();
    private static final String SHAFT_ENGINE_VERSION_PROPERTY_NAME = "shaftEngineVersion";
    private static final String ALLURE_VERSION_PROPERTY_NAME = "allureVersion";
    private static final String REPORT_MANAGER_PREFIX = "[ReportManager] ";
    private static final String SHAFT_ENGINE_LOGS_ATTACHMENT_TYPE = "SHAFT Engine Logs";
    private static final String allureExtractionLocation = System.getProperty("user.home") + File.separator + ".m2"
            + File.separator + "repository" + File.separator + "allure" + File.separator;
    private static String issuesLog = "";
    private static int issueCounter = 1;
    private static boolean discreteLogging = false;
    private static int totalNumberOfTests = 0;
    private static int testCasesCounter = 0;
    private static boolean debugMode = false;
    private static int openIssuesForFailedTestsCounter = 0;
    private static int openIssuesForPassedTestsCounter = 0;
    private static int failedTestsWithoutOpenIssuesCounter = 0;
    private static String allureResultsFolderPath = "";
    private static String allureBinaryPath = "";
    // TODO: refactor to regular class that can be instantiated within the test and
    private static List<List<String>> listOfOpenIssuesForFailedTests = new ArrayList<>();
    private static List<List<String>> listOfOpenIssuesForPassedTests = new ArrayList<>();
    private static List<List<String>> listOfNewIssuesForFailedTests = new ArrayList<>();
    private static String featureName = "";

    private static String extentReportsFolderPath = "";
    private static final ThreadLocal<ExtentTest> extentTest = new ThreadLocal<>();
    private static Logger logger;
    @Getter
    private static String extentReportFileName = "";
    private static boolean generateExtentReports = true;


    private ReportManagerHelper() {
        throw new IllegalStateException("Utility class");
    }

    public static void setOpenIssuesForFailedTestsCounter(int openIssuesForFailedTestsCounter) {
        ReportManagerHelper.openIssuesForFailedTestsCounter = openIssuesForFailedTestsCounter;
    }

    public static void setOpenIssuesForPassedTestsCounter(int openIssuesForPassedTestsCounter) {
        ReportManagerHelper.openIssuesForPassedTestsCounter = openIssuesForPassedTestsCounter;
    }

    public static void setFailedTestsWithoutOpenIssuesCounter(int failedTestsWithoutOpenIssuesCounter) {
        ReportManagerHelper.failedTestsWithoutOpenIssuesCounter = failedTestsWithoutOpenIssuesCounter;
    }

    public static void setListOfOpenIssuesForFailedTests(List<List<String>> listOfOpenIssuesForFailedTests) {
        ReportManagerHelper.listOfOpenIssuesForFailedTests = listOfOpenIssuesForFailedTests;
    }

    public static void setListOfOpenIssuesForPassedTests(List<List<String>> listOfOpenIssuesForPassedTests) {
        ReportManagerHelper.listOfOpenIssuesForPassedTests = listOfOpenIssuesForPassedTests;
    }

    public static void setListOfNewIssuesForFailedTests(List<List<String>> listOfNewIssuesForFailedTests) {
        ReportManagerHelper.listOfNewIssuesForFailedTests = listOfNewIssuesForFailedTests;
    }

    public static void logIssue(String issue) {
        if (issuesLog.trim().equals("")) {
            issuesLog += issueCounter + ", " + issue.trim();
        } else {
            issuesLog += System.lineSeparator() + issueCounter + ", " + issue.trim();
        }
        issueCounter++;
    }

    public static String prepareIssuesLog() {
        if (!listOfNewIssuesForFailedTests.isEmpty()) {
            listOfNewIssuesForFailedTests.forEach(issue -> logIssue("Test Method \"" + issue.get(0) + "." + issue.get(1)
                    + "\" failed. Please investigate and open a new Issue if needed.\n"));
        }
        if (!listOfOpenIssuesForPassedTests.isEmpty()) {
            listOfOpenIssuesForPassedTests.forEach(issue -> {
                if (!issue.get(3).trim().equals("")) {
                    logIssue("Test Method \"" + issue.get(0) + "." + issue.get(1)
                            + "\" passed. Please validate and close this open issue \"" + issue.get(2) + "\": \""
                            + issue.get(3) + "\".\n");
                } else {
                    logIssue("Test Method \"" + issue.get(0) + "." + issue.get(1)
                            + "\" passed. Please validate and close this open issue \"" + issue.get(2) + "\".\n");
                }

            });
        }
        if (!listOfOpenIssuesForFailedTests.isEmpty()) {
            listOfOpenIssuesForFailedTests.forEach(issue -> {
                if (!issue.get(3).trim().equals("")) {
                    logIssue("Test Method \"" + issue.get(0) + "." + issue.get(1) + "\" failed with open issue \""
                            + issue.get(2) + "\": \"" + issue.get(3) + "\".\n");
                } else {
                    logIssue("Test Method \"" + issue.get(0) + "." + issue.get(1) + "\" failed with open issue \""
                            + issue.get(2) + "\".\n");
                }
            });
        }

        if (!issuesLog.trim().equals("")) {
            return "Issue Summary: Total Issues = " + (issueCounter - 1) + ", New issues for Failed Tests = "
                    + failedTestsWithoutOpenIssuesCounter + ", Open issues for Passed Tests = "
                    + openIssuesForPassedTestsCounter + ", Open issues for Failed Tests = "
                    + openIssuesForFailedTestsCounter + ". Kindly check the attached Issue details.";
        } else {
            return "";
        }
    }

    /**
     * @return the discreteLogging
     */
    public static boolean getDiscreteLogging() {
        return discreteLogging;
    }

    /**
     * @param discreteLogging the discreteLogging to set
     */
    public static void setDiscreteLogging(boolean discreteLogging) {
        if (debugMode) {
            ReportManager.logDiscrete("Setting discrete logging to: \"" + discreteLogging + "\"");
        }
        ReportManagerHelper.discreteLogging = discreteLogging;
    }

    public static int getTestCasesCounter() {
        return testCasesCounter;
    }

    public static int getTotalNumberOfTests() {
        return totalNumberOfTests;
    }

    public static void setTotalNumberOfTests(int totalNumberOfTests) {
        ReportManagerHelper.totalNumberOfTests = totalNumberOfTests;
    }

    public static void setDebugMode(Boolean debugMode) {
        ReportManagerHelper.debugMode = debugMode;
    }

    public static void initializeAllureReportingEnvironment() {
        ReportManager.logDiscrete("Initializing Allure Reporting Environment...");
        System.setProperty("disableLogging", "true");
        allureResultsFolderPath = System.getProperty("allureResultsFolderPath").trim();
        cleanAllureResultsDirectory();
        downloadAndExtractAllureBinaries();
        writeGenerateReportShellFilesToProjectDirectory();
        writeEnvironmentVariablesToAllureResultsDirectory();
        System.setProperty("disableLogging", "false");
    }

    public static void logEngineVersion() {
        Configurator.initialize(null, PropertyFileManager.getDefaultPropertiesFolderPath() + "/log4j2.properties");
        logger = LogManager.getLogger(ReportManager.class.getName());
        String engineVersion = "Detected SHAFT Engine Version: \""
                + System.getProperty(SHAFT_ENGINE_VERSION_PROPERTY_NAME) + "\"";
        createImportantReportEntry(engineVersion, true);
    }

    public static void logTestInformation(String className, String testMethodName,
                                                       String testDescription) {
        testCasesCounter++;
        StringBuilder reportMessage = new StringBuilder();

        if (totalNumberOfTests > 0) {
            reportMessage.append("Starting Execution:\t");
            reportMessage.append("\"");
            reportMessage.append(testCasesCounter);
            reportMessage.append(" out of ");
            reportMessage.append(totalNumberOfTests);
            reportMessage.append("\" test cases in the current suite");
        } else {
            //it's only zero in case of CucumberTestRunner
            reportMessage.append("Starting Dynamic Cucumber Feature Execution:\t");
        }
        reportMessage.append("\nTest Method:\t\t\"").append(className).append(".").append(testMethodName).append("\"");

        if (!testDescription.equals("")) {
            reportMessage.append("\nTest Description:\t\"").append(testDescription).append("\"");
        }

        createImportantReportEntry(reportMessage.toString(), false);
    }

    public static void logScenarioInformation(String keyword, String name, String steps) {
        testCasesCounter++;
        createImportantReportEntry("Starting Execution:\t\"" + testCasesCounter + " out of " + totalNumberOfTests
                        + "\" scenarios in the \"" + featureName + "\" feature"
                        + "\n" + keyword + " Name:\t\t\"" + name
                        + "\"\n" + keyword + " Steps:\n" + steps,
                false);
    }

    public static void logConfigurationMethodInformation(String className, String testMethodName) {
        // In TestNG Reporter, this log entry is logged at the end of the previous test
        // (or null for the first test)
        createImportantReportEntry("Starting Execution of a Configuration (Setup or Teardown) Method\nTest Method:\t\t\""
                + className + "." + testMethodName + "\"", false);
    }

    public static String formatStackTraceToLogEntry(Throwable t) {
        return formatStackTraceToLogEntry(t, false);
    }


    /**
     * Adds a new attachment using the input parameters provided. The attachment is
     * displayed as a step in the execution report. Used for Screenshots.
     *
     * @param attachmentType    the type of this attachment
     * @param attachmentName    the name of this attachment
     * @param attachmentContent the content of this attachment
     */
    public static void attach(String attachmentType, String attachmentName, InputStream attachmentContent) {
        createAttachment(attachmentType, attachmentName, attachmentContent);
    }

    /**
     * Adds a new attachment using the input parameters provided. The attachment is
     * displayed as a step in the execution report. Used for Screenshots.
     *
     * @param attachmentType    the type of this attachment
     * @param attachmentName    the name of this attachment
     * @param attachmentContent the content of this attachment
     */
    public static void attach(String attachmentType, String attachmentName, String attachmentContent) {
        if (!attachmentContent.trim().equals("")) {
            createAttachment(attachmentType, attachmentName, new ByteArrayInputStream(attachmentContent.getBytes()));

        }
    }

    /**
     * Returns the log of the current test, and attaches it in the end of the test
     * execution report.
     *
     * @param currentMethodName name of the current test method to be used in the attachment name
     * @param testLog           content of the text log to be used as the attachment value
     */
    public static void attachTestLog(String currentMethodName, String testLog) {
        if (!testLog.isBlank()) {
            createAttachment(SHAFT_ENGINE_LOGS_ATTACHMENT_TYPE, "Current Method log: " + currentMethodName,
                    new ByteArrayInputStream(testLog.getBytes()));
        }
    }

    public static void attachFullLog(String executionEndTimestamp) {
            String fullLogCreated = "Successfully created attachment \"" + SHAFT_ENGINE_LOGS_ATTACHMENT_TYPE + " - "
                    + "Execution log" + "\"";
            createLogEntry(fullLogCreated, true);
            String copyrights = "This test run was powered by SHAFT Engine Version: \""
                    + System.getProperty(SHAFT_ENGINE_VERSION_PROPERTY_NAME) + "\"" + System.lineSeparator()
                    + "SHAFT Engine is licensed under the MIT License: [https://github.com/ShaftHQ/SHAFT_ENGINE/blob/master/LICENSE].";
        createImportantReportEntry(copyrights, true);
        createAttachment(SHAFT_ENGINE_LOGS_ATTACHMENT_TYPE, "Execution log: " + executionEndTimestamp,
                new ByteArrayInputStream(FileActions.getInstance().readFromImageFile(System.getProperty("appender.file.fileName"))));
    }

    public static void attachIssuesLog(String executionEndTimestamp) {
        String issueSummary = prepareIssuesLog();
        if (!issuesLog.trim().equals("")) {
            log(issueSummary,
                    Collections.singletonList(
                            Arrays.asList(SHAFT_ENGINE_LOGS_ATTACHMENT_TYPE, "Issues log CSV: " + executionEndTimestamp,
                                    new ByteArrayInputStream(issuesLog.trim().getBytes()))));
        }
    }

    public static void openAllureReportAfterExecution() {
        String commandToOpenAllureReport;
        if (Boolean.TRUE.equals(Boolean.valueOf(System.getProperty("openAllureReportAfterExecution").trim()))) {
            if (SystemUtils.IS_OS_WINDOWS) {
                commandToOpenAllureReport = ("generate_allure_report.bat");
            } else {
                commandToOpenAllureReport = ("sh generate_allure_report.sh");
            }
            new TerminalActions(true).performTerminalCommand(commandToOpenAllureReport);
        }
    }

    public static void generateAllureReportArchive() {
        if (Boolean.TRUE.equals(Boolean.valueOf(System.getProperty("generateAllureReportArchive").trim()))) {
            ReportManager.logDiscrete("Generating Allure Report Archive...");
            System.setProperty("disableLogging", "true");
            writeOpenReportShellFilesToGeneratedDirectory();
            writeAllureReportToGeneratedDirectory();
            createAllureReportArchiveAndCleanGeneratedDirectory();
            System.setProperty("disableLogging", "false");
        }
    }

    public static String getCallingMethodFullName() {
        StackTraceElement[] callingStack = Thread.currentThread().getStackTrace();
        var callingMethodFullName = new StringBuilder();
        for (var i = 1; i < callingStack.length; i++) {
            if (!callingStack[i].getClassName().contains("com.shaft")) {
                callingMethodFullName.append(callingStack[i].getClassName());
                if (!callingStack[i].getMethodName().isEmpty()) {
                    callingMethodFullName.append(".");
                    callingMethodFullName.append(callingStack[i].getMethodName());
                }
                break;
            }
        }
        return callingMethodFullName.toString();
    }

    public static String getTestClassName() {
        return Reporter.getCurrentTestResult().getMethod().getTestClass().getName();
    }

    public static String getTestMethodName() {
        if (Reporter.getCurrentTestResult() != null) {
            return Reporter.getCurrentTestResult().getMethod().getMethodName();
        } else {
            // this happens when running a cucumber feature file directly because there is no testNG Reporter instance
            return JavaActions.removeSpecialCharacters(CucumberFeatureListener.getLastStartedScenarioName());
        }
    }

    public static void setTestCaseName(String scenarioName) {
        Allure.getLifecycle().updateTestCase(testResult -> testResult.setName(scenarioName));
        if (!"".equals(featureName)) {
            Allure.getLifecycle().updateTestCase(testResult -> testResult.setFullName(featureName + ": " + scenarioName));
        }
    }

    public static void setTestCaseDescription(String scenarioSteps) {
        if (scenarioSteps.contains("و")) {
            Allure.getLifecycle().updateTestCase(testResult -> testResult.setDescriptionHtml("<p dir=\"rtl\">" + scenarioSteps + "</p>"));
        } else {
            Allure.getLifecycle().updateTestCase(testResult -> testResult.setDescriptionHtml("<p dir=\"ltr\">" + scenarioSteps + "</p>"));
        }
    }

    public static Boolean isCurrentTestPassed() {
        if (Reporter.getCurrentTestResult() != null) {
            return Reporter.getCurrentTestResult().isSuccess();
        } else {
            // this happens in case of native cucumber execution without TestNG Test Runner
            return CucumberFeatureListener.getIsLastFinishedStepOK();
        }
    }

    public static void setFeatureName(String featureName) {
        ReportManagerHelper.featureName = featureName;
    }

    public static void initializeExtentReportingEnvironment() {
        generateExtentReports = Boolean.parseBoolean(System.getProperty("generateExtentReports").trim());
        if (generateExtentReports) {
            ReportManager.logDiscrete("Initializing Extent Reporting Environment...");
            System.setProperty("disableLogging", "true");
            extentReportsFolderPath = System.getProperty("extentReportsFolderPath").trim();
            cleanExtentReportsDirectory();
            extentReportFileName = extentReportsFolderPath + "ExtentReports_" + (new SimpleDateFormat("dd-MM-yyyy_HH-mm-ss-SSSS-aaa")).format(System.currentTimeMillis()) + ".html";

            var spark = new ExtentSparkReporter(extentReportFileName)
                    .viewConfigurer()
                    .viewOrder()
                    .as(new ViewName[]{ViewName.DASHBOARD, ViewName.TEST, ViewName.EXCEPTION, ViewName.LOG})
                    .apply();

            spark.config().setTheme(Theme.STANDARD);
            spark.config().setDocumentTitle("Extent Reports");
            spark.config().setReportName("Extent Reports - Powered by SHAFT_Engine");
            extentReport.attachReporter(spark);

            System.setProperty("disableLogging", "false");
        }
    }

    private static void cleanExtentReportsDirectory() {
        if (Boolean.TRUE.equals(
                Boolean.valueOf(System.getProperty("cleanExtentReportsDirectoryBeforeExecution")))) {
            FileActions.getInstance().deleteFolder(extentReportsFolderPath.substring(0, extentReportsFolderPath.length() - 1));
        }

    }

    public static void extentReportsReset() {
        extentTest.remove();
    }

    public static void extentReportsCreateTest(String testName, String testDescription) {
        if (!extentReport.equals(new ExtentReports()) && extentReport != null){
            if (testDescription.equals("")) {
                extentTest.set(extentReport.createTest(testName));
            } else {
                extentTest.set(extentReport.createTest(testDescription));
            }
        }
    }

    public static void extentReportsPass(String message) {
        if (generateExtentReports && extentTest.get()!=null) {
            extentTest.get().pass(message);
        }
    }

    public static void extentReportsFail(String message) {
        if (generateExtentReports && extentTest.get()!=null) {
            extentTest.get().fail(message);
        }
    }

    public static void extentReportsFail(Throwable t) {
        if (generateExtentReports && extentTest.get()!=null) {
            extentTest.get().fail(t);
        }
    }

    public static void extentReportsSkip(String message) {
        if (generateExtentReports && extentTest.get()!=null) {
            extentTest.get().skip(message);
        }
    }

    public static void extentReportsSkip(Throwable t) {
        if (generateExtentReports && extentTest.get()!=null) {
            extentTest.get().skip(t);
        }
    }

    public static void extentReportsFlush() {
        if (generateExtentReports && extentReport!=null) {
            extentReport.flush();
        }
    }

    private static String formatStackTraceToLogEntry(Throwable t, boolean isCause) {
        var logBuilder = new StringBuilder();
        if (t != null) {
            StackTraceElement[] trace = t.getStackTrace();
            if (isCause) {
                logBuilder.append(System.lineSeparator()).append("Caused by: ");
            }
            logBuilder.append(t.getClass().getName()).append(":").append(System.lineSeparator()).append(t.getMessage()).append(System.lineSeparator());
            for (StackTraceElement stackTraceElement : trace) {
                logBuilder.append(stackTraceElement.toString()).append(System.lineSeparator());
            }
            logBuilder.append(formatStackTraceToLogEntry(t.getCause(), true));
        }
        return logBuilder.toString();
    }

    static void createLogEntry(String logText, Level loglevel) {
        if (!Boolean.parseBoolean(System.getProperty("disableLogging"))) {
            String timestamp = (new SimpleDateFormat(TIMESTAMP_FORMAT)).format(new Date(System.currentTimeMillis()));
            if (logText == null) {
                logText = "null";
            }
            String log = REPORT_MANAGER_PREFIX + logText.trim() + " @" + timestamp;
            Reporter.log(log, false);
            logger.log(loglevel, logText.trim());
        }
    }

    private static void createLogEntry(String logText, boolean addToFullLog) {
        if (!Boolean.parseBoolean(System.getProperty("disableLogging"))) {
            String timestamp = (new SimpleDateFormat(TIMESTAMP_FORMAT)).format(new Date(System.currentTimeMillis()));
            if (logText == null) {
                logText = "null";
            }
            String log = REPORT_MANAGER_PREFIX + logText.trim() + " @" + timestamp;
            Reporter.log(log, false);
            if (extentTest.get() !=null && !logText.contains("created attachment") && !logText.contains("<html")) {
                extentTest.get().info(logText);
            }

            if (addToFullLog) {
                logger.log(Level.INFO, logText.trim());
            }
        }
    }

    private static void createImportantReportEntry(String logText, Boolean addToFullLog) {
        boolean initialLoggingStatus = discreteLogging;
        setDiscreteLogging(false); // force log even if discrete logging was turned on
        String log = System.lineSeparator()
                + "################################################################################################################################################"
                + System.lineSeparator() + logText.trim() + System.lineSeparator()
                + "################################################################################################################################################";

        Reporter.log(log, false);
        logger.log(Level.INFO, log);
        setDiscreteLogging(initialLoggingStatus);
    }

    /**
     * Formats logText and adds timestamp, then logs it as a step in the execution
     * report.
     *
     * @param logText the text that needs to be logged in this action
     */
    @Step("{logText}")
    static void writeStepToReport(String logText) {
        createLogEntry(logText, false);
    }

    @Step("{logText}")
    static void writeStepToReport(String logText, List<List<Object>> attachments) {
        createLogEntry(logText, false);

        if (attachments != null && !attachments.isEmpty()) {
            attachments.forEach(attachment -> {
                if (attachment != null && !attachment.isEmpty() && attachment.get(2).getClass().toString().toLowerCase().contains("string")
                        && !attachment.get(2).getClass().toString().contains("StringInputStream")) {
                    if (!attachment.get(2).toString().isEmpty()) {
                        attach(attachment.get(0).toString(), attachment.get(1).toString(),
                                attachment.get(2).toString());
                    }
                } else if (attachment != null && !attachment.isEmpty()) {
                    if (attachment.get(2) instanceof byte[]) {
                        attach(attachment.get(0).toString(), attachment.get(1).toString(), new ByteArrayInputStream((byte[]) attachment.get(2)));
                    } else {
                        attach(attachment.get(0).toString(), attachment.get(1).toString(), (InputStream) attachment.get(2));
                    }
                }
            });
        }
    }

    private static void createAttachment(String attachmentType, String attachmentName, InputStream attachmentContent) {
        var baos = new ByteArrayOutputStream();
        try {
            attachmentContent.transferTo(baos);
        } catch (IOException e) {
            var error = "Error while creating Attachment";
            logger.info(error, e);
            Reporter.log(error, false);
        }

        String attachmentDescription = attachmentType + " - " + attachmentName;
        attachBasedOnFileType(attachmentType, attachmentName, baos, attachmentDescription);
        logAttachmentAction(attachmentType, attachmentName, baos);
    }

    private static void attachBasedOnFileType(String attachmentType, String attachmentName,
                                                           ByteArrayOutputStream attachmentContent, String attachmentDescription) {
        if (attachmentType.toLowerCase().contains("screenshot")) {
            Allure.addAttachment(attachmentDescription, "image/png", new ByteArrayInputStream(attachmentContent.toByteArray()), ".png");
            attachImageToExtentReport("image/png", new ByteArrayInputStream(attachmentContent.toByteArray()));
        } else if (attachmentType.toLowerCase().contains("recording")) {
            Allure.addAttachment(attachmentDescription, "video/mp4", new ByteArrayInputStream(attachmentContent.toByteArray()), ".mp4");
        } else if (attachmentType.toLowerCase().contains("gif")) {
            Allure.addAttachment(attachmentDescription, "image/gif", new ByteArrayInputStream(attachmentContent.toByteArray()), ".gif");
            attachImageToExtentReport("image/gif", new ByteArrayInputStream(attachmentContent.toByteArray()));
        } else if (attachmentType.toLowerCase().contains("csv") || attachmentName.toLowerCase().contains("csv")) {
            Allure.addAttachment(attachmentDescription, "text/csv", new ByteArrayInputStream(attachmentContent.toByteArray()), ".csv");
            attachCodeBlockToExtentReport("text/csv", new ByteArrayInputStream(attachmentContent.toByteArray()));
        } else if (attachmentType.toLowerCase().contains("xml") || attachmentName.toLowerCase().contains("xml")) {
            Allure.addAttachment(attachmentDescription, "text/xml", new ByteArrayInputStream(attachmentContent.toByteArray()), ".xml");
            attachCodeBlockToExtentReport("text/xml", new ByteArrayInputStream(attachmentContent.toByteArray()));
        } else if (attachmentType.toLowerCase().contains("excel") || attachmentName.toLowerCase().contains("excel")) {
            Allure.addAttachment(attachmentDescription, "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet", new ByteArrayInputStream(attachmentContent.toByteArray()), ".xlsx");
        } else if (attachmentType.toLowerCase().contains("json") || attachmentName.toLowerCase().contains("json")) {
            Allure.addAttachment(attachmentDescription, "text/json", new ByteArrayInputStream(attachmentContent.toByteArray()), ".json");
            attachCodeBlockToExtentReport("text/json", new ByteArrayInputStream(attachmentContent.toByteArray()));
        } else if (attachmentType.toLowerCase().contains("properties")) {
            Allure.addAttachment(attachmentDescription, "text/plain", new ByteArrayInputStream(attachmentContent.toByteArray()), ".properties");
        } else if (attachmentType.toLowerCase().contains("link")) {
            Allure.addAttachment(attachmentDescription, "text/uri-list", new ByteArrayInputStream(attachmentContent.toByteArray()), ".uri");
        } else if (attachmentType.toLowerCase().contains("engine logs")) {
            Allure.addAttachment(attachmentDescription, "text/plain", new ByteArrayInputStream(attachmentContent.toByteArray()), ".txt");
        } else {
            Allure.addAttachment(attachmentDescription, new ByteArrayInputStream(attachmentContent.toByteArray()));
        }
    }

    private static void logAttachmentAction(String attachmentType, String attachmentName, ByteArrayOutputStream attachmentContent) {
        createLogEntry("Successfully created attachment \"" + attachmentType + " - " + attachmentName + "\"", Level.INFO);
        if (debugMode && !attachmentType.contains(SHAFT_ENGINE_LOGS_ATTACHMENT_TYPE)
                && !attachmentType.equalsIgnoreCase("Selenium WebDriver Logs")
                && !attachmentType.toLowerCase().contains("screenshot")
                && !attachmentType.toLowerCase().contains("recording") && !attachmentType.toLowerCase().contains("gif")
                && !attachmentType.toLowerCase().contains("engine logs")) {
            String timestamp = (new SimpleDateFormat(TIMESTAMP_FORMAT)).format(new Date(System.currentTimeMillis()));

            String theString;
            var br = new BufferedReader(
                    new InputStreamReader(new ByteArrayInputStream(attachmentContent.toByteArray()), StandardCharsets.UTF_8));
            theString = br.lines().collect(Collectors.joining(System.lineSeparator()));
            if (!theString.isEmpty()) {
                String logEntry = REPORT_MANAGER_PREFIX + "Debugging Attachment Entry" + " @" + timestamp
                        + System.lineSeparator() + theString + System.lineSeparator();
                logger.info(logEntry);
            }
        }
    }

    private static void attachCodeBlockToExtentReport(String attachmentType, InputStream attachmentContent) {
        if (extentTest.get() !=null) {
            try {
                var codeBlock = IOUtils.toString(attachmentContent, StandardCharsets.UTF_8);
                switch (attachmentType) {
                    case "text/json" -> extentTest.get().info(MarkupHelper.createCodeBlock(codeBlock, CodeLanguage.JSON));
                    case "text/xml" -> extentTest.get().info(MarkupHelper.createCodeBlock(codeBlock, CodeLanguage.XML));
                    default -> extentTest.get().info(MarkupHelper.createCodeBlock(codeBlock));
                }
            } catch (IOException e) {
                ReportManager.logDiscrete("Failed to attach code block to extentReport.");
            }
        }
    }

    private static void attachImageToExtentReport(String attachmentType, InputStream attachmentContent) {
        if (extentTest.get() !=null) {
            try {
                var image = Base64.getEncoder().encodeToString(IOUtils.toByteArray(attachmentContent));
                if (attachmentType.toLowerCase().contains("gif")) {
                    extentTest.get().addScreenCaptureFromBase64String(image);
                } else {
                    extentTest.get().info(MediaEntityBuilder.createScreenCaptureFromBase64String(image).build());
                }
            } catch (IOException e) {
                ReportManager.logDiscrete("Failed to attach screenshot to extentReport.");
            }
        }
    }

    private static void cleanAllureResultsDirectory() {
        // clean allure-results directory before execution
        if (Boolean.TRUE.equals(
                Boolean.valueOf(System.getProperty("cleanAllureResultsDirectoryBeforeExecution")))) {
            try {
                FileActions.getInstance().deleteFolder(allureResultsFolderPath.substring(0, allureResultsFolderPath.length() - 1));
            } catch (Throwable t) {
                ReportManager.log("Failed to delete allure-results as it is currently open. Kindly restart your device to unlock the directory.");
            }
        }
    }

    private static void writeEnvironmentVariablesToAllureResultsDirectory() {
        // reads all environment variables and then formats and writes them to be read
        // by the Allure report
        var props = System.getProperties();
        var propertiesFileBuilder = new StringBuilder();
        propertiesFileBuilder.append("<environment>");
        // read properties from any explicit properties files
        for (var i = 0; i < props.size(); i++) {
            String propertyKey = ((String) (props.keySet().toArray())[i]).trim();
            String propertyValue = props.getProperty(propertyKey).trim();

            // excluding empty values, system properties (all system properties have "." in
            // their names), and any git branch issues
            if (!propertyValue.equals("") && !propertyValue.contains("==") && !propertyKey.contains(">>>")
                    && !propertyKey.contains("<<<")) {

                if (propertyValue.contains("&")) {
                    propertyValue = propertyValue.replace("&", "&amp;");
                }
                String parameter = "<parameter>" + "<key>" + propertyKey + "</key>" + "<value>" + propertyValue
                        + "</value>" + "</parameter>";
                if (propertyKey.equals(SHAFT_ENGINE_VERSION_PROPERTY_NAME)) {
                    // there's an open issue, when fixed this will be displayed properly
                    // https://github.com/allure-framework/allure2/issues/382
                    propertiesFileBuilder.insert(13, parameter);
                } else {
                    propertiesFileBuilder.append(parameter);
                }
            }
        }
        propertiesFileBuilder.append("</environment>");
        FileActions.getInstance().writeToFile(System.getProperty("allureResultsFolderPath"), "environment.xml",
                RestActions.formatXML(propertiesFileBuilder.toString()));
    }

    private static void downloadAndExtractAllureBinaries() {
        // extract allure from jar file to src/main/resources directory if it doesn't
        // already exist
        String allureVersion = System.getProperty(ALLURE_VERSION_PROPERTY_NAME);
        allureBinaryPath = allureExtractionLocation + "allure-" + allureVersion + File.separator + "bin" + File.separator + "allure";
        if (!FileActions.getInstance().doesFileExist(allureBinaryPath)) {
            try {
                //Runtime.getRuntime().exec("taskkill /F /IM java.exe");
                FileActions.getInstance().deleteFolder(allureExtractionLocation);
            } catch (AssertionError e) {
                ReportManager.logDiscrete("Couldn't clear the allure extraction directory. Kindly terminate any running java process or restart your machine to fix this issue.");
                ReportManagerHelper.log(e);
            }
            // download allure binary
            URL allureArchive = FileActions.getInstance().downloadFile(
                    "https://repo.maven.apache.org/maven2/io/qameta/allure/allure-commandline/" + allureVersion
                            + "/allure-commandline-" + allureVersion + ".zip",
                    "target" + File.separator + "allureBinary.zip");
            FileActions.getInstance().unpackArchive(allureArchive, allureExtractionLocation);
            // extract allure from SHAFT_Engine jar
            URL allureSHAFTConfigArchive = ReportManagerHelper.class.getResource("/resources/allure/allureBinary_SHAFTEngineConfigFiles.zip");
            FileActions.getInstance().unpackArchive(allureSHAFTConfigArchive,
                    allureExtractionLocation + "allure-" + allureVersion + File.separator);

            if (!SystemUtils.IS_OS_WINDOWS) {
                // make allure executable on Unix-based shells
                (new TerminalActions()).performTerminalCommand("chmod u+x " + allureBinaryPath);
            }
        }
    }

    private static void writeGenerateReportShellFilesToProjectDirectory() {
        String allureVersion = System.getProperty(ALLURE_VERSION_PROPERTY_NAME);
        // create generate_allure_report.sh or generate_allure_report.bat
        List<String> commandsToServeAllureReport;
        if (SystemUtils.IS_OS_WINDOWS) {
            // create windows batch file
            commandsToServeAllureReport = Arrays.asList("@echo off",
                    "set path=" + allureExtractionLocation + "allure-" + allureVersion + "\\bin;" + System.getProperty("java.home") + "\\bin;%path%",
                    "allure serve " + allureResultsFolderPath.substring(0, allureResultsFolderPath.length() - 1),
                    "pause", "exit");
            FileActions.getInstance().writeToFile("", "generate_allure_report.bat", commandsToServeAllureReport);
        } else {
            // create Unix-based sh file
            commandsToServeAllureReport = Arrays
                    .asList("#!/bin/bash", "parent_path=$( cd \"$(dirname \"${BASH_SOURCE[0]}\")\" ; pwd -P )",
                            "cd \"" + allureExtractionLocation + "allure-" + allureVersion + "/bin/\"",
                            "bash allure serve \"$parent_path/"
                                    + allureResultsFolderPath.substring(0, allureResultsFolderPath.length() - 1) + "\"",
                            "exit"

                    );
            FileActions.getInstance().writeToFile("", "generate_allure_report.sh", commandsToServeAllureReport);
            // make allure executable on Unix-based shells
            (new TerminalActions()).performTerminalCommand("chmod u+x generate_allure_report.sh");
        }
    }

    static boolean isInternalStep() {
        var callingMethodName = (new Throwable()).getStackTrace()[2].toString();
        return callingMethodName.contains("com.shaft");
    }

    /**
     * Adds a new attachment using the input parameters provided. The attachment is
     * displayed as a step in the execution report. Used for Screenshots.
     *
     * @param attachmentType    the type of this attachment
     * @param attachmentName    the name of this attachment
     * @param attachmentContent the content of this attachment
     */
    @Step("Attachment: {attachmentType} - {attachmentName}")
    static void attachAsStep(String attachmentType, String attachmentName, InputStream attachmentContent) {
        createAttachment(attachmentType, attachmentName, attachmentContent);
    }

    private static void writeOpenReportShellFilesToGeneratedDirectory() {
        List<String> commandsToOpenAllureReport;
        // create Unix-based sh file
        commandsToOpenAllureReport = Arrays.asList("#!/bin/bash",
                "parent_path=$( cd \"$(dirname \"${BASH_SOURCE[0]}\")\" ; pwd -P )",
                "cd \"$parent_path/allure/allure-" + System.getProperty(ALLURE_VERSION_PROPERTY_NAME) + "/bin/\"",
                "bash allure open \"$parent_path/allure-report\"", "exit");
        FileActions.getInstance().writeToFile("generatedReport/", "open_allure_report.sh", commandsToOpenAllureReport);

        // create windows batch file
        commandsToOpenAllureReport = Arrays.asList("@echo off",
                "set path=allure\\allure-" + System.getProperty(ALLURE_VERSION_PROPERTY_NAME) + "\\bin;" + System.getProperty("java.home") + ";%path%",
                "allure open allure-report", "pause", "exit");
        FileActions.getInstance().writeToFile("generatedReport/", "open_allure_report.bat", commandsToOpenAllureReport);

    }

    private static void writeAllureReportToGeneratedDirectory() {
        // add correct file extension based on target OS
        String commandToCreateAllureReport;
        allureBinaryPath = allureExtractionLocation + "allure-" + System.getProperty(ALLURE_VERSION_PROPERTY_NAME)
                + "/bin/allure";

        if (SystemUtils.IS_OS_WINDOWS) {
            commandToCreateAllureReport = allureBinaryPath + ".bat" + " generate \""
                    + allureResultsFolderPath.substring(0, allureResultsFolderPath.length() - 1)
                    + "\" -o \"generatedReport/allure-report\"";
        } else {
            commandToCreateAllureReport = allureBinaryPath + " generate "
                    + allureResultsFolderPath.substring(0, allureResultsFolderPath.length() - 1)
                    + " -o generatedReport/allure-report";
        }
        (new TerminalActions(false)).performTerminalCommand(commandToCreateAllureReport);
    }

    private static void createAllureReportArchiveAndCleanGeneratedDirectory() {
        if (FileActions.getInstance().doesFileExist(allureExtractionLocation)) {
            FileActions.getInstance().copyFolder(FileActions.getInstance().getAbsolutePath(allureExtractionLocation), "generatedReport/allure");
            FileActions.getInstance().zipFiles("generatedReport/", "generatedReport_" + new SimpleDateFormat("yyyyMMdd-HHmmss").format(new Date()) + ".zip");
        }
        FileActions.getInstance().deleteFile("generatedReport/");
    }

    public static void log(String logText, List<List<Object>> attachments) {
        if (getDiscreteLogging() && !logText.toLowerCase().contains("failed") && isInternalStep()) {
            createLogEntry(logText, Level.INFO);
            if (attachments != null && !attachments.isEmpty()) {
                attachments.forEach(attachment -> {
                    if (attachment != null && !attachment.isEmpty()) {
                        if (attachment.get(2) instanceof String) {
                            attachAsStep(attachment.get(0).toString(), attachment.get(1).toString(),
                                    new ByteArrayInputStream(attachment.get(2).toString().getBytes()));
                        } else {
                            attachAsStep(attachment.get(0).toString(), attachment.get(1).toString(),
                                    (InputStream) attachment.get(2));
                        }
                    }
                });
            }
        } else {
            if (attachments != null && !attachments.isEmpty() && (attachments.size()>1 || (attachments.get(0) !=null && !attachments.get(0).isEmpty()))) {
                writeStepToReport(logText, attachments);
            }else {
                writeStepToReport(logText);
            }
        }
    }

    public static void logNestedSteps(String logText, List<String> customLogMessages, List<List<Object>> attachments) {
        if (customLogMessages != null && customLogMessages.size() > 0 && !"".equals(customLogMessages.get(0).trim())) {
            String customLogText = customLogMessages.get(0);
            if (logText.toLowerCase().contains("passed")) {
                if (logText.toLowerCase().contains("verification")) {
                    customLogText = "Verification Passed: " + customLogText;
                } else {
                    customLogText = "Assertion Passed: " + customLogText;
                }
            } else {
                if (logText.toLowerCase().contains("verification")) {
                    customLogText = "Verification Failed: " + customLogText;
                } else {
                    customLogText = "Assertion Failed: " + customLogText;
                }
            }
            writeNestedStepsToReport(customLogText, logText, attachments);
        } else {
            if (attachments != null && !attachments.isEmpty() && !attachments.get(0).isEmpty()) {
                writeStepToReport(logText, attachments);
            }else {
                writeStepToReport(logText);
            }
        }
    }

    @Step("{customLog}")
    private static void writeNestedStepsToReport(String customLog, String stepLog, List<List<Object>> attachments) {
        createLogEntry(customLog, false);
        if (attachments != null && !attachments.isEmpty()) {
            attachments.forEach(attachment -> {
                if (attachment != null && !attachment.isEmpty() && attachment.get(2).getClass().toString().toLowerCase().contains("string")
                        && !attachment.get(2).getClass().toString().contains("StringInputStream")) {
                    if (!attachment.get(2).toString().isEmpty()) {
                        attach(attachment.get(0).toString(), attachment.get(1).toString(),
                                attachment.get(2).toString());
                    }
                } else if (attachment != null && !attachment.isEmpty()) {
                    if (attachment.get(2) instanceof byte[]) {
                        attach(attachment.get(0).toString(), attachment.get(1).toString(), new ByteArrayInputStream((byte[]) attachment.get(2)));
                    } else {
                        attach(attachment.get(0).toString(), attachment.get(1).toString(), (InputStream) attachment.get(2));
                    }
                }
            });
        }
    }

    /**
     * Format an exception message and stack trace, and calls attach to add it as a
     * log entry.
     *
     * @param t the throwable (exception or error) that will be logged in this
     *          action
     */
    public static void log(Throwable t) {
        String logText;
        logText = formatStackTraceToLogEntry(t);
        if (t.getMessage() != null) {
            log("An Exception Occured with this Message: " + t.getMessage().split("\n")[0].trim() + ".",
                    Collections.singletonList(Arrays.asList("Exception Stack Trace", t.getClass().getName(), logText)));
        } else {
            log("An Exception Occured",
                    Collections.singletonList(Arrays.asList("Exception Stack Trace", t.getClass().getName(), logText)));
        }
    }

    public static void logDiscrete(Throwable t) {
        createLogEntry(formatStackTraceToLogEntry(t), Level.INFO);
    }
}
