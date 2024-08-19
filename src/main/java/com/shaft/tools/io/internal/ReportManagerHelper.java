package com.shaft.tools.io.internal;

import com.shaft.cli.FileActions;
import com.shaft.cli.TerminalActions;
import com.shaft.driver.SHAFT;
import com.shaft.listeners.CucumberFeatureListener;
import com.shaft.properties.internal.PropertyFileManager;
import com.shaft.tools.internal.support.JavaHelper;
import com.shaft.tools.io.ReportManager;
import io.qameta.allure.Allure;
import io.qameta.allure.Step;
import io.qameta.allure.model.Status;
import io.qameta.allure.model.StatusDetails;
import lombok.Getter;
import org.apache.commons.lang3.SystemUtils;
import org.apache.logging.log4j.Level;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.apache.logging.log4j.core.config.Configurator;
import org.testng.ITestResult;
import org.testng.Reporter;

import java.io.*;
import java.nio.charset.StandardCharsets;
import java.text.SimpleDateFormat;
import java.util.*;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;


//@Getter
@SuppressWarnings("unused")
public class ReportManagerHelper {
    private static final String TIMESTAMP_FORMAT = "dd-MM-yyyy HH:mm:ss.SSSS aaa";
    private static final String REPORT_MANAGER_PREFIX = "[ReportManager] ";
    private static final String SHAFT_ENGINE_LOGS_ATTACHMENT_TYPE = "SHAFT Engine Logs";
    private static String issuesLog = "";
    private static int issueCounter = 1;
    private static boolean discreteLogging = false;
    @Getter
    private static int totalNumberOfTests = 0;
    private static int testCasesCounter = 0;
    private static boolean debugMode = false;
    private static int openIssuesForFailedTestsCounter = 0;
    @Getter
    private static int openIssuesForPassedTestsCounter = 0;
    @Getter
    private static int failedTestsWithoutOpenIssuesCounter = 0;
    // TODO: refactor to regular class that can be instantiated within the test
    private static List<List<String>> listOfOpenIssuesForFailedTests = new ArrayList<>();
    private static List<List<String>> listOfOpenIssuesForPassedTests = new ArrayList<>();
    private static List<List<String>> listOfNewIssuesForFailedTests = new ArrayList<>();
    private static String featureName = "";
    private static Logger logger;

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

    public static int getIssueCounter() {
        return (issueCounter - 1);
    }

    public static int getOpenIssuesForFailedTestsCounters() {
        return openIssuesForFailedTestsCounter;
    }

    public static void logIssue(String issue) {
        if (issuesLog.trim().isEmpty()) {
            issuesLog += issueCounter + ", " + issue.trim();
        } else {
            issuesLog += System.lineSeparator() + issueCounter + ", " + issue.trim();
        }
        issueCounter++;
    }

    public static String prepareIssuesLog() {
        if (!listOfNewIssuesForFailedTests.isEmpty()) {
            listOfNewIssuesForFailedTests.forEach(issue -> logIssue("Test Method '" + issue.getFirst() + "." + issue.get(1)
                    + "' failed. Please investigate and open a new Issue if needed.\n"));
        }
        if (!listOfOpenIssuesForPassedTests.isEmpty()) {
            listOfOpenIssuesForPassedTests.forEach(issue -> {
                if (issue.get(3) != null && !issue.get(3).trim().isEmpty()) {
                    logIssue("Test Method '" + issue.get(0) + "." + issue.get(1)
                            + "' passed. Please validate and close this open issue '" + issue.get(2) + "': '"
                            + issue.get(3) + "'.\n");
                } else {
                    logIssue("Test Method '" + issue.get(0) + "." + issue.get(1)
                            + "' passed. Please validate and close this open issue '" + issue.get(2) + "'.\n");
                }

            });
        }
        if (!listOfOpenIssuesForFailedTests.isEmpty()) {
            listOfOpenIssuesForFailedTests.forEach(issue -> {
                if (issue.get(3) != null && !issue.get(3).trim().isEmpty()) {
                    logIssue("Test Method '" + issue.get(0) + "." + issue.get(1) + "' failed with open issue '"
                            + issue.get(2) + "': '" + issue.get(3) + "'.\n");
                } else {
                    logIssue("Test Method '" + issue.get(0) + "." + issue.get(1) + "' failed with open issue '"
                            + issue.get(2) + "'.\n");
                }
            });
        }

        if (!issuesLog.trim().isEmpty()) {
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
            ReportManager.logDiscrete("Setting discrete logging to: '" + discreteLogging + "'");
        }
        ReportManagerHelper.discreteLogging = discreteLogging;
    }

    public static void setTotalNumberOfTests(int totalNumberOfTests) {
        ReportManagerHelper.totalNumberOfTests = totalNumberOfTests;
    }

    public static void setDebugMode(Boolean debugMode) {
        ReportManagerHelper.debugMode = debugMode;
    }

    private static void initializeLogger() {
        // delete previous run execution log
        FileActions.getInstance(true).deleteFile(System.getProperty("appender.file.fileName"));
        // initialize
        Configurator.initialize(null, PropertyFileManager.getCUSTOM_PROPERTIES_FOLDER_PATH() + "/log4j2.properties");
        logger = LogManager.getLogger(ReportManager.class.getName());
    }

    public static void logImportantEntry(String logText, Level logLevel) {
        createImportantReportEntry(logText, logLevel);
    }

    public static void logEngineVersion() {
        if (logger == null) {
            initializeLogger();
        }
        System.setOut(new PrintStream(new LogRedirector(logger, Level.INFO)));
        System.setErr(new PrintStream(new LogRedirector(logger, Level.WARN)));
        String engineVersion = "Powered by \033[1mSHAFT v." + SHAFT.Properties.internal.shaftEngineVersion() + "\033[22m";
        createImportantReportEntry(engineVersion + "\n" + "Visit SHAFT's user guide \033[4mhttps://shafthq.github.io/\033[24m to learn more");
    }

    public static void logEngineClosure() {
        // https://stackoverflow.com/questions/4842424/list-of-ansi-color-escape-sequences
        String copyrights = "This test run was powered by "
                + "\033[1mSHAFT v." + SHAFT.Properties.internal.shaftEngineVersion() + "\033[22m\n"
                + "SHAFT \033[1;4mis and will always be 100% FREE\033[22;24m for commercial and private use\n"
                + "in compliance with the \033[1mMIT license\033[22m\n"
                + "Visit SHAFT's user guide \033[4mhttps://shafthq.github.io/\033[24m to learn more";
        createImportantReportEntry(copyrights);
    }

    public static void logTestInformation(String className, String testMethodName,
                                          String testDescription) {
        testCasesCounter++;
        StringBuilder reportMessage = new StringBuilder();

        if (totalNumberOfTests > 0) {
            reportMessage.append("Starting Execution: ");
            reportMessage.append("'");
            reportMessage.append(testCasesCounter);
            reportMessage.append(" out of ");
            reportMessage.append(totalNumberOfTests);
            reportMessage.append("' test cases in the current suite");
        } else {
            //it should never be ZERO
            reportMessage.append("Starting Dynamic Test Suite Execution: ");
        }
        reportMessage.append("\nTest Method: '").append(className).append(".").append(testMethodName).append("'");

        if (!testDescription.isEmpty()) {
            reportMessage.append("\nTest Description: '").append(testDescription).append("'");
        }

        createImportantReportEntry(reportMessage.toString());
    }

    public static void logScenarioInformation(String keyword, String name, String steps) {
        testCasesCounter++;
        createImportantReportEntry("Starting Execution: \"" + testCasesCounter + " out of " + totalNumberOfTests
                + "\" scenarios in the \"" + featureName + "\" feature"
                + System.lineSeparator() + keyword + " Name: \"" + name
                + "\"" + System.lineSeparator() + keyword + " Steps:" + System.lineSeparator() + steps);
    }

    public static void logConfigurationMethodInformation(String className, String testMethodName, String configurationMethodType) {
        // In TestNG Reporter, this log entry is logged at the end of the previous test
        // (or null for the first test)
        createImportantReportEntry("Starting execution of " + JavaHelper.convertToSentenceCase(configurationMethodType).toLowerCase() + " configuration method\n'"
                + className + "." + testMethodName + "'");
    }

    public static void logExecutionSummary(String total, String passed, String failed, String skipped) {
        String copyrights = "Test Execution Summary Results" + "\n"
                + "Total Cases: " + total + " --> Passed: " + passed + " | Failed: " + failed + " | Skipped: " + skipped;
        createImportantReportEntry(copyrights);
    }

    public static void logFinishedTestInformation(String className, String testMethodName, String testDescription, String testStatus) {
        StringBuilder reportMessage = new StringBuilder();
        reportMessage.append("\nFinished Execution of Test Method: '").append(className).append(".").append(testMethodName).append("'");
        if (!testDescription.isEmpty()) {
            reportMessage.append("\nTest Description: '").append(testDescription).append("'");
        }
        reportMessage.append("\nTest Status: '").append(testStatus).append("'");

        createImportantReportEntry(reportMessage.toString());
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
        if (!attachmentContent.trim().isEmpty()) {
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

    public static void attachEngineLog(String executionEndTimestamp) {
        if (!SHAFT.Properties.reporting.disableLogging()) {
            String engineLogCreated = "Successfully created attachment '" + SHAFT_ENGINE_LOGS_ATTACHMENT_TYPE + " - "
                    + "Execution log" + "'";
            var initialLoggingState = ReportManagerHelper.getDiscreteLogging();
            ReportManagerHelper.setDiscreteLogging(true);
            createLogEntry(engineLogCreated, true);
            byte[] engineLog = new byte[0];
            try {
                engineLog = FileActions.getInstance(true).readFileAsByteArray(System.getProperty("appender.file.fileName"));
                FileActions.getInstance(true).deleteFile(System.getProperty("appender.file.fileName"));
            } catch (Exception throwable) {
                logDiscrete(throwable);
            }
            ReportManagerHelper.setDiscreteLogging(initialLoggingState);
            createAttachment(SHAFT_ENGINE_LOGS_ATTACHMENT_TYPE, "Execution log: " + executionEndTimestamp,
                    new ByteArrayInputStream(engineLog));
        }
    }

    public static void attachIssuesLog(String executionEndTimestamp) {
        String issueSummary = prepareIssuesLog();
        if (!issuesLog.trim().isEmpty()) {
            log(issueSummary,
                    Collections.singletonList(
                            Arrays.asList(SHAFT_ENGINE_LOGS_ATTACHMENT_TYPE, "Issues log CSV: " + executionEndTimestamp,
                                    new ByteArrayInputStream(issuesLog.trim().getBytes()))));
        }
    }

    public static String getCallingMethodFullName() {
        StackTraceElement[] callingStack = Thread.currentThread().getStackTrace();
        var callingMethodFullName = new StringBuilder();
        for (var i = 1; i < callingStack.length; i++) {
            if (!callingStack[i].getClassName().contains("shaft")) {
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

    public static String getCallingClassFullName() {
        StackTraceElement[] callingStack = Thread.currentThread().getStackTrace();
        var getCallingClassFullName = new StringBuilder();
        for (var i = 1; i < callingStack.length; i++) {
            if (!callingStack[i].getClassName().contains("shaft")) {
                getCallingClassFullName.append(callingStack[i].getClassName());
                break;
            }
        }
        return getCallingClassFullName.toString();
    }

    public static String getTestClassName() {
        var result = Reporter.getCurrentTestResult();
        return result != null ? result.getMethod().getTestClass().getName() : "";
//        return Reporter.getCurrentTestResult().getMethod().getTestClass().getName();
    }

    public static String getTestMethodName() {
        if (Reporter.getCurrentTestResult() != null) {
            return Reporter.getCurrentTestResult().getMethod().getMethodName();
        } else {
            // this happens when running a cucumber feature file directly because there is no testNG Reporter instance
            return JavaHelper.removeSpecialCharacters(CucumberFeatureListener.getLastStartedScenarioName());
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
            Allure.getLifecycle().updateTestCase(testResult -> testResult.setDescriptionHtml("<p dir='rtl'>" + scenarioSteps + "</p>"));
        } else {
            Allure.getLifecycle().updateTestCase(testResult -> testResult.setDescriptionHtml("<p dir='ltr'>" + scenarioSteps + "</p>"));
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

    private static String formatStackTraceToLogEntry(Throwable t, boolean isCause) {
        var logBuilder = new StringBuilder();
        if (t != null) {
            StackTraceElement[] trace = t.getStackTrace();
            if (isCause) {
                logBuilder.append(System.lineSeparator()).append("Caused by: ");
            }
            logBuilder.append(t.getClass().getName()).append(":").append(" ").append(t.getMessage()).append(System.lineSeparator());
            for (StackTraceElement stackTraceElement : trace) {
                logBuilder.append(" ").append(stackTraceElement.toString()).append(System.lineSeparator());
            }
            logBuilder.append(formatStackTraceToLogEntry(t.getCause(), true));
        }
        return logBuilder.toString();
    }

    public static void createLogEntry(String logText, Level loglevel) {
        if (SHAFT.Properties.reporting != null && !SHAFT.Properties.reporting.disableLogging()) {
            String timestamp = (new SimpleDateFormat(TIMESTAMP_FORMAT)).format(new Date(System.currentTimeMillis()));
            if (logText == null) {
                logText = "null";
            }
            String log = REPORT_MANAGER_PREFIX + logText.trim() + " @" + timestamp;
            Reporter.log(log, false);
            if (logger == null) {
                initializeLogger();
            }
            logger.log(loglevel, logText.trim());
        }
    }

    private static void createLogEntry(String logText, boolean addToConsoleLog) {
        if (!SHAFT.Properties.reporting.disableLogging()) {
            String timestamp = (new SimpleDateFormat(TIMESTAMP_FORMAT)).format(new Date(System.currentTimeMillis()));
            if (logText == null) {
                logText = "null";
            }
            String log = REPORT_MANAGER_PREFIX + logText.trim() + " @" + timestamp;
            Reporter.log(log, false);
            if (addToConsoleLog) {
                if (logger == null) {
                    initializeLogger();
                }
                logger.log(Level.INFO, logText.trim());
            }
        }
    }

    private static String addSpacing(String log) {
        StringBuilder augmentedText = new StringBuilder();
        StringBuilder lineByLine = new StringBuilder();

        augmentedText.append(System.lineSeparator());
        Arrays.stream(log.split("\n")).toList().forEach(line -> {
            var trailingSpacing = "";
            var spaces = Math.round((float) (144 - line.trim().length()) / 2);
            if (spaces > 0) {
                lineByLine.append(" ".repeat(spaces));
                trailingSpacing = lineByLine.toString();
            }
            lineByLine.append(line.trim());
            lineByLine.append(trailingSpacing);
            augmentedText.append(lineByLine);
            augmentedText.append(System.lineSeparator());
            lineByLine.delete(0, lineByLine.length());
        });
        return augmentedText.toString();
    }

    private static String createSeparator(@SuppressWarnings("SameParameterValue") char ch) {
        return String.valueOf(ch).repeat(144);
    }

    private static void createImportantReportEntry(String logText) {
        createImportantReportEntry(logText, Level.INFO);
    }

    private static void createImportantReportEntry(String logText, Level loglevel) {
        boolean initialLoggingStatus = discreteLogging;
        setDiscreteLogging(false); // force log even if discrete logging was turned on

        var color = switch (loglevel.name()) {
            case "WARN" -> "\033[1;33m"; //yellow
            case "ERROR" -> "\033[1;31m"; //red
            default -> "\033[0;7m"; //white
        };

        String log = System.lineSeparator() +
                color +
                createSeparator('-') +
                addSpacing(logText.trim()) +
                createSeparator('-') +
                System.lineSeparator() +
                "\033[0m";

        Reporter.log(log, false);
        if (logger == null) {
            initializeLogger();
        }
        logger.log(loglevel, log);
        setDiscreteLogging(initialLoggingStatus);
    }

    /**
     * Formats logText and adds timestamp, then logs it as a step in the execution
     * report.
     *
     * @param logText the text that needs to be logged in this action
     */
    public static void writeStepToReport(String logText) {
        if (!SHAFT.Properties.reporting.disableLogging()) {
            createLogEntry(logText, true);
            Allure.step(logText, getStepStatus(logText));
        }
    }

    private static Status getStepStatus(String logText) {
        if (logText != null && logText.toLowerCase().contains("failed")) {
            return Status.FAILED;
        }

        if (Reporter.getCurrentTestResult() != null) {
            var testNgStatus = Reporter.getCurrentTestResult().getStatus();
            return switch (testNgStatus) {
                case ITestResult.FAILURE -> Status.FAILED;
                case ITestResult.SKIP -> Status.SKIPPED;
                default -> Status.PASSED;
            };
        } else {
            return Status.PASSED;
        }
    }

    @Step("{logText}")
    static void writeStepToReport(String logText, List<List<Object>> attachments, CheckpointStatus status) {
        createLogEntry(logText, true);
        if (attachments != null && !attachments.isEmpty()) {
            attachments.forEach(attachment -> {
                if (attachment != null && !attachment.isEmpty() && attachment.get(2).getClass().toString().toLowerCase().contains("string")
                        && !attachment.get(2).getClass().toString().contains("StringInputStream")) {
                    if (!attachment.get(2).toString().isEmpty()) {
                        attach(attachment.get(0).toString(), attachment.get(1).toString(), attachment.get(2).toString());
                    }
                } else if (attachment != null && !attachment.isEmpty()) {
                    if (attachment.get(2) instanceof byte[]) {
                        attach(attachment.get(0).toString(), attachment.get(1).toString(), new ByteArrayInputStream((byte[]) attachment.get(2)));
                    } else {
                        attach(attachment.get(0).toString(), attachment.get(1).toString(), (InputStream) attachment.get(2));
                    }
                }
                if (status.equals(CheckpointStatus.FAIL)) {
                    Allure.getLifecycle().updateStep(update -> {
                        update.setStatus(Status.FAILED);
                        if (attachment != null && !attachment.isEmpty() && attachment.get(2) != null) {
                            String trace = update.getStatusDetails() == null ? attachment.get(2).toString() : update.getStatusDetails().getTrace() + System.lineSeparator() + attachment.get(2).toString();
                            StatusDetails details = update.getStatusDetails() == null ? new StatusDetails() : update.getStatusDetails();
                            details.setTrace(trace.trim());
                            update.setStatusDetails(details);
                        }
                    });
                }
            });
        }
    }

    private static void createAttachment(String attachmentType, String attachmentName, InputStream attachmentContent) {
        if (attachmentContent != null) {
            var byteArrayOutputStream = new ByteArrayOutputStream();
            try {
                attachmentContent.transferTo(byteArrayOutputStream);
            } catch (IOException e) {
                var error = "Error while creating Attachment";
                if (logger == null) {
                    initializeLogger();
                }
                logger.info(error, e);
                Reporter.log(error, false);
            }
            String attachmentDescription = attachmentType + " - " + attachmentName;
            AttachmentReporter.attachBasedOnFileType(attachmentType, attachmentName, byteArrayOutputStream, attachmentDescription);
            logAttachmentAction(attachmentType, attachmentName, byteArrayOutputStream);
        }
    }

    private static void logAttachmentAction(String attachmentType, String attachmentName, ByteArrayOutputStream attachmentContent) {
        createLogEntry("Successfully created attachment '" + attachmentType + " - " + attachmentName + "'", Level.INFO);
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
                if (logger == null) {
                    initializeLogger();
                }
                logger.info(logEntry);
            }
        }
    }

    public static boolean isInternalStep() {
        var callingMethodName = (new Throwable()).getStackTrace()[2].toString();
        return callingMethodName.contains("shaft");
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
    public static void attachAsStep(String attachmentType, String attachmentName, InputStream attachmentContent) {
        createAttachment(attachmentType, attachmentName, attachmentContent);
    }

    public static void cleanExecutionSummaryReportDirectory() {
        if (SHAFT.Properties.reporting.cleanSummaryReportsDirectoryBeforeExecution()) {
            ReportManager.logDiscrete("Initializing Summary Reporting Environment...");
            String executionSummaryReportFolderPath = SHAFT.Properties.paths.executionSummaryReport();
            FileActions.getInstance(true).deleteFolder(executionSummaryReportFolderPath.substring(0, executionSummaryReportFolderPath.length() - 1));
        }
    }

    public static void openExecutionSummaryReportAfterExecution() {
        if (SHAFT.Properties.reporting.openExecutionSummaryReportAfterExecution()) {
            if (SystemUtils.IS_OS_WINDOWS) {
                TerminalActions.getInstance(false, false, true).performTerminalCommand(".\\" + SHAFT.Properties.paths.executionSummaryReport() + "ExecutionSummaryReport_*.html");
            } else {
                TerminalActions.getInstance(false, false, true).performTerminalCommand("open ./" + SHAFT.Properties.paths.executionSummaryReport() + "ExecutionSummaryReport_*.html");
            }
        }
    }

    public static void log(String logText, List<List<Object>> attachments) {
        if (!SHAFT.Properties.reporting.disableLogging()) {
            if (!logText.toLowerCase().contains("failed") && getDiscreteLogging() && isInternalStep()) {
                createLogEntry(logText, Level.INFO);
                if (attachments != null && !attachments.isEmpty() && (attachments.size() > 1 || (attachments.getFirst() != null && !attachments.getFirst().isEmpty()))) {
                    attachments.forEach(attachment -> {
                        if (attachment != null && !attachment.isEmpty()) {
                            if (attachment.get(2) instanceof String string) {
                                attachAsStep(attachment.get(0).toString(), attachment.get(1).toString(),
                                        new ByteArrayInputStream(string.getBytes()));
                            } else if (attachment.get(2) instanceof StringBuilder stringBuilder) {
                                attachAsStep(attachment.get(0).toString(), attachment.get(1).toString(),
                                        new ByteArrayInputStream(stringBuilder.toString().getBytes()));
                            } else {
                                attachAsStep(attachment.get(0).toString(), attachment.get(1).toString(),
                                        (InputStream) attachment.get(2));
                            }
                        }
                    });
                }
            } else {
                if (attachments != null && !attachments.isEmpty() && (attachments.size() > 1 || (attachments.getFirst() != null && !attachments.getFirst().isEmpty()))) {
                    CheckpointStatus status = (!logText.toLowerCase().contains("fail")) ? CheckpointStatus.PASS : CheckpointStatus.FAIL;
                    writeStepToReport(logText, attachments, status);
                } else {
                    writeStepToReport(logText);
                }
            }
        }
    }

    public static void logNestedSteps(String logText, List<String> customLogMessages, List<List<Object>> attachments) {
        CheckpointStatus status = (logText.toLowerCase().contains("passed")) ? CheckpointStatus.PASS : CheckpointStatus.FAIL;
        CheckpointType type = (logText.toLowerCase().contains("verification")) ? CheckpointType.VERIFICATION : CheckpointType.ASSERTION;

        if (type.equals(CheckpointType.VERIFICATION) && status.equals(CheckpointStatus.FAIL)
                || !SHAFT.Properties.reporting.disableLogging()) {
            if (customLogMessages != null && !customLogMessages.isEmpty() && !customLogMessages.getFirst().trim().isEmpty()) {
                String customLogText = customLogMessages.getFirst();
                if (status == CheckpointStatus.PASS) {
                    customLogText = (type == CheckpointType.VERIFICATION) ? "Verification Passed: " + customLogText : "Assertion Passed: " + customLogText;
                } else {
                    customLogText = (type == CheckpointType.VERIFICATION) ? "Verification Failed: " + customLogText : "Assertion Failed: " + customLogText;
                }
                ReportManager.logDiscrete(logText);
                if (attachments != null && !attachments.isEmpty()) {
                    writeNestedStepsToReport(customLogText, attachments);
                } else {
                    writeNestedStepsToReport(customLogText);
                }
                CheckpointCounter.increment(type, customLogMessages.getFirst(), status);
                ExecutionSummaryReport.validationsIncrement(status);
            } else {
                if (attachments != null && !attachments.isEmpty() && !attachments.getFirst().isEmpty()) {
                    writeStepToReport(logText, attachments, status);
                } else {
                    writeStepToReport(logText);
                }
                CheckpointCounter.increment(type, logText, status);
                ExecutionSummaryReport.validationsIncrement(status);
            }
        }
    }

    public static void logNestedSteps(String logText, List<List<Object>> attachments) {
        writeNestedStepsToReport(logText, attachments);
    }

    public static void attach(List<List<Object>> attachments) {
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

    @Step("{customLog}")
    private static void writeNestedStepsToReport(String customLog, List<List<Object>> attachments) {
        createLogEntry(customLog, false);
        attach(attachments);
    }

    @Step("{customLog}")
    private static void writeNestedStepsToReport(String customLog) {
        createLogEntry(customLog, false);
    }

    /**
     * Format an exception message and stack trace, and calls attach to add it as a
     * log entry.
     *
     * @param throwable the throwable (exception or error) that will be logged in this
     *                  action
     */
    public static void log(Throwable throwable) {
        String logText;
        logText = formatStackTraceToLogEntry(throwable);
        if (throwable.getMessage() != null) {
            log("An Exception Occurred with this Message: " + throwable.getMessage().split("\n")[0].trim() + ".",
                    Collections.singletonList(Arrays.asList("Exception Stack Trace", throwable.getClass().getName(), logText)));
        } else {
            log("An Exception Occurred",
                    Collections.singletonList(Arrays.asList("Exception Stack Trace", throwable.getClass().getName(), logText)));
        }
    }

    public static void logDiscrete(Throwable t) {
        createLogEntry(formatStackTraceToLogEntry(t), Level.ERROR);
    }

    public static void logDiscrete(Throwable t, org.apache.logging.log4j.Level logLevel) {
        createLogEntry(formatStackTraceToLogEntry(t), logLevel);
    }

    /**
     * Creates a custom log entry that will not be added as a step in the execution report, but you can see it in the attached execution log txt file
     *
     * @param logText  the text that will be logged by action
     * @param logLevel Level.ERROR, TRACE, INFO, WARN, DEBUG, FATAL
     */
    public static void logDiscrete(String logText, org.apache.logging.log4j.Level logLevel) {
        createLogEntry(logText, logLevel);
    }

    public static String getExecutionDuration(long startTime, long endTime) {
        long durationWithMillis = TimeUnit.MILLISECONDS.toMillis(endTime - startTime);

        String duration = "";
        if (durationWithMillis > 0 && durationWithMillis < 1000) {
            duration = durationWithMillis + " millis";
        } else if (durationWithMillis >= 1000 && durationWithMillis < 60000) {
            duration = String.format("%02d sec, %02d millis",
                    TimeUnit.MILLISECONDS.toSeconds(durationWithMillis),
                    TimeUnit.MILLISECONDS.toMillis(durationWithMillis) - TimeUnit.SECONDS.toMillis(TimeUnit.MILLISECONDS.toSeconds(durationWithMillis))
            );
        } else if (durationWithMillis >= 60000 && durationWithMillis < 3600000) {
            duration = String.format("%02d min, %02d sec",
                    TimeUnit.MILLISECONDS.toMinutes(durationWithMillis),
                    TimeUnit.MILLISECONDS.toSeconds(durationWithMillis) - TimeUnit.MINUTES.toSeconds(TimeUnit.MILLISECONDS.toMinutes(durationWithMillis))
            );
        } else if (durationWithMillis >= 3600000) {
            duration = String.format("%02d hr, %02d min",
                    TimeUnit.MILLISECONDS.toHours(durationWithMillis),
                    TimeUnit.MILLISECONDS.toMinutes(durationWithMillis) - TimeUnit.HOURS.toMinutes(TimeUnit.MILLISECONDS.toHours(durationWithMillis))
            );
        }
        return duration;
    }
}
