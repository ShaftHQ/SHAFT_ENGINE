package com.shaft.tools.io;

import com.shaft.api.RestActions;
import com.shaft.cli.FileActions;
import com.shaft.cli.TerminalActions;
import io.qameta.allure.Allure;
import io.qameta.allure.Step;
import org.apache.commons.lang3.SystemUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.testng.Reporter;

import java.io.*;
import java.net.URL;
import java.nio.charset.StandardCharsets;
import java.text.SimpleDateFormat;
import java.util.*;
import java.util.stream.Collectors;

public class ReportManager {

    private static final String TIMESTAMP_FORMAT = "dd-MM-yyyy HH:mm:ss.SSSS aaa";
    private static final Logger slf4jLogger = LoggerFactory.getLogger(ReportManager.class);
    private static final String SHAFT_ENGINE_VERSION_PROPERTY_NAME = "shaftEngineVersion";
    private static final String TARGET_OS_PROPERTY_NAME = "targetOperatingSystem";
    private static final String ALLURE_VERSION_PROPERTY_NAME = "allureVersion";
    private static final String REPORT_MANAGER_PREFIX = "[ReportManager] ";
    private static final String SHAFT_ENGINE_LOGS_ATTACHMENT_TYPE = "SHAFT Engine Logs";
    private static final String OS_WINDOWS = "Windows-64";
    private static final String allureExtractionLocation = System.getProperty("user.home") + File.separator + ".m2"
            + File.separator + "repository" + File.separator + "allure" + File.separator;
    private static String fullLog = "";
    private static String issuesLog = "";
    private static int issueCounter = 1;
    private static boolean discreteLogging = false;
    private static int totalNumberOfTests = 0;
    private static int testCasesCounter = 0;
    private static boolean debugMode = false;
    //    private static String currentTestLog = "";
    private static int actionCounter = 1;
    private static int openIssuesForFailedTestsCounter = 0;
    private static int openIssuesForPassedTestsCounter = 0;
    private static int failedTestsWithoutOpenIssuesCounter = 0;
    private static String allureResultsFolderPath = "";
    private static String allureExecutablePath = "";

    // TODO: refactor to regular class that can be instanciated within the test and
    // used in a thread-safe way
    private static List<List<String>> listOfOpenIssuesForFailedTests = new ArrayList<>();
    // class name, method name, link name, link url
    private static List<List<String>> listOfOpenIssuesForPassedTests = new ArrayList<>();
    // class name, method name, link name, link url
    private static List<List<String>> listOfNewIssuesForFailedTests = new ArrayList<>();

    private ReportManager() {
        throw new IllegalStateException("Utility class");
    }

    public static void setOpenIssuesForFailedTestsCounter(int openIssuesForFailedTestsCounter) {
        ReportManager.openIssuesForFailedTestsCounter = openIssuesForFailedTestsCounter;
    }

    public static void setOpenIssuesForPassedTestsCounter(int openIssuesForPassedTestsCounter) {
        ReportManager.openIssuesForPassedTestsCounter = openIssuesForPassedTestsCounter;
    }
    // class name, method name

    public static void setFailedTestsWithoutOpenIssuesCounter(int failedTestsWithoutOpenIssuesCounter) {
        ReportManager.failedTestsWithoutOpenIssuesCounter = failedTestsWithoutOpenIssuesCounter;
    }

    public static void setListOfOpenIssuesForFailedTests(List<List<String>> listOfOpenIssuesForFailedTests) {
        ReportManager.listOfOpenIssuesForFailedTests = listOfOpenIssuesForFailedTests;
    }

    public static void setListOfOpenIssuesForPassedTests(List<List<String>> listOfOpenIssuesForPassedTests) {
        ReportManager.listOfOpenIssuesForPassedTests = listOfOpenIssuesForPassedTests;
    }

    public static void setListOfNewIssuesForFailedTests(List<List<String>> listOfNewIssuesForFailedTests) {
        ReportManager.listOfNewIssuesForFailedTests = listOfNewIssuesForFailedTests;
    }

    /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    //////////////////////////////////// [private] Preparation and Support Actions
    /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    private static void createLogEntry(String logText) {
        String timestamp = (new SimpleDateFormat(TIMESTAMP_FORMAT)).format(new Date(System.currentTimeMillis()));
        if (logText ==null){
            logText="null";
        }
        String log = REPORT_MANAGER_PREFIX + logText.trim() + " @" + timestamp;
        slf4jLogger.info(log);
        Reporter.log(log, false);
//	appendToFullLog(log);
//	appendToFullLog(System.lineSeparator());
    }

    /**
     * Appends a log entry to the complete log of the current execution session.
     *
     * @param log the log entry that needs to be appended to the full log
     */
    private static void appendToFullLog(String log) {
        fullLog += log;
//	currentTestLog += log;
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

//    /**
//     * Clears the current test log to prepare for a new test
//     */
//    private static void clearTestLog() {
//	currentTestLog = "";
//    }

    private static void createReportEntry(String logText, Boolean addToFullLog) {
        String timestamp = (new SimpleDateFormat(TIMESTAMP_FORMAT)).format(new Date(System.currentTimeMillis()));
        if (logText == null) {
            logText = "null";
        }
        String log = REPORT_MANAGER_PREFIX + logText.trim() + " @" + timestamp;
        Reporter.log(log, true);
        if (addToFullLog) {
            appendToFullLog(log);
            appendToFullLog(System.lineSeparator());
        }
    }

    protected static void logClosureActivitiesInitialization() {
        String closureActivities = "Test Closure Activities";
        createImportantReportEntry(closureActivities, true);
    }

    private static void createImportantReportEntry(String logText, Boolean addToFullLog) {
        Boolean initialLoggingStatus = discreteLogging;
        setDiscreteLogging(false); // force log even if discrete logging was turned on
        String log = System.lineSeparator()
                + "################################################################################################################################################"
                + System.lineSeparator() + logText.trim() + System.lineSeparator()
                + "################################################################################################################################################";

        Reporter.log(log, true);
        if (Boolean.TRUE.equals(addToFullLog)) {
            appendToFullLog(log);
            appendToFullLog(System.lineSeparator());
        }
        setDiscreteLogging(initialLoggingStatus);
    }

    /**
     * Formats logText and adds timestamp, then logs it as a step in the execution
     * report.
     *
     * @param logText       the text that needs to be logged in this action
     * @param actionCounter a number that represents the serial number of this
     *                      action within this test run
     */
    @Step("Action [{actionCounter}]: {logText}")
    private static void writeStepToReport(String logText, int actionCounter) {
        createReportEntry(logText, false);
    }

    @Step("Action [{actionCounter}]: {logText}")
    private static void writeStepToReport(String logText, int actionCounter, List<List<Object>> attachments) {
        createReportEntry(logText, false);
        if (attachments != null) {
            attachments.forEach(attachment -> {
                if (attachment != null && attachment.get(2).getClass().toString().toLowerCase().contains("string")
                        && !attachment.get(2).getClass().toString().contains("StringInputStream")) {
                    if (!attachment.get(2).toString().isEmpty()) {
                        attach(attachment.get(0).toString(), attachment.get(1).toString(),
                                attachment.get(2).toString());
                    }
                } else if (attachment != null) {
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
        InputStream attachmentContentCopy = null;
        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        byte[] buffer = new byte[1024];
        int len;
        try {
            while ((len = attachmentContent.read(buffer)) > -1) {
                baos.write(buffer, 0, len);
            }
            baos.flush();
        } catch (IOException e) {
            String error = "Error while creating Attachment";
            slf4jLogger.info(error, e);
            Reporter.log(error, false);
        }

        attachmentContent = new ByteArrayInputStream(baos.toByteArray());
        attachmentContentCopy = new ByteArrayInputStream(baos.toByteArray());

        String attachmentDescription = "Attachment: " + attachmentType + " - " + attachmentName;

        attachBasedOnFileType(attachmentType, attachmentName, attachmentContent, attachmentDescription);

        if (!(attachmentType.equals(SHAFT_ENGINE_LOGS_ATTACHMENT_TYPE) && attachmentName.equals("Execution log"))) {
            createReportEntry("Successfully created attachment [" + attachmentType + " - " + attachmentName + "]",
                    false);
        }

        if (debugMode && !attachmentType.contains(SHAFT_ENGINE_LOGS_ATTACHMENT_TYPE)
                && !attachmentType.equalsIgnoreCase("Selenium WebDriver Logs")
                && !attachmentType.toLowerCase().contains("screenshot")
                && !attachmentType.toLowerCase().contains("recording") && !attachmentType.toLowerCase().contains("gif")
                && !attachmentType.toLowerCase().contains("engine logs")) {
            String timestamp = (new SimpleDateFormat(TIMESTAMP_FORMAT)).format(new Date(System.currentTimeMillis()));

            String theString = "";
            BufferedReader br = new BufferedReader(
                    new InputStreamReader(attachmentContentCopy, StandardCharsets.UTF_8));
            theString = br.lines().collect(Collectors.joining(System.lineSeparator()));
            if (!theString.isEmpty()) {
                String logEntry = REPORT_MANAGER_PREFIX + "Debugging Attachment Entry" + " @" + timestamp
                        + System.lineSeparator() + theString + System.lineSeparator();
                slf4jLogger.info(logEntry);
//		appendToFullLog(logEntry);
            }
        }
    }

    private static synchronized void attachBasedOnFileType(String attachmentType, String attachmentName,
                                                           InputStream attachmentContent, String attachmentDescription) {
        if (attachmentType.toLowerCase().contains("screenshot")) {
            Allure.addAttachment(attachmentDescription, "image/png", attachmentContent, ".png");
        } else if (attachmentType.toLowerCase().contains("recording")) {
            Allure.addAttachment(attachmentDescription, "video/quicktime", attachmentContent, ".mov");
            // attachmentName, "video/mp4", attachmentContent, ".mp4"
        } else if (attachmentType.toLowerCase().contains("gif")) {
            Allure.addAttachment(attachmentDescription, "image/gif", attachmentContent, ".gif");
        } else if (attachmentType.toLowerCase().contains("csv") || attachmentName.toLowerCase().contains("csv")) {
            Allure.addAttachment(attachmentDescription, "text/csv", attachmentContent, ".csv");
        } else if (attachmentType.toLowerCase().contains("xml") || attachmentName.toLowerCase().contains("xml")) {
            Allure.addAttachment(attachmentDescription, "text/xml", attachmentContent, ".xml");
        } else if (attachmentType.toLowerCase().contains("json") || attachmentName.toLowerCase().contains("json")) {
            Allure.addAttachment(attachmentDescription, "text/json", attachmentContent, ".json");
        } else if (attachmentType.toLowerCase().contains("engine logs")) {
//	    if (attachmentName.equals("Current Method log")) {
//		Allure.addAttachment(attachmentDescription, "text/plain",
//			new ByteArrayInputStream(currentTestLog.trim().getBytes()), ".txt");
//	    } else {
            Allure.addAttachment(attachmentDescription, "text/plain", attachmentContent, ".txt");
//	    }
        } else {
            Allure.addAttachment(attachmentDescription, attachmentContent);
        }
    }

    /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    //////////////////////////////////// [Public] Core Reporting Actions
    /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    /**
     * @return the discreteLogging
     */
    public static boolean isDiscreteLogging() {
        return discreteLogging;
    }

    /**
     * @param discreteLogging the discreteLogging to set
     */
    public static void setDiscreteLogging(boolean discreteLogging) {
        if (debugMode) {
            ReportManager.logDiscrete("Setting discrete logging to: \"" + discreteLogging + "\"");
        }
        ReportManager.discreteLogging = discreteLogging;
    }

    public static int getTestCasesCounter() {
        return testCasesCounter;
    }

    public static int getTotalNumberOfTests() {
        return totalNumberOfTests;
    }

    public static void setTotalNumberOfTests(int totalNumberOfTests) {
        ReportManager.totalNumberOfTests = totalNumberOfTests;
    }

    public static void setDebugMode(Boolean debugMode) {
        ReportManager.debugMode = debugMode;
    }

    private static void cleanAllureResultsDirectory() {
        // clean allure-results directory before execution
        if (Boolean.TRUE.equals(
                Boolean.valueOf(System.getProperty("cleanAllureResultsDirectoryBeforeExecution")))) {
            FileActions.deleteFolder(allureResultsFolderPath.substring(0, allureResultsFolderPath.length() - 1));
        }
    }

    private static void writeEnvironmentVariablesToAllureResultsDirectory() {
        // reads all environment variables and then formats and writes them to be read
        // by the Allure report
        Properties props = System.getProperties();
        StringBuilder propertiesFileBuilder = new StringBuilder();
        propertiesFileBuilder.append("<environment>");
        // read properties from any explicit properties files
        for (int i = 0; i < props.size(); i++) {
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
        FileActions.writeToFile(System.getProperty("allureResultsFolderPath"), "environment.xml",
                RestActions.formatXML(propertiesFileBuilder.toString()));
    }

    private static void downloadAndExtractAllureBinaries() {
        // extract allure from jar file to src/main/resources directory if it doesn't
        // already exist
        String allureVersion = System.getProperty(ALLURE_VERSION_PROPERTY_NAME);
        allureExecutablePath = allureExtractionLocation + "allure-" + allureVersion + "/bin/allure";
        if (!(new File(allureExecutablePath)).exists()) {
            FileActions.deleteFolder(allureExtractionLocation);
            // download allure binary
            URL allureArchive = FileActions.downloadFile(
                    "https://repo.maven.apache.org/maven2/io/qameta/allure/allure-commandline/" + allureVersion
                            + "/allure-commandline-" + allureVersion + ".zip",
                    "target/allureBinary.zip");
            FileActions.unpackArchive(allureArchive, allureExtractionLocation);
            // extract allure from SHAFT_Engine jar
            URL allureSHAFTConfigArchive = ReportManager.class
                    .getResource("/allure/allureBinary_SHAFTEngineConfigFiles.zip");
            FileActions.unpackArchive(allureSHAFTConfigArchive,
                    allureExtractionLocation + "allure-" + allureVersion + "/");

            if (!System.getProperty(TARGET_OS_PROPERTY_NAME).equals(OS_WINDOWS)) {
                // make allure executable on unix-based shells
                (new TerminalActions()).performTerminalCommand("chmod u+x " + allureExecutablePath);
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
                    "set path=" + allureExtractionLocation + "allure-" + allureVersion + "\\bin;%path%",
                    "allure serve " + allureResultsFolderPath.substring(0, allureResultsFolderPath.length() - 1),
                    "pause", "exit");
            FileActions.writeToFile("", "generate_allure_report.bat", commandsToServeAllureReport);
        } else {
            // create unix-based sh file
            commandsToServeAllureReport = Arrays
                    .asList("#!/bin/bash", "parent_path=$( cd \"$(dirname \"${BASH_SOURCE[0]}\")\" ; pwd -P )",
                            "cd \"" + allureExtractionLocation + "allure-" + allureVersion + "/bin/\"",
                            "bash allure serve \"$parent_path/"
                                    + allureResultsFolderPath.substring(0, allureResultsFolderPath.length() - 1) + "\"",
                            "exit"

                    );
            FileActions.writeToFile("", "generate_allure_report.sh", commandsToServeAllureReport);
            // make allure executable on unix-based shells
            (new TerminalActions()).performTerminalCommand("chmod u+x generate_allure_report.sh");
        }
    }

    public static void prepareAllureReportingEnvironment() {
        logDiscrete("Preparing Allure Reporting Environment...");
        Boolean discreteLoggingState = isDiscreteLogging();
        allureResultsFolderPath = System.getProperty("allureResultsFolderPath").trim();
        if (System.getProperty("executionAddress").trim().equals("local")
                || !System.getProperty("appium_platformName").trim().equals("")) {
            setDiscreteLogging(true);
            cleanAllureResultsDirectory();
            downloadAndExtractAllureBinaries();
            writeGenerateReportShellFilesToProjectDirectory();
        }
        writeEnvironmentVariablesToAllureResultsDirectory();
        setDiscreteLogging(discreteLoggingState);
    }

    public static void logEngineVersion() {
        String engineVersion = "Detected SHAFT Engine Version: ["
                + System.getProperty(SHAFT_ENGINE_VERSION_PROPERTY_NAME) + "]";
        createImportantReportEntry(engineVersion, true);
    }

    public static synchronized void logTestInformation(String className, String testMethodName,
                                                       String testDescription) {
//	clearTestLog();
        testCasesCounter++;
        if (!testDescription.equals("")) {
            createImportantReportEntry("Starting Execution:\t[" + testCasesCounter + " out of " + totalNumberOfTests
                    + "] test cases in the current suite\nTest Method:\t\t[" + className + "." + testMethodName
                    + "]\nTest Description:\t[" + testDescription + "]", false);
        } else {
            createImportantReportEntry("Starting Execution:\t[" + testCasesCounter + " out of " + totalNumberOfTests
                            + "] test cases in the current suite\nTest Method:\t\t[" + className + "." + testMethodName + "]",
                    false);
        }
    }

    public static void logConfigurationMethodInformation(String className, String testMethodName) {
        // In TestNG Reporter, this log entry is logged at the end of the previous test
        // (or null for the first test)
        createImportantReportEntry("Starting Execution of a Configuration (Setup or Teardown) Method\nTest Method:\t\t["
                + className + "." + testMethodName + "]", false);
    }

    /**
     * Manages action counter and calls writeLog to format and print the log entry.
     *
     * @param logText the text that needs to be logged in this action
     */
    public static void log(String logText) {
        if (isDiscreteLogging() && !logText.toLowerCase().contains("failed")) {
            createLogEntry(logText);
        } else {
            writeStepToReport(logText, actionCounter);
            actionCounter++;
        }
    }

    public static void log(String logText, List<List<Object>> attachments) {
        if (isDiscreteLogging() && !logText.toLowerCase().contains("failed")) {
            createLogEntry(logText);
            if (attachments != null) {
                attachments.forEach(attachment -> {
                    if (attachment != null) {
                        if (attachment.get(2) instanceof java.lang.String) {
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
            writeStepToReport(logText, actionCounter, attachments);
            actionCounter++;
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
        String logText = "";

        logText = formatStackTraceToLogEntry(t);
        if (t.getMessage() != null) {
            ReportManager.log("An Exception Occured with this Message: " + t.getMessage().split("\n")[0].trim() + ".",
                    Arrays.asList(Arrays.asList("Exception Stack Trace", t.getClass().getName(), logText)));
        } else {
            ReportManager.log("An Exception Occured",
                    Arrays.asList(Arrays.asList("Exception Stack Trace", t.getClass().getName(), logText)));
        }
        actionCounter++;
    }

    public static String formatStackTraceToLogEntry(Throwable t) {
        StringBuilder logBuilder = new StringBuilder();
        StackTraceElement[] trace = t.getStackTrace();

        logBuilder.append(
                t.getClass().getName() + ":" + System.lineSeparator() + t.getMessage() + System.lineSeparator());

        for (int i = 0; i < trace.length; ++i) {
            logBuilder.append(trace[i].toString() + System.lineSeparator());
        }
        return logBuilder.toString();
    }

    public static void logDiscrete(String logText) {
        createLogEntry(logText);
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
    private static void attachAsStep(String attachmentType, String attachmentName, InputStream attachmentContent) {
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
        appendToFullLog(testLog);
        appendToFullLog(System.lineSeparator());
        if (!testLog.isBlank()) {
            createAttachment(SHAFT_ENGINE_LOGS_ATTACHMENT_TYPE, "Current Method log: " + currentMethodName,
                    new ByteArrayInputStream(testLog.getBytes()));
        }
    }

    public static void attachFullLog(String executionEndTimestamp) {
        if (!fullLog.trim().equals("")) {
            String fullLogCreated = "Successfully created attachment [" + SHAFT_ENGINE_LOGS_ATTACHMENT_TYPE + " - "
                    + "Execution log" + "]";
            createReportEntry(fullLogCreated, true);
            String copyrights = "This test run was powered by SHAFT Engine Version: ["
                    + System.getProperty(SHAFT_ENGINE_VERSION_PROPERTY_NAME) + "]" + System.lineSeparator()
                    + "SHAFT Engine is licensed under the MIT License: [https://github.com/MohabMohie/SHAFT_ENGINE/blob/master/LICENSE].";
            createImportantReportEntry(copyrights, true);
            createAttachment(SHAFT_ENGINE_LOGS_ATTACHMENT_TYPE, "Execution log: " + executionEndTimestamp,
                    new ByteArrayInputStream(fullLog.trim().getBytes()));
        }
    }

    public static void attachIssuesLog(String executionEndTimestamp) {
        String issueSummary = prepareIssuesLog();
        if (!issuesLog.trim().equals("")) {
            log(issueSummary,
                    Arrays.asList(
                            Arrays.asList(SHAFT_ENGINE_LOGS_ATTACHMENT_TYPE, "Issues log CSV: " + executionEndTimestamp,
                                    new ByteArrayInputStream(issuesLog.trim().getBytes()))));
        }
    }

    private static void writeOpenReportShellFilesToGeneratedDirectory() {
        List<String> commandsToOpenAllureReport = null;
        // create unix-based sh file
        commandsToOpenAllureReport = Arrays.asList("#!/bin/bash",
                "parent_path=$( cd \"$(dirname \"${BASH_SOURCE[0]}\")\" ; pwd -P )",
                "cd \"$parent_path/allure/allure-" + System.getProperty(ALLURE_VERSION_PROPERTY_NAME) + "/bin/\"",
                "bash allure open \"$parent_path/allure-report\"", "exit");
        FileActions.writeToFile("generatedReport/", "open_allure_report.sh", commandsToOpenAllureReport);

        // create windows batch file
        commandsToOpenAllureReport = Arrays.asList("@echo off",
                "set path=allure\\allure-" + System.getProperty(ALLURE_VERSION_PROPERTY_NAME) + "\\bin;%path%",
                "allure open allure-report", "pause", "exit");
        FileActions.writeToFile("generatedReport/", "open_allure_report.bat", commandsToOpenAllureReport);

    }

    private static void writeAllureReportToGeneratedDirectory() {
        // add correct file extension based on target OS
        String targetOperatingSystem = System.getProperty(TARGET_OS_PROPERTY_NAME);
        String commandToCreateAllureReport = "";

        allureExecutablePath = allureExtractionLocation + "allure-" + System.getProperty(ALLURE_VERSION_PROPERTY_NAME)
                + "/bin/allure";

        if (targetOperatingSystem.equals(OS_WINDOWS)) {
            commandToCreateAllureReport = allureExecutablePath + ".bat" + " generate \""
                    + allureResultsFolderPath.substring(0, allureResultsFolderPath.length() - 1)
                    + "\" -o \"generatedReport/allure-report\"";
        } else {
            commandToCreateAllureReport = allureExecutablePath + " generate \""
                    + allureResultsFolderPath.substring(0, allureResultsFolderPath.length() - 1)
                    + "\" -o \"generatedReport/allure-report\"";
        }
        (new TerminalActions()).performTerminalCommand(commandToCreateAllureReport);
    }

    private static void createAllureReportArchiveAndCleanGeneratedDirectory() {
        FileActions.copyFolder(FileActions.getAbsolutePath("target/", "allure"), "generatedReport/allure");
        FileActions.zipFiles("generatedReport/", "generatedReport_" + new SimpleDateFormat("yyyyMMdd-HHmmss").format(new Date()) + ".zip");
        FileActions.deleteFile("generatedReport/");
    }

    public static void openAllureReportAfterExecution() {
	String commandToOpenAllureReport = "";
	if (Boolean.TRUE.equals(Boolean.valueOf(System.getProperty("openAllureReportAfterExecution").trim()))
		&& System.getProperty("executionAddress").trim().equals("local")) {

	    if (SystemUtils.IS_OS_WINDOWS) {
		commandToOpenAllureReport = ("cd \"" + allureResultsFolderPath + "&&" + allureExecutablePath + ".bat");
	    } else {
		commandToOpenAllureReport = (allureExecutablePath + ".sh");
	    }
	    (new TerminalActions()).performTerminalCommand(commandToOpenAllureReport);
	}
    }
    
    public static void generateAllureReportArchive() {
        if (Boolean.TRUE.equals(Boolean.valueOf(System.getProperty("generateAllureReportArchive").trim()))
                && System.getProperty("executionAddress").trim().equals("local")) {
            logDiscrete("Generating Allure Report Archive...");
            Boolean discreteLoggingState = isDiscreteLogging();
            setDiscreteLogging(true);
            writeOpenReportShellFilesToGeneratedDirectory();
            writeAllureReportToGeneratedDirectory();
            createAllureReportArchiveAndCleanGeneratedDirectory();
            setDiscreteLogging(discreteLoggingState);
        }
    }

    public static String getCallingMethodFullName() {
        StackTraceElement[] callingStack = Thread.currentThread().getStackTrace();
        StringBuilder callingMethodFullName = new StringBuilder();
        for (int i = 1; i < callingStack.length; i++) {
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
}