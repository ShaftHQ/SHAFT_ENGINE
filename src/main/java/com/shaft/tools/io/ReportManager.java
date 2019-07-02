package com.shaft.tools.io;

import java.io.BufferedReader;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.nio.charset.StandardCharsets;
import java.text.SimpleDateFormat;
import java.util.Arrays;
import java.util.Date;
import java.util.List;
import java.util.stream.Collectors;

import org.apache.tools.ant.filters.StringInputStream;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.testng.Reporter;

import com.shaft.cli.FileActions;
import com.shaft.cli.TerminalActions;

import io.qameta.allure.Allure;
import io.qameta.allure.Step;

public class ReportManager {

    private static String fullLog = "";
    private static String issuesLog = "";
    private static String currentTestLog = "";
    private static int actionCounter = 1;
    private static int issueCounter = 1;
    private static boolean discreteLogging = false;
    private static int totalNumberOfTests = 0;
    private static int testCasesCounter = 0;
    private static boolean debugMode = false;
    private static final String TIMESTAMP_FORMAT = "dd-MM-yyyy HH:mm:ss.SSSS aaa";
    private static final Logger slf4jLogger = LoggerFactory.getLogger(ReportManager.class);

    private ReportManager() {
	throw new IllegalStateException("Utility class");
    }

    /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    //////////////////////////////////// [private] Preparation and Support Actions
    /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    private static void createLogEntry(String logText) {
	String timestamp = (new SimpleDateFormat(TIMESTAMP_FORMAT)).format(new Date(System.currentTimeMillis()));
	String log = "[ReportManager] " + logText.trim() + " @" + timestamp;
	appendToLog(log);
	appendToLog(System.lineSeparator());
    }

    /**
     * Appends a log entry to the complete log of the current execution session.
     * 
     * @param log the log entry that needs to be appended to the full log
     */
    private static void appendToLog(String log) {
	fullLog += log;
	currentTestLog += log;
    }

    public static void logIssue(String issue) {
	if (issuesLog.trim().equals("")) {
	    issuesLog = "################################################################################################################################################"
		    + System.lineSeparator() + "Issues Analysis" + System.lineSeparator()
		    + "################################################################################################################################################";
	}
	String timestamp = (new SimpleDateFormat(TIMESTAMP_FORMAT)).format(new Date(System.currentTimeMillis()));
	String log = System.lineSeparator() + "[ReportManager] " + issueCounter + ". " + issue.trim() + " @"
		+ timestamp;
	issuesLog += log;
	issueCounter++;
    }

    public static void logIssuesSummary(int openIssuesForFailedTestsCounter, int openIssuesForPassedTestsCounter,
	    int failedTestsWithoutOpenIssuesCounter) {
	issuesLog += System.lineSeparator()
		+ "################################################################################################################################################"
		+ System.lineSeparator() + "Total Issues: " + (issueCounter - 1) + ", Failed tests with open issues: "
		+ openIssuesForFailedTestsCounter + ", Failed tests without open issues: "
		+ failedTestsWithoutOpenIssuesCounter + ", Passed tests with open issues: "
		+ openIssuesForPassedTestsCounter + System.lineSeparator()
		+ "################################################################################################################################################";
    }

    /**
     * Clears the current test log to prepare for a new test
     */
    private static void clearTestLog() {
	currentTestLog = "";
    }

    private static void createReportEntry(String logText) {
	String timestamp = (new SimpleDateFormat(TIMESTAMP_FORMAT)).format(new Date(System.currentTimeMillis()));
	String log = "[ReportManager] " + logText.trim() + " @" + timestamp;
	Reporter.log(log, true);
	appendToLog(log);
	appendToLog(System.lineSeparator());
    }

    private static void createImportantReportEntry(String logText) {
	Boolean initialLoggingStatus = discreteLogging;
	setDiscreteLogging(false); // force log even if discrete logging was turned on
	String log = System.lineSeparator()
		+ "################################################################################################################################################"
		+ System.lineSeparator() + logText.trim() + System.lineSeparator()
		+ "################################################################################################################################################";

	Reporter.log(log, true);
	appendToLog(log);
	appendToLog(System.lineSeparator());
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
	createReportEntry(logText);
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
	    slf4jLogger.info("Error while creating Attachment", e);
	}

	attachmentContent = new ByteArrayInputStream(baos.toByteArray());
	attachmentContentCopy = new ByteArrayInputStream(baos.toByteArray());

	String attachmentDescription = "Attachment: " + attachmentType + " - " + attachmentName;

	if (attachmentType.toLowerCase().contains("screenshot")) {
	    Allure.addAttachment(attachmentDescription, "image/png", attachmentContent, ".png");
	} else if (attachmentType.toLowerCase().contains("recording")) {
	    Allure.addAttachment(attachmentDescription, "video/quicktime", attachmentContent, ".mov");
	    // attachmentName, "video/mp4", attachmentContent, ".mp4"
	} else if (attachmentType.toLowerCase().contains("gif")) {
	    Allure.addAttachment(attachmentDescription, "image/gif", attachmentContent, ".gif");
	} else if (attachmentType.toLowerCase().contains("engine logs")) {
	    if (attachmentName.equals("Current Method log")) {
		Allure.addAttachment(attachmentDescription, "text/plain", new StringInputStream(currentTestLog.trim()),
			".txt");
	    } else {
		Allure.addAttachment(attachmentDescription, "text/plain", attachmentContent, ".txt");
	    }
	} else {
	    Allure.addAttachment(attachmentDescription, attachmentContent);
	}
	createReportEntry("Successfully created attachment [" + attachmentType + " - " + attachmentName + "]");

	if (debugMode && !attachmentType.contains("SHAFT Engine Logs")
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
		String logEntry = "[ReportManager] " + "Debugging Attachment Entry" + " @" + timestamp
			+ System.lineSeparator() + theString + System.lineSeparator();
		slf4jLogger.info(logEntry);
		appendToLog(logEntry);
	    }
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

    public static void populateEnvironmentData() {
	// reads all environment variables and then formats and writes them to be read
	// by the Allure report
	FileActions.writeToFile(System.getProperty("allureResultsFolderPath"), "environment.properties",
		Arrays.asList(System.getProperties().toString().trim()
			.substring(1, System.getProperties().toString().trim().length() - 1).replaceAll(", ", "\n")
			.replaceAll("=", "\t").split("\n")));
    }

    public static void logEngineVersion(Boolean isStartingExecution) {
	if (isStartingExecution) {
	    createImportantReportEntry(
		    "Detected SHAFT Engine Version: [" + System.getProperty("shaftEngineVersion") + "]");
	} else {
	    createImportantReportEntry("This test run was powered by SHAFT Engine, which was created by [Mohab Mohie]."
		    + System.lineSeparator()
		    + "SHAFT Engine is licensed under the MIT License: [https://github.com/MohabMohie/SHAFT_ENGINE/blob/master/LICENSE].");
	}
    }

    public static void logTestInformation(String className, String testMethodName, String testDescription) {
	clearTestLog();
	testCasesCounter++;
	if (!testDescription.equals("")) {
	    createImportantReportEntry("Starting Execution:\t[" + testCasesCounter + " out of " + totalNumberOfTests
		    + "] test cases in the current suite\nTest Method:\t\t[" + className + "." + testMethodName
		    + "]\nTest Description:\t[" + testDescription + "]");
	} else {
	    createImportantReportEntry("Starting Execution:\t[" + testCasesCounter + " out of " + totalNumberOfTests
		    + "] test cases in the current suite\nTest Method:\t\t[" + className + "." + testMethodName + "]");
	}
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

    /**
     *
     * Format an exception message and stack trace, and calls attach to add it as a
     * log entry.
     * 
     * @param t the throwable (exception or error) that will be logged in this
     *          action
     */
    public static void log(Throwable t) {
	StringBuilder logBuilder = new StringBuilder();
	String logText = "";
	StackTraceElement[] trace = t.getStackTrace();

	// enhance to include exception type

	logBuilder.append(
		t.getClass().getName() + ":" + System.lineSeparator() + t.getMessage() + System.lineSeparator());

	for (int i = 0; i < trace.length; ++i) {
	    logBuilder.append(trace[i].toString() + System.lineSeparator());
	}
	logText = logBuilder.toString();

	attachAsStep("Exception Stack Trace", t.getClass().getName(), logText);
	actionCounter++;
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
    public static void attachAsStep(String attachmentType, String attachmentName, InputStream attachmentContent) {
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
    @Step("Attachment: {attachmentType} - {attachmentName}")
    public static void attachAsStep(String attachmentType, String attachmentName, String attachmentContent) {
	if (!attachmentContent.trim().equals("")) {
	    createAttachment(attachmentType, attachmentName, new StringInputStream(attachmentContent));
	}
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
	    createAttachment(attachmentType, attachmentName, new StringInputStream(attachmentContent));

	}
    }

    /**
     * Returns the log of the current test, and attaches it in the end of the test
     * execution report.
     * 
     */
    public static void attachTestLog() {
	String trimmed = currentTestLog.trim();
	if (!currentTestLog.trim().equals("") && (!(String.valueOf(trimmed.charAt(0)).equals("#")
		&& String.valueOf(trimmed.charAt(trimmed.length() - 1)).equals("#")))) {
	    createAttachment("SHAFT Engine Logs", "Current Method log", new StringInputStream(currentTestLog));
	}
	clearTestLog();
    }

    public static void attachFullLog() {
	if (!fullLog.trim().equals("")) {
	    createAttachment("SHAFT Engine Logs", "Execution log", new StringInputStream(fullLog.trim()));

	}
    }

    public static void attachIssuesLog() {
	if (!issuesLog.trim().equals("")) {
	    createAttachment("SHAFT Engine Logs", "Issues log", new StringInputStream(issuesLog.trim()));
	}
    }

    public static void generateAllureReportArchive() {
	if (Boolean.valueOf(System.getProperty("automaticallyGenerateAllureReport").trim())) {
	    logDiscrete("Generating Allure Report Archive...");
	    Boolean discreteLoggingState = isDiscreteLogging();
	    setDiscreteLogging(true);

	    // add correct file extension based on target OS
	    String targetOperatingSystem = System.getProperty("targetOperatingSystem");
	    String commandToCreateAllureReport = "";
	    List<String> commandsToOpenAllureReport = null;
	    String allureReportFileExtension;

	    if (targetOperatingSystem.equals("Windows-64")) {
		commandToCreateAllureReport = "src/main/resources/allure/bin/allure.bat generate \"allure-results\" -o \"generatedReport/allure-report\"";
	    } else {
		commandToCreateAllureReport = "src/main/resources/allure/bin/allure generate \"allure-results\" -o \"generatedReport/allure-report\"";
	    }

	    // create unix-based sh file
	    commandsToOpenAllureReport = Arrays.asList("#!/bin/bash",
		    "parent_path=$( cd \"$(dirname \"${BASH_SOURCE[0]}\")\" ; pwd -P )",
		    "cd \"$parent_path/allure/bin/\"", "bash allure open \"$parent_path/allure-report\"", "exit");
	    allureReportFileExtension = ".sh";
	    FileActions.writeToFile("generatedReport/", "open_allure_report" + allureReportFileExtension,
		    commandsToOpenAllureReport);

	    if (!targetOperatingSystem.equals("Windows-64")) {
		// make file executable on unix-based shells
		(new TerminalActions()).performTerminalCommand(
			"chmod +x generatedReport/open_allure_report" + allureReportFileExtension);
	    }
	    // create windows batch file
	    commandsToOpenAllureReport = Arrays.asList("@echo off", "set path=allure\\bin;%path%",
		    "allure open allure-report", "pause", "exit");
	    allureReportFileExtension = ".bat";
	    FileActions.writeToFile("generatedReport/", "open_allure_report" + allureReportFileExtension,
		    commandsToOpenAllureReport);

	    (new TerminalActions()).performTerminalCommand(commandToCreateAllureReport);

	    FileActions.copyFolder(FileActions.getAbsolutePath("src/main/resources/", "allure"),
		    "generatedReport/allure");

	    FileActions.zipFiles("generatedReport/", "generatedReport.zip");

	    FileActions.deleteFile("generatedReport/");
	    setDiscreteLogging(discreteLoggingState);
	}
    }
}