package com.shaft.tools.io;

import java.io.BufferedReader;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.net.URL;
import java.nio.charset.StandardCharsets;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.List;
import java.util.Properties;
import java.util.stream.Collectors;

import org.apache.tools.ant.filters.StringInputStream;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.testng.Reporter;

import com.shaft.api.RestActions;
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
    private static final String SHAFT_ENGINE_VERSION_PROPERTY_NAME = "shaftEngineVersion";

    private static int openIssuesForFailedTestsCounter = 0;
    private static int openIssuesForPassedTestsCounter = 0;
    private static int failedTestsWithoutOpenIssuesCounter = 0;

    private static String allureResultsFolderPath = "";
    private static String allureExecutablePath = "target/allure/bin/allure";

    private static final String OS_WINDOWS = "Windows-64";

    public static void setOpenIssuesForFailedTestsCounter(int openIssuesForFailedTestsCounter) {
	ReportManager.openIssuesForFailedTestsCounter = openIssuesForFailedTestsCounter;
    }

    public static void setOpenIssuesForPassedTestsCounter(int openIssuesForPassedTestsCounter) {
	ReportManager.openIssuesForPassedTestsCounter = openIssuesForPassedTestsCounter;
    }

    public static void setFailedTestsWithoutOpenIssuesCounter(int failedTestsWithoutOpenIssuesCounter) {
	ReportManager.failedTestsWithoutOpenIssuesCounter = failedTestsWithoutOpenIssuesCounter;
    }

    private static List<List<String>> listOfOpenIssuesForFailedTests = new ArrayList<>();
    // class name, method name, link name, link url
    private static List<List<String>> listOfOpenIssuesForPassedTests = new ArrayList<>();
    // class name, method name, link name, link url
    private static List<List<String>> listOfNewIssuesForFailedTests = new ArrayList<>();
    // class name, method name

    public static void setListOfOpenIssuesForFailedTests(List<List<String>> listOfOpenIssuesForFailedTests) {
	ReportManager.listOfOpenIssuesForFailedTests = listOfOpenIssuesForFailedTests;
    }

    public static void setListOfOpenIssuesForPassedTests(List<List<String>> listOfOpenIssuesForPassedTests) {
	ReportManager.listOfOpenIssuesForPassedTests = listOfOpenIssuesForPassedTests;
    }

    public static void setListOfNewIssuesForFailedTests(List<List<String>> listOfNewIssuesForFailedTests) {
	ReportManager.listOfNewIssuesForFailedTests = listOfNewIssuesForFailedTests;
    }

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

    public static void prepareIssuesLog() {
	// TODO: refactor
	// read different log array lists
	if (!listOfNewIssuesForFailedTests.isEmpty()) {
	    listOfNewIssuesForFailedTests.forEach(issue -> logIssue("Test Method \"" + issue.get(0) + "." + issue.get(1)
		    + "\" failed. Please investigate and open a new Issue if needed.\n"));
	}
	if (!listOfOpenIssuesForPassedTests.isEmpty()) {
	    listOfOpenIssuesForPassedTests.forEach(issue -> logIssue("Test Method \"" + issue.get(0) + "."
		    + issue.get(1) + "\" passed. Please validate and close this open issue \"" + issue.get(2) + "\": \""
		    + issue.get(3) + "\".\n"));
	}
	if (!listOfOpenIssuesForFailedTests.isEmpty()) {
	    listOfOpenIssuesForFailedTests
		    .forEach(issue -> logIssue("Test Method \"" + issue.get(0) + "." + issue.get(1)
			    + "\" failed with open issue \"" + issue.get(2) + "\": \"" + issue.get(3) + "\".\n"));

	}
	// display them in the desired order with the proper messages for each issue
	// type
	// append the summary at the start instead of at the finish
	if (!issuesLog.trim().equals("")) {
	    issuesLog += System.lineSeparator()
		    + "################################################################################################################################################"
		    + System.lineSeparator() + "Total Issues: " + (issueCounter - 1) + ", New issues for Failed Tests: "
		    + failedTestsWithoutOpenIssuesCounter + ", Open issues for Passed Tests: "
		    + openIssuesForPassedTestsCounter + ", Open issues for Failed Tests: "
		    + openIssuesForFailedTestsCounter + System.lineSeparator()
		    + "################################################################################################################################################";
	}
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

    protected static void createImportantReportEntry(String logText) {
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

	if (!(attachmentType.equals("SHAFT Engine Logs") && attachmentName.equals("Execution log"))) {
	    createReportEntry("Successfully created attachment [" + attachmentType + " - " + attachmentName + "]");
	}

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

    private static void cleanAllureResultsDirectory() {
	// clean allure-results directory before execution
	if (Boolean.valueOf(System.getProperty("automaticallyCleanAllureResultsDirectoryBeforeExecution"))) {
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

	    // excluding empty values and system properties (all system properties have "."
	    // in their names
	    if (!propertyValue.equals("") && !propertyKey.contains(".")) {
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

    private static void extractAllureBinariesFromJarFile() {
	// extract allure from jar file to src/main/resources directory if it doesn't
	// already exist
	if (!(new File("target/allure/")).exists()) {
	    URL allureFolder = ReportManager.class.getResource("/allure/allureBinary.zip");
	    FileActions.unpackArchive(allureFolder, "target/allure/");
	    if (!System.getProperty("targetOperatingSystem").equals(OS_WINDOWS)) {
		// make allure executable on unix-based shells
		(new TerminalActions()).performTerminalCommand("chmod u+x " + allureExecutablePath);
	    }
	}
    }

    private static void writeGenerateReportShellFilesToProjectDirectory() {
	// create generate_allure_report.sh and generate_allure_report.bat
	List<String> commandsToServeAllureReport;
	if (!(new File("generate_allure_report.bat").exists())
		&& System.getProperty("targetOperatingSystem").equals(OS_WINDOWS)) {
	    // create windows batch file
	    commandsToServeAllureReport = Arrays.asList("@echo off", "set path=target\\allure\\bin;%path%",
		    "allure serve " + allureResultsFolderPath.substring(0, allureResultsFolderPath.length() - 1),
		    "pause", "exit");
	    FileActions.writeToFile("", "generate_allure_report.bat", commandsToServeAllureReport);
	} else if (!(new File("generate_allure_report.sh").exists())
		&& !System.getProperty("targetOperatingSystem").equals(OS_WINDOWS)) {
	    // create unix-based sh file
	    commandsToServeAllureReport = Arrays
		    .asList("#!/bin/bash", "parent_path=$( cd \"$(dirname \"${BASH_SOURCE[0]}\")\" ; pwd -P )",
			    "cd \"$parent_path/target/allure/bin/\"",
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
	if (System.getProperty("executionAddress").trim().equals("local")) {
	    setDiscreteLogging(true);
	    cleanAllureResultsDirectory();
	    extractAllureBinariesFromJarFile();
	    writeGenerateReportShellFilesToProjectDirectory();
	}
	writeEnvironmentVariablesToAllureResultsDirectory();
	setDiscreteLogging(discreteLoggingState);
    }

    public static void logEngineVersion() {
	createImportantReportEntry(
		"Detected SHAFT Engine Version: [" + System.getProperty(SHAFT_ENGINE_VERSION_PROPERTY_NAME) + "]");
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

    public static void logConfigurationMethodInformation(String className, String testMethodName) {
	clearTestLog();
	createImportantReportEntry("Starting Execution of a Configuration (Setup or Teardown) Method\nTest Method:\t\t["
		+ className + "." + testMethodName + "]");
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
	if (t.getMessage() != null) {
	    ReportManager.log("An Exception Occured with this Message: "
		    + t.getMessage().replace(System.lineSeparator(), " ").trim() + ".");
	}
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
	    createReportEntry(
		    "Successfully created attachment [" + "SHAFT Engine Logs" + " - " + "Execution log" + "]");
	    createImportantReportEntry("This test run was powered by SHAFT Engine Version: ["
		    + System.getProperty(SHAFT_ENGINE_VERSION_PROPERTY_NAME) + "]" + System.lineSeparator()
		    + "SHAFT Engine is licensed under the MIT License: [https://github.com/MohabMohie/SHAFT_ENGINE/blob/master/LICENSE].");
	    createAttachment("SHAFT Engine Logs", "Execution log", new StringInputStream(fullLog.trim()));
	}
    }

    public static void attachIssuesLog() {
	prepareIssuesLog();
	if (!issuesLog.trim().equals("")) {
	    createAttachment("SHAFT Engine Logs", "Issues log", new StringInputStream(issuesLog.trim()));
	}
    }

    private static void writeOpenReportShellFilesToGeneratedDirectory() {
	List<String> commandsToOpenAllureReport = null;
	// create unix-based sh file
	commandsToOpenAllureReport = Arrays.asList("#!/bin/bash",
		"parent_path=$( cd \"$(dirname \"${BASH_SOURCE[0]}\")\" ; pwd -P )", "cd \"$parent_path/allure/bin/\"",
		"bash allure open \"$parent_path/allure-report\"", "exit");
	FileActions.writeToFile("generatedReport/", "open_allure_report.sh", commandsToOpenAllureReport);

	// create windows batch file
	commandsToOpenAllureReport = Arrays.asList("@echo off", "set path=allure\\bin;%path%",
		"allure open allure-report", "pause", "exit");
	FileActions.writeToFile("generatedReport/", "open_allure_report.bat", commandsToOpenAllureReport);

    }

    private static void writeAllureReportToGeneratedDirectory() {
	// add correct file extension based on target OS
	String targetOperatingSystem = System.getProperty("targetOperatingSystem");
	String commandToCreateAllureReport = "";

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
	FileActions.zipFiles("generatedReport/", "generatedReport.zip");
	FileActions.deleteFile("generatedReport/");
    }

    public static void generateAllureReportArchive() {
	if (Boolean.valueOf(System.getProperty("automaticallyGenerateAllureReport").trim())
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
}