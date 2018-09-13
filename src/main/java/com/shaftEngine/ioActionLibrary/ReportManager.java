package com.shaftEngine.ioActionLibrary;

import java.io.InputStream;
import java.text.SimpleDateFormat;
import java.util.Arrays;
import java.util.Date;
import java.util.List;

import org.testng.Reporter;
import io.qameta.allure.Allure;
import io.qameta.allure.Attachment;
import io.qameta.allure.Step;
import com.shaftEngine.supportActionLibrary.SSHActions;
import com.shaftEngine.ioActionLibrary.FileManager;

public class ReportManager {

	private static String fullLog = "";
	private static int actionCounter = 1;
	private static boolean isHeaderTyped = false;
	private static boolean log = true;
	private static String lastTestCase = "";

	private ReportManager() {
		throw new IllegalStateException("Utility class");
	}

	// private static int attachmentCounter = 1;
	// TODO : implement attachemntCounter for both attachment functions

	/**
	 * Manages action counter and calls writeLog to format and print the log entry.
	 * 
	 * @param logText
	 *            the text that needs to be logged in this action
	 */
	public static void log(String logText) {
		logEngineVersionAndEnvironmentData();
		logTestInformation();

		if (log) {
			writeLogStepToReport(logText, actionCounter);
			actionCounter++;
		}
	}

	/**
	 *
	 * Format an exception message and stack trace, and calls attach to add it as a
	 * log entry.
	 * 
	 * @param e
	 *            the exception that will be logged in this action
	 */
	public static void log(Exception e) {
		logEngineVersionAndEnvironmentData();
		logTestInformation();

		if (log) {
			StringBuilder logBuilder = new StringBuilder();
			String logText = "";
			StackTraceElement[] trace = e.getStackTrace();

			logBuilder.append(e.getMessage() + System.lineSeparator());

			for (int i = 0; i < trace.length; ++i) {
				logBuilder.append(trace[i].toString() + System.lineSeparator());
			}
			logText = logBuilder.toString();

			attach("Exception Stack Trace for [" + e.toString().substring(0, 30) + "...]", logText);
			actionCounter++;
		}
	}

	/**
	 * Formats logText and adds timestamp, then logs it as a step in the execution
	 * report.
	 * 
	 * @param logText
	 *            the text that needs to be logged in this action
	 * @param actionCounter
	 *            a number that represents the serial number of this action within
	 *            this test run
	 */
	@Step("Action [{actionCounter}]: {logText}")
	private static void writeLogStepToReport(String logText, int actionCounter) {
		performLogEntry(logText);
	}

	private static void performLogEntry(String logText) {
		String timestamp = (new SimpleDateFormat("dd-MM-yyyy HH:mm:ss.SSSS aaa"))
				.format(new Date(System.currentTimeMillis()));

		String log = "[ReportManager] " + logText.trim() + " @" + timestamp + System.lineSeparator();
		Reporter.log(log, true);
		appendToFullLog(log);
	}

	/**
	 * Adds a new attachment using the input parameters provided. The attachment is
	 * displayed as a step in the execution report. Used for Screenshots.
	 * 
	 * @param attachmentType
	 *            the type of this attachment
	 * @param attachmentName
	 *            the name of this attachment
	 * @param attachmentContent
	 *            the content of this attachment
	 */
	@Step("Attachement: {attachmentType} - {attachmentName}")
	public static void attach(String attachmentType, String attachmentName, InputStream attachmentContent) {
		if (log) {
			Allure.addAttachment(attachmentName, attachmentContent);
			performLogEntry("Successfully created attachment [" + attachmentType + " - " + attachmentName + "]");
		}
	}

	/**
	 * Adds a new attachment using the input parameters provided. The attachment is
	 * displayed as a step in the execution report. Used for REST API Responses.
	 * 
	 * @param attachmentName
	 *            the name of this attachment
	 * @param attachmentContent
	 *            the content of this attachment
	 */
	@Step("Attachment: {attachmentName}")
	public static void attach(String attachmentName, String attachmentContent) {
		if (log) {
			Allure.addAttachment(attachmentName, attachmentContent);
			performLogEntry("Successfully created attachment [" + attachmentName + "]");
		}
	}

	/**
	 * Returns the log of the current test, and attaches it in the end of the test
	 * execution report.
	 * 
	 * @return the log for the current test
	 */
	@Attachment("Test log")
	public static String getTestLog() {
		String log = "";

		for (String s : Reporter.getOutput()) {
			s = s.replace("<br>", System.lineSeparator());
			log = log + s;
		}
		Reporter.clear();
		return log;
	}

	/**
	 * Returns the complete log of the current execution session, and attaches it in
	 * the end of the test execution report.
	 * 
	 */
	public static void getFullLog() {
		attachFullLog();
		if (Boolean.valueOf(System.getProperty("automaticallyGenerateAllureReport").trim())) {
			generateAllureReportArchive();
		}
	}

	@Attachment("Full log")
	private static String attachFullLog() {
		return fullLog;
	}

	/**
	 * Appends a log entry to the complete log of the current execution session.
	 * 
	 * @param log
	 *            the log entry that needs to be appended to the full log
	 */
	private static void appendToFullLog(String log) {
		fullLog += log;
	}

	private static void generateAllureReportArchive() {
		log("Generating Allure Report Archive...");
		log = false;
		List<String> commandToCreateAllureReport = Arrays.asList(
				"src/main/resources/allure/bin/allure generate \"allure-results\" -o \"generatedReport/allure-report\"");
		SSHActions.executeShellCommand(commandToCreateAllureReport);

		FileManager.copyFolder(FileManager.getAbsolutePath("src/main/resources/", "allure"), "generatedReport/allure");

		// "src/main/resources/allure/bin/allure open \"allure-report\""

		List<String> commandToOpenAllureReport = Arrays.asList("#!/bin/bash",
				"parent_path=$( cd \"$(dirname \"${BASH_SOURCE[0]}\")\" ; pwd -P )", "cd \"$parent_path/allure/bin/\"",
				"bash allure open \"$parent_path/allure-report\"", "exit");
		FileManager.writeToFile("generatedReport/", "open_allure_report.sh", commandToOpenAllureReport);

		FileManager.zipFiles("generatedReport/", "generatedReport.zip");

		FileManager.deleteFile("generatedReport/");
		log = true;
		log("This test run was powered by SHAFT Engine Version: [" + System.getProperty("shaftEngineVersion")
				+ "] © Copyrights Reserved to [Mohab Mohie].");
	}

	private static void populateEnvironmentData() {
		// sets up some parameters to the allure report
		FileManager.writeToFile(System.getProperty("allureResultsFolderPath"), "environment.properties",
				Arrays.asList("Engine=" + System.getProperty("shaftEngineVersion"),
						"OS=" + System.getProperty("targetOperatingSystem"),
						"Browser=" + System.getProperty("targetBrowserName"),
						"Location=" + System.getProperty("executionAddress")));
		/*
		 * "Flags_AutoMaximizeBrowserWindow=" +
		 * System.getProperty("autoMaximizeBrowserWindow"), "Flags_ScreenshotManager=" +
		 * System.getProperty("screenshotParams_whenToTakeAScreenshot"),
		 * "Config_DefaultElementIdentificationTimeout=" +
		 * System.getProperty("defaultElementIdentificationTimeout")));
		 */
	}

	private static void logEngineVersion() {
		log("Detected SHAFT Engine Version: [" + System.getProperty("shaftEngineVersion")
				+ "] © Copyrights Reserved to [Mohab Mohie].");
		// calls parent log function recursively in order to log shaft version before
		// performing the first action
	}

	private static void logEngineVersionAndEnvironmentData() {
		if (!isHeaderTyped) {
			isHeaderTyped = true;

			logEngineVersion();
			populateEnvironmentData();
		}
	}

	private static void logTestInformation() {
		try {
			String currentTestCase = "";

			if (Reporter.getCurrentTestResult().getMethod().getDescription() != null) {
				currentTestCase = Reporter.getCurrentTestResult().getMethod().getDescription();
			} else {
				currentTestCase = Reporter.getCurrentTestResult().getMethod().getMethodName();
			}

			if ((!currentTestCase.equals(lastTestCase)) && (!currentTestCase.equals(""))) {
				String testClass = Reporter.getCurrentTestResult().getTestClass().getName();
				performLogEntry(
						"Starting Execution; class name [" + testClass + "], and test name [" + currentTestCase + "].");
				lastTestCase = currentTestCase;
			}
		} catch (Throwable e) {
			// several errors are thrown in case of calling this method outside a test case,
			// like in browser setup or teardown, therefor the correct response here is to
			// do nothing
			// this method gets called with every single action
		}
	}

}