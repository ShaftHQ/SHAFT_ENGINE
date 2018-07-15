package com.shaftEngine.ioActionLibrary;

import java.io.InputStream;
import java.text.SimpleDateFormat;
import java.util.Date;
import org.testng.Reporter;
import io.qameta.allure.Allure;
import io.qameta.allure.Attachment;
import io.qameta.allure.Step;

public class ReportManager {

	private static String fullLog = "";
	private static int actionCounter = 1;
	private static boolean isHeaderTyped = false;

	// private static int attachementCounter = 1;
	// TODO : implement attachemntCounter for both attachement functions

	/**
	 * Manages action counter and calls writeLog to format and print the log entry.
	 * 
	 * @param logText
	 *            the text that needs to be logged in this action
	 */
	public static void log(String logText) {
		if (!isHeaderTyped) {
			isHeaderTyped = true;
			log("Detected SHAFT Engine Version: [" + System.getProperty("shaftEngineVersion")
					+ "] © Copyrights Reserved to [Mohab Mohie].");
			// calls parent log function recursively in order to log shaft version before
			// performing the first action
		}
		writeLog(logText, actionCounter);
		actionCounter++;
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
	public static void writeLog(String logText, int actionCounter) {
		performLogEntry(logText);
		actionCounter++;
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
		Allure.addAttachment(attachmentName, attachmentContent);
		performLogEntry("Successfully created attachment [" + attachmentType + " - " + attachmentName + "]");
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
		Allure.addAttachment(attachmentName, attachmentContent);
		performLogEntry("Successfully created attachment [" + attachmentName + "]");
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
	 * @return the full execution log of the whole run
	 */
	@Attachment("Full log")
	public static String getFullLog() {
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
}