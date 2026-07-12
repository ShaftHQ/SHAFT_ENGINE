package com.shaft.tools.io;

import io.qameta.allure.model.Status;
import org.apache.logging.log4j.Level;

import static com.shaft.tools.io.internal.ReportManagerHelper.*;

/**
 * Public facade for SHAFT's reporting and logging subsystem.
 *
 * <p>Provides static methods to emit log entries and report steps during
 * test execution. Messages logged via {@link #log(String)} appear as
 * visible steps in the Allure execution report, while messages logged
 * via {@link #logDiscrete(String)} are recorded only in the execution
 * log file.
 *
 * <p>This is a utility class and cannot be instantiated.
 *
 * @see com.shaft.driver.SHAFT.Report
 * @see <a href="https://shafthq.github.io/">SHAFT User Guide</a>
 */
public class ReportManager {

    private ReportManager() {
        throw new IllegalStateException("Utility class");
    }

    /**
     * Creates a custom log entry that will also be added as a step in the execution report
     *
     * @param logText the text that will be logged by action
     */
    public static void log(String logText) {
        if (logText != null && !logText.isBlank()) {
            if (getDiscreteLogging() && isInternalStep()) {
                createLogEntry(logText, Level.INFO);
            } else {
                writeStepToReport(logText);
            }
        }
    }

    /**
     * Creates a custom log entry at the specified log level that will also be added as a step in the execution report
     *
     * @param logText  the text that will be logged by action
     * @param logLevel the log level to use (e.g., Level.ERROR, Level.WARN, Level.INFO)
     */
    public static void log(String logText, Level logLevel) {
        if (logText != null && !logText.isBlank()) {
            if (getDiscreteLogging() && isInternalStep()) {
                createLogEntry(logText, logLevel);
            } else {
                writeStepToReport(logText, logLevel);
            }
        }
    }

    /**
     * Creates a custom log entry that will also be added as a step in the execution report,
     * rendered with the exact provided status instead of inferring it from the log text.
     *
     * <p>Failed and broken steps are always visible in the execution report, even while
     * discrete logging is enabled, and are logged to the console at ERROR level.
     *
     * @param logText    the text that will be logged by action
     * @param stepStatus the exact status to render for this step (e.g., {@link Status#PASSED}, {@link Status#FAILED})
     */
    public static void log(String logText, Status stepStatus) {
        if (logText != null && !logText.isBlank()) {
            boolean isFailure = Status.FAILED.equals(stepStatus) || Status.BROKEN.equals(stepStatus);
            if (!isFailure && getDiscreteLogging() && isInternalStep()) {
                createLogEntry(logText, Level.INFO);
            } else {
                writeStepToReport(logText, isFailure ? Level.ERROR : Level.INFO, stepStatus);
            }
        }
    }

    /**
     * Creates a custom log entry that will not be added as a step in the execution report, but you can see it in the attached execution log txt file
     *
     * @param logText the text that will be logged by action
     */
    public static void logDiscrete(String logText) {
        logDiscrete(logText, isInternalStep() ? Level.DEBUG : Level.INFO);
    }

    /**
     * Creates a custom log entry at the specified log level that will not appear
     * as a step in the execution report, but is recorded in the attached execution
     * log text file.
     *
     * @param logText  the text that will be logged
     * @param logLevel the log level to use (e.g., {@link Level#INFO}, {@link Level#DEBUG})
     */
    public static void logDiscrete(String logText, Level logLevel) {
        createLogEntry(logText, logLevel);
    }

}
